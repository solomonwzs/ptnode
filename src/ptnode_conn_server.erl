%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_conn_server).

-include("ptnode.hrl").
-include("ptnode_conn_proto.hrl").

-behaviour(gen_server).

-export([start_master_conn_link/4]).
-export([start_slaver_conn_link/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         code_change/3,
         format_status/2,
         terminate/2
        ]).
-export([cast/2, call/2]).
-export([close_conn/1]).

-record(state, {
          role              :: master | slaver,
          slaver_name       :: atom(),
          cookie            :: bitstring(),
          sup               :: supervisor:sup_ref() | undefined,
          protocol_module   :: module(),
          serv_state        :: any(),
          server_args       :: any(),
          server_module     :: module(),
          socket            :: ptnode_proto:socket() | undefined,
          status            :: wait | ready
         }).

-define(RECONN_WAIT_TIME,
        (?MASTER_SUP_RESTART_PERIOD div
         ?MASTER_SUP_RESTART_INTENSITY + 1) * 1000).

-define(HEARTBEAT_INTENSITY, 5000).


%% behaviour callback
-callback init(Args::any()) -> {ok, State::any()} | {error, Reason::any()}.

-callback handle_call(Req::any(), From::tuple(), ServState::any()) ->
    {ok, Re::any(), NewServState::any()} |
    {ok, Data::iodata(), Re::any(), NewServState::any()} |
    {stop, Reason::any(), Data::iodata(), Re::any(), NewServState::any()} |
    {stop, Reason::any(), Re::any(), NewServState::any()}.

-callback handle_cast(Req::any(), ServState::any()) ->
    {ok, NewServState::any()} |
    {ok, Data::iodata(), NewServState::any()} |
    {stop, Reason::any(), Data::iodata(), NewServState::any()} |
    {stop, Reason::any(), NewServState::any()}.

-callback handle_data(Data::any(), ServState::any()) ->
    {ok, NewServState::any()} |
    {ok, Data::iodata(), NewServState::any()} |
    {stop, Reason::any(), Data::iodata(), NewServState::any()} |
    {stop, Reason::any(), NewServState::any()}.

-callback terminate(Reason::any(), ServState::any()) -> any().
%%


-spec(start_master_conn_link(ptnode:node_opts(), ptnode:proto_opts(),
                             ptnode:serv_spec(), any())
      -> pid() | {error, any()}).
start_master_conn_link(NodeOpts, ProtoOpts, ServSpec, ExtArgs) ->
    gen_server:start_link(
      ?MODULE, [master, NodeOpts, ProtoOpts, ServSpec, ExtArgs], []).


-spec(start_slaver_conn_link(ptnode:node_opts(), ptnode:proto_opts(),
                             ptnode:serv_spec(), pid())
      -> pid() | {error, any()}).
start_slaver_conn_link(NodeOpts, ProtoOpts, ServSpec, SupRef) ->
    gen_server:start_link(
      ?MODULE, [slaver, NodeOpts, ProtoOpts, ServSpec, SupRef], []).


init([master, NodeOpts, ProtoOpts, ServSpec,
      {MasterSupRef, Socket}]) ->
    timer:apply_after(?PROTO_REG_TIMEOUT,
                      gen_server, cast, [self(), '$reg_timeout']),
    {ok, #state{
            role = master,
            slaver_name = undefined,
            cookie = maps:get(cookie, NodeOpts),
            sup = MasterSupRef,
            protocol_module = maps:get(module, ProtoOpts),
            serv_state = undefined,
            server_module = maps:get(module, ServSpec),
            server_args = maps:get(init_args, ServSpec),
            socket = Socket,
            status = wait
           }};
init([slaver, NodeOpts, ProtoOpts, ServSpec, SupRef]) ->
    ProtoModule = maps:get(module, ProtoOpts),
    Host = maps:get(connect_host, ProtoOpts),
    Port = maps:get(connect_port, ProtoOpts),
    Opts = maps:get(connect_opts, ProtoOpts),
    Timeout = maps:get(timeout, ProtoOpts),

    Name = maps:get(name, NodeOpts),
    Cookie = maps:get(cookie, NodeOpts),

    ServModule = maps:get(module, ServSpec),
    ServArgs = maps:get(init_args, ServSpec),

    ConnReq = {'$conn_master', Host, Port, Opts, Timeout},
    gen_server:cast(self(), ConnReq),

    {ok, #state{
            role = slaver,
            slaver_name = Name,
            cookie = Cookie,
            sup = SupRef,
            protocol_module = ProtoModule,
            serv_state = undefined,
            server_args = ServArgs,
            server_module = ServModule,
            socket = undefined,
            status = wait
           }}.


handle_call('$init_serv', _From,
            State = #state{
                       role = master,
                       server_module = ServModule,
                       server_args = ServArgs,
                       protocol_module = ProtoModule,
                       socket = Socket}) ->
    case ServModule:init(ServArgs) of
        {ok, ServState} ->
            case ProtoModule:setopts(Socket, [{active, true}]) of
                ok -> {reply, ok, State#state{serv_state = ServState}};
                Err = {error, _} -> {reply, Err, State}
            end;
        Err = {error, _} -> {reply, Err, State}
    end;
handle_call('$stop', _From, State) ->
    {stop, normal, ok, State};
handle_call({'$serv_call', Req}, From, State) ->
    handle_reply(Req, From, State);
handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast('$heartbeat', State = #state{
                                     protocol_module = ProtoModule,
                                     socket = Socket,
                                     status = ready
                                    }) ->
    Cmd = ptnode_conn_proto:wrap_heartbeat_cmd(),
    case ProtoModule:send(Socket, Cmd) of
        ok -> {noreply, State};
        Err = {error, _} -> {stop, Err, State}
    end;
handle_cast('$reg_timeout', State = #state{status = wait}) ->
    {stop, {error, "register timeout"}, State};
handle_cast({'$conn_master', Host, Port, ConnectOpts, Timeout},
            State = #state{
                       role = slaver,
                       protocol_module = ProtoModule,
                       slaver_name = Name,
                       cookie = Cookie
                      }) ->
    case ProtoModule:connect(Host, Port, ConnectOpts, Timeout) of
        {ok, Socket} ->
            NewState = State#state{socket = Socket},
            Cmd = ptnode_conn_proto:wrap_register_cmd(Name, Cookie),
            case ProtoModule:send(Socket, Cmd) of
                ok ->
                    timer:apply_after(?PROTO_REG_TIMEOUT,
                                      gen_server, cast,
                                      [self(), '$reg_timeout']),
                    {noreply, NewState};
                Err = {error, _} ->
                    timer:sleep(?RECONN_WAIT_TIME),
                    {stop, Err, NewState}
            end;
        Err = {error, _} ->
            timer:sleep(?RECONN_WAIT_TIME),
            {stop, Err, State}
    end;
% handle_cast({'$serv_cast', Req}, State) ->
%     handle_noreply(handle_cast, Req, State);
handle_cast(_Req, State) ->
    {noreply, State}.


handle_info('$stop', State) ->
    {stop, normal, State};
handle_info(Message, State = #state{
                                protocol_module = ProtoModule
                               }) ->
    case ProtoModule:parse_message(Message) of
        {ok, Data} ->
            handle_data(Data, State);
        {error, Reason} ->
            ?dlog("~p~n", [Reason]),
            {noreply, Reason, State};
        close ->
            {stop, normal, State};
        _ ->
            {noreply, State}
    end.


handle_reply(Req, From,
             State = #state{
                        socket = Socket,
                        protocol_module = ProtoModule,
                        server_module = ServModule,
                        serv_state = ServState
                       }) ->
    case ServModule:handle_call(Req, From, ServState) of
        {ok, Re, Data, NewServState} ->
            case ProtoModule:send(Socket, Data) of
                ok -> {reply, Re, State#state{serv_state = NewServState}};
                Err = {error, _} ->
                    {stop, Err, State#state{serv_state = NewServState}}
            end;
        {ok, Re, NewServState} ->
            {reply, Re, State#state{serv_state = NewServState}};
        {stop, Reason, Re, Data, NewServState} ->
            ProtoModule:send(Socket,  Data),
            {stop, Reason, Re, State#state{serv_state = NewServState}};
        {stop, Reason, Re, NewServState} ->
            {stop, Reason, Re, State#state{serv_state = NewServState}}
    end.


handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_HEARTBEAT:8/unsigned-little
            >>, State) ->
    {noreply, State};
handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_REG:8/unsigned-little,
              NameLen:8/unsigned-little,
              BinName:NameLen/binary,
              CookieLen:8/unsigned-little,
              Cookie:CookieLen/binary
            >>,
            State = #state{
                       role = master,
                       sup = MasterSupRef,
                       protocol_module = ProtoModule,
                       socket = Socket,
                       cookie = MasterCookie,
                       status = wait
                      }) ->
    if MasterCookie =:= Cookie ->
           Cmd = ptnode_conn_proto:wrap_register_res_cmd(
                   ?PROTO_REG_RES_OK),
           case ProtoModule:send(Socket, Cmd) of
               ok ->
                   Name = binary_to_atom(BinName, utf8),
                   case ptnode_master_sup:register_slaver(
                          MasterSupRef, Name, self()) of
                       ok ->
                           {noreply, State#state{
                                       slaver_name = Name,
                                       status = ready
                                      }};
                       Err = {error, _} ->
                           {stop, Err, State}
                   end;
               Err = {error, _} -> {stop, Err, State}
           end;
       true ->
           Cmd = ptnode_conn_proto:wrap_register_res_cmd(
                   ?PROTO_REG_RES_ERR),
           Reason = case ProtoModule:send(Socket, Cmd) of
                        ok -> {error, "register error"};
                        Err = {error, _} -> Err
                    end,
           {stop, Reason, State}
    end;
handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_REG_RES:8/unsigned-little,
              ResCode:8/unsigned-little
            >>,
            State = #state{
                       role = slaver,
                       status = wait
                      }) ->
    if ResCode =:= ?PROTO_REG_RES_OK ->
           case timer:apply_interval(
                  ?HEARTBEAT_INTENSITY,
                  gen_server, cast, [self(), '$heartbeat']) of
               {ok, _} -> {noreply, State#state{status = ready}};
               Err = {error, _} -> {stop, Err, State}
           end;
       true ->
           {stop, {error, "register fail"}, State}
    end;
handle_data(_Data, State) ->
    {noreply, State}.


handle_continue(_Continue, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format_status(_Opt, [_PDict, _State]) ->
    ok.


terminate(Reason, #state{
                     role = Role,
                     sup = SupRef,
                     slaver_name = Name,
                     socket = Socket,
                     protocol_module = ProtoModule,
                     server_module = ServModule,
                     serv_state = ServState
                    }) ->
    ?dlog("~p~n", [Reason]),
    if Socket =/= undefined -> ProtoModule:close(Socket);
       true -> ok
    end,
    if Role =:= master andalso Name =/= undefined ->
           ptnode_master_sup:unregister_slaver(SupRef, Name);
       true -> ok
    end,
    ServModule:terminate(Reason, ServState),
    ok.


cast(Ref, Req) -> gen_server:cast(Ref, {'$serv_cast', Req}).


call(Ref, Req) -> gen_server:call(Ref, {'$serv_call', Req}).


close_conn(Ref) -> gen_server:call(Ref, '$stop', 5 * 1000).
