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

-record(waiter, {
          create_time = ?NOW_SECS   :: integer(),
          from                      :: pid() | bitstring()
         }).

-record(state, {
          role              :: master | slaver,
          slaver_name       :: bitstring() | undefined,
          cookie            :: bitstring(),
          sup               :: supervisor:sup_ref() | undefined,
          protocol_module   :: module(),
          server_module     :: module(),
          server_args       :: any(),
          server_state      :: any(),
          socket            :: ptnode_proto:socket() | undefined,
          req_id = 0        :: integer(),
          waiters = #{}     :: map(),
          status = wait     :: wait | ready
         }).

-define(RECONN_WAIT_TIME,
        (?MASTER_SUP_RESTART_PERIOD div
         ?MASTER_SUP_RESTART_INTENSITY + 1) * 1000).

-define(HEARTBEAT_INTENSITY, 5000).

-define(mark(ReqId), {self(), ReqId}).


%% behaviour callback
-callback init(Args::any()) -> {ok, State::any()} | {stop, Reason::any()}.

-callback handle_call(Req::any(), From::tuple(), ServState::any()) ->
    {reply, Reply::any(), NewServState::any()} |
    {stop, Reason::any(), Reply::any(), NewServState::any()}.

-callback handle_cast(Req::any(), ServState::any()) ->
    {noreply, NewServState::any()} |
    {stop, Reason::any(), NewServState::any()}.

-callback terminate(Reason::any(), ServState::any()) -> any().
%%


next_req_id(ID) when ID =:= ?PROTO_MAX_REQ_ID -> 0;
next_req_id(ID) -> ID + 1.


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
            cookie = maps:get(cookie, NodeOpts),
            sup = MasterSupRef,
            protocol_module = maps:get(module, ProtoOpts),
            server_module = maps:get(module, ServSpec),
            server_args = maps:get(init_args, ServSpec),
            socket = Socket
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
            slaver_name = ?a2b(Name),
            cookie = Cookie,
            sup = SupRef,
            protocol_module = ProtoModule,
            server_args = ServArgs,
            server_module = ServModule
           }}.


handle_call({'$reply_request', Req}, {Pid, _},
            State = #state{
                       req_id = ReqId,
                       waiters = Waiters,
                       socket = Socket,
                       protocol_module = ProtoModule,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_reply_request(ReqId, Req),
    case ProtoModule:send(Socket, Cmd) of
        ok ->
            NewWaiters = maps:put(ReqId, #waiter{
                                            from = Pid
                                           }, Waiters),
            {reply, {ok, ?mark(ReqId)},
             State#state{
               req_id = next_req_id(ReqId),
               waiters = NewWaiters
              }};
        Err = {error, _} -> {stop, Err, State}
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

handle_cast({'$serv_cast', Req}, State) ->
    serv_handle_cast(Req, State);

handle_cast({'$send', Data},
            State = #state{
                       role = master,
                       protocol_module = ProtoModule,
                       socket = Socket,
                       status = ready
                      }) ->
    case ProtoModule:send(Socket, Data) of
        ok -> {noreply, State};
        Err = {error, _} -> {stop, Err, State}
    end;

handle_cast({'$noreply_request', Req},
            State = #state{
                       role = master,
                       protocol_module = ProtoModule,
                       socket = Socket,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_noreply_request(Req),
    case ProtoModule:send(Socket, Cmd) of
        ok -> {noreply, State};
        Err = {error, _} -> {stop, Err, State}
    end;

handle_cast({'$noreply_request', To, Req},
            State = #state{
                       role = slaver,
                       slaver_name = To,
                       status = ready
                      }) ->
    serv_handle_cast(Req, State);
handle_cast({'$noreply_request', To, Req},
            State = #state{
                       role = slaver,
                       protocol_module = ProtoModule,
                       socket = Socket,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_noreply_request(To, Req),
    case ProtoModule:send(Socket, Cmd) of
        ok -> {noreply, State};
        Err = {error, _} -> {stop, Err, State}
    end;

handle_cast(Req, State) ->
    ?dlog("~p~n", [Req]),
    ?dlog("~p~n", [State]),
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
            ?dlog("close: ~p~n", [Reason]),
            {noreply, Reason, State};
        close ->
            {stop, normal, State}
    end.


handle_reply(Req, From,
             State = #state{
                        socket = Socket,
                        protocol_module = ProtoModule,
                        server_module = ServModule,
                        server_state = ServState
                       }) ->
    case ServModule:handle_call(Req, From, ServState) of
        {ok, Re, Data, NewServState} ->
            case ProtoModule:send(Socket, Data) of
                ok -> {reply, Re, State#state{server_state = NewServState}};
                Err = {error, _} ->
                    {stop, Err, State#state{server_state = NewServState}}
            end;
        {ok, Re, NewServState} ->
            {reply, Re, State#state{server_state = NewServState}};
        {stop, Reason, Re, Data, NewServState} ->
            ProtoModule:send(Socket,  Data),
            {stop, Reason, Re, State#state{server_state = NewServState}};
        {stop, Reason, Re, NewServState} ->
            {stop, Reason, Re, State#state{server_state = NewServState}}
    end.


handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_HEARTBEAT:8/unsigned-little
            >>, State) ->
    {noreply, State};

handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_REG:8/unsigned-little,
              NameLen:8/unsigned-little,
              Name:NameLen/binary,
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
                   case ptnode_master_sup:register_slaver(
                          MasterSupRef, Name, self()) of
                       ok ->
                           serv_init(State#state{
                                       slaver_name = Name,
                                       status = ready
                                      });
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
               {ok, _} -> serv_init(State#state{status = ready});
               Err = {error, _} -> {stop, Err, State}
           end;
       true ->
           {stop, {error, "register fail"}, State}
    end;

handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_NOREPLY_REQUEST:8/unsigned-little,
              ToLen:8/unsigned-little,
              To:ToLen/binary,
              BLen:?PROTO_TERM_LEN_BITS/unsigned-little,
              B:BLen/binary
            >>,
            State = #state{
                       role = master,
                       sup = MasterSupRef,
                       status = ready
                      }) ->
    case ptnode_master_sup:get_node_conn(MasterSupRef, To) of
        master ->
            case catch binary_to_term(B) of
                {'EXIT', _} -> do_nothing;
                Term -> ?dlog("~p~n", [Term])
            end;
        Pid when is_pid(Pid) ->
            gen_server:cast(
              Pid, {'$send',
                    ptnode_conn_proto:wrap_noreply_request_binary(B)});
        {error, _} -> ok
    end,
    {noreply, State};

handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_NOREPLY_REQUEST:8/unsigned-little,
              NameLen:8/unsigned-little,
              Name:NameLen/binary,
              BLen:?PROTO_TERM_LEN_BITS/unsigned-little,
              B:BLen/binary
            >>,
            State = #state{
                       role = slaver,
                       slaver_name = Name,
                       status = ready
                      }) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term -> serv_handle_cast(Term, State)
    end;

handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_MS_NOREPLY_REQUEST:8/unsigned-little,
              BLen:?PROTO_TERM_LEN_BITS/unsigned-little,
              B:BLen/binary
            >>,
            State = #state{status = ready}) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term -> serv_handle_cast(Term, State)
    end;

handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_MS_REPLY_REQUEST:8/unsigned-little,
              ReqId:32/unsigned-little,
              BLen:?PROTO_TERM_LEN_BITS/unsigned-little,
              B:BLen/binary
            >>,
            State = #state{status = ready}) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term -> serv_handle_call(ReqId, Term, undefined, State)
    end;

handle_data(<<?PROTO_VERSION:8/unsigned-little,
              ?PROTO_CMD_REPLY_REPLY:8/unsigned-little,
              ReqId:32/unsigned-little,
              BLen:?PROTO_TERM_LEN_BITS/unsigned-little,
              B:BLen/binary
            >>,
            State = #state{
                       waiters = Waiters,
                       status = ready
                      }) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term ->
            case maps:find(ReqId, Waiters) of
                error -> {noreply, State};
                {ok, #waiter{
                        from = From
                       }} when is_pid(From) ->
                    NewWaiters = maps:remove(ReqId, Waiters),
                    From ! {?mark(ReqId), Term},
                    {noreply, State#state{waiters = NewWaiters}}
            end
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
                     server_state = ServState
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


serv_init(State = #state{
                     protocol_module = ProtoModule,
                     socket = Socket,
                     server_module = ServModule,
                     server_args = ServArgs
                    }) ->
    ?dlog("~p~n", [ProtoModule:peername(Socket)]),
    case ServModule:init(ServArgs) of
        {ok, ServState} -> {noreply, State#state{server_state = ServState}};
        {stop, Reason} -> {stop, Reason, State}
    end.


serv_handle_cast(Req, State = #state{
                                 server_module = ServModule,
                                 server_state = ServState
                                }) ->
    case ServModule:handle_cast(Req, ServState) of
        {noreply, NewServState} ->
            {noreply, State#state{server_state = NewServState}};
        {stop, Reason, NewServState} ->
            {stop, Reason, State#state{server_state = NewServState}}
    end.


serv_handle_call(ReqId, Req, From,
                 State = #state{
                            protocol_module = ProtoModule,
                            socket = Socket,
                            server_module = ServModule,
                            server_state = ServState
                           }) ->
    case ServModule:handle_call(Req, From, ServState) of
        {reply, Reply, NewServState} ->
            Cmd = ptnode_conn_proto:wrap_reply_request_reply(ReqId, Reply),
            case ProtoModule:send(Socket, Cmd) of
                ok -> {noreply, State#state{server_state = NewServState}};
                Err = {error, _} ->
                    {stop, Err, State#state{server_state = NewServState}}
            end;
        {stop, Reason, NewServState} ->
            {stop, Reason, State#state{server_state = NewServState}}
    end.


cast(Ref, Req) -> gen_server:cast(Ref, {'$serv_cast', Req}).


call(Ref, Req) -> gen_server:call(Ref, {'$serv_call', Req}).


close_conn(Ref) -> gen_server:call(Ref, '$stop', 5 * 1000).
