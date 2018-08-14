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
          master_name       :: bitstring() | undefined,
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

-define(node_name(State),
        if State#state.role =:= master -> State#state.master_name;
           true -> State#state.slaver_name
        end).

-define(next_req_id(Id),
        if Id =:= ?PROTO_MAX_REQ_ID -> 0;
           true -> Id + 1
        end).


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
            master_name = ?a2b(maps:get(name, NodeOpts)),
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
                       role = master,
                       master_name = Name,
                       req_id = ReqId,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_reply_request(ReqId, Name, Req),
    reply_request(ReqId, Cmd, Pid, State);

handle_call({'$reply_request', To, Req}, {Pid, _},
            State = #state{
                       role = slaver,
                       slaver_name = Name,
                       req_id = ReqId,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_reply_request(ReqId, Name, To, Req),
    reply_request(ReqId, Cmd, Pid, State);

handle_call('$stop', _From, State) ->
    {stop, normal, ok, State};
handle_call({'$serv_call', Req}, From, State) ->
    handle_reply(Req, From, State);
handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast('$heartbeat',
            State = #state{
                       protocol_module = ProtoModule,
                       socket = Socket,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_heartbeat(),
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
            Cmd = ptnode_conn_proto:wrap_register(Name, Cookie),
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
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_noreply_request(Req),
    noreply_request(Cmd, State);

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
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_noreply_request(To, Req),
    noreply_request(Cmd, State);

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


handle_data(?PROTO_P_HEARTBEAT, State) ->
    {noreply, State};

handle_data(?PROTO_P_REG(NameLen, Name, CookieLen, Cookie),
            State = #state{
                       role = master,
                       master_name = MasterName,
                       sup = MasterSupRef,
                       protocol_module = ProtoModule,
                       socket = Socket,
                       cookie = MasterCookie,
                       status = wait
                      }) ->
    if MasterCookie =:= Cookie ->
           Cmd = ptnode_conn_proto:wrap_register_res(MasterName),
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
       true -> {stop, {error, register_fail}, State}
    end;

handle_data(?PROTO_P_REG_RES(NameLen, Name),
            State = #state{
                       role = slaver,
                       status = wait
                      }) ->
    case timer:apply_interval(
           ?HEARTBEAT_INTENSITY,
           gen_server, cast, [self(), '$heartbeat']) of
        {ok, _} -> serv_init(State#state{
                               master_name = Name,
                               status = ready
                              });
        Err = {error, _} -> {stop, Err, State}
    end;

handle_data(?PROTO_P_NOREPLY_REQUEST(ToLen, To, BLen, B),
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

handle_data(?PROTO_P_NOREPLY_REQUEST(NameLen, Name, BLen, B),
            State = #state{
                       role = slaver,
                       slaver_name = Name,
                       status = ready
                      }) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term -> serv_handle_cast(Term, State)
    end;

handle_data(?PROTO_P_MS_NOREPLY_REQUEST(BLen, B),
            State = #state{status = ready}) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term -> serv_handle_cast(Term, State)
    end;

handle_data(?PROTO_P_MS_REPLY_REQUEST(ReqId, FromLen, From, BLen, B),
            State = #state{status = ready}) ->
    case catch binary_to_term(B) of
        {'EXIT', _} -> {noreply, State};
        Term -> serv_handle_call(ReqId, Term, ?b2a(From), State)
    end;

handle_data(?PROTO_P_MS_REPLY_REPLY(ReqId, BLen, B),
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

handle_data(Data, State) ->
    ?dlog("~p~n", [Data]),
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


serv_handle_cast(Req,
                 State = #state{
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


reply_request(ReqId, Cmd, Pid,
              State = #state{
                         waiters = Waiters,
                         socket = Socket,
                         protocol_module = ProtoModule
                        }) ->
    case ProtoModule:send(Socket, Cmd) of
        ok ->
            NewWaiters = maps:put(ReqId, #waiter{
                                            from = Pid
                                           }, Waiters),
            {reply, {ok, ?mark(ReqId)},
             State#state{
               req_id = ?next_req_id(ReqId),
               waiters = NewWaiters
              }};
        Err = {error, _} -> {stop, Err, Err, State}
    end.


noreply_request(Cmd,
                State = #state{
                           protocol_module = ProtoModule,
                           socket = Socket
                          }) ->
    case ProtoModule:send(Socket, Cmd) of
        ok -> {noreply, State};
        Err = {error, _} -> {stop, Err, State}
    end.


cast(Ref, Req) -> gen_server:cast(Ref, {'$serv_cast', Req}).


call(Ref, Req) -> gen_server:call(Ref, {'$serv_call', Req}).


close_conn(Ref) -> gen_server:call(Ref, '$stop', 5 * 1000).
