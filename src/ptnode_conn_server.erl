%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_conn_server).

-include("ptnode.hrl").
-include("ptnode_conn_proto.hrl").
-include("ptnode_conn_server.hrl").

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
-export([cast/2,
         call/2,
         close_conn/1,
         noreply_request/2,
         noreply_request/3,
         reply_request/3,
         reply_request/4
        ]).

-record(waiter, {
          create_time = ?NOW_MILL_SECS  :: integer(),
          from                          :: pid()
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
          waiter_timeout    :: pos_integer(),
          status = wait     :: wait | ready
         }).

-define(RECONN_WAIT_TIME,
        (?MASTER_SUP_RESTART_PERIOD div
         ?MASTER_SUP_RESTART_INTENSITY + 1) * 1000).

-define(HEARTBEAT_INTERVAL, 10000).
-define(CLEAN_WAITERS_INTERVAL, 5000).

-define(WAITER_TIMEOUT, 5000).

-define(MARK(ReqId), {self(), ReqId}).

-define(NODE_NAME(State),
        if State#state.role =:= master -> State#state.master_name;
           true -> State#state.slaver_name
        end).

-define(NEXT_REQ_ID(Id),
        if Id =:= ?PROTO_MAX_REQ_ID -> 0;
           true -> Id + 1
        end).

-type mark() :: {pid(), integer()}.
-export_type([mark/0]).


%% behaviour callback
-callback init(Args::any()) -> {ok, State::any()} | {stop, Reason::any()}.

-callback handle_call(Req::any(), From::ptnode:serv_ref(),
                      ServState::any()) ->
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
                      gen_server, cast, [self(), ?MSG_REG_TIMEOUT]),
    {ok, #state{
            role = master,
            master_name = ?A2B(maps:get(name, NodeOpts)),
            cookie = maps:get(cookie, NodeOpts),
            sup = MasterSupRef,
            protocol_module = maps:get(module, ProtoOpts),
            server_module = maps:get(module, ServSpec),
            server_args = maps:get(init_args, ServSpec),
            waiter_timeout = maps:get(request_timeout, NodeOpts,
                                      ?WAITER_TIMEOUT),
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

    ConnReq = ?MSG_CONN_MASTER(Host, Port, Opts, Timeout),
    gen_server:cast(self(), ConnReq),

    {ok, #state{
            role = slaver,
            slaver_name = ?A2B(Name),
            cookie = Cookie,
            sup = SupRef,
            protocol_module = ProtoModule,
            server_args = ServArgs,
            server_module = ServModule,
            waiter_timeout = maps:get(request_timeout, NodeOpts,
                                      ?WAITER_TIMEOUT)
           }}.


handle_call(?MSG_REPLY_REQUEST(Req), {Pid, _},
            State = #state{
                       role = master,
                       master_name = Name,
                       req_id = ReqId,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_reply_request(ReqId, Name, Req),
    handle_call_reply_request(ReqId, Cmd, Pid, State);

handle_call(?MSG_REPLY_REQUEST(To, Req), {Pid, _},
            State = #state{
                       role = slaver,
                       master_name = MasterName,
                       slaver_name = SlaverName,
                       req_id = ReqId,
                       status = ready
                      }) ->
    Cmd = if To =:= MasterName ->
                 ptnode_conn_proto:wrap_reply_request(
                   ReqId, SlaverName, Req);
             true ->
                 ptnode_conn_proto:wrap_reply_request(
                   ReqId, SlaverName, To, Req)
          end,
    handle_call_reply_request(ReqId, Cmd, Pid, State);

handle_call(?MSG_CONN_STOP, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast(?MSG_CLEAN_TIMEOUT_WAITERS,
            State = #state{
                       waiters = Waiters,
                       waiter_timeout = Timeout,
                       status = ready
                      }) ->
    Now = ?NOW_MILL_SECS,
    Func = fun(ReqId, #waiter{
                         create_time = CT,
                         from = From
                        }, Ids) ->
                   if Now - CT > Timeout ->
                          From ! {?MARK(ReqId), {error, response_timeout}},
                          [ReqId | Ids];
                      true -> Ids
                   end
           end,
    TimeoutReqIds = maps:fold(Func, [], Waiters),
    NewWaiters = lists:foldl(fun maps:remove/2, Waiters, TimeoutReqIds),
    {noreply, State#state{waiters = NewWaiters}};

handle_cast(?MSG_CONN_HEARTBEAT,
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

handle_cast(?MSG_REG_TIMEOUT, State = #state{status = wait}) ->
    {stop, {error, "register timeout"}, State};

handle_cast(?MSG_CONN_MASTER(Host, Port, ConnectOpts, Timeout),
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
                                      [self(), ?MSG_REG_TIMEOUT]),
                    {noreply, NewState};
                Err = {error, _} ->
                    timer:sleep(?RECONN_WAIT_TIME),
                    {stop, Err, NewState}
            end;
        Err = {error, _} ->
            timer:sleep(?RECONN_WAIT_TIME),
            {stop, Err, State}
    end;

handle_cast(?MSG_SERV_CAST(Req), State) ->
    serv_handle_cast(Req, State);

handle_cast(?MSG_SEND_DATA(Data),
            State = #state{
                       role = master,
                       status = ready
                      }) ->
    handle_cast_noreply_request(Data, State);

handle_cast(?MSG_NOREPLY_REQUEST(Req),
            State = #state{
                       role = master,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_noreply_request(Req),
    handle_cast_noreply_request(Cmd, State);

handle_cast(?MSG_NOREPLY_REQUEST(To, Req),
            State = #state{
                       role = slaver,
                       slaver_name = To,
                       status = ready
                      }) ->
    serv_handle_cast(Req, State);

handle_cast(?MSG_NOREPLY_REQUEST(To, Req),
            State = #state{
                       role = slaver,
                       status = ready
                      }) ->
    Cmd = ptnode_conn_proto:wrap_noreply_request(To, Req),
    handle_cast_noreply_request(Cmd, State);

handle_cast(Req, State) ->
    ?DLOG("~p~n", [Req]),
    ?DLOG("~p~n", [State]),
    {noreply, State}.


handle_info(?MSG_CONN_STOP, State) ->
    {stop, normal, State};
handle_info(Message, State = #state{
                                protocol_module = ProtoModule
                               }) ->
    case ProtoModule:parse_message(Message) of
        {ok, Data} ->
            handle_data(Data, State);
        {error, Reason} ->
            ?DLOG("close: ~p~n", [Reason]),
            {noreply, State};
        close ->
            {stop, normal, State}
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
                   case ptnode_master:register_slaver(
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
           ?HEARTBEAT_INTERVAL,
           gen_server, cast, [self(), ?MSG_CONN_HEARTBEAT]) of
        {ok, _} -> serv_init(State#state{
                               master_name = Name,
                               status = ready
                              });
        Err = {error, _} -> {stop, Err, State}
    end;

handle_data(?PROTO_P_NOREPLY_REQUEST(ToLen, To, BLen, B),
            State = #state{
                       role = master,
                       status = ready
                      }) ->
    Data = ptnode_conn_proto:wrap_noreply_request_binary(B),
    handle_data_forward(To, Data, State);

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
        Term -> serv_handle_call(ReqId, Term, From, State)
    end;

handle_data(?PROTO_P_REPLY_REQUEST(
               ReqId, FromLen, From, ToLen, To, BLen, B),
            State = #state{
                       role = master,
                       status = ready
                      }) ->
    Data = ptnode_conn_proto:wrap_reply_request_binary(ReqId, From, B),
    handle_data_forward(To, Data, State);

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
                    From ! {?MARK(ReqId), Term},
                    {noreply, State#state{waiters = NewWaiters}}
            end
    end;

handle_data(?PROTO_P_REPLY_REPLY(ReqId, ToLen, To, BLen, B),
            State = #state{
                       role = master,
                       status = ready
                      }) ->
    Data = ptnode_conn_proto:wrap_reply_request_reply_binary(ReqId, B),
    handle_data_forward(To, Data, State);

handle_data(Data, State) ->
    ?DLOG("~p~n", [Data]),
    {noreply, State}.


handle_continue(_Continue, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format_status(_Opt, [_PDict, _State]) ->
    ok.


terminate(Reason, State =#state{
                            server_module = ServModule,
                            server_state = ServState
                           }) ->
    ?DLOG("~p~n", [Reason]),
    terminate_unregister_slaver(State),
    terminate_close_socket(State),
    ServModule:terminate(Reason, ServState),
    ok.


terminate_close_socket(#state{
                          socket = Socket,
                          protocol_module = ProtoModule
                         }) when Socket =/= undefined ->
    ProtoModule:close(Socket);
terminate_close_socket(_) -> ok.


terminate_unregister_slaver(#state{
                               role = master,
                               sup = SupRef,
                               slaver_name = Name,
                               status = ready
                              }) ->
    ptnode_master:unregister_slaver(SupRef, Name);
terminate_unregister_slaver(_) -> ok.


serv_init(State = #state{
                     protocol_module = ProtoModule,
                     socket = Socket,
                     server_module = ServModule,
                     server_args = ServArgs
                    }) ->
    ?DLOG("~p~n", [ProtoModule:peername(Socket)]),
    case ServModule:init(ServArgs) of
        {ok, ServState} ->
            NewState = State#state{server_state = ServState},
            start_clean_timeout_waiters_timer(),
            start_report_alive_timer(State),
            {noreply, NewState};
        {stop, Reason} ->
            {stop, Reason, State}
    end.


start_clean_timeout_waiters_timer() ->
    {ok, _} = timer:apply_interval(
                ?CLEAN_WAITERS_INTERVAL,
                gen_server, cast,
                [self(), ?MSG_CLEAN_TIMEOUT_WAITERS]).


start_report_alive_timer(#state{
                            role = master,
                            slaver_name = Name,
                            sup = MasterSupRef,
                            status = ready
                           }) ->
    {ok, _} = timer:apply_interval(
                ?SERV_REPORT_ALIVE_INTERVAL,
                ptnode_master, serv_report_alive,
                [MasterSupRef, Name, self()]);
start_report_alive_timer(_) -> ok.


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
                            role = Role,
                            master_name = MasterName,
                            protocol_module = ProtoModule,
                            socket = Socket,
                            server_module = ServModule,
                            server_state = ServState
                           }) ->
    FromRef = {?B2A(?NODE_NAME(State)), ?B2A(From)},
    case ServModule:handle_call(Req, FromRef, ServState) of
        {reply, Reply, NewServState} ->
            Cmd =
            if Role =:= master orelse From =:= MasterName ->
                   ptnode_conn_proto:wrap_reply_request_reply(
                     ReqId, Reply);
               true ->
                   ptnode_conn_proto:wrap_reply_request_reply(
                     ReqId, From, Reply)
            end,
            case ProtoModule:send(Socket, Cmd) of
                ok ->
                    {noreply, State#state{server_state = NewServState}};
                Err = {error, _} ->
                    {stop, Err, State#state{server_state = NewServState}}
            end;
        {stop, Reason, NewServState} ->
            {stop, Reason, State#state{server_state = NewServState}}
    end.


handle_data_forward(To, Data,
                    State = #state{
                               role = master,
                               sup = MasterSupRef
                              }) ->
    case ptnode_master:get_node_conn(MasterSupRef, To) of
        {ok, Pid} -> gen_server:cast(Pid, ?MSG_SEND_DATA(Data));
        _ -> do_nothing
    end,
    {noreply, State}.


handle_call_reply_request(ReqId, Data, Pid,
                          State = #state{
                                     waiters = Waiters,
                                     socket = Socket,
                                     protocol_module = ProtoModule
                                    }) ->
    case ProtoModule:send(Socket, Data) of
        ok ->
            NewWaiters = maps:put(ReqId, #waiter{
                                            from = Pid
                                           }, Waiters),
            {reply, {ok, ?MARK(ReqId)},
             State#state{
               req_id = ?NEXT_REQ_ID(ReqId),
               waiters = NewWaiters
              }};
        Err = {error, _} -> {stop, Err, Err, State}
    end.


handle_cast_noreply_request(Data,
                            State = #state{
                                       protocol_module = ProtoModule,
                                       socket = Socket
                                      }) ->
    case ProtoModule:send(Socket, Data) of
        ok -> {noreply, State};
        Err = {error, _} -> {stop, Err, State}
    end.


-spec(cast(pid(), term()) -> ok).
cast(Ref, Req) -> gen_server:cast(Ref, ?MSG_SERV_CAST(Req)).


-spec(call(pid(), term()) -> term()).
call(Ref, Req) -> gen_server:call(Ref, ?MSG_SERV_CALL(Req)).


-spec(close_conn(pid()) -> ok).
close_conn(Ref) -> gen_server:call(Ref, ?MSG_CONN_STOP, 5 * 1000).


-spec(reply_request(pid(), term(), integer() | infinity)
      -> {ok, mark()} | {error, any()}).
reply_request(Ref, Req, Timeout) ->
    gen_server:call(Ref, ?MSG_REPLY_REQUEST(Req), Timeout).


-spec(reply_request(pid(), atom() | binary(), term(), integer() | infinity)
      -> {ok, mark()} | {error, any()}).
reply_request(Ref, To, Req, Timeout) when is_atom(To) ->
    reply_request(Ref, ?A2B(To), Req, Timeout);
reply_request(Ref, To, Req, Timeout) ->
    gen_server:call(Ref, ?MSG_REPLY_REQUEST(To, Req), Timeout).


-spec(noreply_request(pid(), term()) -> ok).
noreply_request(Ref, Req) ->
    gen_server:cast(Ref, ?MSG_NOREPLY_REQUEST(Req)).


-spec(noreply_request(pid(), atom() | binary(), term()) -> ok).
noreply_request(Ref, To, Req) when is_atom(To) ->
    noreply_request(Ref, ?A2B(To), Req);
noreply_request(Ref, To, Req) ->
    gen_server:cast(Ref, ?MSG_NOREPLY_REQUEST(To, Req)).
