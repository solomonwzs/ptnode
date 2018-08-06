-module(ptnode_conn_server).

-include("ptnode.hrl").

-behaviour(gen_server).

-export([start_link/3, start_link/4]).
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
          role              ::master | slaver,
          master_sup        ::supervisor:sup_ref() | undefined,
          serv_state        ::any(),
          socket            ::ptnode_proto:socket() | undefined,
          server_module     ::atom(),
          server_args       ::any(),
          protocol_module   ::atom()
         }).


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


start_link(master, ServSpec, ExtArgs) ->
    gen_server:start_link(?MODULE, [master, ServSpec, ExtArgs], []).


start_link(slaver, Name, ServSpec, ExtArgs) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [slaver, ServSpec, ExtArgs], []).


init([master,
      {ServModule, ServArgs},
      {MasterSupRef, Socket, ProtoModule}]) ->
    {ok, #state{
            role = master,
            master_sup = MasterSupRef,
            serv_state = undefined,
            socket = Socket,
            server_module = ServModule,
            server_args = ServArgs,
            protocol_module = ProtoModule
           }};
init([slaver, {ServModule, ServArgs},
      {ProtoModule, Host, Port, ConnectOpts, Timeout}]) ->
    ConnReq = {'$conn_master', Host, Port, ConnectOpts, Timeout},
    gen_server:cast(self(), ConnReq),
    {ok, #state{
            role = slaver,
            socket = undefined,
            server_module = ServModule,
            server_args = ServArgs,
            protocol_module = ProtoModule,
            serv_state = undefined
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


handle_cast({'$conn_master', Host, Port, ConnectOpts, Timeout},
            State = #state{
                       role = slaver,
                       protocol_module = ProtoModule,
                       server_module = ServModule,
                       server_args = ServArgs
                      }) ->
    case ServModule:init(ServArgs) of
        {ok, ServState} ->
            case ProtoModule:connect(Host, Port, ConnectOpts, Timeout) of
                {ok, Socket} ->
                    {noreply, State#state{
                                socket = Socket,
                                serv_state = ServState
                               }};
                {error, Reason} ->
                    timer:sleep(3 * 1000),
                    {stop, Reason, State#state{
                                     serv_state = ServState
                                    }}
            end;
        {error, Reason} -> {stop, Reason, State}
    end;
handle_cast({'$serv_cast', Req}, State) ->
    handle_noreply(handle_cast, Req, State);
handle_cast(_Req, State) ->
    {noreply, State}.


handle_info('$stop', State) ->
    {stop, normal, State};
handle_info(Message, State = #state{
                                protocol_module = ProtoModule
                               }) ->
    case ProtoModule:parse_message(Message) of
        {ok, Data} ->
            handle_noreply(handle_data, Data, State);
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


handle_noreply(Func, Req,
               State = #state{
                          socket = Socket,
                          protocol_module = ProtoModule,
                          server_module = ServModule,
                          serv_state = ServState
                         }) ->
    case ServModule:Func(Req, ServState) of
        {ok, Data, NewServState} ->
            case ProtoModule:send(Socket, Data) of
                ok -> {noreply, State#state{serv_state = NewServState}};
                Err = {error, _} ->
                    {stop, Err, State#state{serv_state = NewServState}}
            end;
        {ok, NewServState} ->
            {noreply, State#state{serv_state = NewServState}};
        {stop, Reason, Data, NewServState} ->
            ProtoModule:send(Socket,  Data),
            {stop, Reason, State#state{serv_state = NewServState}};
        {stop, Reason, NewServState} ->
            {stop, Reason, State#state{serv_state = NewServState}}
    end.


handle_continue(_Continue, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format_status(_Opt, [_PDict, _State]) ->
    ok.


terminate(Reason, #state{
                     socket = Socket,
                     protocol_module = ProtoModule,
                     server_module = ServModule,
                     serv_state = ServState
                    }) ->
    if Socket =/= undefined -> ProtoModule:close(Socket);
       true -> ok
    end,
    ServModule:terminate(Reason, ServState),
    ok.


cast(Ref, Req) -> gen_server:cast(Ref, {'$serv_cast', Req}).


call(Ref, Req) -> gen_server:call(Ref, {'$serv_call', Req}).


close_conn(Ref) -> gen_server:call(Ref, '$stop', 5 * 1000).
