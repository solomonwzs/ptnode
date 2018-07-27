-module(ptnode_master_server).

-include("ptnode.hrl").

-behaviour(gen_server).

-export([start_link/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         code_change/3,
         format_status/2,
         terminate/2
        ]).

-record(state, {
          master_sup        ::supervisor:sup_ref(),
          conn_state        ::any(),
          socket            ::ptnode_proto:socket(),
          server_module     ::atom(),
          protocol_module   ::atom()
         }).


%% behaviour callback
-callback init() -> {ok, State::any()} | {error, Reason::any()}.
%%


start_link(ServModule, MasterSupRef, Socket, ProtoModule) ->
    gen_server:start_link(
      ?MODULE, [ServModule, MasterSupRef, Socket, ProtoModule], []).


init([ServModule, MasterSupRef, Socket, ProtoModule]) ->
    ?dlog("~p~n", [ServModule]),
    {ok, #state{
            master_sup = MasterSupRef,
            socket = Socket,
            server_module = ServModule,
            protocol_module = ProtoModule
           }}.


handle_call('$active_socket', _From, State = #state{
                                             protocol_module = ProtoModule,
                                             socket = Socket}) ->
    R = ProtoModule:setopts(Socket, [{active, true}]),
    {reply, R, State};
handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast(_Req, State) ->
    {noreply, State}.


handle_info('$stop', State) ->
    {stop, normal, State};
handle_info(Message, State = #state{
                              protocol_module = ProtoModule
                             }) ->
    case ProtoModule:parse_message(Message) of
        {ok, Data} ->
            ?dlog("~p~n", [Data]),
            {noreply, State};
        {error, Reason} ->
            ?dlog("~p~n", [Reason]),
            {noreply, Reason, State};
        close ->
            {stop, normal, State};
        _ ->
            {noreply, State}
    end.


handle_continue(_Continue, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format_status(_Opt, [_PDict, _State]) ->
    ok.


terminate(Reason, #state{
                     socket = Socket,
                     protocol_module = ProtoModule
                    }) ->
    ?dlog("~p~n", [Reason]),
    ProtoModule:close(Socket),
    ok.
