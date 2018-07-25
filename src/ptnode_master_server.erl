-module(ptnode_master_server).

-include("ptnode.hrl").

-behaviour(gen_server).

-export([start_link/3]).
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
          conn_state::any(),
          socket::ptnode_proto:socket(),
          protocol_module::atom()
         }).


start_link(Socket, ProtoModule, ConnState) ->
    gen_server:start_link(?MODULE, [Socket, ProtoModule, ConnState], []).


init([Socket, ProtoModule, ConnState]) ->
    ?dlog("~p~n", [Socket]),
    {ok, #state{
            conn_state = ConnState,
            socket = Socket,
            protocol_module = ProtoModule
           }}.


handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(stop, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    ?dlog("~p~n", [_Info]),
    {noreply, State}.


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
