-module(master_echo_server).

-include("../include/ptnode.hrl").

-behaviour(ptnode_conn_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_data/2,
         terminate/2
        ]).


init(_) -> {ok, #{}}.


handle_call(_Req, _From, State) -> {ok, {error, undefined}, State}.


handle_cast(Req, State) ->
    {ok, term_to_binary(Req), State}.


handle_data(Data, State) ->
    ?dlog("~p~n", [Data]),
    {ok, State}.


terminate(_Reason, _State) ->
    ok.
