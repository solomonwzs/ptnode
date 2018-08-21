%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(example_conn_server).

-include("../include/ptnode.hrl").

-behaviour(ptnode_conn_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2
        ]).


init(_) ->
    ?DLOG("init~n", []),
    {ok, #{}}.


handle_call(Req, From, State) ->
    ?DLOG("from: ~p, req: ~p~n", [From, Req]),
    {reply, {error, undefined}, State}.


handle_cast(Req, State) ->
    ?DLOG("~p~n", [Req]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.
