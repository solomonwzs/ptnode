-module(ptnode_master_accepter).

-include("ptnode.hrl").

-export([start_link/3]).
-export([loop/3]).


start_link(SupRef, ListenSocket, ProtoModule) ->
    Pid = spawn_link(?MODULE, loop, [SupRef, ListenSocket, ProtoModule]),
    {ok, Pid}.


loop(MasterSupRef, ListenSocket, ProtoModule) ->
    case ProtoModule:accept(ListenSocket) of
        {ok, SslSocket} ->
            Child = supervisor:which_children(MasterSupRef),
            ?dlog("~p~n", [Child]),
            handle(SslSocket);
        Err -> ?dlog("~p~n", [Err])
    end,
    loop(MasterSupRef, ListenSocket, ProtoModule).


handle(SslSocket) ->
    receive
        Msg ->
            ?dlog("~p~n", [Msg]),
            ssl:setopts(SslSocket, [{active, once}]),
            handle(SslSocket)
    end.
