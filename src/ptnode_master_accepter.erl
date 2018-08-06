-module(ptnode_master_accepter).

-include("ptnode.hrl").

-export([start_link/3]).
-export([loop/3]).


start_link(SupRef, ListenSocket, Spec) ->
    Pid = spawn_link(?MODULE, loop, [SupRef, ListenSocket, Spec]),
    {ok, Pid}.


loop(MasterSupRef, ListenSocket, Spec) ->
    accept(MasterSupRef, ListenSocket, Spec),
    loop(MasterSupRef, ListenSocket, Spec).


accept(MasterSupRef, ListenSocket,
       Spec = {ProtoModule, _, _, AccepterOpts0, _}) ->
    AccepterOpts = AccepterOpts0 ++ [{active, false}],
    case ProtoModule:accept(ListenSocket, AccepterOpts) of
        {ok, SslSocket} ->
            handshake(MasterSupRef, SslSocket, Spec);
        Err -> ?dlog("~p~n", [Err])
    end.


handshake(MasterSupRef, SslSocket, {ProtoModule, _, _, _, HandshakeOpts}) ->
    case ProtoModule:handshake(SslSocket, HandshakeOpts) of
        {ok, Socket} ->
            get_conn_sup(MasterSupRef, Socket, ProtoModule);
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(SslSocket)
    end.


get_conn_sup(MasterSupRef, Socket, ProtoModule) ->
    case ptnode_master_sup:get_conn_sup(MasterSupRef) of
        {ok, SupRef} ->
            start_conn_server(MasterSupRef, SupRef, Socket, ProtoModule);
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.


start_conn_server(MasterSupRef, SupRef, Socket, ProtoModule) ->
    case supervisor:start_child(
           SupRef, [{MasterSupRef, Socket, ProtoModule}]) of
        {ok, Ref} when is_pid(Ref) ->
            case ProtoModule:controlling_process(Socket, Ref) of
                ok -> init_serv(Ref);
                Err ->
                    ?dlog("~p~n", [Err]),
                    ProtoModule:close(Socket)
            end;
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.


init_serv(Ref) ->
    case gen_server:call(Ref, '$init_serv') of
        ok -> ok;
        Err = {error, _} ->
            ?dlog("~p~n", [Err]),
            Ref ! '$stop'
    end.
