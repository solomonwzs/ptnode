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
       Spec = {ProtoModule, _, _, AccepterOpts, _}) ->
    case ProtoModule:accept(ListenSocket, AccepterOpts) of
        {ok, SslSocket} ->
            handshake(MasterSupRef, SslSocket, Spec);
        Err -> ?dlog("~p~n", [Err])
    end.


handshake(MasterSupRef, SslSocket, {ProtoModule, _, _, _, HandshakeOpts}) ->
    case ProtoModule:handshake(SslSocket, HandshakeOpts) of
        {ok, Socket, ConnState} ->
            get_conn_sup(MasterSupRef, Socket, ProtoModule, ConnState);
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(SslSocket)
    end.


get_conn_sup(MasterSupRef, Socket, ProtoModule, ConnState) ->
    case ptnode_master_sup:get_conn_sup(MasterSupRef) of
        {ok, SupRef} ->
            start_conn_server(SupRef, Socket, ProtoModule, ConnState);
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.


start_conn_server(SupRef, Socket, ProtoModule, ConnState) ->
    case supervisor:start_child(SupRef, [Socket, ProtoModule, ConnState]) of
        {ok, Ref} when is_pid(Ref) ->
            case ProtoModule:controlling_process(Socket, Ref) of
                ok -> ok;
                Err ->
                    ?dlog("~p~n", [Err]),
                    ProtoModule:close(Socket)
            end;
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.


% handle(SslSocket) ->
%     receive
%         Msg ->
%             ?dlog("~p~n", [Msg]),
%             ssl:setopts(SslSocket, [{active, once}]),
%             handle(SslSocket)
%     end.
