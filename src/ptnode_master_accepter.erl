%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master_accepter).

-include("ptnode.hrl").

-export([start_link/3]).
-export([loop/5]).


start_link(SupRef, ListenSocket, ProtoOpts) ->
    ProtoModule = maps:get(module, ProtoOpts),
    AccepterOpts = maps:get(accept_opts, ProtoOpts),
    HandshakeOpts = maps:get(handshake_opts, ProtoOpts),
    Pid = spawn_link(?MODULE, loop,
                     [SupRef, ListenSocket,
                      ProtoModule, AccepterOpts, HandshakeOpts]),
    {ok, Pid}.


loop(MasterSupRef, ListenSocket,
     ProtoModule, AccepterOpts, HandshakeOpts) ->
    accept(MasterSupRef, ListenSocket,
           ProtoModule, AccepterOpts, HandshakeOpts),
    loop(MasterSupRef, ListenSocket,
         ProtoModule, AccepterOpts, HandshakeOpts).


accept(MasterSupRef, ListenSocket,
       ProtoModule, AccepterOpts, HandshakeOpts) ->
    case ProtoModule:accept(ListenSocket, AccepterOpts) of
        {ok, SslSocket} ->
            handshake(MasterSupRef, SslSocket,
                      ProtoModule, HandshakeOpts);
        Err -> ?dlog("~p~n", [Err])
    end.


handshake(MasterSupRef, SslSocket,
          ProtoModule, HandshakeOpts) ->
    case ProtoModule:handshake(SslSocket, HandshakeOpts) of
        {ok, Socket} ->
            get_conn_sup(MasterSupRef, Socket, ProtoModule);
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(SslSocket)
    end.


get_conn_sup(MasterSupRef, Socket, ProtoModule) ->
    case ?get_master_conn_sup(MasterSupRef) of
        {ok, SupRef} ->
            start_conn_server(MasterSupRef, SupRef, Socket, ProtoModule);
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.


start_conn_server(MasterSupRef, SupRef, Socket, ProtoModule) ->
    case supervisor:start_child(
           SupRef, [{MasterSupRef, Socket}]) of
        {ok, Ref} when is_pid(Ref) ->
            case ProtoModule:controlling_process(Socket, Ref) of
                ok ->
                    case ProtoModule:setopts(Socket, [{active, true}]) of
                        ok -> ok;
                        Err = {error, _} ->
                            ?dlog("~p~n", [Err]),
                            ProtoModule:close(Socket)
                    end;
                Err ->
                    ?dlog("~p~n", [Err]),
                    ProtoModule:close(Socket)
            end;
        Err ->
            ?dlog("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.
