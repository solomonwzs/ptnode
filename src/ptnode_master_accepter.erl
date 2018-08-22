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
        {ok, Socket} ->
            handshake(MasterSupRef, Socket,
                      ProtoModule, HandshakeOpts);
        Err -> ?DLOG("~p~n", [Err])
    end.


handshake(MasterSupRef, Socket0,
          ProtoModule, HandshakeOpts) ->
    ?DLOG("~p~n", [ProtoModule:peername(Socket0)]),
    case ProtoModule:handshake(Socket0, HandshakeOpts) of
        {ok, Socket1} ->
            ?DLOG("~p~n", [ProtoModule:peername(Socket1)]),
            get_conn_sup(MasterSupRef, Socket1, ProtoModule);
        Err ->
            ?DLOG("~p~n", [Err]),
            ProtoModule:close(Socket0)
    end.


get_conn_sup(MasterSupRef, Socket, ProtoModule) ->
    case ?GET_MASTER_CONN_SUP(MasterSupRef) of
        {ok, SupRef} ->
            start_conn_server(MasterSupRef, SupRef, Socket, ProtoModule);
        Err ->
            ?DLOG("~p~n", [Err]),
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
                            ?DLOG("~p~n", [Err]),
                            ProtoModule:close(Socket)
                    end;
                Err ->
                    ?DLOG("~p~n", [Err]),
                    ProtoModule:close(Socket)
            end;
        Err ->
            ?DLOG("~p~n", [Err]),
            ProtoModule:close(Socket)
    end.
