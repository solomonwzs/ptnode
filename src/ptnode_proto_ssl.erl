-module(ptnode_proto_ssl).

-behaviour(ptnode_proto).

-export([name/0,
         listen/2,
         accept/2,
         handshake/2,
         close/1,
         controlling_process/2
        ]).

-if(?OTP_RELEASE =:= 21).
-define(ssl_handshake(Socket, Opts), ssl:handshake(Socket, Opts, infinity)).
-else.
-define(ssl_handshake(Socket, Opts), handshake0(Socket, Opts)).
handshake0(Socket, _) ->
    case ssl:ssl_accept(Socket) of
        ok -> {ok, Socket};
        Err = {error, _} -> Err
    end.
-endif.


name() -> ssl.


listen(Port, Opts) ->
    ssl:listen(Port, Opts).


accept(ListenSocket, _) ->
    ssl:transport_accept(ListenSocket).


handshake(Socket, Opts) ->
    case ?ssl_handshake(Socket, Opts) of
        {ok, SslSocket} -> {ok, SslSocket, undefined};
        Err = {error, _} -> Err
    end.


close(Socket) ->
    ssl:close(Socket).


controlling_process(Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).
