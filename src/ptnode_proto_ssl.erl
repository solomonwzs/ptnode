-module(ptnode_proto_ssl).

-export([listen/1]).
-export([accept/1]).

-if(?OTP_RELEASE =:= 21).
-define(ssl_accept(Socket), ssl:handshake(Socket)).
-else.
-define(ssl_accept(Socket), handshake(Socket)).

handshake(Socket) ->
    case ssl:ssl_accept(Socket) of
        ok -> {ok, Socket};
        Err = {error, _} -> Err
    end.
-endif.


-spec(listen(map())
      -> {ok, ssl:sslsocket()} | {error, term()}).
listen(Opts) ->
    Port = maps:get(port, Opts),
    CertFile = maps:get(certfile, Opts, "cacert.pem"),
    KeyFile = maps:get(keyfile, Opts, "key.pem"),
    ssl:listen(
      Port, [{certfile, CertFile},
             {keyfile, KeyFile},
             {active, once},
             {reuseaddr, true}]).


-spec(accept(ssl:sslsocket())
      -> {ok, ssl:sslsocket()} | {error, term()}).
accept(ListenSocket) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    ?ssl_accept(Socket).
