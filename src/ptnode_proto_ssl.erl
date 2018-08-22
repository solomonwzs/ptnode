%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_proto_ssl).

-behaviour(ptnode_proto).

-export([accept/2,
         close/1,
         connect/4,
         controlling_process/2,
         handshake/2,
         listen/2,
         name/0,
         parse_message/2,
         peername/1,
         send/2,
         setopts/2
        ]).

-ifdef(OTP_RELEASE).
-define(SSL_HANDSHAKE(Socket, Opts), ssl:handshake(Socket, Opts, infinity)).
-else.
-define(SSL_HANDSHAKE(Socket, Opts), handshake0(Socket, Opts)).
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


connect(Host, Port, Options, Timeout) ->
    ssl:connect(Host, Port, Options, Timeout).


handshake(Socket, Opts) ->
    ?SSL_HANDSHAKE(Socket, Opts).


close(Socket) ->
    ssl:close(Socket).


controlling_process(Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).


setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).


parse_message(Socket, {ssl, Socket, Data}) -> {ok, list_to_binary(Data)};
parse_message(Socket, {ssl_closed, Socket}) -> close;
parse_message(Socket, {ssl_error, Socket, Reason}) -> {error, Reason};
parse_message(_, _) -> ignore.


send(Socket, Data) ->
    ssl:send(Socket, Data).


peername(Socket) ->
    ssl:peername(Socket).
