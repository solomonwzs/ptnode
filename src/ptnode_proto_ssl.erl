-module(ptnode_proto_ssl).

-behaviour(ptnode_proto).

-export([accept/2,
         close/1,
         connect/4,
         controlling_process/2,
         handshake/2,
         listen/2,
         name/0,
         parse_message/1,
         peername/1,
         send/2,
         setopts/2
        ]).

-ifdef(OTP_RELEASE).
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


connect(Host, Port, Options, Timeout) ->
    ssl:connect(Host, Port, Options, Timeout).


handshake(Socket, Opts) ->
    ?ssl_handshake(Socket, Opts).


close(Socket) ->
    ssl:close(Socket).


controlling_process(Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).


setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).


parse_message({ssl, _Socket, Data}) -> {ok, list_to_binary(Data)};
parse_message({ssl_closed, _Socket}) -> close;
parse_message({ssl_error, _Socket, Reason}) -> {error, Reason};
parse_message(_) -> ignore.


send(Socket, Data) ->
    ssl:send(Socket, Data).


peername(Socket) ->
    ssl:peername(Socket).
