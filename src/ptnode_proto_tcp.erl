%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_proto_tcp).

-include("ptnode.hrl").

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


name() -> tcp.


listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).


accept(ListenSocket, _) ->
    gen_tcp:accept(ListenSocket).


connect(Host, Port, Options, Timeout) ->
    gen_tcp:connect(Host, Port, Options, Timeout).


handshake(Socket, _) ->
    {ok, Socket}.


close(Socket) ->
    gen_tcp:close(Socket).


controlling_process(Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid).


setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).


parse_message(Socket, {tcp, Socket, Data}) -> {ok, list_to_binary(Data)};
parse_message(Socket, {tcp_closed, Socket}) -> close;
parse_message(Socket, {tcp_error, Socket, Reason}) -> {error, Reason};
parse_message(_, _) -> ignore.


send(Socket, Data) ->
    gen_tcp:send(Socket, Data).


peername(Socket) ->
    inet:peername(Socket).
