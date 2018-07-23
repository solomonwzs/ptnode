-module(ptnode_master_accepter).

-export([start_link/1]).
-export([loop/1]).

-if(?OTP_RELEASE =:= 21).
-define(ssl_accept(Socket), ssl:handshake(Socket)).
-else.
-define(ssl_accept(Socket), ssl:ssl_accept(Socket)).
-endif.


start_link(Opts) ->
    Port = proplists:get_value(port, Opts),
    CertFile = proplists:get_value(certfile, Opts),
    KeyFile = proplists:get_value(keyfile, Opts),
    {ok, ListenSocket} = ssl:listen(
                           Port, [{certfile, CertFile},
                                  {keyfile, KeyFile},
                                  {reuseaddr, true}]),
    Pid = spawn_link(?MODULE, loop, [ListenSocket]),
    {ok, Pid}.


loop(ListenSocket) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    case ?ssl_accept(Socket) of
        ok -> ok;
        _ -> ok
    end,
    loop(ListenSocket).
