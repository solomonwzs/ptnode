-module(ptnode_master_conn_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(Serv) ->
    supervisor:start_link(?MODULE, [Serv]).


init([Serv]) ->
    ServSpec = {serv,
                {Serv, start_link, []},
                temporary,
                5000,
                worker,
                [Serv]},
    {ok, {{simple_one_for_one, 5, 10}, [ServSpec]}}.
