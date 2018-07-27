-module(ptnode_master_conn_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ServModule) ->
    supervisor:start_link(?MODULE, [ServModule]).


init([ServModule]) ->
    ServSpec = {serv,
                {ptnode_master_server, start_link, [ServModule]},
                temporary,
                5000,
                worker,
                [ptnode_master_server]},
    {ok, {{simple_one_for_one, 5, 10}, [ServSpec]}}.
