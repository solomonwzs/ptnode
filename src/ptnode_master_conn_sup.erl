-module(ptnode_master_conn_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ServSpec) ->
    supervisor:start_link(?MODULE, [ServSpec]).


init([ServSpec]) ->
    ChildSpec = {'$serv',
                {ptnode_conn_server, start_link,
                 [master, ServSpec]},
                temporary,
                5000,
                worker,
                [ptnode_conn_server]},
    {ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.
