-module(ptnode_master_conn_sup).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(NodeInfo, ServSpec) ->
    supervisor:start_link(?MODULE, [NodeInfo, ServSpec]).


init([{_, Cookie}, ServSpec]) ->
    ChildSpec = {'$serv',
                {ptnode_conn_server, start_link,
                 [master, Cookie, ServSpec]},
                temporary,
                5000,
                worker,
                [ptnode_conn_server]},
    {ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.
