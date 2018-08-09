%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master_conn_sup).

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).


-spec(start_link(ptnode:node_opts(), ptnode:proto_opts(), ptnode:serv_spec())
      -> pid() | {error, any()}).
start_link(NodeOpts, ProtoOpts, ServSpec) ->
    supervisor:start_link(?MODULE, [NodeOpts, ProtoOpts, ServSpec]).


init([NodeOpts, ProtoOpts, ServSpec]) ->
    ChildSpec = {'$serv',
                {ptnode_conn_server, start_master_conn_link,
                 [NodeOpts, ProtoOpts, ServSpec]},
                temporary,
                5000,
                worker,
                [ptnode_conn_server]},
    {ok, {{simple_one_for_one, 5, 10}, [ChildSpec]}}.
