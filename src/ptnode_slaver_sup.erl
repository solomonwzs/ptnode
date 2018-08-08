-module(ptnode_slaver_sup).

-include("ptnode.hrl").

-export([start_link/3]).
-export([init/1]).


start_link(NodeInfo, ServSpec, ProtoSpec) ->
    supervisor:start_link(?MODULE, [NodeInfo, ServSpec, ProtoSpec]).


init([NodeInfo, ServSpec, ProtoSpec]) ->
    Spec = {
      '$slaver',
      {ptnode_conn_server, start_link,
       [slaver, NodeInfo, ServSpec, ProtoSpec]},
      permanent,
      5000,
      worker,
      [ptnode_conn_server]
     },
    {ok, {{one_for_one, 5, 10}, [Spec]}}.
