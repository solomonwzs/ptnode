%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_slaver_sup).

-include("ptnode.hrl").

-export([start_link/3]).
-export([init/1]).


-spec(start_link(ptnode:node_opts(), ptnode:proto_opts(),
                 ptnode:serv_spec())
      -> ok | {error, any()}).
start_link(NodeOpts, ProtoOpts, ServSpec) ->
    case maps:get(named_node, NodeOpts, false) of
        true ->
            supervisor:start_link({local, maps:get(name, NodeOpts)},
                                  ?MODULE, [NodeOpts, ProtoOpts, ServSpec]);
        false ->
            supervisor:start_link(
              ?MODULE, [NodeOpts, ProtoOpts, ServSpec])
    end.


init([NodeOpts, ProtoOpts, ServSpec]) ->
    Spec = {
      ?SLAVER_CONN_ID,
      {ptnode_conn_server, start_slaver_conn_link,
       [NodeOpts, ProtoOpts, ServSpec, self()]},
      permanent,
      5000,
      worker,
      [ptnode_conn_server]
     },
    {ok, {{one_for_one, 5, 10}, [Spec]}}.
