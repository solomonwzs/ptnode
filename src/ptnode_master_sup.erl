%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master_sup).

-include("ptnode.hrl").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).
-export([get_node_conns/1
        ]).

-define(SUP_FLAGS, {one_for_one,
                    ?MASTER_SUP_RESTART_INTENSITY,
                    ?MASTER_SUP_RESTART_PERIOD
                   }).


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
    AccepterSup = {
      ?MASTER_ACCEPTER_SUP_ID,
      {ptnode_master_accepter_sup, start_link,
       [self(), NodeOpts, ProtoOpts]},
      permanent,
      5000,
      supervisor,
      [ptnode_master_accepter_sup]},
    ConnSup = {
      ?MASTER_CONN_SUP_ID,
      {ptnode_master_conn_sup, start_link, [NodeOpts, ProtoOpts, ServSpec]},
      permanent,
      5000,
      supervisor,
      [ptnode_conn_sup]
     },
    Mgmt = {
      ?MASTER_MGMT_ID,
      {ptnode_master_mgmt, start_link, [self(), NodeOpts]},
      permanent,
      5000,
      worker,
      [ptnode_master_mgmt]
     },
    {ok, {?SUP_FLAGS, [AccepterSup, ConnSup, Mgmt]}}.


get_node_conns(MasterSupRef) ->
    {ok, ConnSup} = ?get_master_conn_sup(MasterSupRef),
    ?dlog("~p~n", [supervisor:count_children(ConnSup)]),
    supervisor:which_children(ConnSup).
