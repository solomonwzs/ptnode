%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master_sup).

-include("ptnode.hrl").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).
-export([get_conn_sup/1,
         get_mgmt/1,
         get_all_conns/1]).
-export([register_slaver/3,
         unregister_slaver/2,
         get_node_conn/2,
         get_nodes/1
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


-spec(get_conn_sup(supervisor:sup_ref())
      -> {ok, pid()} | {error, any()}).
get_conn_sup(MasterSupRef) ->
    Children = supervisor:which_children(MasterSupRef),
    get_child(Children, '$conn_sup').


-spec(get_mgmt(supervisor:sup_ref())
      -> {ok, pid()} | {error, any()}).
get_mgmt(MasterSupRef) ->
    Children = supervisor:which_children(MasterSupRef),
    get_child(Children, '$mgmt').


get_child([], _) -> {error, "not found"};
get_child([{ID, Ref, _, _} | _], ID) -> {ok, Ref};
get_child([_ | Tail], ID) -> get_child(Tail, ID).


-spec(get_all_conns(supervisor:sup_ref())
      -> {ok, list()} | {error, any()}).
get_all_conns(MasterSupRef) ->
    case get_conn_sup(MasterSupRef) of
        {ok, Ref} -> {ok, supervisor:which_children(Ref)};
        Err -> Err
    end.


init([NodeOpts, ProtoOpts, ServSpec]) ->
    AccepterSup = {
      '$accepter_sup',
      {ptnode_master_accepter_sup, start_link,
       [self(), NodeOpts, ProtoOpts]},
      permanent,
      5000,
      supervisor,
      [ptnode_master_accepter_sup]},
    ConnSup = {
      '$conn_sup',
      {ptnode_master_conn_sup, start_link, [NodeOpts, ProtoOpts, ServSpec]},
      permanent,
      5000,
      supervisor,
      [ptnode_conn_sup]
     },
    Mgmt = {
      '$mgmt',
      {ptnode_master_mgmt, start_link, [self(), NodeOpts]},
      permanent,
      5000,
      worker,
      [ptnode_master_mgmt]
     },
    {ok, {?SUP_FLAGS, [AccepterSup, ConnSup, Mgmt]}}.


get_node_conn(MasterSupRef, Name) when is_atom(Name) ->
    get_node_conn(MasterSupRef, ?a2b(Name));
get_node_conn(MasterSupRef, Name) ->
    {ok, Mgmt} = get_mgmt(MasterSupRef),
    gen_server:call(Mgmt, {'$get_node_conn', Name}, 1000).


get_nodes(MasterSupRef) ->
    {ok, Mgmt} = get_mgmt(MasterSupRef),
    gen_server:call(Mgmt, '$nodes', 1000).


register_slaver(MasterSupRef, Name, Pid) ->
    {ok, Mgmt} = get_mgmt(MasterSupRef),
    gen_server:call(Mgmt, {'$register_slaver', Name, Pid}).


unregister_slaver(MasterSupRef, Name) ->
    case get_mgmt(MasterSupRef) of
        {ok, Mgmt} ->
            gen_server:call(Mgmt, {'$unregister_slaver', Name});
        _ -> ok
    end.
