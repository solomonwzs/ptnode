-module(ptnode_master_sup).

-include("ptnode.hrl").

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).
-export([get_conn_sup/1,
         get_mgmt/1,
         get_all_conns/1]).
-export([register_slaver/3,
         unregister_slaver/2]).

-define(SUP_FLAGS, {one_for_one,
                    ?MASTER_SUP_RESTART_INTENSITY,
                    ?MASTER_SUP_RESTART_PERIOD
                   }).


start_link(NodeInfo, ProtoSpec, AccepterOpts, ServSpec) ->
    supervisor:start_link(
      ?MODULE, [NodeInfo, ProtoSpec, AccepterOpts, ServSpec]).


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


init([NodeInfo, ProtoSpec, AccepterOpts, ServSpec]) ->
    AccepterSup = {
      '$accepter_sup',
      {ptnode_master_accepter_sup, start_link,
       [self(), ProtoSpec, AccepterOpts]},
      permanent,
      5000,
      supervisor,
      [ptnode_master_accepter_sup]},
    ConnSup = {
      '$conn_sup',
      {ptnode_master_conn_sup, start_link, [NodeInfo, ServSpec]},
      permanent,
      5000,
      supervisor,
      [ptnode_conn_sup]
     },
    Mgmt = {
      '$mgmt',
      {ptnode_master_mgmt, start_link, [self(), NodeInfo]},
      permanent,
      5000,
      worker,
      [ptnode_master_mgmt]
     },
    {ok, {?SUP_FLAGS, [AccepterSup, ConnSup, Mgmt]}}.


register_slaver(MasterSupRef, Name, Pid) ->
    {ok, Mgmt} = get_mgmt(MasterSupRef),
    gen_server:call(Mgmt, {'$register_slaver', Name, Pid}).


unregister_slaver(MasterSupRef, Name) ->
    Mgmt = get_mgmt(MasterSupRef),
    gen_server:call(Mgmt, {'$unregister_slaver', Name}).
