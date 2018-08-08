-module(ptnode).

-include("ptnode.hrl").

-type serv_spec()::{ServModule::atom(), ServArgs::any()}.
-type node_info()::{Name::binary(), Cookie::binary()}.

-export([start/0, stop/0]).
-export([start_master/4, stop_master/1]).
-export([start_slaver/3, stop_slaver/1]).


start() -> application:start(ptnode).


stop() ->
    application:stop(ptnode).


try_delete_child(ChildId) ->
    case supervisor:delete_child(ptnode_sup, ChildId) of
        ok -> ok;
        {error, not_found} -> ok;
        Err = {error, _} -> Err
    end.


-spec(start_master(node_info(), ptnode_proto:server_proto_spec(),
                   map(), serv_spec())
      -> {ok, pid()} | {error, any()}).
start_master(NodeInfo = {Name, _}, ProtoSpec, AccepterOpts, ServSpec) ->
    ChildId = {'$master', Name},
    case try_delete_child(ChildId) of
        ok ->
            supervisor:start_child(
              ptnode_sup,
              {{'$master', Name},
               {ptnode_master_sup, start_link,
                [NodeInfo, ProtoSpec, AccepterOpts, ServSpec]},
               transient,
               5000,
               supervisor,
               [ptnode_master_sup]});
        Err = {error, _} -> Err
    end.


-spec(stop_master(atom()) -> ok | {error, any()}).
stop_master(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, {'$master', Name}),
    supervisor:delete_child(ptnode_sup, {'$master', Name}).


-spec(start_slaver(node_info(), ptnode_proto:client_proto_spec(),
                   serv_spec())
      -> {ok, pid()} | {error, any()}).
start_slaver(NodeInfo = {Name, _}, ProtoSpec, ServSpec) ->
    ChildId = {'$slaver', Name},
    case try_delete_child(ChildId) of
        ok ->
            supervisor:start_child(
              ptnode_sup,
              {{'$slaver', Name},
               {ptnode_slaver_sup, start_link,
                [NodeInfo, ServSpec, ProtoSpec]},
               transient,
               5000,
               supervisor,
               [ptnode_slaver_sup]});
        Err = {error, _} -> Err
    end.


-spec(stop_slaver(atom()) -> ok | {error, any()}).
stop_slaver(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, {'$slaver', Name}),
    supervisor:delete_child(ptnode_sup, {'$slaver', Name}).
