%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode).

-include("ptnode.hrl").

-type node_opts() :: #{
        name => atom(),
        cookie => bitstring(),
        num_acceptors => pos_integer(),
        named_node => boolean()
       }.
-export_type([node_opts/0]).

-type proto_opts() :: #{
        module => module(),
        listen_port => pos_integer(),
        listen_opts => list(),
        accept_opts => list(),
        handshake_opts => list(),
        connect_host => ptnode_proto:host(),
        connect_port => pos_integer(),
        connect_opts => list(),
        timeout => pos_integer() | infinity
       }.
-export_type([proto_opts/0]).

-type serv_spec() :: #{
        module => module(),
        init_args => any()
       }.
-export_type([serv_spec/0]).

-export([start/0, stop/0]).
-export([start_master/3, stop_master/1]).
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


-spec(start_master(node_opts(), proto_opts(), serv_spec())
      -> {ok, pid()} | {error, any()}).
start_master(NodeOpts, ProtoOpts, ServSpec) ->
    ChildId = {'$master', maps:get(name, NodeOpts)},
    case try_delete_child(ChildId) of
        ok ->
            supervisor:start_child(
              ptnode_sup,
              {ChildId,
               {ptnode_master_sup, start_link,
                [NodeOpts, ProtoOpts, ServSpec]},
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


-spec(start_slaver(node_opts(), proto_opts(), serv_spec())
      -> {ok, pid()} | {error, any()}).
start_slaver(NodeOpts, ProtoOpts, ServSpec) ->
    ChildId = {'$slaver', maps:get(name, NodeOpts)},
    case try_delete_child(ChildId) of
        ok ->
            supervisor:start_child(
              ptnode_sup,
              {ChildId,
               {ptnode_slaver_sup, start_link,
                [NodeOpts, ProtoOpts, ServSpec]},
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
