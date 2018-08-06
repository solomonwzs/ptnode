-module(ptnode).

-include("ptnode.hrl").

-type serv_spec()::{ServModule::atom(), ServArgs::any()}.

-export([start/0, stop/0]).
-export([start_master/4, stop_master/1]).
-export([start_slaver/3, stop_slaver/1]).


start() -> application:start(ptnode).


stop() ->
    application:stop(ptnode).


-spec(start_master(atom(), ptnode_proto:server_proto_spec(),
                   map(), serv_spec())
      -> {ok, pid()} | {error, any()}).
start_master(Name, ProtoSpec, AccepterOpts, ServSpec) ->
    supervisor:start_child(
      ptnode_sup,
      {{'$master', Name},
       {ptnode_master_sup, start_link,
        [Name, ProtoSpec, AccepterOpts, ServSpec]},
       permanent,
       5000,
       supervisor,
       [ptnode_master_sup]}).


-spec(stop_master(atom()) -> ok | {error, any()}).
stop_master(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, {'$master', Name}),
    supervisor:delete_child(ptnode_sup, {'$master', Name}).


-spec(start_slaver(atom(), ptnode_proto:client_proto_spec(),
                   serv_spec())
      -> {ok, pid()} | {error, any()}).
start_slaver(Name, ProtoSpec, ServSpec) ->
    supervisor:start_child(
      ptnode_sup,
      {{'$slaver', Name},
       {ptnode_conn_server, start_link,
        [slaver, Name, ServSpec, ProtoSpec]},
       permanent,
       5000,
       worker,
       [ptnode_conn_server]}).


-spec(stop_slaver(atom()) -> ok | {error, any()}).
stop_slaver(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, {'$slaver', Name}),
    supervisor:delete_child(ptnode_sup, {'$slaver', Name}).
