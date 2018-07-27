-module(ptnode).

-include("ptnode.hrl").

-export([start/0, stop/0]).
-export([start_master/4, stop_master/1]).
-export([foo/0]).


foo() ->
    ProtoSpec = {
      ptnode_proto_ssl,
      9998,
      [{certfile, "./priv/example-pem/cert.pem"},
       {keyfile, "./priv/example-pem/key.pem"}
      ],
      [],
      []
     },
    AccepterOpts = #{
      num_acceptors => 1
     },
    start_master(name, ProtoSpec, AccepterOpts, ptnode_master_server).


start() ->
    ssl:start(),
    application:start(ptnode).


stop() ->
    application:stop(ptnode).


-spec(start_master(atom(), emq_rf_node_proto:proto_spec(), map(), atom())
      -> {ok, pid()} | {error, any}).
start_master(Name, ProtoSpec, AccepterOpts, ServModule) ->
    supervisor:start_child(
      ptnode_sup,
      {{master, Name},
       {ptnode_master_sup, start_link,
        [Name, ProtoSpec, AccepterOpts, ServModule]},
       permanent,
       5000,
       supervisor,
       dynamic}).


stop_master(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, {master, Name}),
    supervisor:delete_child(ptnode_sup, {master, Name}).
