-module(helper).

-export([start_master/0, stop_master/0]).
-export([start_slaver/0, stop_slaver/0]).


start_master() ->
    ProtoSpec = {
      ptnode_proto_ssl,
      6666,
      [{certfile, "./priv/example-pem/cert.pem"},
       {keyfile, "./priv/example-pem/key.pem"}
      ],
      [],
      []
     },
    AccepterOpts = #{
      num_acceptors => 2
     },
    ServSpec = {master_echo_server, undefined},
    ptnode:start_master(example_master, ProtoSpec, AccepterOpts,
                        ServSpec).


stop_master() -> ptnode:stop_master(example_master).


start_slaver() ->
    ProtoSpec = {
      ptnode_proto_ssl,
      "127.0.0.1",
      6666,
      [],
      infinity
     },
    ServSpec = {master_echo_server, undefined},
    ptnode:start_slaver(example_slaver, ProtoSpec, ServSpec).


stop_slaver() -> ptnode:stop_slaver(example_slaver).
