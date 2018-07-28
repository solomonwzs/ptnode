-module(helper).

-export([start_master/0, stop_master/0]).


start_master() ->
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
    ptnode:start_master(example_master, ProtoSpec, AccepterOpts,
                        master_echo_server).


stop_master() -> ptnode:stop_master(example_master).
