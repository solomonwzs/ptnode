-module(helper).

-export([start_master/0, stop_master/0]).
-export([start_slaver/0, stop_slaver/0]).


start_master() ->
    NodeOpts = #{
      name => example_master,
      cookie => <<"cookie12345">>,
      num_acceptors => 2,
      named_node => true
     },
    ProtoOpts = #{
      module => ptnode_proto_ssl,
      listen_port => 6666,
      listen_opts => [{certfile, "./priv/example-pem/cert.pem"},
                      {keyfile, "./priv/example-pem/key.pem"}
                     ],
      accept_opts => [],
      handshake_opts => []
     },
    ServSpec = #{
      module => master_echo_server,
      init_args => undefined
     },
    ptnode:start_master(NodeOpts, ProtoOpts, ServSpec).


stop_master() -> ptnode:stop_master(example_master).


start_slaver() ->
    NodeOpts = #{
      name => example_slaver,
      cookie => <<"cookie12345">>,
      named_node => true
     },
    ProtoOpts = #{
      module => ptnode_proto_ssl,
      connect_host => "127.0.0.1",
      connect_port => 6666,
      connect_opts => [],
      timeout => infinity
     },
    ServSpec = #{
      module => master_echo_server,
      init_args => undefined
     },
    ptnode:start_slaver(NodeOpts, ProtoOpts, ServSpec).


stop_slaver() -> ptnode:stop_slaver(example_slaver).
