-module(helper).

-export([start/0]).


start() ->
    case node() of
        'a@127.0.0.1' -> start_master();
        _ -> start_slaver()
    end.


start_master() ->
    NodeOpts = #{
      name => node(),
      role => master,
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
    ptnode:start_node(NodeOpts, ProtoOpts, ServSpec).


start_slaver() ->
    NodeOpts = #{
      name => node(),
      role => slaver,
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
    ptnode:start_node(NodeOpts, ProtoOpts, ServSpec).
