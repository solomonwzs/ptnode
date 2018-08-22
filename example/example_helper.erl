%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(example_helper).

-export([start/0]).

-define(MASTER_SSL_PROTO_OPTS, #{
          module => ptnode_proto_ssl,
          listen_port => 6666,
          listen_opts => [{certfile, "./priv/example-pem/cert.pem"},
                          {keyfile, "./priv/example-pem/key.pem"}
                         ],
          accept_opts => [],
          handshake_opts => []
         }).

-define(MASTER_TCP_PROTO_OPTS, #{
          module => ptnode_proto_tcp,
          listen_port => 6666,
          listen_opts => [],
          accept_opts => [],
          handshake_opts => []
         }).

-define(SLAVER_SSL_PROTO_OPTS, #{
          module => ptnode_proto_ssl,
          connect_host => "127.0.0.1",
          connect_port => 6666,
          connect_opts => [],
          timeout => infinity
         }).

-define(SLAVER_TCP_PROTO_OPTS, #{
          module => ptnode_proto_tcp,
          connect_host => "127.0.0.1",
          connect_port => 6666,
          connect_opts => [],
          timeout => infinity
         }).


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
    ProtoOpts = case os:getenv("PT_PROTOCOL") of
                    "ssl" -> ?MASTER_SSL_PROTO_OPTS;
                    _ -> ?MASTER_TCP_PROTO_OPTS
                end,
    ServSpec = #{
      module => example_conn_server,
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
    ProtoOpts = case os:getenv("PT_PROTOCOL") of
                    "ssl" -> ?SLAVER_SSL_PROTO_OPTS;
                    _ -> ?SLAVER_TCP_PROTO_OPTS
                end,
    ServSpec = #{
      module => example_conn_server,
      init_args => undefined
     },
    ptnode:start_node(NodeOpts, ProtoOpts, ServSpec).
