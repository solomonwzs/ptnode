-module(ptnode).

-include("ptnode.hrl").

-export([start/0]).
-export([start_master/3, stop_master/1]).
-export([foo/0]).


foo() ->
    start_master(a, ptnode_proto_ssl,
                 [{port, 9998},
                  {num_acceptors, 1},
                  {certfile, "./priv/example-pem/cert.pem"},
                  {keyfile, "./priv/example-pem/key.pem"}]).


start() ->
    ssl:start(),
    application:start(ptnode).


-spec(start_master(atom(), atom(), list() | map())
      -> {ok, pid()} | {error, any}).
start_master(Name, ProtoModule, Opts) when is_list(Opts) ->
    start_master(Name, ProtoModule, maps:from_list(Opts));
start_master(Name, ProtoModule, Opts) ->
    supervisor:start_child(
      ptnode_sup,
      {Name,
       {ptnode_master_sup, start_link, [Name, ProtoModule, Opts]},
       permanent,
       5000,
       supervisor,
       dynamic}).


stop_master(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, Name),
    supervisor:delete_child(ptnode_sup, Name).
