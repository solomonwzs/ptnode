-module(ptnode).

-include("ptnode.hrl").

-export([start/0]).
-export([start_master/2, stop_master/1]).
-export([foo/0]).


foo() ->
    start_master(a, [{port, 9998},
                     {certfile, "./priv/example-pem/cert.pem"},
                     {keyfile, "./priv/example-pem/key.pem"}]).


start() ->
    application:start(ptnode).


-spec(start_master(atom(), list())
      -> {ok, pid()} | {error, any}).
start_master(Name, Opts) ->
    supervisor:start_child(
      ptnode_sup,
      {Name,
       {ptnode_master_sup, start_link, [Name, Opts]},
       permanent,
       5000,
       supervisor,
       dynamic}).


stop_master(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, Name),
    supervisor:delete_child(ptnode_sup, Name).
