-module(ptnode_master_accepter_sup).

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).


start_link(MasterSupRef, ProtoModule, Opts) ->
    supervisor:start_link(?MODULE, [MasterSupRef, ProtoModule, Opts]).


init([MasterSupRef, ProtoModule, Opts]) ->
    NumAcceptors = maps:get(num_acceptors, Opts, 10),
    {ok, ListenSocket} = ProtoModule:listen(Opts),
    Accepters = [{
      {accepter, N},
      {ptnode_master_accepter, start_link,
       [MasterSupRef, ListenSocket, ProtoModule]},
      permanent,
      5000,
      worker,
      [ptnode_master_accepter]} || N <- lists:seq(1, NumAcceptors)],
    {ok, {{one_for_one, 5, 10}, Accepters}}.
