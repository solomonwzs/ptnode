-module(ptnode_master_accepter_sup).

-include("ptnode.hrl").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).


start_link(MasterSupRef, ProtoSpec, AccepterOpts) ->
    supervisor:start_link(?MODULE, [MasterSupRef, ProtoSpec, AccepterOpts]).


init(A = [MasterSupRef,
          Spec = {ProtoModule, Port, ListenOpts, _, _},
          AccepterOpts]) ->
    NumAcceptors = maps:get(num_acceptors, AccepterOpts, 10),
    case ProtoModule:listen(Port, ListenOpts ++ [{reuseaddr, true}]) of
        {ok, ListenSocket} ->
            ProtoModule:listen(Port, ListenOpts),
            Accepters = [{
              {accepter, N},
              {ptnode_master_accepter, start_link,
               [MasterSupRef, ListenSocket, Spec]},
              permanent,
              5000,
              worker,
              [ptnode_master_accepter]} ||
                         N <- lists:seq(1, NumAcceptors)],
            {ok, {{one_for_one, 5, 10}, Accepters}};
        Err ->
            ?dlog("~p~n", [Err]),
            timer:sleep(1000),
            init(A)
    end.
