%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master_accepter_sup).

-include("ptnode.hrl").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).


-spec(start_link(pid(), ptnode:node_opts(), ptnode:proto_opts())
      -> pid() | {error, any()}).
start_link(MasterSupRef, NodeOpts, ProtoOpts) ->
    supervisor:start_link(
      ?MODULE, [MasterSupRef, NodeOpts, ProtoOpts]).


init(A = [MasterSupRef, NodeOpts, ProtoOpts]) ->
    NumAcceptors = maps:get(num_acceptors, NodeOpts, 10),
    ProtoModule = maps:get(module, ProtoOpts),
    Port = maps:get(listen_port, ProtoOpts),
    ListenOpts = [{reuseaddr, true}, {active, false} |
                  maps:get(listen_opts, ProtoOpts)],

    case ProtoModule:listen(Port, ListenOpts) of
        {ok, ListenSocket} ->
            ProtoModule:listen(Port, ListenOpts),
            Accepters = [{
              {accepter, N},
              {ptnode_master_accepter, start_link,
               [MasterSupRef, ListenSocket, ProtoOpts]},
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
