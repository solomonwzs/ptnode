-module(ptnode_master_sup).

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-define(ACCEPTER_SUP, ptnode_master_accepter_sup).


-spec(start_link(atom(), atom(), map())
      -> supervisor:startlink_ret()).
start_link(Name, ProtoModule, Opts) ->
    supervisor:start_link({local, Name}, ?MODULE, [ProtoModule, Opts]).


init([ProtoModule, Opts]) ->
    AccepterSup = {
      accepter_sup,
      {?ACCEPTER_SUP, start_link, [self(), ProtoModule, Opts]},
      permanent,
      5000,
      worker,
      [?ACCEPTER_SUP]},
    {ok, {{one_for_one, 5, 10}, [AccepterSup]}}.
