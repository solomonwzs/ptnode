-module(ptnode_master_sup).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).


start_link(Name, Opts) ->
    supervisor:start_link({local, Name}, ?MODULE, [Opts]).


init([Opts]) ->
    Accepter = {ptnode_master_accepter,
                {ptnode_master_accepter, start_link, [Opts]},
                permanent,
                5000,
                worker,
                dynamic},
    {ok, {{one_for_one, 5, 10},
          [Accepter]}}.
