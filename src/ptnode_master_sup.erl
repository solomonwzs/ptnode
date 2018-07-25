-module(ptnode_master_sup).

-include("ptnode.hrl").

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).
-export([get_conn_sup/1,
         get_all_conns/1]).


start_link(Name, ProtoSpec, AccepterOpts, Serv) ->
    supervisor:start_link({local, Name}, ?MODULE,
                          [ProtoSpec, AccepterOpts, Serv]).


-spec(get_conn_sup(supervisor:sup_ref())
      -> {ok, pid()} | {error, any()}).
get_conn_sup(MasterSupRef) ->
    Children = supervisor:which_children(MasterSupRef),
    get_conn_sup0(Children).


get_conn_sup0([]) -> {error, "no supervisors found"};
get_conn_sup0([{conn_sup, Ref, supervisor, _} | _]) -> {ok, Ref};
get_conn_sup0([_ | Tail]) -> get_conn_sup0(Tail).


-spec(get_all_conns(supervisor:sup_ref())
      -> {ok, list()} | {error, any()}).
get_all_conns(MasterSupRef) ->
    case get_conn_sup(MasterSupRef) of
        {ok, Ref} -> {ok, supervisor:which_children(Ref)};
        Err -> Err
    end.


init([ProtoSpec, AccepterOpts, Serv]) ->
    AccepterSup = {
      accepter_sup,
      {ptnode_master_accepter_sup, start_link,
       [self(), ProtoSpec, AccepterOpts]},
      permanent,
      5000,
      supervisor,
      [ptnode_master_accepter_sup]},
    ConnSup = {
      conn_sup,
      {ptnode_master_conn_sup, start_link, [Serv]},
      permanent,
      5000,
      supervisor,
      [ptnode_conn_sup]
     },
    {ok, {{one_for_one, 5, 10}, [AccepterSup, ConnSup]}}.
