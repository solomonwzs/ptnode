%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([get_child/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),
        {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.


-spec(get_child(supervisor:sup_ref(), term())
      -> {ok, pid()} | {error, any()}).
get_child(SupRef, ChildId) ->
    Children = supervisor:which_children(SupRef),
    get_child0(Children, ChildId).


get_child0([], _) -> {error, not_found};
get_child0([{ID, Ref, _, _} | _], ID) -> {ok, Ref};
get_child0([_ | Tail], ID) -> get_child0(Tail, ID).
