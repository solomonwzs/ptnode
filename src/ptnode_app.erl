%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_app).

-include("ptnode.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ptnode_sup:start_link().

stop(_State) ->
    ok.
