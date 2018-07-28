-module(master_echo_server).

-behaviour(ptnode_master_server).

-export([init/0]).


init() -> {ok, #{}}.
