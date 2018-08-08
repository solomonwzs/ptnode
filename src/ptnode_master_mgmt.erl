-module(ptnode_master_mgmt).

-include("ptnode.hrl").

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         code_change/3,
         format_status/2,
         terminate/2
        ]).

-record(state, {
          name          ::bitstring(),
          cookie        ::bitstring(),
          master_sup    ::pid(),
          slavers       ::map()
         }).


start_link(MasterSupRef, NodeInfo) ->
    gen_server:start_link(?MODULE, [MasterSupRef, NodeInfo], []).


init([MasterSupRef, {Name, Cookie}]) ->
    {ok, #state{
            name = Name,
            cookie = Cookie,
            master_sup = MasterSupRef,
            slavers = #{}
           }}.


handle_call({'$register_slaver', Name, Pid}, _From,
            State = #state{
                       slavers = Slavers
                      }) ->
    case maps:find(Name, Slavers) of
        {ok, _} -> {reply, {error, "slaver exist"}, State};
        error ->
            NewSlavers = maps:put(Name, Pid, Slavers),
            {reply, ok, State#state{slavers = NewSlavers}}
    end;
handle_call({'$unregister_slaver', Name}, _From,
            State = #state{
                       slavers = Slavers
                      }) ->
    case maps:find(Name, Slavers) of
        {ok, _} ->
            NewSlavers = maps:remove(Name, Slavers),
            {reply, ok, State#state{slavers = NewSlavers}};
        error -> {reply, {error, "slaver not exist"}, State}
    end;
handle_call('$get_all_slavers', _From, State) ->
    {reply, State#state.slavers, State};
handle_call({'$get_slaver', Name}, _From,
            State = #state{
                       slavers = Slavers
                      }) ->
    case maps:find(Name, Slavers) of
        {ok, Pid} -> {reply, Pid, State};
        error -> {reply, {error, "slaver not exist"}, State}
    end;
handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(_Req, State) ->
    {noreply, State}.


handle_continue(_Continue, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


format_status(_Opt, [_PDict, _State]) ->
    ok.


terminate(_Reason, _State) ->
    ok.
