%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master_mgmt).

-include("ptnode.hrl").
-include("ptnode_master_mgmt.hrl").

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

-record(slaver, {
          pid               :: pid(),
          last_report_time  :: integer()
         }).

-record(state, {
          name          :: bitstring(),
          cookie        :: bitstring(),
          master_sup    :: pid(),
          slavers       :: map()
         }).


-spec(start_link(pid(), ptnode:node_opts()) -> pid() | {error, any()}).
start_link(MasterSupRef, NodeOpts) ->
    gen_server:start_link(?MODULE, [MasterSupRef, NodeOpts], []).


init([MasterSupRef, NodeOpts]) ->
    {ok, _} = timer:apply_interval(
                ?SERV_REPORT_ALIVE_INTERVAL,
                gen_server, cast, [self(), '$check_serv_alive']),
    {ok, #state{
            name = ?A2B(maps:get(name, NodeOpts)),
            cookie = maps:get(cookie, NodeOpts),
            master_sup = MasterSupRef,
            slavers = #{}
           }}.


handle_call(?MSG_REGISTER_SLAVER(Name, Pid), _From,
            State = #state{
                       slavers = Slavers
                      }) ->
    case maps:find(Name, Slavers) of
        {ok, _} -> {reply, {error, "slaver exist"}, State};
        error ->
            Slaver = #slaver{
                        pid = Pid,
                        last_report_time = ?NOW_MILL_SECS
                       },
            NewSlavers = maps:put(Name, Slaver, Slavers),
            {reply, ok, State#state{slavers = NewSlavers}}
    end;

handle_call(?MSG_UNREGISTER_SLAVER(Name), _From,
            State = #state{
                       slavers = Slavers
                      }) ->
    case maps:find(Name, Slavers) of
        {ok, _} ->
            NewSlavers = maps:remove(Name, Slavers),
            {reply, ok, State#state{slavers = NewSlavers}};
        error -> {reply, {error, "slaver not exist"}, State}
    end;

handle_call(?MSG_GET_ALL_SLAVERS, _From, State) ->
    {reply, State#state.slavers, State};

handle_call(?MSG_GET_NODE_CONN(Name), _From,
            State = #state{name = Name}) ->
    {reply, {error, master}, State};
handle_call(?MSG_GET_NODE_CONN(Name), _From,
            State = #state{slavers = Slavers}) ->
    case maps:find(Name, Slavers) of
        {ok, #slaver{
                pid = Pid
               }} -> {reply, {ok, Pid}, State};
        error -> {reply, {error, slaver_not_exist}, State}
    end;

handle_call(?MSG_GET_NODES, _From,
            State = #state{
                       name = Name,
                       slavers = Slavers
                      }) ->
    SlaverList = maps:fold(
                   fun(K, _, Acc) ->
                           [{?B2A(K), slaver} | Acc]
                   end, [], Slavers),
    {reply, [{?B2A(Name), master} | SlaverList], State};

handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast(?MSG_SERV_ALIVE(Name, Pid),
            State = #state{
                       slavers = Slavers
                      }) ->
    case maps:find(Name, Slavers) of
        error -> {noreply, State};
        {ok, _} ->
            Slaver = #slaver{
                        pid = Pid,
                        last_report_time = ?NOW_MILL_SECS
                       },
            NewSlavers = maps:put(Name, Slaver, Slavers),
            {noreply, State#state{slavers = NewSlavers}}
    end;

handle_cast(?MSG_CHECK_SERV_ALIVE,
            State = #state{
                       slavers = Slavers
                      }) ->
    Now = ?NOW_MILL_SECS,
    Timeout = ?SERV_REPORT_ALIVE_INTERVAL * 2,
    Func = fun(Name, #slaver{
                        last_report_time = Time
                       }, Names) ->
                   if Now - Time > Timeout -> [Name | Names];
                      true -> Names
                   end
           end,
    InvalidNames = maps:fold(Func, [], Slavers),
    NewSlavers = lists:foldl(fun maps:remove/2, Slavers, InvalidNames),
    {noreply, State#state{slavers = NewSlavers}};

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
