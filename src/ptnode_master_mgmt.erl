%% @author Solomon Ng <solomon.wzs@gmail.com>

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
-export([register_slaver/3,
         unregister_slaver/2,
         get_node_conn/2,
         get_nodes/1,
         serv_report_alive/3
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
            name = ?a2b(maps:get(name, NodeOpts)),
            cookie = maps:get(cookie, NodeOpts),
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
            Slaver = #slaver{
                        pid = Pid,
                        last_report_time = ?NOW_MILL_SECS
                       },
            NewSlavers = maps:put(Name, Slaver, Slavers),
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

handle_call({'$get_node_conn', Name}, _From,
            State = #state{name = Name}) ->
    {reply, master, State};
handle_call({'$get_node_conn', Name}, _From,
            State = #state{slavers = Slavers}) ->
    case maps:find(Name, Slavers) of
        {ok, #slaver{
                pid = Pid
               }} -> {reply, Pid, State};
        error -> {reply, {error, "slaver not exist"}, State}
    end;

handle_call('$nodes', _From,
            State = #state{
                       name = Name,
                       slavers = Slavers
                      }) ->
    SlaverList = maps:fold(
                   fun(K, _, Acc) ->
                           [{?b2a(K), slaver} | Acc]
                   end, [], Slavers),
    {reply, [{?b2a(Name), master} | SlaverList], State};

handle_call(_Req, _From, State) ->
    {reply, undefined, State}.


handle_cast({'$serv_alive', Name, Pid},
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

handle_cast('$check_serv_alive',
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


get_node_conn(MasterSupRef, Name) when is_atom(Name) ->
    get_node_conn(MasterSupRef, ?a2b(Name));
get_node_conn(MasterSupRef, Name) ->
    {ok, Mgmt} = ?get_master_mgmt(MasterSupRef),
    gen_server:call(Mgmt, {'$get_node_conn', Name}, 1000).


get_nodes(MasterSupRef) ->
    {ok, Mgmt} = ?get_master_mgmt(MasterSupRef),
    gen_server:call(Mgmt, '$nodes', 1000).


register_slaver(MasterSupRef, Name, Pid) ->
    {ok, Mgmt} = ?get_master_mgmt(MasterSupRef),
    gen_server:call(Mgmt, {'$register_slaver', Name, Pid}).


unregister_slaver(MasterSupRef, Name) ->
    case ?get_master_mgmt(MasterSupRef) of
        {ok, Mgmt} ->
            gen_server:call(Mgmt, {'$unregister_slaver', Name});
        _ -> ok
    end.


serv_report_alive(MasterSupRef, Name, Pid) ->
    case ?get_master_mgmt(MasterSupRef) of
        {ok, Mgmt} ->
            gen_server:cast(Mgmt, {'$serv_alive', Name, Pid});
        _ -> ok
    end.
