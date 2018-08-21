%% @author Solomon Ng <solomon.wzs@gmail.com>

-ifndef(__PTNODE_HRL).
-define(__PTNODE_HRL, 1).

-define(DATETIME_RFC3339, calendar:system_time_to_rfc3339(
                            erlang:system_time() div 1000000000)).

-define(DLOG(Format, Args),
        io:format(
          lists:append("\033[0;33m~p [~s:~p]~n\033[0m", Format),
          lists:append([self(), ?FILE, ?LINE], Args))).

-define(MASTER_SUP_RESTART_INTENSITY, 5).
-define(MASTER_SUP_RESTART_PERIOD, 10).

-define(B2A(Name), binary_to_atom(Name, utf8)).
-define(A2B(Name), atom_to_binary(Name, utf8)).

-define(MASTER_MGMT_ID, '$mgmt').
-define(MASTER_CONN_SUP_ID, '$conn_sup').
-define(MASTER_ACCEPTER_SUP_ID, '$accepter_sup').
-define(SLAVER_CONN_ID, '$slaver').

-define(NOW_SECS, erlang:system_time(second)).
-define(NOW_MILL_SECS, erlang:system_time(millisecond)).

-define(GET_MASTER_MGMT(MasterSupRef),
        ptnode_sup:get_child(MasterSupRef, ?MASTER_MGMT_ID)).

-define(GET_MASTER_CONN_SUP(MasterSupRef),
        ptnode_sup:get_child(MasterSupRef, ?MASTER_CONN_SUP_ID)).

-define(GET_SLAVER_CONN(SlaverSupRef),
        ptnode_sup:get_child(SlaverSupRef, ?SLAVER_CONN_ID)).

-define(SERV_REPORT_ALIVE_INTERVAL, 2000).

-define(RECEIVE_REPLY(Res, Timeout),
        case Res of
            {ok, Mark} ->
                receive
                    {Mark, Reply} -> Reply
                after Timeout -> {error, timeout}
                end;
            Err = {error, _} -> Err
        end).

-define(CALL_MGMT(MasterSupRef, Message),
        case ?GET_MASTER_MGMT(MasterSupRef) of
            {ok, Mgmt} ->
                gen_server:call(Mgmt, Message, 1000);
            Err = {error, _} -> Err
        end).

-define(CAST_MGMT(MasterSupRef, Message),
        case ?GET_MASTER_MGMT(MasterSupRef) of
            {ok, Mgmt} ->
                gen_server:cast(Mgmt, Message);
            Err = {error, _} -> Err
        end).

-endif.
