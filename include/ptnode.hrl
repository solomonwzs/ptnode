%% @author Solomon Ng <solomon.wzs@gmail.com>

-ifndef(__PTNODE_HRL).
-define(__PTNODE_HRL, 1).

-define(DATETIME_RFC3339, calendar:system_time_to_rfc3339(
                            erlang:system_time() div 1000000000)).

-define(dlog(Format, Args),
        io:format(
          lists:append("\033[0;33m~p [~s:~p]~n\033[0m", Format),
          lists:append([self(), ?FILE, ?LINE], Args))).

-define(MASTER_SUP_RESTART_INTENSITY, 5).
-define(MASTER_SUP_RESTART_PERIOD, 10).

-define(b2a(Name), binary_to_atom(Name, utf8)).
-define(a2b(Name), atom_to_binary(Name, utf8)).

-define(MASTER_MGMT_ID, '$mgmt').
-define(MASTER_CONN_SUP_ID, '$conn_sup').
-define(MASTER_ACCEPTER_SUP_ID, '$accepter_sup').
-define(SLAVER_CONN_ID, '$slaver').

-define(NOW_SECS, erlang:system_time(second)).
-define(NOW_MILL_SECS, erlang:system_time(millisecond)).

-define(get_master_mgmt(MasterSupRef),
        ptnode_sup:get_child(MasterSupRef, ?MASTER_MGMT_ID)).

-define(get_master_conn_sup(MasterSupRef),
        ptnode_sup:get_child(MasterSupRef, ?MASTER_CONN_SUP_ID)).

-define(get_slaver_conn(SlaverSupRef),
        ptnode_sup:get_child(SlaverSupRef, ?SLAVER_CONN_ID)).

-endif.
