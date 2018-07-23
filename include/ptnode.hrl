-ifndef(__PTNODE_HRL).
-define(__PTNODE_HRL, 1).

-define(DATETIME_RFC3339, calendar:system_time_to_rfc3339(
                            erlang:system_time() div 1000000000)).

-define(dlog(Format, Args),
        io:format(
          lists:append("\033[0;33m~s ~p [~s:~p]~n\033[0m", Format),
          lists:append([?DATETIME_RFC3339, self(), ?FILE, ?LINE], Args))).

-endif.
