%% @author Solomon Ng <solomon.wzs@gmail.com>

-ifndef(__PTNODE_CONN_SERVER_HRL).
-define(__PTNODE_CONN_SERVER_HRL, 1).

-define(MSG_REPLY_REQUEST(Req), {'$reply_request', Req}).

-define(MSG_REPLY_REQUEST(To, Req), {'$reply_request', To, Req}).

-define(MSG_CONN_STOP, '$stop').

-define(MSG_CLEAN_TIMEOUT_WAITERS, '$clean_timeout_waiters').

-define(MSG_CONN_HEARTBEAT, '$heartbeat').

-define(MSG_REG_TIMEOUT, '$reg_timeout').

-define(MSG_CONN_MASTER(Host, Port, ConnOpts, Timeout),
        {'$conn_master', Host, Port, ConnOpts, Timeout}).

-define(MSG_SERV_CAST(Req), {'$serv_cast', Req}).

-define(MSG_SERV_CALL(Req), {'$serv_call', Req}).

-define(MSG_SEND_DATA(Data), {'$send', Data}).

-define(MSG_NOREPLY_REQUEST(Req), {'$noreply_request', Req}).

-define(MSG_NOREPLY_REQUEST(To, Req), {'$noreply_request', To, Req}).

-endif.
