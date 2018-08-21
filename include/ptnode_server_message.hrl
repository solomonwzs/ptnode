%% @author Solomon Ng <solomon.wzs@gmail.com>

-ifndef(__PTNODE_SERVER_MESSAGE_HRL).
-define(__PTNODE_SERVER_MESSAGE_HRL, 1).

-define(MSG_REPLY_REQUEST(Req), {'$reply_request', Req}).

-define(MSG_REPLY_REQUEST(To, Req), {'$reply_request', To, Req}).

-define(MSG_REPLY_REQUEST_I(Req), {'$reply_request_i', Req}).

-define(MSG_REPLY_REQUEST_I(To, Req), {'$reply_request_i', To, Req}).

-define(MSG_CONN_STOP, '$stop').

-define(MSG_CLEAN_TIMEOUT_WAITERS, '$clean_timeout_waiters').

-define(MSG_CONN_HEARTBEAT, '$heartbeat').

-define(MSG_REG_TIMEOUT, '$reg_timeout').

-define(MSG_CONN_MASTER(Host, Port, Opts, Timeout),
        {'$conn_master', Host, Port, Opts, Timeout}).

-define(MSG_SERV_CAST(Req), {'$serv_cast', Req}).

-define(MSG_SERV_CALL(Req), {'$serv_call', Req}).

-define(MSG_SEND_DATA(Data), {'$send', Data}).

-define(MSG_NOREPLY_REQUEST(Req), {'$noreply_request', Req}).

-define(MSG_NOREPLY_REQUEST(To, Req), {'$noreply_request', To, Req}).

-define(MSG_NOREPLY_REQUEST_I(Req), {'$noreply_request_i', Req}).

-define(MSG_NOREPLY_REQUEST_I(To, Req), {'$noreply_request_i', To, Req}).

-define(MSG_REGISTER_SLAVER(Name, Pid), {'$register_slaver', Name, Pid}).

-define(MSG_UNREGISTER_SLAVER(Name), {'$unregister_slaver', Name}).

-define(MSG_GET_ALL_SLAVERS, '$get_all_slavers').

-define(MSG_GET_NODE_CONN(Name), {'$get_node_conn', Name}).

-define(MSG_GET_NODES, '$get_nodes').

-define(MSG_SERV_ALIVE(Name, Pid), {'$ser_alive', Name, Pid}).

-define(MSG_CHECK_SERV_ALIVE, '$check_serv_alive').

-endif.
