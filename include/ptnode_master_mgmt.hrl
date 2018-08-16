%% @author Solomon Ng <solomon.wzs@gmail.com>

-ifndef(__PTNODE_MASTER_MGMT_HRL).
-define(__PTNODE_MASTER_MGMT_HRL, 1).

-define(MSG_REGISTER_SLAVER(Name, Pid), {'$register_slaver', Name, Pid}).

-define(MSG_UNREGISTER_SLAVER(Name), {'$unregister_slaver', Name}).

-define(MSG_GET_ALL_SLAVERS, '$get_all_slavers').

-define(MSG_GET_NODE_CONN(Name), {'$get_node_conn', Name}).

-define(MSG_GET_NODES, '$get_nodes').

-define(MSG_SERV_ALIVE(Name, Pid), {'$ser_alive', Name, Pid}).

-define(MSG_CHECK_SERV_ALIVE, '$check_serv_alive').

-endif.
