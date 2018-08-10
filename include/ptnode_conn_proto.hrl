%% @author Solomon Ng <solomon.wzs@gmail.com>

-ifndef(__PTNODE_CONN_PROTO).
-define(__PTNODE_CONN_PROTO, 1).

-define(PROTO_VERSION, 1).

-define(PROTO_REG_TIMEOUT, 5000).

%% register
%%
%% master <-- slaver
%% +-----+-----+----------+------+------------+--------+
%% | ver | cmd | name_len | name | cookie_len | cookie |
%% +-----+-----+----------+------+------------+--------+
%% |  1  |  1  |    1     | var  |     1      |  var   |
%% +-----+-----+----------+------+------------+--------+
-define(PROTO_CMD_REG, 16#00).

%% register result
%%
%% master --> slaver
%% +-----+-----+----------+
%% | ver | cmd | res_code |
%% +-----+-----+----------+
%% |  1  |  1  |    1     |
%% +-----+-----+----------+
-define(PROTO_CMD_REG_RES, 16#01).

-define(PROTO_REG_RES_OK, 16#01).
-define(PROTO_REG_RES_ERR, 16#02).

%% heartbeat
%%
%% master <-> slaver
%% +-----+-----+
%% | ver | cmd |
%% +-----+-----+
%% |  1  |  1  |
%% +-----+-----+
-define(PROTO_CMD_HEARTBEAT, 16#02).

%% noreply-request (external term format)
%%
%% master <-- slaver
%% +-----+-----+--------+-----+---------+-----+
%% | ver | cmd | to_len | to  | req_len | req |
%% +-----+-----+--------+-----+---------+-----+
%% |  1  |  1  |    1   | var |    2    | var |
%% +-----+-----+--------+-----+---------+-----+
-define(PROTO_CMD_NOREPLY_REQUEST, 16#03).

%% reply-request (external term format)
%%
%% master <-- slaver
%% +-----+-----+--------+--------+-----+---------+-----+
%% | ver | cmd | req_id | to_len | to  | req_len | req |
%% +-----+-----+--------+--------+-----+---------+-----+
%% |  1  |  1  |    4   |    1   | var |    2    | var |
%% +-----+-----+--------+--------+-----+---------+-----+
-define(PROTO_CMD_REPLY_REQUEST, 16#04).

%% reply-request reply (external term format)
%%
%% master <-- slaver
%% +-----+-----+--------+--------+-----+-----------+-------+
%% | ver | cmd | req_id | to_len | to  | reply_len | reply |
%% +-----+-----+--------+--------+-----+-----------+-------+
%% |  1  |  1  |    4   |    1   | var |     2     |  var  |
%% +-----+-----+--------+--------+-----+-----------+-------+
-define(PROTO_CMD_REPLY_REPLY, 16#05).

%% m-s noreply-request (external term format)
%%
%% master/slaver <-> slaver
%% +-----+-----+---------+-----+
%% | ver | cmd | req_len | req |
%% +-----+-----+---------+-----+
%% |  1  |  1  |    2    | var |
%% +-----+-----+---------+-----+
-define(PROTO_CMD_MS_NOREPLY_REQUEST, 16#06).

%% m-s reply-request (external term format)
%%
%% master/slaver <-> slaver
%% +-----+-----+--------+---------+-----+
%% | ver | cmd | req_id | req_len | req |
%% +-----+-----+--------+---------+-----+
%% |  1  |  1  |    4   |    2    | var |
%% +-----+-----+--------+---------+-----+
-define(PROTO_CMD_MS_REPLY_REQUEST, 16#07).

%% m-s reply-request reply (external term format)
%%
%% master/slaver <-> slaver
%% +-----+-----+--------+-----------+-------+
%% | ver | cmd | req_id | reply_len | reply |
%% +-----+-----+--------+-----------+-------+
%% |  1  |  1  |    4   |     2     |  var  |
%% +-----+-----+--------+-----------+-------+
-define(PROTO_CMD_MS_REPLY_REPLY, 16#08).

-define(PROTO_TERM_LEN_BITS, 16).

-endif.
