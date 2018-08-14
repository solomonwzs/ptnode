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
-define(PROTO_P_REG(NameLen, Name, CookieLen, Cookie),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_REG:8/unsigned-little,
          NameLen:8/unsigned-little,
          Name:NameLen/binary,
          CookieLen:8/unsigned-little,
          Cookie:CookieLen/binary
        >>).

%% register result
%%
%% master --> slaver
%% +-----+-----+----------+------+
%% | ver | cmd | name_len | name |
%% +-----+-----+----------+------+
%% |  1  |  1  |    1     | var  |
%% +-----+-----+----------+------+
-define(PROTO_CMD_REG_RES, 16#01).
-define(PROTO_P_REG_RES(NameLen, Name),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_REG_RES:8/unsigned-little,
          NameLen:8/unsigned-little,
          Name:NameLen/binary
        >>).

%% heartbeat
%%
%% master <-> slaver
%% +-----+-----+
%% | ver | cmd |
%% +-----+-----+
%% |  1  |  1  |
%% +-----+-----+
-define(PROTO_CMD_HEARTBEAT, 16#02).
-define(PROTO_P_HEARTBEAT,
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_HEARTBEAT:8/unsigned-little
        >>).

%% noreply-request (external term format)
%%
%% master <-- slaver
%% +-----+-----+--------+-----+---------+-----+
%% | ver | cmd | to_len | to  | req_len | req |
%% +-----+-----+--------+-----+---------+-----+
%% |  1  |  1  |    1   | var |    2    | var |
%% +-----+-----+--------+-----+---------+-----+
-define(PROTO_CMD_NOREPLY_REQUEST, 16#03).
-define(PROTO_P_NOREPLY_REQUEST(ToLen, To, ReqLen, Req),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_NOREPLY_REQUEST:8/unsigned-little,
          ToLen:8/unsigned-little,
          To:ToLen/binary,
          ReqLen:16/unsigned-little,
          Req:ReqLen/binary
        >>).

%% reply-request (external term format)
%%
%% master <-- slaver
%% +-----+-----+--------+----------+------+--------+-----+---------+-----+
%% | ver | cmd | req_id | from_len | from | to_len | to  | req_len | req |
%% +-----+-----+--------+----------+------+--------+-----+---------+-----+
%% |  1  |  1  |    4   |    1     |  var |   1    | var |    2    | var |
%% +-----+-----+--------+----------+------+--------+-----+---------+-----+
-define(PROTO_CMD_REPLY_REQUEST, 16#04).
-define(PROTO_P_REPLY_REQUEST(ReqId, FromLen, From, ToLen, To, ReqLen, Req),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_REPLY_REQUEST:8/unsigned-little,
          ReqId:32/unsigned-little,
          FromLen:8/unsigned-little,
          From:FromLen/binary,
          ToLen:8/unsigned-little,
          To:ToLen/binary,
          ReqLen:16/unsigned-little,
          Req:ReqLen/binary
        >>).

%% reply-request reply (external term format)
%%
%% master <-- slaver
%% +-----+-----+--------+--------+-----+-----------+-------+
%% | ver | cmd | req_id | to_len | to  | reply_len | reply |
%% +-----+-----+--------+--------+-----+-----------+-------+
%% |  1  |  1  |    4   |    1   | var |     2     |  var  |
%% +-----+-----+--------+--------+-----+-----------+-------+
-define(PROTO_CMD_REPLY_REPLY, 16#05).
-define(PROTO_P_REPLY_REPLY(ReqId, ToLen, To, ReplyLen, Reply),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_REPLY_REPLY:8/unsigned-little,
          ReqId:32/unsigned-little,
          ToLen:8/unsigned-little,
          To:ToLen/binary,
          ReplyLen:16/unsigned-little,
          Reply:ReplyLen/binary
        >>).

%% m-s noreply-request (external term format)
%%
%% master/slaver <-> slaver
%% +-----+-----+---------+-----+
%% | ver | cmd | req_len | req |
%% +-----+-----+---------+-----+
%% |  1  |  1  |    2    | var |
%% +-----+-----+---------+-----+
-define(PROTO_CMD_MS_NOREPLY_REQUEST, 16#06).
-define(PROTO_P_MS_NOREPLY_REQUEST(ReqLen, Req),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_MS_NOREPLY_REQUEST:8/unsigned-little,
          ReqLen:16/unsigned-little,
          Req:ReqLen/binary
        >>).

%% m-s reply-request (external term format)
%%
%% master/slaver <-> slaver
%% +-----+-----+--------+----------+------+---------+-----+
%% | ver | cmd | req_id | from_len | from | req_len | req |
%% +-----+-----+--------+----------+------+---------+-----+
%% |  1  |  1  |    4   |    1     |  var |    2    | var |
%% +-----+-----+--------+----------+------+---------+-----+
-define(PROTO_CMD_MS_REPLY_REQUEST, 16#07).
-define(PROTO_P_MS_REPLY_REQUEST(ReqId, FromLen, From, ReqLen, Req),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_MS_REPLY_REQUEST:8/unsigned-little,
          ReqId:32/unsigned-little,
          FromLen:8/unsigned-little,
          From:FromLen/binary,
          ReqLen:16/unsigned-little,
          Req:ReqLen/binary
        >>).

%% m-s reply-request reply (external term format)
%%
%% master/slaver <-> slaver
%% +-----+-----+--------+-----------+-------+
%% | ver | cmd | req_id | reply_len | reply |
%% +-----+-----+--------+-----------+-------+
%% |  1  |  1  |    4   |     2     |  var  |
%% +-----+-----+--------+-----------+-------+
-define(PROTO_CMD_MS_REPLY_REPLY, 16#08).
-define(PROTO_P_MS_REPLY_REPLY(ReqId, ReplyLen, Reply),
        <<?PROTO_VERSION:8/unsigned-little,
          ?PROTO_CMD_MS_REPLY_REPLY:8/unsigned-little,
          ReqId:32/unsigned-little,
          ReplyLen:16/unsigned-little,
          Reply:ReplyLen/binary
        >>).

-define(PROTO_MAX_REQ_ID, 16#ffffffff).

-endif.
