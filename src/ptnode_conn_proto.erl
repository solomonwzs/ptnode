%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_conn_proto).

-include("ptnode.hrl").
-include("ptnode_conn_proto.hrl").

-export([wrap_register_cmd/2,
         wrap_register_res_cmd/1,
         wrap_heartbeat_cmd/0,
         wrap_noreply_request/1,
         wrap_noreply_request/2,
         wrap_noreply_request_binary/1,
         wrap_noreply_request_binary/2,
         wrap_reply_request/2,
         wrap_reply_request/3,
         wrap_reply_request_reply/2
        ]).

wrap_register_cmd(Name, Cookie) when is_atom(Name) ->
    wrap_register_cmd(atom_to_binary(Name, utf8), Cookie);
wrap_register_cmd(Name, Cookie) when is_list(Name) ->
    wrap_register_cmd(list_to_binary(Name), Cookie);
wrap_register_cmd(Name, Cookie) when is_list(Cookie) ->
    wrap_register_cmd(Name, list_to_binary(Cookie));
wrap_register_cmd(Name, Cookie)
  when is_binary(Name) andalso is_binary(Cookie) andalso
       size(Name) =< 16#ff andalso size(Cookie) =< 16#ff->
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_REG:8/unsigned-little,
      (size(Name)):8/unsigned-little,
      Name/binary,
      (size(Cookie)):8/unsigned-little,
      Cookie/binary
    >>.


wrap_register_res_cmd(ResCode)
  when ResCode =:= ?PROTO_REG_RES_OK orelse
       ResCode =:= ?PROTO_REG_RES_ERR ->
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_REG_RES:8/unsigned-little,
      ResCode:8/unsigned-little
    >>.


wrap_heartbeat_cmd() ->
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_HEARTBEAT:8/unsigned-little
    >>.


wrap_noreply_request(To, Req) when is_atom(To) ->
    wrap_noreply_request(?a2b(To), Req);
wrap_noreply_request(To, Req) ->
    B = term_to_binary(Req),
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_NOREPLY_REQUEST:8/unsigned-little,
      (size(To)):8/unsigned-little,
      To/binary,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.


wrap_noreply_request_binary(To, B) when is_atom(To) ->
    wrap_noreply_request_binary(?a2b(To), B);
wrap_noreply_request_binary(To, B) ->
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_NOREPLY_REQUEST:8/unsigned-little,
      (size(To)):8/unsigned-little,
      To/binary,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.


wrap_noreply_request(Req) ->
    B = term_to_binary(Req),
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_MS_NOREPLY_REQUEST:8/unsigned-little,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.


wrap_noreply_request_binary(B) ->
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_MS_NOREPLY_REQUEST:8/unsigned-little,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.


wrap_reply_request(ReqId, Req) ->
    B = term_to_binary(Req),
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_MS_REPLY_REQUEST:8/unsigned-little,
      ReqId:32/unsigned-little,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.


wrap_reply_request(To, ReqId, Req) when is_atom(To) ->
    wrap_reply_request(?a2b(To), ReqId, Req);
wrap_reply_request(To, ReqId, Req) ->
    B = term_to_binary(Req),
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_REPLY_REQUEST:8/unsigned-little,
      ReqId:32/unsigned-little,
      (size(To)):8/unsigned-little,
      To/binary,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.


wrap_reply_request_reply(ReqId, Reply) ->
    B = term_to_binary(Reply),
    <<?PROTO_VERSION:8/unsigned-little,
      ?PROTO_CMD_MS_REPLY_REPLY:8/unsigned-little,
      ReqId:32/unsigned-little,
      (size(B)):?PROTO_TERM_LEN_BITS/unsigned-little,
      B/binary
    >>.
