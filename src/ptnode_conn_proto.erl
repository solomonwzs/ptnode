-module(ptnode_conn_proto).

-include("ptnode_conn_proto.hrl").

-export([wrap_register_cmd/2,
         wrap_register_res_cmd/1
        ]).


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
