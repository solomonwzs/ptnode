%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_conn_proto).

-include("ptnode.hrl").
-include("ptnode_conn_proto.hrl").

-export([wrap_register/2,
         wrap_register_res/1,
         wrap_heartbeat/0,
         wrap_noreply_request/2,
         wrap_noreply_request/3,
         wrap_noreply_request_binary/2,
         wrap_noreply_request_binary/3,
         wrap_reply_request/4,
         wrap_reply_request/5,
         wrap_reply_request_binary/4,
         wrap_reply_request_reply/2,
         wrap_reply_request_reply/3,
         wrap_reply_request_reply_binary/2
        ]).

wrap_register(Name, Cookie) when is_atom(Name) ->
    wrap_register(atom_to_binary(Name, utf8), Cookie);
wrap_register(Name, Cookie) when is_list(Name) ->
    wrap_register(list_to_binary(Name), Cookie);
wrap_register(Name, Cookie) when is_list(Cookie) ->
    wrap_register(Name, list_to_binary(Cookie));
wrap_register(Name, Cookie)
  when is_binary(Name) andalso is_binary(Cookie) andalso
       size(Name) =< 16#ff andalso size(Cookie) =< 16#ff->
    NameLen = size(Name),
    CookieLen = size(Cookie),
    ?PROTO_P_REG(NameLen, Name, CookieLen, Cookie).


wrap_register_res(Name) when is_atom(Name) ->
    wrap_register_res(?A2B(Name));
wrap_register_res(Name) ->
    NameLen = size(Name),
    ?PROTO_P_REG_RES(NameLen, Name).


wrap_heartbeat() ->
    ?PROTO_P_HEARTBEAT.


wrap_noreply_request(Req, I) ->
    B = term_to_binary(Req),
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_MS_NOREPLY_REQUEST(BLen, B);
       I =:= internal ->
           ?PROTO_P_MS_NOREPLY_REQUEST_I(BLen, B)
    end.


wrap_noreply_request(To, Req, I) when is_atom(To) ->
    wrap_noreply_request(?A2B(To), Req, I);
wrap_noreply_request(To, Req, I) ->
    B = term_to_binary(Req),
    ToLen = size(To),
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_NOREPLY_REQUEST(ToLen, To, BLen, B);
       I =:= internal ->
           ?PROTO_P_NOREPLY_REQUEST_I(ToLen, To, BLen, B)
    end.


wrap_noreply_request_binary(B, I) ->
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_MS_NOREPLY_REQUEST(BLen, B);
       I =:= internal ->
           ?PROTO_P_MS_NOREPLY_REQUEST_I(BLen, B)
    end.

wrap_noreply_request_binary(To, B, I) when is_atom(To) ->
    wrap_noreply_request_binary(?A2B(To), B, I);
wrap_noreply_request_binary(To, B, I) ->
    ToLen = size(To),
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_NOREPLY_REQUEST(ToLen, To, BLen, B);
       I =:= internal ->
           ?PROTO_P_NOREPLY_REQUEST_I(ToLen, To, BLen, B)
    end.


wrap_reply_request_binary(ReqId, From, B, I) when is_atom(From) ->
    wrap_reply_request_binary(ReqId, ?A2B(From), B, I);
wrap_reply_request_binary(ReqId, From, B, I) ->
    FromLen = size(From),
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_MS_REPLY_REQUEST(ReqId, FromLen, From, BLen, B);
       I =:= internal ->
           ?PROTO_P_MS_REPLY_REQUEST_I(ReqId, FromLen, From, BLen, B)
    end.


wrap_reply_request(ReqId, From, Req, I) when is_atom(From) ->
    wrap_reply_request(ReqId, ?A2B(From), Req, I);
wrap_reply_request(ReqId, From, Req, I) ->
    FromLen = size(From),
    B = term_to_binary(Req),
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_MS_REPLY_REQUEST(ReqId, FromLen, From, BLen, B);
       I =:= internal ->
           ?PROTO_P_MS_REPLY_REQUEST_I(ReqId, FromLen, From, BLen, B)
    end.

wrap_reply_request(ReqId, From, To, Req, I) when is_atom(From) ->
    wrap_reply_request(ReqId, ?A2B(From), To, Req, I);
wrap_reply_request(ReqId, From, To, Req, I) when is_atom(To) ->
    wrap_reply_request(ReqId, From, ?A2B(To), Req, I);
wrap_reply_request(ReqId, From, To, Req, I) ->
    FromLen = size(From),
    ToLen = size(To),
    B = term_to_binary(Req),
    BLen = size(B),
    if I =:= external ->
           ?PROTO_P_REPLY_REQUEST(ReqId, FromLen, From, ToLen, To, BLen, B);
       I =:= internal ->
           ?PROTO_P_REPLY_REQUEST_I(ReqId, FromLen, From, ToLen, To,
                                    BLen, B)
    end.


wrap_reply_request_reply(ReqId, Reply) ->
    B = term_to_binary(Reply),
    BLen = size(B),
    ?PROTO_P_MS_REPLY_REPLY(ReqId, BLen, B).

wrap_reply_request_reply(ReqId, To, Reply) when is_atom(To) ->
    wrap_reply_request_reply(ReqId, ?A2B(To), Reply);
wrap_reply_request_reply(ReqId, To, Reply) ->
    ToLen = size(To),
    B = term_to_binary(Reply),
    BLen = size(B),
    ?PROTO_P_REPLY_REPLY(ReqId, ToLen, To, BLen, B).


wrap_reply_request_reply_binary(ReqId, B) ->
    BLen = size(B),
    ?PROTO_P_MS_REPLY_REPLY(ReqId, BLen, B).
