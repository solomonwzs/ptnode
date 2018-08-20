%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_slaver).

-include("ptnode.hrl").

-export([get_node_conn/1]).
-export([call/3,
         cast/2,
         cast_i/2
        ]).


get_node_conn(Name) when is_atom(Name) ->
    case ptnode:get_node_sup_pid(Name) of
        {ok, Pid} -> ?GET_SLAVER_CONN(Pid);
        Err = {error, _} -> Err
    end.


-spec(call(ptnode:serv_ref(), term(), pos_integer() | infinity) -> term()).
call({Name, To}, Req, Timeout) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            Res = ptnode_conn_server:reply_request(Ref, To, Req, Timeout),
            ?RECEIVE_REPLY(Res, Timeout);
        Err = {error, _} -> Err
    end.


-spec(cast(ptnode:serv_ref(), term()) -> ok | {error, any()}).
cast({Name, To}, Req) ->
    case get_node_conn(Name) of
        {ok, Ref} -> ptnode_conn_server:noreply_request(Ref, To, Req);
        Err = {error, _} -> Err
    end.


-spec(cast_i(ptnode:serv_ref(), term()) -> ok | {error, any()}).
cast_i({Name, To}, Req) ->
    case get_node_conn(Name) of
        {ok, Ref} -> ptnode_conn_server:noreply_request_i(Ref, To, Req);
        Err = {error, _} -> Err
    end.
