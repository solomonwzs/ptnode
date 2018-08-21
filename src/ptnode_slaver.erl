%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_slaver).

-include("ptnode.hrl").
-include("ptnode_server_message.hrl").

-export([get_node_conn/1,
         get_nodes/1
        ]).
-export([call/3,
         cast/2,
         call_master/3,
         cast_master/2,
         call_i/3,
         cast_i/2,
         call_master_i/3,
         cast_master_i/2
        ]).


get_node_conn(Name) when is_atom(Name) ->
    case ptnode:get_node_sup_pid(Name) of
        {ok, Pid} -> ?GET_SLAVER_CONN(Pid);
        Err = {error, _} -> Err
    end.


-spec(get_nodes(atom()) -> list() | {error, any()}).
get_nodes(Name) -> call_master_i(Name, ?MSG_GET_NODES, 1000).


-spec(call(ptnode:serv_ref(), term(), pos_integer() | infinity) -> term()).
call({Name, To}, Req, Timeout) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            Res = gen_server:call(Ref, ?MSG_REPLY_REQUEST(?B2A(To), Req),
                                  Timeout),
            ?RECEIVE_REPLY(Res, Timeout);
        Err = {error, _} -> Err
    end.


-spec(call_master(atom(), term(), pos_integer() | infinity) -> term()).
call_master(Name, Req, Timeout) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            Res = gen_server:call(Ref, ?MSG_REPLY_REQUEST(Req), Timeout),
            ?RECEIVE_REPLY(Res, Timeout);
        Err = {error, _} -> Err
    end.


-spec(call_i(ptnode:serv_ref(), term(), pos_integer() | infinity) -> term()).
call_i({Name, To}, Req, Timeout) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            Res = gen_server:call(Ref, ?MSG_REPLY_REQUEST_I(?A2B(To), Req),
                                  Timeout),
            ?RECEIVE_REPLY(Res, Timeout);
        Err = {error, _} -> Err
    end.


-spec(call_master_i(atom(), term(), pos_integer() | infinity) -> term()).
call_master_i(Name, Req, Timeout) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            Res = gen_server:call(Ref, ?MSG_REPLY_REQUEST_I(Req), Timeout),
            ?RECEIVE_REPLY(Res, Timeout);
        Err = {error, _} -> Err
    end.


-spec(cast(ptnode:serv_ref(), term()) -> ok | {error, any()}).
cast({Name, To}, Req) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            gen_server:cast(Ref, ?MSG_NOREPLY_REQUEST(?A2B(To), Req));
        Err = {error, _} -> Err
    end.


-spec(cast_master(atom(), term()) -> ok | {error, any()}).
cast_master(Name, Req) ->
    case get_node_conn(Name) of
        {ok, Ref} -> gen_server:cast(Ref, ?MSG_NOREPLY_REQUEST(Req));
        Err = {error, _} -> Err
    end.


-spec(cast_i(ptnode:serv_ref(), term()) -> ok | {error, any()}).
cast_i({Name, To}, Req) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            gen_server:cast(Ref, ?MSG_NOREPLY_REQUEST_I(?A2B(To), Req));
        Err = {error, _} -> Err
    end.


-spec(cast_master_i(atom(), term()) -> ok | {error, any()}).
cast_master_i(Name, Req) ->
    case get_node_conn(Name) of
        {ok, Ref} ->
            gen_server:cast(Ref, ?MSG_NOREPLY_REQUEST_I(Req));
        Err = {error, _} -> Err
    end.
