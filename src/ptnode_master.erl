%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_master).

-include("ptnode.hrl").
-include("ptnode_master_mgmt.hrl").

-export([register_slaver/3,
         unregister_slaver/2,
         get_node_conn/1,
         get_node_conn/2,
         get_nodes/1,
         serv_report_alive/3
        ]).
-export([call/3,
         cast/2,
         cast_i/2
        ]).

-define(CALL_MGMT(MasterSupRef, Message),
        case ?GET_MASTER_MGMT(MasterSupRef) of
            {ok, Mgmt} ->
                gen_server:call(Mgmt, Message, 1000);
            Err = {error, _} -> Err
        end).

-define(CAST_MGMT(MasterSupRef, Message),
        case ?GET_MASTER_MGMT(MasterSupRef) of
            {ok, Mgmt} ->
                gen_server:cast(Mgmt, Message);
            Err = {error, _} -> Err
        end).


-spec(get_node_conn(atom() | pid(), atom() | bitstring())
      -> {ok, pid()} | {error, any()}).
get_node_conn(MasterSupRef, Name) when is_atom(Name) ->
    get_node_conn(MasterSupRef, ?A2B(Name));
get_node_conn(MasterSupRef, Name) ->
    ?CALL_MGMT(MasterSupRef, ?MSG_GET_NODE_CONN(Name)).


-spec(get_node_conn(ptnode:serv_ref()) -> {ok, pid()} | {error, any()}).
get_node_conn({Name, To}) ->
    case ptnode:get_node_sup_pid(Name) of
        {ok, Pid} -> get_node_conn(Pid, To);
        Err = {error, _} -> Err
    end.


-spec(get_nodes(atom() | pid()) -> list() | {error, any()}).
get_nodes(MasterSupRef) ->
    ?CALL_MGMT(MasterSupRef, ?MSG_GET_NODES).


-spec(register_slaver(atom() | pid(), bitstring(), pid())
      -> ok | {error, any()}).
register_slaver(MasterSupRef, Name, Pid) ->
    ?CALL_MGMT(MasterSupRef, ?MSG_REGISTER_SLAVER(Name, Pid)).


-spec(unregister_slaver(atom() | pid(), bitstring())
      -> ok | {error, any()}).
unregister_slaver(MasterSupRef, Name) ->
    ?CALL_MGMT(MasterSupRef, ?MSG_UNREGISTER_SLAVER(Name)).


-spec(serv_report_alive(atom() | pid(), bitstring(), pid())
      -> ok | {error, any()}).
serv_report_alive(MasterSupRef, Name, Pid) ->
    ?CAST_MGMT(MasterSupRef, ?MSG_SERV_ALIVE(Name, Pid)).


-spec(call(ptnode:serv_ref(), term(), pos_integer() | infinity) -> term()).
call(ServerRef, Req, Timeout) ->
    case get_node_conn(ServerRef) of
        {ok, Ref} ->
            Res = ptnode_conn_server:reply_request(Ref, Req, Timeout),
            ?RECEIVE_REPLY(Res, Timeout);
        Err = {error, _} -> Err
    end.


-spec(cast(ptnode:serv_ref(), term()) -> ok | {error, any()}).
cast(ServerRef, Req) ->
    case get_node_conn(ServerRef) of
        {ok, Ref} -> ptnode_conn_server:noreply_request(Ref, Req);
        Err = {error, _} -> Err
    end.


-spec(cast_i(ptnode:serv_ref(), term()) -> ok | {error, any()}).
cast_i(ServerRef, Req) ->
    case get_node_conn(ServerRef) of
        {ok, Ref} -> ptnode_conn_server:noreply_request_i(Ref, Req);
        Err = {error, _} -> Err
    end.
