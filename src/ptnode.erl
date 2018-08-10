%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode).

-include("ptnode.hrl").

-type node_opts() :: #{
        name => atom(),
        role => master | slaver,
        cookie => bitstring(),
        num_acceptors => pos_integer(),
        named_node => boolean()
       }.
-export_type([node_opts/0]).

-type proto_opts() :: #{
        module => module(),
        listen_port => pos_integer(),
        listen_opts => list(),
        accept_opts => list(),
        handshake_opts => list(),
        connect_host => ptnode_proto:host(),
        connect_port => pos_integer(),
        connect_opts => list(),
        timeout => pos_integer() | infinity
       }.
-export_type([proto_opts/0]).

-type serv_spec() :: #{
        module => module(),
        init_args => any()
       }.
-export_type([serv_spec/0]).

-export([start/0, stop/0]).
-export([start_node/3, stop_node/1]).
-export([node_role/1,
         get_nodes/1,
         cast/3
        ]).


start() -> application:start(ptnode).


stop() ->
    application:stop(ptnode).


try_delete_child(ChildId) ->
    case supervisor:delete_child(ptnode_sup, ChildId) of
        ok -> ok;
        {error, not_found} -> ok;
        Err = {error, _} -> Err
    end.


node_role(Name) ->
    case supervisor:get_childspec(ptnode_sup, {'$node', Name}) of
        {ok, ChildSpec} ->
            {SupModule, _, _} =
            if is_tuple(ChildSpec) -> element(2, ChildSpec);
               is_map(ChildSpec) -> maps:get(start, ChildSpec)
            end,
            if SupModule =:= ptnode_master_sup -> master;
               SupModule =:= ptnode_slaver_sup -> slaver
            end;
        Err = {error, _} -> Err
    end.


-spec(start_node(node_opts(), proto_opts(), serv_spec())
      -> {ok, pid()} | {error, any()}).
start_node(NodeOpts, ProtoOpts, ServSpec) ->
    SupModule = case maps:get(role, NodeOpts) of
                    master -> ptnode_master_sup;
                    slaver -> ptnode_slaver_sup
                end,
    ChildId = {'$node', maps:get(name, NodeOpts)},
    case try_delete_child(ChildId) of
        ok ->
            supervisor:start_child(
              ptnode_sup,
              {ChildId,
               {SupModule, start_link,
                [NodeOpts, ProtoOpts, ServSpec]},
               transient,
               5000,
               supervisor,
               [SupModule]});
        Err = {error, _} -> Err
    end.


-spec(stop_node(atom()) -> ok | {error, any()}).
stop_node(Name) ->
    ok = supervisor:terminate_child(ptnode_sup, {'$node', Name}),
    supervisor:delete_child(ptnode_sup, {'$node', Name}).


get_node_sup_pid(Name) when is_atom(Name) ->
    ChildId = {'$node', Name},
    Children = supervisor:which_children(ptnode_sup),
    case lists:keyfind(ChildId, 1, Children) of
        {_, Pid, supervisor, _} -> {ok, Pid};
        _ -> {error, not_found}
    end.


% get_mgmt(Name) when is_atom(Name) ->
%     case get_node_sup_pid(Name) of
%         {ok, Pid} -> ptnode_master_sup:get_mgmt(Pid);
%         Err = {error, _} -> Err
%     end.


-spec(get_nodes(atom()) -> {ok, map()} | {error, any()}).
get_nodes(Name) ->
    case node_role(Name) of
        master -> get_nodes(master, Name);
        slaver -> get_nodes(slaver, Name);
        Err = {error, _} -> Err
    end.


get_nodes(master, Name) ->
    case get_node_sup_pid(Name) of
        {ok, Pid} -> ptnode_master_sup:get_nodes(Pid);
        Err = {error, _} -> Err
    end.


-spec(cast(atom(), atom(), term()) -> ok | {error, any()}).
cast(Name, Node, Req) ->
    case node_role(Name) of
        master -> cast(master, Name, Node, Req);
        slaver -> cast(slaver, Name, Node, Req);
        Err = {error, _} -> Err
    end.


cast(master, Name, Node, Req) ->
    case get_node_sup_pid(Name) of
        {ok, Pid} ->
            Serv = ptnode_master_sup:get_node_conn(Pid, Node),
            Cmd = ptnode_conn_proto:wrap_noreply_request(Node, Req),
            gen_server:cast(Serv, {'$forward', Cmd});
        Err = {error, _} -> Err
    end.
