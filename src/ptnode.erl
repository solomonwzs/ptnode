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

-type ptnode_serv_ref() :: {Name::atom(), To::atom()}.

-export([start/0, stop/0]).
-export([start_node/3, stop_node/1]).
-export([node_role/1,
         call/3,
         cast/2
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
        _ -> {error, supervisor_not_found}
    end.


get_node_serv(master, {Name, To}) ->
    case get_node_sup_pid(Name) of
        {ok, Pid} ->
            case ptnode_master_sup:get_node_conn(Pid, To) of
                master -> {error, master};
                Err = {error, _} -> Err;
                Serv -> {ok, Serv}
            end;
        Err = {error, _} -> Err
    end;
get_node_serv(slaver, {Name, _}) ->
    case get_node_sup_pid(Name) of
        {ok, Pid} -> ?get_slaver_conn(Pid);
        Err = {error, _} -> Err
    end.


-spec(call(ptnode_serv_ref(), term(), pos_integer() | infinity) -> term()).
call(ServerRef = {Name, To}, Req, Timeout) ->
    case node_role(Name) of
        Err = {error, _} -> Err;
        Role ->
            case get_node_serv(Role, ServerRef) of
                {ok, Serv} ->
                    ReplyReq =
                    if Role =:= master -> {'$reply_request', Req};
                       Role =:= slaver -> {'$reply_request', ?a2b(To), Req}
                    end,
                    case gen_server:call(Serv, ReplyReq, Timeout) of
                        {ok, Mark} ->
                            receive
                                {Mark, Reply} -> Reply
                            after Timeout -> {error, timeout}
                            end;
                        Err = {error, _} -> Err
                    end;
                Err = {error, _} -> Err
            end
    end.


-spec(cast(ptnode_serv_ref(), term()) -> ok | {error, any()}).
cast(ServerRef = {Name, To}, Req) ->
    case node_role(Name) of
        Err = {error, _} -> Err;
        Role ->
            case get_node_serv(Role, ServerRef) of
                {ok, Serv} ->
                    NoreplyReq =
                    if Role =:= master ->
                           {'$noreply_request', Req};
                       Role =:= slaver ->
                           {'$noreply_request', ?a2b(To), Req}
                    end,
                    gen_server:cast(Serv, NoreplyReq);
                Err = {error, _} -> Err
            end
    end.
