-module(ptnode_proto).

-type socket() :: ssl:sslsocket().
-export_type([socket/0]).

-type conn_state() :: any().
-export_type([conn_state/0]).

-type proto_spec() :: {
        ProtocolModule::atom(),
        Port:: integer(),
        ListenOptions::any(),
        AcceptOptions::any(),
        HandshakeOptions::any()
       }.
-export_type([proto_spec/0]).

-callback name() -> atom().

-callback listen(Port::integer(), Options::any()) ->
    {ok, socket()} | {error, any()}.

-callback accept(Socket::socket(), Options::any()) ->
    {ok, socket()} | {error, any()}.

-callback handshake(Socket::socket(), Options::any()) ->
    {ok, socket(), conn_state()} | {error, any()}.

-callback close(Socket::socket()) -> ok | {error, any()}.

-callback controlling_process(Socket::socket(), pid()) ->
    ok | {error, any()}.
