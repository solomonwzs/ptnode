-module(ptnode_proto).

-type socket()::ssl:sslsocket().
-export_type([socket/0]).

-type hostname()::string().
-type ip_address()::tuple().
-type host()::hostname() | ip_address().
-export_type([host/0]).

-type server_proto_spec()::{
        ProtocolModule::atom(),
        Port::integer(),
        ListenOptions::any(),
        AcceptOptions::any(),
        HandshakeOptions::any()
       }.
-type client_proto_spec()::{
        ProtoModule::atom(),
        Host::host(),
        Port::integer(),
        ConnectOpts::any(),
        Timeout::integer() | infinity
       }.
-export_type([server_proto_spec/0, client_proto_spec/0]).

-callback name() -> atom().

-callback listen(Port::integer(), Options::any()) ->
    {ok, socket()} | {error, any()}.

-callback accept(Socket::socket(), Options::any()) ->
    {ok, socket()} | {error, any()}.

-callback handshake(Socket::socket(), Options::any()) ->
    {ok, socket()} | {error, any()}.

-callback connect(Host::host(), Port::integer(), Options::list(),
                  Timeout::integer() | infinity) ->
    {ok, socket()} | {error, Reason::any()}.

-callback close(Socket::socket()) -> ok | {error, any()}.

-callback controlling_process(Socket::socket(), pid()) ->
    ok | {error, any()}.

-callback setopts(Socket::socket(), Options::any()) ->
    ok | {error, any()}.

-callback parse_message(Message::any()) ->
    {ok, Data::any()} |
    {error, Reason::any()} |
    ignore |
    close.

-callback send(Socket::socket(), Data::any()) ->
    ok | {error, Reason::any()}.
