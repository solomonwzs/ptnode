%% @author Solomon Ng <solomon.wzs@gmail.com>

-module(ptnode_proto).

-type socket()::ssl:sslsocket().
-export_type([socket/0]).

-type hostname()::string().
-type ip_address()::tuple().
-type host()::hostname() | ip_address().
-export_type([host/0]).

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

-callback peername(Socket::socket()) ->
    {ok, Peername::any()} | {error, any()}.
