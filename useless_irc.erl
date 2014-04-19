-module(useless_irc).
-behavior(gen_server).
%-export([start/0, connect/2, calculate/1]).
-export([start/2, login/1, join/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

%% tcp socket and read and process messages
%% it's an irc client but also a calculation server
-define(SERVER, ?MODULE).
-define(REALNAME, "Julia MandelBot").
-define(CRLF, "\r\n").

-record(state, {server, port, nick, pass, username, realname, channel, sock}).

%% Client APIs
%% all Synchronous calls right now
start(Server, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Server, Port], []).

stop() -> gen_server:call(?SERVER, stop).

login(Nick) ->
    gen_server:call(?SERVER, {login, Nick, ?REALNAME}).

join(Channel) ->
    gen_server:call(?SERVER, {join_channel, Channel}).

%% Server APIs
init([Server, Port]) ->
    Options = [binary, {active, true},
               {packet, line}, {keepalive, true}],
    {ok,Socket} = gen_tcp:connect(Server,Port,Options),
    io:format("connected to:~p~n",[Socket]),
    {ok, #state{server = Server, port = Port, sock = Socket}}.

% an array of maps containing nick, and session id ?
handle_call({login, Nick, Realname}, _From, State) ->
    %% write to socket with NICK and USER
    ok = gen_tcp:send(State#state.sock, "NICK " ++ Nick ++ ?CRLF),
    Reply = gen_tcp:send(State#state.sock,
                         "USER " ++ Nick ++ " 0 * : " ++
                         Realname ++ ?CRLF),
    {reply, Reply, State#state{nick = Nick, realname = Realname}};

%% worry about chankeys later
handle_call({join_channel, Channel}, _From, State) ->
    Reply = gen_tcp:send(State#state.sock,
                         "JOIN " ++ Channel ++ ?CRLF),
    {reply, Reply, State#state{channel = Channel}}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

handle_info({tcp_closed, Reason}, State) ->
    io:format("Tcp closed with reason: ~p~n",[Reason]),
    {noreply, State};

handle_info({tcp, _Socket, Msg}, State) ->
    %case string:tokens(binary_to_list(Msg)," ") ->
        %[_,
    io:format("Unexpected message rcvd: ~p~n",[Msg]),
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpected message rcvd: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
