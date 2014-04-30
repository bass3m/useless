-module(useless_irc).
-behavior(gen_server).
-export([start/2, login/1, join/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(REALNAME, "Julia MandelBot"). % XXX shoud change this really
-define(CRLF, "\r\n").

%% should be map that contains an an array of plugin maps
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
    {reply, Reply, State#state{channel = Channel}};

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

terminate(_Reason, _State) -> ok.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp_closed, Reason}, State) ->
    io:format("Tcp closed with reason: ~p~n",[Reason]),
    {noreply, State};

%% TODO use ref as a way to identify the response
handle_info({tcp, _Socket, Msg}, State) ->
    case useless_irc_parser:parse_msg(Msg) of
        {response, Response} ->
            io:format("Respond server with ~p~n",[Response]),
            gen_tcp:send(State#state.sock, Response ++ ?CRLF);
        {msg, FromNick, [ToNick | Privmsg]} when ToNick =:= State#state.nick ->
            io:format("Got a private msg from ~p ~p~n",[FromNick,Privmsg]),
            %% not pass state and create a new process to handle the request
            %% look at return value and if it's good then create a process
            %% for each registered function call it's handler
            [Service|Request] = useless_irc_parser:process_private_msg(Privmsg),
            io:format("Nick request msg ~p request ~p~n",[Service,Request]),
            %% find something that matches the prefix
            {_, _, {ServicePid, _, Mod}} = useless_irc_services:get_service(Service),
            io:format("Service at ~p Module ~p Request ~p~n",[ServicePid,Mod,Request]),
            ServicePid ! {run, Request, "", FromNick, self()};
        {msg, FromNick, [Chan | Chanmsg]} when Chan =:= State#state.channel ->
            %% not pass state and create a new process to handle the request
            [Service|Request] = useless_irc_parser:process_channel_msg(Chanmsg),
            io:format("Channel request to Chan ~p From ~p msg ~p request ~p~n",
                      [Chan,FromNick,Service,Request]),
            %% for now, ignore the node, and just use the pid
            %% make this a fun to handle no service case
            {_, _, {ServicePid, _, Mod}} = useless_irc_services:get_service(Service),
            io:format("Service at ~p Module ~p Request ~p~n",[ServicePid,Mod,Request]),
            %% from nick might be channel here for the response XXX
            ServicePid ! {run, Request, Chan, FromNick, self()};
        _ -> io:format("Unexpected message rcvd: ~p~n",[Msg])
    end,
    {noreply, State};

handle_info({cmd_resp, User, Chan, Result}, State) when Chan =:= "" ->
    io:format("IRC Server Cmd Result: ~p rcvd for User: ~p~n",[Result,User]),
    %% use string:join here with spaces
    gen_tcp:send(State#state.sock,
                 "PRIVMSG " ++ User ++ ":" ++  Result ++ ?CRLF),
    {noreply, State};

handle_info({cmd_resp, User, Chan, Result}, State) ->
    io:format("IRC Server Chan ~p Cmd Result: ~p rcvd for User: ~p~n",
              [Chan,Result,User]),
    gen_tcp:send(State#state.sock,
                 string:join(["PRIVMSG",Chan,":" ++ Result ++ ?CRLF]," ")),
                 %"PRIVMSG " ++ Chan ++ ":" ++ Result ++ ?CRLF),
    %gen_tcp:send(State#state.sock,
                 %"PRIVMSG " ++ Chan ++ ":" ++ Result ++ ?CRLF),
    {noreply, State};


handle_info(Msg, State) ->
    io:format("IRC Server Unexpected message rcvd: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
