-module(session_gen_server).
-behavior(gen_server).
-export([start/0, stop/0, create_session/2, get_session/1,
         destroy_session/1, terminate_server/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later
-define(SERVER, ?MODULE).

%% Client APIs
start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

%% Asynchronous call
create_session(Identifier, Expr) ->
    gen_server:cast(?MODULE, {create, Identifier, Expr}).

%% Synchronous call
get_session(Identifier) ->
    gen_server:call(?MODULE, {get, Identifier}).

destroy_session(Identifier) ->
    gen_server:call(?MODULE, {destroy, Identifier}).

%% Synchronous call
terminate_server() ->
    gen_server:call(?MODULE, terminate).

%% Server APIs

init([]) -> {ok, maps:new()}.

%% Check if we have room for another session, or user has an existing
%% session already
handle_cast({create, Identifier, Expr}, Sessions) ->
    %% if session already exists, ignore, if key is in Sessions
    case maps:is_key(Identifier, Sessions) of
        false -> {noreply, maps:put(Identifier,Expr,Sessions)};
        true -> io:format("Can't create an already existing session ~p~n",
                          [Identifier]),
                {noreply, Sessions}
    end.

handle_call({destroy, Identifier}, _From, Sessions) ->
    case maps:is_key(Identifier, Sessions) of
        true -> {reply, {ok, Identifier}, maps:remove(Identifier,Sessions)};
        false -> io:format("Identifier not found in Sessions ~p~n",
                          [Identifier]),
                {reply, {not_found, Identifier}, Sessions}
    end;

handle_call({get, Identifier}, _From, Sessions) ->
    Reply = case maps:is_key(Identifier, Sessions) of
                true -> {ok, maps:get(Identifier,Sessions)};
                false -> io:format("Identifier not found in Sessions ~p~n",
                                  [Identifier]),
                        {not_found, Identifier}
            end,
    {reply, Reply, Sessions};

handle_call({terminate}, _From, Sessions) ->
    {stop, normal, ok, Sessions}.

terminate(normal, Sessions) ->
    [io:format("~p Id was trashed.~n",[K]) || K <- maps:keys(Sessions)],
    ok.

handle_info(Msg, Sessions) ->
    io:format("Unexpected messageL ~p~n",[Msg]),
    {noreply, Sessions}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
