-module(session_gen_server).
-behavior(gen_server).
-compile(export_all). % replace with export later

-record(session, {id, expr}).

%% Client APIs
start_link() -> gen_server:start_link(?MODULE, [], []).

%% XXX want to timeout these sessions due to inactivity
%% Synchronous call
create_session(Pid, Identifier, Expr) ->
    gen_server:call(Pid, {create, Identifier, Expr}).

%% Asynchronous call
destroy_session(Pid, Session = #session{}) ->
    gen_server:cast(Pid, {destroy, Session}).

%% Synchronous call
terminate_server(Pid) ->
    gen_server:call(Pid, terminate).

%% Server APIs

init([]) -> {ok, []}.

%% Check if we have room for another session, or user has an existing
%% session already
handle_call({create, Identifier, Expr}, _From, Sessions) ->

