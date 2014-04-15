-module(session_gen_server).
-behavior(gen_server).
-compile(export_all). % replace with export later

-record(session, {id, expr}).

%% Client APIs
start() -> gen_server:start_link(?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

%% XXX want to timeout these sessions due to inactivity
%% Asynchronous call
create_session(Identifier, Expr) ->
    gen_server:cast(?MODULE, {create, Identifier, Expr}).

%% Synchronous call
%% should add a fetch result of expression
destroy_session(Session = #session{}) ->
    gen_server:call(?MODULE, {destroy, Session}).

%% Synchronous call
terminate_server() ->
    gen_server:call(?MODULE, terminate).

%% XXX need another call to fetch the result ?
%%
%% Server APIs

init([]) -> {ok, maps:new()}.

%% Check if we have room for another session, or user has an existing
%% session already
handle_cast({create, Identifier, Expr}, _From, Sessions) ->
    %% if session already exists, ignore, if key is in Sessions
    case lists:member(Identifier,Sessions) of
        false -> {noreply, handle_create_session(Identifier,Expr), Sessions};
        true -> io:format("Can't create an already existing session ~p~n",
                          [Identifier]),
                {noreply, Sessions}
    end.

handle_call({destroy, Identifier}, _From, Sessions) ->
    {reply, Sessions};
handle_call({terminate}, _From, Sessions) ->
    {stop, normal, ok, Sessions}.

handle_create_session(Identifier, Expr) ->
    #session{id=Identifier, expr=Expr}.

terminate(normal, Sessions) ->
    [io:format("~p was trashed.~n",[S#session.expr]) || S <- Sessions],
    ok.

handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
%terminate(_Reason, _State) -> ok.
%code_change(_OldVsn, State, Extra) -> {ok, State}.
