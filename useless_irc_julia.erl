-module(useless_irc_julia).

%% this should be a supervisor somehow which each pending
%% session as a worker
-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(JULIAPREFIX,"julia").
-define(TIMEOUT,3000).
-define(MAX_SESSIONS,3).

%% pending will have a list of tuple containing nick, calc etc..
-record(state, {pending = []}).
-record(worker_state, {user, id, parent, time}).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

get_num_pending() ->
    gen_server:call(?SERVER,num_pending).

run(CommandToRun, User) ->
    gen_server:call(?MODULE, {run, CommandToRun, User}, ?TIMEOUT).

init([]) ->
    inets:start(), % to start using httpc client
    %% register our julia service
    useless_irc_services:register_service(julia, ?JULIAPREFIX, self(), node(), ?SERVER),
    {ok, #state{}}.

handle_call({num_pending}, _From, State) ->
    {reply, length(State#state.pending), State};

handle_call({run, CommandToRun, User}, From, State) ->
    case lists:keyfind(User,1,State#state.pending) of
        %% check that user could be one of the active sessions
        {_, _, Worker} ->
            %% try not accept another command if have one outstanding for that
            %% same user
            Worker ! {cmd, CommandToRun};
        false ->
            case length(State#state.pending) >= ?MAX_SESSIONS of
                true ->
                    {reply, busy, State};
                false ->
                    Worker = spawn_link(?MODULE, run_julia_run,
                                        [#worker_state{user = User, id = "",
                                                       parent = self()}]),
                    Worker ! {new_cmd, CommandToRun},
                    NewState = #state{pending = [{User, From, Worker} | State#state.pending]},
                    %% here we send the message to julia
                    % simulate with a timeout, i.e. after 3 secs respond
                    io:format("Worker newstate ~p From ~p user ~p~n",
                              [NewState,From,User]),
                    {noreply, NewState}
            end
    end;

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

%% response will go back to handle_info ? XXX
run_julia_run(State = #worker_state{user=User,parent=ParentPid}) ->
    receive
        {new_cmd, Cmd} ->
            %% will create an http connection and
            io:format("Worker Got a new cmd ~p to run for user ~p~n",[Cmd,User]),
            %% we have to create a session etc.. and store in id the session id etc..
            Id = random:uniform(1000), %% for now
            ParentPid ! {new_cmd_resp, User, Id},
            run_julia_run(State#worker_state{id = Id});
        {cmd, Cmd} ->
            %% can use the Id for subsequent requests
            %% State#worker_state.id
            io:format("Worker Got cmd ~p to run for user ~p~n",[Cmd,User]),
            ParentPid ! {cmd_resp, User, Cmd ++ " : " ++ Cmd},
            run_julia_run(State)
    after 20000 -> %% make this a minute or 2
        %% really for the response
        %% remove julia session
        ParentPid ! {timeout, User}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

%% use the User as the lookup key, since we shouldn't create more than
%% 1 session per user
handle_info({timeout, User}, State) ->
    io:format("Timeout for User ~p State ~p~n",[User,State]),
    {noreply, State};

handle_info({run, CommandToRun, User, FromPid}, State) ->
    case lists:keyfind(User,1,State#state.pending) of
        %% check that user could be one of the active sessions
        {_, _, Worker} ->
            %% try not accept another command if have one outstanding for that
            %% same user
            Worker ! {cmd, CommandToRun};
        false ->
            case length(State#state.pending) >= ?MAX_SESSIONS of
                true ->
                    {reply, busy, State};
                false ->
                    Worker = spawn_link(?MODULE, run_julia_run,
                                        [#worker_state{user = User, id = "",
                                                       parent = self()}]),
                    Worker ! {new_cmd, CommandToRun},
                    NewState = #state{pending = [{User, FromPid, Worker} | State#state.pending]},
                    %% here we send the message to julia
                    % simulate with a timeout, i.e. after 3 secs respond
                    io:format("Worker newstate ~p From ~p user ~p~n",
                              [NewState,FromPid,User]),
                    {noreply, NewState}
            end
    end;

handle_info({new_cmd_resp, User, Id}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back to
    % useless_irc
    io:format("New Cmd User ~p Session Id ~p State ~p~n",[User,Id,State]),
    io:format("State Pending ~p~n",[State#state.pending]),
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found New Cmd Resp ~p Session Id ~p State ~p~n",
                      [User,Id,State]),
            not_found;
        {User, From, _Worker} ->
            io:format("New Cmd User ~p From ~p Session Id ~p State ~p~n",
                      [User,From,Id,State]),
            From ! {new_cmd_resp, User, Id}
            %element(1,From) ! {new_cmd_resp, User, Id}
            % XXX this is wrong, need to send to the right place
            %gen_server:reply(From, {new_cmd_resp, User, Id})
    end,
    {noreply, State};

handle_info({cmd_resp, User, CmdResp}, State) ->
    % when get response, now we have to send that response back to
    % useless_irc
    io:format("Cmd response User ~p CmdRsp ~p State ~p~n",[User,CmdResp,State]),
    %gen_server:reply(),
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpected message rcvd: ~p State ~p~n",[Msg,State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
