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
-record(worker_state, {user, chan, id, parent, time}).

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

%% XXX get rid of ?? Only useful if we use the local api functions
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

mandelbrot_api_request(Url,UrlEncoding,QueryStr) ->
    case httpc:request(post,{Url,[],UrlEncoding,QueryStr},[],[]) of
        {ok, {{_HttpVer, Code, _Msg}, _Headers, Body}}
          when 200 =< Code andalso Code < 300 ->
            {_, JsonBody} = mochijson2:decode(Body),
            {ok, JsonBody};
        FailedSession ->
            io:format("Failed to get a session ~p~n",[FailedSession]),
            {failed}
    end.

%% XXX make url configurable
create_julia_session() ->
    case mandelbrot_api_request("http://localhost:8080/sessions","","") of
        {ok, JsonBody} ->
            Id = binary_to_list(proplists:get_value(<<"sid">>,JsonBody)),
            io:format("Got a session ~p~n",[Id]),
            {created, Id};
        {failed} -> {failed}
    end.

execute_julia_cmd(Id,Cmd) ->
    QueryStr = "sid=" ++ Id ++
               "&fetch=true&format=text&proc=" ++ http_uri:encode(Cmd),
    case mandelbrot_api_request("http://localhost:8080/references",
                                "application/x-www-form-urlencoded",QueryStr) of
        {ok, JsonBody} ->
            %% extract the result in val
            Result = binary_to_list(proplists:get_value(<<"value">>,JsonBody)),
            io:format("Got the result ~p~n",[Result]),
            {result, Result};
        {failed} -> {failed}
    end.


%% response will go back to handle_info ? XXX
%% ParentPid is the gen_server which spawned this worker process
run_julia_run(State = #worker_state{user=User,parent=ParentPid}) ->
    receive
        {new_cmd, Cmd} ->
            io:format("Worker Got a new cmd ~p to run for user ~p~n",[Cmd,User]),
            case create_julia_session() of
                {created, Id} ->
                    % do the fetch
                    case execute_julia_cmd(Id,Cmd) of
                        {result, Result} ->
                            ParentPid ! {new_cmd_resp, User, Id, Result},
                            run_julia_run(State#worker_state{id = Id});
                        {failed} ->
                            ParentPid ! {cmd_run_failed, User}
                    end;
                {failed} ->
                    ParentPid ! {session_create_failed, User}
            end;
        {cmd, Cmd} ->
            Id = State#worker_state.id,
            case execute_julia_cmd(Id,Cmd) of
                {result, Result} ->
                    ParentPid ! {cmd_resp, User, Id, Result},
                    run_julia_run(State);
                {failed} ->
                    ParentPid ! {cmd_run_failed, User, Id}
            end
    after 240000 -> %% make this a minute or 2
        %% TODO remove julia session
        ParentPid ! {timeout, User}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

%% use the User as the lookup key, since we shouldn't create more than
%% 1 session per user
handle_info({timeout, User}, State) ->
    io:format("Timeout for User ~p State ~p~n",[User,State]),
    {noreply, State};

handle_info({run, CommandToRun, Chan, User, FromPid}, State) ->
    case lists:keyfind(User,1,State#state.pending) of
        %% check that user could be one of the active sessions
        {_, _, _, Worker} ->
            %% try not accept another command if have one outstanding for that
            %% same user
            Worker ! {cmd, CommandToRun},
            {noreply, State};
        false ->
            case length(State#state.pending) >= ?MAX_SESSIONS of
                true ->
                    {reply, busy, State};
                false ->
                    Worker = spawn_link(?MODULE, run_julia_run,
                                        [#worker_state{user = User, chan = Chan,
                                                       id = "", parent = self()}]),
                    Worker ! {new_cmd, CommandToRun},
                    NewState = #state{pending = [{User, Chan, FromPid, Worker} | State#state.pending]},
                    %% here we send the message to julia
                    % simulate with a timeout, i.e. after 3 secs respond
                    io:format("Worker newstate ~p From ~p user ~p~n",
                              [NewState,FromPid,User]),
                    {noreply, NewState}
            end
    end;

%% perhaps use Id and add to pending for user
handle_info({new_cmd_resp, User, Id, Result}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back to
    % useless_irc
    io:format("New Cmd User ~p Session Id ~p State ~p~n",[User,Id,State]),
    io:format("State Pending ~p~n",[State#state.pending]),
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found New Cmd Resp ~p Session Id ~p State ~p~n",
                      [User,Id,State]),
            not_found;
        {User, Chan, From, _Worker} ->
            io:format("New Cmd User ~p From ~p Session Id ~p State ~p~n",
                      [User,From,Id,State]),
            %% server doesn't care about new vs. not, just make a generic send
            From ! {cmd_resp, User, Chan, Result}
    end,
    {noreply, State};

handle_info({cmd_resp, User, Id, Result}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back to
    io:format("Cmd response User ~p Result ~p State ~p~n",[User,Result,State]),
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found New Cmd Resp ~p Result ~p State ~p~n",
                      [User,Result,State]),
            not_found;
        {User, Chan, From, _Worker} ->
            io:format("New Cmd User ~p From ~p Session Id ~p State ~p~n",
                      [User,From,Id,State]),
            From ! {cmd_resp, User, Chan, Id, Result}
    end,
    {noreply, State};

handle_info({session_create_failed, User}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back to
    % useless_irc
    io:format("Session Create failed User ~p State ~p~n",[User,State]),
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found User ~p State ~p~n",
                      [User,State]),
            not_found;
        {User, From, _Worker} ->
            io:format("Session Creation failed ~p From ~p State ~p~n",
                      [User,From,State]),
            From ! {session_create_failed, User}
    end,
    {noreply, State};

handle_info({cmd_run_failed, User, Id}, #state{pending=Pending} = State) ->
    io:format("Cmd failed to run. Id ~p User ~p State ~p~n",[Id,User,State]),
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found User ~p State ~p~n",
                      [User,State]),
            not_found;
        {User, From, _Worker} ->
            io:format("Cmd run failed User ~p From ~p State ~p~n",
                      [User,From,State]),
            From ! {cmd_run_failed, User}
    end,
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpected message rcvd: ~p State ~p~n",[Msg,State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
