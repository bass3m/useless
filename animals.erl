-module(animals).
-compile(export_all). % replace with export later

start() ->
    spawn(?MODULE,restarter,[]).

pet_sounds() ->
    receive
        {From, Ref, {"Miaw"}} ->
            From ! {Ref, cat};
        {From, Ref, {"Ruff"}} ->
            From ! {Ref, dog};
        {From, Ref, {_}} ->
            From ! {Ref, what_says_you}
    end,
    pet_sounds().

pet_analyze(AnimalSound) ->
    Ref = make_ref(),
    pet_soundboard ! {self(), Ref, {AnimalSound}},
    receive
        {Ref, WhoDoneIt} -> WhoDoneIt
    after 2000 ->
        timeout
    end.

restarter() ->
    process_flag(trap_exit,true),
    Pid = spawn_link(?MODULE, pet_sounds, []),
    register(pet_soundboard,Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual shutdown, not a crash
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

%% Some usage:
%% c(animals).
%% animals:start().
%% animals:pet_analyze("Miaw").
%% animals:pet_analyze("Dog").
%% animals:pet_analyze("Ruff").
%% exit(whereis(pet_soundboard),kill).
%% whereis(pet_soundboard).
