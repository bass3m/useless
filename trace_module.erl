-module(trace_module).
-compile(export_all). % replace with export later

trace_module(Mod,Func) ->
    spawn(fun() -> trace_module1(Mod, Func) end).

trace_module1(Mod, Func) ->
    erlang:trace_pattern({Mod, '_','_'},
                         [{'_',[],[{return_trace}]}],
                         [local]),
    Pid = spawn(fun()-> do_trace(Func) end),
    erlang:trace(Pid, true, [call,procs]),
    Pid ! {start, self()},
    trace_loop().

do_trace(Func) ->
    receive
        {start, _Pid} -> Func();
        _ -> io:format("Unexpected Msg Rcvd~n")
    end.

trace_loop() ->
    receive
        {trace,_,call, X} ->
            io:format("Call: ~p~n",[X]),
            trace_loop();
        {trace,_,return_from, Call, Ret} ->
            io:format("Return From: ~p => ~p~n",[Call, Ret]),
            trace_loop();
        Other ->
            io:format("Other = ~p~n",[Other]),
            trace_loop()
    end.
