-module(trace_test).
-compile(export_all). % replace with export later

trace_test(Func) ->
    trace_module:trace_module(?MODULE,Func).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibo(N) -> fibo1(N,{0,1}).
fibo1(0,{A,_}) -> A;
fibo1(N,{A,B}) -> fibo1(N-1,{A+B,A}).
