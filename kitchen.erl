-module(kitchen).
-compile(export_all). % replace with export later

start(Foodlist) ->
    spawn(?MODULE,fridge,Foodlist).

store(Pid,Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid,Msg} -> Msg
    end.

take(Pid,Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid,Msg} -> Msg
    end.


fridge(Foodlist) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge([Food|Foodlist]);
        {From, {take, Food}} ->
            case lists:member(Food,Foodlist) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge(lists:delete(Food,Foodlist));
                false ->
                    From ! {self(), not_found},
                    fridge(Foodlist)
            end;
        terminate ->
            ok
    end.
