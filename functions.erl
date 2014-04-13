-module(functions).
-compile(export_all). % replace with export later

head([H|_]) -> H.
second([_,S|_]) -> S.
tail([_|T]) -> T.

same(X,X) -> true;
same(_,_) -> false.

test_the_if(N) ->
    if N =:= 2 -> succeeds;
    true -> fails
end.

insert_set(X,[]) -> [X];
insert_set(X,Set) ->
    case lists:member(X,Set) of
        true -> Set;
        false -> [X|Set]
    end.

fact(0) -> 1;
fact(N) when N > 0 -> N * fact(N-1).

list_len([]) -> 0;
list_len([_|T]) -> 1 + list_len(T).

tail_len(L) -> tail_len(0,L).
tail_len(N,[]) -> N;
tail_len(N,[_|T]) -> tail_len(N+1,T).

reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

tail_reverse(T) -> tail_reverse([],T).
tail_reverse(Acc,[]) -> Acc;
tail_reverse(Acc,[H|T]) -> tail_reverse([H|Acc],T).

zip(L1,L2) -> reverse(zip([],L1,L2)).
zip(Acc,[],[]) -> Acc;
zip(Acc,[H1|T1],[H2|T2]) -> zip([{H1,H2}|Acc],T1,T2).

%% {node, {Key, Val, Smaller, Larger}}
empty() -> {node, 'nil'}.
tree_insert({Key,Val},{node,'nil'}) -> {node, {Key, Val, {node,'nil'}, {node,'nil'}}};
tree_insert({NewKey,NewVal},{node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, tree_insert({NewKey,NewVal}, Smaller), Larger}};
tree_insert({NewKey,NewVal},{node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, tree_insert({NewKey,NewVal}, Larger)}};
tree_insert({Key,NewVal},{node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, NewVal, Smaller, Larger}}.

map(_,[]) -> [];
map(F,[H|T]) -> [F(H)|map(F,T)].

filter(Pred,L) -> lists:reverse(filter(Pred,L,[])).
filter(_,[],Acc) -> Acc;
filter(Pred,[H|T],Acc) ->
    case Pred(H) of
        true -> filter(Pred,T,[H|Acc]);
        false -> filter(Pred,T,Acc)
    end.

max([H|T]) -> max2(H,[H|T]).
max2(Max,[]) -> Max;
max2(Max,[H|T]) ->
    case H >= Max of
        true -> max2(H,T);
        false -> max2(Max,T)
    end.

%% max without the pattern matching/case
maxy([H|T]) -> maxy2(H,[H|T]).
maxy2(Max,[]) -> Max;
maxy2(Max,[H|T]) when H >= Max -> maxy2(H,T);
maxy2(Max,[_|T]) -> maxy2(Max,T).

sum(Xs) -> sum(0,Xs).
sum(Sum,[]) -> Sum;
sum(Sum,[H|T]) -> sum(Sum+H,T).

fold(_,Start,[]) -> Start;
fold(F,Start,[X|Xs]) -> fold(F,F(Start,X),Xs).

%% using folds for filter/map/reverse XXX
fold_rev([])->[];
%% lower version is better
fold_rev([H|T])->fold(fun(Acc,El)->[El|Acc] end,[H],T).

fold_rev2(Xs)->fold(fun(Acc,El)->[El|Acc] end,[],Xs).

