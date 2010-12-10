%% P08 (**) Eliminate consecutive duplicates of list elements.

-module(p08).

-export([elminate_duplicate/1]).

elminate_duplicate([H|T]) ->
    elminate_duplicate(T, [H]).

elminate_duplicate([H|T], Acc = [H|_T1]) ->
    elminate_duplicate(T, Acc);
elminate_duplicate([H|T], Acc = [_H1|_T1]) ->
    elminate_duplicate(T, [H | Acc]);
elminate_duplicate([], Acc) ->
    lists:reverse(Acc).
