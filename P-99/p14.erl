%% P14 (*) Duplicate the elements of a list.

-module(p14).

-export([duplicate_elements/2, repeat/2]).

duplicate_elements(List, Times) ->
    duplicate_elements(List, Times, []).

duplicate_elements([H|T], Times, Acc) ->
    Repeat = repeat(H, Times),
    duplicate_elements(T, Times, lists:append(Acc, Repeat));
duplicate_elements([], _, Acc) ->
    Acc.

repeat(H, Times) ->
    repeat(H, Times, []).

repeat(_, 0, Acc) ->
    Acc;
repeat(H, Times, Acc) ->
    repeat(H, Times - 1, [H|Acc]).
