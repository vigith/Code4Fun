%% P15 (**) Duplicate the elements of a list a given number of times.

-module(p15).

-export([duplicate_elements/3]).

%% LOGIC: same as p14.erl, except for it takes in result list along with

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
