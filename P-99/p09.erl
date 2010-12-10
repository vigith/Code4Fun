%% P09 (**) Pack consecutive duplicates of list elements into sublists.

-module(p09).

-export([consecutive_sublist/1]).

%% LOGIC: get the consecutive duplicates and the rest, append the consecutive duplicates
%%        and recurse on the rest.

consecutive_sublist(List) ->
    consecutive_sublist(List, []).

consecutive_sublist([H|T], Acc) ->
    {Duplicates, Rest} = get_duplicate(T, [H]),
    consecutive_sublist(Rest, lists:append(Acc, [Duplicates]));
consecutive_sublist([], Acc) ->
    Acc.


get_duplicate([H|T], Acc = [H|_]) ->
    get_duplicate(T, [H|Acc]);
get_duplicate([], Acc) ->
    {Acc, []};
get_duplicate(Rest, Acc) ->
    {Acc, Rest}.
