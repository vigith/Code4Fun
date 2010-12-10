%% P05 (*) Reverse a list.

-module(p05).

-export([reverse_list/1]).

reverse_list(List) ->
    reverse_list(List, []).

reverse_list([], Rev) ->
    Rev;
reverse_list([H|T], Rev) ->
    reverse_list(T, [H|Rev]).
