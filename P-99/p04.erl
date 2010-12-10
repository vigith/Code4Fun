%% Find the number of elements of a list.

-module(p04).

-export([length_of_list/1]).

length_of_list(List) ->
    length_of_list(0, List).

length_of_list(Len, []) ->
    Len;
length_of_list(Len, [_|T]) ->
    length_of_list(Len + 1, T).
