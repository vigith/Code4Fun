%% P17 (*) Split a list into two parts; the length of the first part is given.

-module(p17).

-export([split_list/2]).

split_list(List, Index) ->
    split_list(List, Index, []).

split_list(List, 0, Head) ->
    {lists:reverse(Head), List};
split_list([H|T], Index, Head) ->
    split_list(T, Index - 1, [H|Head]);
split_list([], _, _) ->
    out_of_index.
