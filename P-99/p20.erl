%% P20 (*) Remove the K'th element from a list.

%% Example:
%% ?- remove_at(X,[a,b,c,d],2,R).
%% X = b
%% R = [a,c,d]

-module(p20).

-export([remove_kth/2]).

remove_kth(List, Kth) ->
    remove_kth(List, Kth, []).

remove_kth([H|T], 1, Acc) ->
    {H, lists:append(lists:reverse(Acc), T)};
remove_kth([H|T], Kth, Acc) ->
    remove_kth(T, Kth - 1, [H|Acc]);
remove_kth([], _, _) ->
    out_of_index.
