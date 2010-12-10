%% P03 (*) Find the K'th element of a list.
%% The first element in the list is number 1.

-module(p03).

-export([kth_element/2]).

kth_element(1, [H|_]) ->
    H;
kth_element(1, []) ->
    false;
kth_element(K, [_|T]) ->
    kth_element(K-1, T).
