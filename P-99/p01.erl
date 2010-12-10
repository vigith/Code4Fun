%% P01 (*) Find the last element of a list.

-module(p01).

-export([last_element/1]).

last_element(List) ->
    last_element([], List).

last_element(_, [H|T]) ->
    last_element(H, T);
last_element(Res, []) ->
    Res.
