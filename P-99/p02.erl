%% P02 (*) Find the last but one element of a list.

-module(p02).

-export([last_but_one/1]).

last_but_one(List) ->
    last_but_one([], [], List).

last_but_one(Lst, _, [H|T]) ->
    last_but_one(H, Lst, T);
last_but_one(_, LbOne, []) ->
    LbOne.
