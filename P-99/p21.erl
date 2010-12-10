%% P21 (*) Insert an element at a given position into a list.

%% Example:
%% ?- insert_at(alfa,[a,b,c,d],2,L).
%% L = [a,alfa,b,c,d]

-module(p21).

-export([insert_at/3]).

insert_at(List, Element, Postn) ->
    insert_at(List, Element, Postn, []).

insert_at(List, Elem, 1, Acc) ->
    lists:append(lists:reverse(Acc), [Elem | List]);
insert_at([H|T], Elem, Kth, Acc) ->
    insert_at(T, Elem, Kth - 1, [H|Acc]);
insert_at([], _, _, _) ->
    out_of_index.
