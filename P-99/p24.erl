%% P24 (*) Lotto: Draw N different random numbers from the set 1..M.

%% Example:
%% ?- rnd_select(6,49,L).
%% L = [23,1,17,33,21,37]

-module(p24).

-export([lotto/3, extract_random/2]).

%% LOGIC
%% rewrite extract_random of the previous Q to remove the repeatition and feed in the range code 
%% output into extract random


lotto(Start, End, Count) ->
    Range = p22:range(Start, End),
    select(Range, Count, []).

select(_, 0, Acc) ->
    Acc;
select([], _, _) ->
out_of_index;
select(List, Count, Acc) ->
    {NewList, Elem} = extract_random(List, random:uniform(length(List))),
    select(NewList, Count - 1, [Elem | Acc]).


extract_random(List, Kth) ->
    {Elem, NewList} = get_element(List, Kth),
    {NewList, Elem}.


get_element(List, Kth) ->
    p20:remove_kth(List, Kth).
