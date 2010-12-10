%% P25 (*) Generate a random permutation of the elements of a list.

%% Example:
%% ?- rnd_permu([a,b,c,d,e,f],L).
%% L = [b,a,d,c,e,f]

%% Hint: Use the solution of problem P23.

-module(p25).

-export([random_permutation/1]).

%% LOGIC:
%% solution p23 won't help me because repeatition can occur, so will use extract_random in p24 (an internal 
%% function on p24.erl)

random_permutation(List) ->
    random_permutation(List, []).


random_permutation([], Acc) ->
    Acc;
random_permutation(List, Acc) ->
    {NewList, Elem} = p24:extract_random(List, random:uniform(length(List))),
    random_permutation(NewList, [Elem | Acc]).
