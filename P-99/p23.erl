%% P23 (**) Extract a given number of randomly selected elements from a list.

%% The selected items shall be put into a result list.
%% Example:
%% ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
%% L = [e,d,a]

%% Hint: Use the built-in random number generator random/2 and the result of problem P20.
%% Erlang's random:uniform(150) will be used

-module(p23).

-export([extract_random/2]).

%% LOGIC: select kth element from the list and k is random, repeatition can occur here

extract_random(List, Count) ->
    extract_random(List, Count, []).

extract_random(_, 0, Acc) ->
    Acc; %% don't bother to reverse, they require random
extract_random(List, Count, Acc) ->
    extract_random(List, Count - 1, [get_element(List, random:uniform(length(List))) | Acc]).

get_element(List, Kth) ->
    p03:kth_element(Kth, List).
