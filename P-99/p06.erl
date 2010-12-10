%% P06 (*) Find out whether a list is a palindrome.

%% LOGIC:
%% 1. reverse the list and compare against the 1st list
%% 2. iterate towards the center from each end and compare
%%
%% IMPMENTATION:
%% Here second will be implemented even though going to nth element is costly in a 
%% LL (linked list) but 1st one is very easy with pattern match

-module(p06).

-export([is_palindrome/1]).

is_palindrome([]) ->
    true;
is_palindrome(List) ->
    %% in prob p03 we did finding the "kth element" of list (lists:nth is better though)
    %% and in p04 we solved how to find the length of a list
    is_palindrome(List, 
		  p03:kth_element(1, List), 
		  p03:kth_element(p04:length_of_list(List), List), 
		  1, p04:length_of_list(List)). 

is_palindrome(List, Elem, Elem, Begin, End) when Begin =< End ->
    is_palindrome(List, 
		  p03:kth_element(Begin + 1, List), 
		  p03:kth_element(End - 1, List), 		  
		  Begin + 1, End - 1);
is_palindrome(_, _E, _E1, Begin, End) when Begin < End ->
    io:format("~p == ~p | ~w ~w ~n", [_E, _E1, Begin, End]),
    false;
is_palindrome(_, _, _, _, _) ->
    true.
