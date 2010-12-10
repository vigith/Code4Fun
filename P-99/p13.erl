%% P13 (**) Run-length encoding of a list (direct solution).
%% Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly 
%% create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, 
%% simplify the result list by replacing the singleton terms [1,X] by X. 

-module(p13).

-export([modified_rlc/1]).

%% NOTE:
%% *I AM NOT SURE HOW THIS PROBLEM DIFFERENT FROM p11.erl* USING THE SAME p11.erl CODE HERE
%% OUTPUT format has also been changed
%% eg.
%% X = [[4,a],b,[2,c],[2,a],d,[4,e]] is of the form [Number, Elem] has been changed to
%% [Elem, Number]

modified_rlc(List) ->
        modified_rlc(List, []).

modified_rlc([H|T], Acc) ->
    {LenDup, Rest} = get_duplicate_len(T, [H], 1),    
    %% append the acc, with the length (if length > 1) and the Head for which the duplicate count is done
    if
	LenDup > 1 ->
	    modified_rlc(Rest, lists:append(Acc, [[H | [LenDup]]]));
	true ->
	    modified_rlc(Rest, lists:append(Acc, [H]))
    end;    
modified_rlc([], Acc) ->
    Acc.


get_duplicate_len([H|T], Acc = [H|_], Cnt) ->
    get_duplicate_len(T, [H|Acc], Cnt + 1);
get_duplicate_len([], _, Cnt) ->
    {Cnt, []};
get_duplicate_len(Rest, _, Cnt) ->
    {Cnt, Rest}.
