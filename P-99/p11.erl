%% P11 (*) Modified run-length encoding.
%% Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.

-module(p11).

-export([modified_rlc/1]).

%% LOGIC:
%% if get_duplicate_len > 1 then create a sub-list

%% NOTE:
%% OUTPUT format has been changed
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
