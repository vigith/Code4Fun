%% P10 (*) Run-length encoding of a list.

-module(p10).

-export([run_length_encoding/1]).

%% LOGIC:
%% 1. i could have easily used the p09 code and append the length of duplicates to Acc
%% 2. change p09 code such that it returns Length and Rest

run_length_encoding(List) ->
    run_length_encoding(List, []).

run_length_encoding([H|T], Acc) ->
    {LenDup, Rest} = get_duplicate_len(T, [H], 1),
    %% append the acc, with the length and the Head for which the duplicate count is done
    run_length_encoding(Rest, lists:append(Acc, [[LenDup |[H]]]));
run_length_encoding([], Acc) ->
    Acc.


get_duplicate_len([H|T], Acc = [H|_], Cnt) ->
    get_duplicate_len(T, [H|Acc], Cnt + 1);
get_duplicate_len([], _, Cnt) ->
    {Cnt, []};
get_duplicate_len(Rest, _, Cnt) ->
    {Cnt, Rest}.
