%% P22 (*) Create a list containing all integers within a given range.

%% Example:
%% ?- range(4,9,L).
%% L = [4,5,6,7,8,9]

-module(p22).

-export([range/2]).

range(Start, End) ->
    range(Start, End, []).

range(End, End, Acc) ->
    lists:reverse([End|Acc]);
range(Start, End, Acc) ->
    range(Start + 1, End, [Start|Acc]).
