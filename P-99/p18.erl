%% P18 (**) Extract a slice from a list.
%% Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element
%%  of the original list (both limits included). Start counting the elements with 1.

%% Example:
%% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%% X = [c,d,e,f,g]

-module(p18).

-export([splice/3]).

splice(List, Begin, End) ->
    splice(List, Begin, End, []).

splice(_, 1, 0, Acc) ->
    lists:reverse(Acc);
splice([], _, _, _) ->
    out_of_index;
splice([H|T], 1, End, Acc) ->
    splice(T, 1, End - 1, [H|Acc]);
splice([_|T], Begin, End, Acc) ->
    splice(T, Begin - 1, End, Acc).
