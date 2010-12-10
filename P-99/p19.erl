%% P19 (**) Rotate a list N places to the left.

%% Examples:
%% ?- rotate([a,b,c,d,e,f,g,h],3,X).
%% X = [d,e,f,g,h,a,b,c]

%% ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%% X = [g,h,a,b,c,d,e,f]

-module(p19).

-export([rotate_list/2]).

%% LOGIC: split the list and append the first to the tail

rotate_list(List, Shift) when Shift >= 0 ->
    {A, B} = p17:split_list(List, Shift),
    lists:append(B, A);
rotate_list(List, Shift) when Shift < 0 ->
    {A, B} = p17:split_list(List, length(List) + Shift),
    lists:append(B,A).
