%% If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

%% Find the sum of all the multiples of 3 or 5 below 1000.


%% LOGIC:
%% number divisible by 3 are 3, 6, 9, 12, .. = 3 * (1, 2, 3, 4...)
%% same is with divisible by 5
%% now n ( n + 1 ) / 2 will give the sum.
%% Naive Ans: 3 x n ( n + 1 ) / 2 + 5 x n ( n + 1 ) / 2
%% where n = 1000/3 in first case and 1000 / 5 - 1 in second (- 1 because all lower than 1000)
%% CATCH: 3 and 5 have common number 15, 30, 45 etc, we have to remove that
%% Ans: 3 x n ( n + 1 ) / 2 + 5 x n ( n + 1 ) / 2 - 15 * 66 * (67) / 2 
%% (66 == 1000 / 15)


-module(p1).

-export([run/0]).

run() ->
    X = trunc(1000/3),     %% not a perfect divisor, so it is less than 1000
    Y = 1000/5 - 1,        %% need to substract 1
    Z = trunc(1000/15),

    Ans = 3 * X * (X + 1) / 2
	+ 5 * Y * (Y + 1) / 2
	- 15 * Z * (Z + 1) / 2,

    io:format("Ans: ~w", [Ans]).
