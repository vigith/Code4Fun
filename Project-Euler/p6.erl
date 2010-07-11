%% The sum of the squares of the first ten natural numbers is,
%% 1^(2) + 2^(2) + ... + 10^(2) = 385

%% The square of the sum of the first ten natural numbers is,
%% (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

%% Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.

%% Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

%% LOGIC
%% 1^2 + 2^2 + 3^2 + 4^2 + 5^2 + ... = n (n + 1) (2n + 1) / 6
%% (1 + 2 + 3 + 4 ..) ^ 2 = (n (n + 1) / 2 ) ^ 2

-module(p6).

-export([run/0]).

run() ->
    math:pow(100 * 101 / 2, 2) - 100 * 101 * 201 / 6.
