%% P36 (**) Determine the prime factors of a given positive integer (2).

%% Construct a list containing the prime factors and their multiplicity.
%% Example:
%% ?- prime_factors_mult(315, L).
%% L = [[3,2],[5,1],[7,1]]

%% Hint: The problem is similar to problem P13.

-module(p36).

-export([prime_factors_multiple/1]).


prime_factors_multiple(Number) ->
    p13:modified_rlc(p35:prime_factors(Number)).
