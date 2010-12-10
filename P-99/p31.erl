%% P31 (**) Determine whether a given integer number is prime.

%% Example:
%% ?- is_prime(7).
%% Yes

-module(p31).

-export([is_prime/1]).

%% LOGIC:
%% divide the number N with all numbers less than or equal to square_root(N)

is_prime(2) ->
    true;
is_prime(N) ->
    is_prime(N, erlang:trunc(math:sqrt(N)) + 1).

is_prime(N, Divisor) when Divisor > 1 andalso N rem Divisor =:= 0 ->
    false;
is_prime(N, Divisor) when Divisor > 1 ->
    is_prime(N, Divisor - 1);
is_prime(_, _) ->
    true.
