

%% The prime factors of 13195 are 5, 7, 13 and 29.

%% What is the largest prime factor of the number 600851475143 ?

%% LOGIC:
%% get all the prime factors
%%   start dividing with 2, 3, next_prime
%% test if a number is prime by checking whether it has divisor from 2 - sqrt(number)

-module(p3).

-export([prime_factors/1]).


prime_factors(Number) ->
    prime_factors(Number, []).


prime_factors(Number, Acc) when Number =< 1 ->
    lists:reverse(Acc);
prime_factors(Number, Acc) ->
    {NewNum, PrimeNum} = divide(Number, 2),
    prime_factors(NewNum, [PrimeNum | Acc]).

divide(Number, Prime) ->
    if
	Number rem Prime =:= 0->
	    {Number div Prime, Prime};
	true ->
	    divide(Number, next_prime(Prime + 1))
    end.

next_prime(N) ->
    case is_prime(N) of
	true ->
	    N;
	false ->
	    next_prime(N+1)
    end.


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
