%% P35 (**) Determine the prime factors of a given positive integer.

%% Construct a flat list containing the prime factors in ascending order.
%% Example:
%% ?- prime_factors(315, L).
%% L = [3,3,5,7]

-module(p35).

-export([prime_factors/1]).

%% LOGIC:
%% find all elements such that each element < sqrt(N) and is a divisor of N. Filter out prime numbers from
%% this list
%% sorting will be done by default because we always divide with smaller number first and never a small number
%% once checked won't come again as a result of division due to someother number

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
    case p31:is_prime(N) of
	true ->
	    N;
	false ->
	    next_prime(N+1)
    end.
