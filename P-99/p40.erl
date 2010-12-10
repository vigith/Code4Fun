%% P40 (**) Goldbach's conjecture.

%% Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically  confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

%% Example:
%% ?- goldbach(28, L).
%% L = [5,23]

-module(p40).

-export([goldbach/1]).

goldbach(Number) when Number =< 2; Number rem 2 =/= 0 ->
    error_goldbach;
goldbach(Number) ->
    goldbach(p39:list_prime_numbers(2, Number), Number, []).


goldbach([H|_], Number, Acc) when H > Number div 2 -> %% div by 2 because N/2 + N/2 = N
    lists:reverse(Acc);
goldbach([H|T], Number, Acc) ->
    case p31:is_prime(Number - H) of
	true ->
	    goldbach(T, Number, [{H, Number - H} | Acc]);
	false ->
	    goldbach(T, Number, Acc)
    end.
