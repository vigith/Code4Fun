%% P34 (**) Calculate Euler's totient function phi(m).

%% Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

%% Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

%% ?- Phi is totient_phi(10).
%% Phi = 4

%% Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).

-module(p34).

-export([totient_func/1]).

%% LOGIC:
%% this is a very simple logic using coprime function (different version is yet another question i think)

totient_func(N) ->
    totient_func(N, N - 1, []). %% instead of Acc of lists i could have used a counter and thus save memory

totient_func(1, _, _) ->
    1;
totient_func(_, 0, Acc) ->
    io:format("~w~n", [Acc]),
    length(Acc);
totient_func(N, M, Acc) ->
    case p33:coprime(N, M) of
	yes ->
	    totient_func(N, M - 1, [M | Acc]);
	_ ->
	    totient_func(N, M - 1, Acc)
    end.
