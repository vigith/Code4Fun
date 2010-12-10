%% P37 (**) Calculate Euler's totient function phi(m) (improved).

%% See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m) can be efficiently calculated as follows: Let [[p1,m1],[p2,m2],[p3,m3],...] be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

%% phi(m) = (p1 - 1) * p1**(m1 - 1) * (p2 - 1) * p2**(m2 - 1) * (p3 - 1) * p3**(m3 - 1) * ...
%%        = Mul(Pi - 1) * Pi ** (mi - 1)) from i <= i <= N
%% where P is prime factor and M is the number of times it occur (ie, power)

%% Note that a**b stands for the b'th power of a.

-module(p37).

-export([totient_func/1]).

totient_func(Number) ->
    PrimeFactors = p36:prime_factors_multiple(Number),
    totient_func(PrimeFactors, 1).

totient_func([H|T], Output) ->
    if
	length(H) > 1 ->
	    [First|Second] = H,
	    totient_func(T, Output * square((First - 1) * First, (lists:nth(1,Second) - 1)));
	true ->
	    totient_func(T, Output * (H - 1))
    end;
totient_func([], Output) ->
    Output.


square(_, M) when M < 1 ->
    1;
square(N, M) when M >= 1 ->
    if
	M rem 2 =:= 0 ->
	    A = square(N*N, M div 2);	
	true ->	    
	    A = N * square(N, M - 1)
    end,
    A.
