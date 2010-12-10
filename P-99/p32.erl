%% P32 (**) Determine the greatest common divisor of two positive integer numbers.

%% Use Euclid's algorithm.
%% Example:
%% ?- gcd(36, 63, G).
%% G = 9

%% Define gcd as an arithmetic function; so you can use it like this:
%% ?- G is gcd(36,63).
%% G = 9

-module(p32).

-export([gcd/2]).

%% LOGIC: 
%% Euclid's Algorithm
%% The GCD of two numbers is the largest number that divides both of them without leaving a remainder. The Euclidean algorithm is based
%% on the principle that the greatest common divisor of two numbers does not change if the smaller number is subtracted from the larger number.
%% Since the larger of the two numbers is reduced, repeating this process gives successively smaller numbers until one of them is zero. 
%% When that occurs, the GCD is the remaining nonzero number.

gcd(0, Sec) ->
    Sec;
gcd(Fir, 0) ->
    Fir;
gcd(Fir, Sec) when Fir >= Sec ->
    gcd(Fir-Sec, Sec);
gcd(Fir, Sec) when Fir < Sec ->
    gcd(Fir, Sec - Fir).
