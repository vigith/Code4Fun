%% P33 (*) Determine whether two positive integer numbers are coprime.

%% Two numbers are coprime if their greatest common divisor equals 1.
%% Example:
%% ?- coprime(35, 64).
%% Yes

-module(p33).

-export([coprime/2]).

%% LOGIC
%% Euclid's Algorithm, if they return 1 then the numbers are coprime

coprime(Fir, Sec) ->
    case p32:gcd(Fir, Sec) of
	1 ->
	    yes;
	_ ->
	    false
    end.
