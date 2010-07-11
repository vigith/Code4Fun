%% 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

%% What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

%% LOGIC
%% 1. take all the prime numbers from 1 to 20
%% 2. take all the power 4 (2^4 = 16) which lies till 20, dont take 16, take 16 / 2 (2 is already there, so 8 * 2 makes 16)
%% 3. take the cubes which has no power of 4 till 20 ( 2^3 = 8 won't qualify)
%% 4. take the squares which are not cubes or ^4 (3^2 = 9) (don't take 9, take 9 / 3 since 3 is already there)
%%
%% evenly divisible is nothing but divisible with reminder 0 

-module(p5).

-export([run/0]).

run() ->
    1 * 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 8 * 3.
