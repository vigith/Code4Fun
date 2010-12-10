%% P46 (**) Truth tables for logical expressions.

%% Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. Note that A and B can be Prolog goals (not only the constants true and fail).

%% A logical expression in two variables can then be written in prefix notation, as in the following example: and(or(A,B),nand(A,B)).

%% Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

%% Example:
%% ?- table(A,B,and(A,or(A,B))).
%% true true true
%% true fail true
%% fail true fail
%% fail fail fail

-module(p46).

-export([table/3, 
	 my_and/2, my_or/2,
	 my_nand/2, my_nor/2, 
	 my_impl/2, my_not/1,
	 my_xor/2, my_equ/2]).

%% LOGIC:
%% iterate over the given Lists and apply the function

%% eg.
%% 37> c(p42).
%% {ok,p42}
%% 38> p42:table(A, B, fun(A,B) -> p42:my_and(A, p42:my_or(A,B)) end).
%% true true  true
%% true fail  true
%% fail true  false
%% fail fail  false
%% done
%% 39> 

%% NOTE: I have prepended with *my_* because erlang has and, not etc as BIFs

table([Ha|Ta], [Hb|Tb], Fun) ->
    %% Fun is an anonymous function (see the eg.)
    Result = Fun(Ha, Hb),
    io:format("~w ~w  ~w~n", [Ha, Hb, Result]),
    table(Ta, Tb, Fun);
table([], [], _) ->
    done.
    

my_not(A) when A =:= true ->
    false;
my_not(_) ->
    true.

my_nand(A, B) ->
    my_not(my_and(A, B)).

my_nor(A, B) ->
    my_not(my_or(A, B)).

my_and(A, B) when A =:= true, B =:= true ->
    true;
my_and(_, _) ->
    false.

my_or(A, B) when A =:= false, B =:= false ->
    false;
my_or(_, _) ->
    true.

%% impl = or(!A, B).
my_impl(A, B) ->
    my_or(my_not(A), B).

%% same boolean = false
%% test: (A xor B) xor A = B.
my_xor(A, A) ->
    false;
my_xor(_, _) ->
    true.


%% equ is not of xor
my_equ(A, B) ->
    my_not(my_xor(A, B)).
