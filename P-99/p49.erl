%% P49 (**) Gray code.

%% An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
%% n = 1: C(1) = ['0','1'].
%% n = 2: C(2) = ['00','01','11','10'].
%% n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

%% Find out the construction rules and write a predicate with the following specification:

%% % gray(N,C) :- C is the N-bit Gray code

%% Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly? 


-module(p49).

-export([gray/1]).

%% LOGIC:
%% Constructing an n-bit gray code by Binary Reflection
%%   n = 1                   n = 2                                 n = 3
%%  +--0     0  => (prepend 0) 00                    => (prepend 0) 000
%%  |  1-+   1  => (    ""   ) 01                    => (   ""   0) 001
%%  |  --|----------------------- (reflection line)
%%  |    +-> 1  => (prepend 1) 11                    => (   ""   0) 011
%%  +------> 0  => (prepend 1) 10                    => (   ""   0) 010
%%                             ------------------------------------------ (reflection line)
%%                             10                    => (prepend 1) 110
%%                             11                    => (   ""   1) 111
%%                             01                    => (   ""   1) 101
%%                             00                    => (   ""   1) 100
%%                                                                  ----------------------- ......
%% Logic In Words:
%% take (n - 1)th gray code, and make a reflection of it. Append reversed (or reflected) list on to the original
%% list. Prepend Orginal list elements with "0" and reversed list elements with "1". This will give you nth gray code

%% TODO:
%% do the same using binary operations :-)

gray(Nth) ->
    gray(Nth, ['0','1']).

gray(1, Acc) ->
    Acc;
gray(Nth, Acc) ->
    Reflected = lists:reverse(Acc),
    PreOrig = [ list_to_atom("0" ++ atom_to_list(Elem)) || Elem <- Acc ],
    PreRefl = [ list_to_atom("1" ++ atom_to_list(Elem)) || Elem <- Reflected ],
    gray(Nth - 1, lists:append(PreOrig, PreRefl)).
