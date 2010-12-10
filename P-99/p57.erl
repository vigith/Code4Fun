%% P57 (**) Binary search trees (dictionaries)

%% Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.
%% Example:
%% ?- construct([3,2,5,7,1],T).
%% T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))

%% Then use this predicate to test the solution of the problem P56.
%% Example:
%% ?- test_symmetric([5,3,18,1,4,12,21]).
%% Yes
%% ?- test_symmetric([3,2,5,7,4]).
%% No

-module(p57).

-export([bst/1, test_symmetric_bst/1]).

%% LOGIC:
%% if Element >= Node then traverse to the right else traverse left. Do until a leaf node is reached, insert
%% there and unwind the recursion

%% e.g
%% 26> c(p57).
%% {ok,p57}
%% 27> p57:bst([3,2,5,7,1]).
%% {3,{2,{1,nil,nil},nil},{5,nil,{7,nil,nil}}}
%% 28> p57:test_symmetric_bst([3,2,5,7,1]).
%% true
%% 29> p57:test_symmetric_bst([3,2,5,7,4]).
%% false
%% 30> p57:test_symmetric_bst([5,3,18,1,4,12,21]).
%% true
%% 31> 

bst(List) ->
    bst(List, {}).

bst([H|T], Tree) ->
    bst(T, insert(H, Tree));
bst([], Tree) ->
    Tree.

insert(Elem, {}) ->
    {Elem, nil ,nil};
insert(Elem, {V, L, nil}) when Elem >= V ->
    {V, L, {Elem, nil, nil}};
insert(Elem, {V, L, R}) when Elem >= V ->
    {V, L, insert(Elem, R)};
insert(Elem, {V, nil, R}) when Elem < V ->
    {V, {Elem, nil, nil}, R};
insert(Elem, {V, L, R}) when Elem < V ->
    {V, insert(Elem, L), R}.

test_symmetric_bst(List) ->
    p56:is_symmetric(bst(List)).
