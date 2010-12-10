%% P56 (**) Symmetric binary trees

%% Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

-module(p56).

-export([is_symmetric/1]).

%% LOGIC:
%% Left Node's Left child should be symmetric to Right Node's Right child and likewise
%% Left Node's Right child should be symmetric to Right Node's Left child

%% e.g
%% 19> c(p56).
%% {ok,p56}
%% 20> p56:is_symmetric({x,{x,{b,nil, nil}, nil}, {x,{a,nil,nil},nil}}).
%% false
%% 21> p56:is_symmetric({x,{x,nil,{b,nil, nil}}, {x,{a,nil,nil},nil}}).
%% true
%% 22>

is_symmetric({}) ->
    true;
is_symmetric({_, A, B}) ->
    is_mirror(A, B).

is_mirror({_, A, B}, {_, C, D}) when is_tuple(A), is_tuple(B), is_tuple(C), is_tuple(D) ->
    is_mirror(A, D), is_mirror(B, C),
    true;
is_mirror({_, nil, B}, {_, C, nil}) when is_tuple(B), is_tuple(C) ->
    is_mirror(B, C),
    true;
is_mirror({_, A, nil}, {_, nil, D}) when is_tuple(A), is_tuple(D) ->
    is_mirror(A, D),
    true;
is_mirror({_, nil, nil}, {_, nil, nil}) ->
    true;
is_mirror(_, _) ->
    false.
