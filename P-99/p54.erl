%% P54 (*) Check whether a given term represents a binary tree

%% Write a predicate istree/1 which succeeds if and only if its argument is a Prolog term representing a binary tree.
%% Example:
%% ?- istree(t(a,t(b,nil,nil),nil)).
%% Yes
%% ?- istree(t(a,t(b,nil,nil))).
%% No

-module(p54).

-export([is_tree/1]).

is_tree(nil) ->
    true;
is_tree({_Root, Left, Right}) ->
    io:format("Root: ~w~n", [_Root]),
    is_tree(Left),
    is_tree(Right);
is_tree({}) ->
    true;
is_tree(_) ->
    false.
