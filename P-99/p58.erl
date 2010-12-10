%% P58 (**) Generate-and-test paradigm

%% Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. Example:
%% ?- sym_cbal_trees(5,Ts).
%% Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

%% How many such trees are there with 57 nodes? Investigate about how many solutions there are for a given number of nodes? What if the number is even? Write an appropriate predicate.

-module(p58).

-export([symm_cbalanced_trees/1]).

-compile(export_all).

%% LOGIC:
%% use the previous code to generate all the sets of complete balanced tree and filter off unsymmetric trees
%% using test_symmetric_bst function written in p57.

symm_cbalanced_trees(N) ->
    symm_cbalanced_trees(p55:complete_balanced_tree(N), []).

symm_cbalanced_trees([H|T], Acc) ->
    symm_cbalanced_trees(T, lists:append(Acc, get_all_trees(H)));
symm_cbalanced_trees([], Acc) ->
    Acc.

get_all_trees({Elem, L, R}) when is_list(L), is_list(R) ->
    [ {Elem, get_all_trees(Left), get_all_trees(Right)} || {Left, Right} <- combination(L, R) ],    
get_all_trees({Elem, L, R}) when is_list(L) ->
    [ {Elem, get_all_trees(Left), get_all_trees(Right)} || {Left, Right} <- combination(L, [R]) ];
get_all_trees({Elem, L, R}) when is_list(R) ->
    [ {Elem, get_all_trees(Left), get_all_trees(Right)} || {Left, Right} <- combination([L], R) ];
get_all_trees({Elem, L, R}) ->
    [ {Elem, Left, Right} || {Left, Right} <- combination([L], [R]) ].


combination(L, R) ->
    combination(L, R, []).

combination([Hl|Tl], [Hr|Tr], Acc) when is_list(Hl), is_list(Tl) ->
    combination(Tl, Tr, lists:append(Acc, combination(Hl, Hr, Acc)));
combination(L = [Hl|Tl], R = [Hr | Tr], Acc) ->
    if
	is_list(Hr) ->
	    combination(L, Tr, lists:append(Acc, combination(L, Hr, Acc)));
	is_list(Hl) ->
	    combination(Tl, R, lists:append(Acc, combination(Hl, R, Acc)));
	true ->
	    combination(Tl, R, lists:append(Acc, [ {Hl, R1} || R1 <- R ]))
    end;
combination([], _, Acc) ->
    Acc.
    
    
