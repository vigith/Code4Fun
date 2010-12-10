%% P55 (**) Construct completely balanced binary trees

%% In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

%% Write a predicate cbal_tree/2 to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
%% Example:
%% ?- cbal_tree(4,T).
%% T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
%% T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
%% etc......No

-module(p55).


-compile(export_all).

%% LOGIC:
%% for each node count create sub trees with N/2 and N/2 - 1 (-1 because root node is to be excluded),
%% create a list comprehension for each different combination of output possible and repeat the above
%% till we hit node count as 1
%% eg.
%% 15> c(p55).
%% {ok,p55}
%% 16> p55:complete_balanced_tree(3).
%% [{x,{x,nil,nil},{x,nil,nil}}]
%% 17> p55:complete_balanced_tree(2).
%% [{x,nil,{x,nil,nil}},{x,{x,nil,nil},nil}]
%% 18> p55:complete_balanced_tree(1).
%% {x,nil,nil}
%% 19>

%% complete_balanced_tree(0) ->
%%     {};
%% complete_balanced_tree(N) ->
%%     complete_balanced_tree(N, {x, nil, nil}).

%% complete_balanced_tree(0, _) ->
%%     nil;
%% complete_balanced_tree(N, Tree = {Elem, _, _}) ->
%%     if
%% 	N rem 2 =:= 0 ->
%% 	    N0 = N div 2,
%% 	    %% remove the parent
%% 	    N1 = N - N0 - 1;
%% 	true ->
%% 	    %% remove the parent
%% 	    N0 = (N + 1) div 2 - 1,
%% 	    N1 = N - 1 - N0
%%     end,
%%     Trees = tree(N0, N1),
%%     [{X1,X2}|_] = Trees,
%%     if
%% 	length(Trees) =:= 2 ->
%% 	    A1 = {Elem, complete_balanced_tree(X1, Tree), complete_balanced_tree(X2, Tree) },
%% 	    A2 = {Elem, complete_balanced_tree(X2, Tree), complete_balanced_tree(X1, Tree) },
%% 	    join(A1, A2),
%% 	true ->
%% 	    A1 = {Elem, complete_balanced_tree(X2, Tree), complete_balanced_tree(X1, Tree) }
%%     end.
		

complete_balanced_tree(0, _Acc) ->
    [nil];
complete_balanced_tree(N, Acc) ->    
    if
	N rem 2 =:= 0 ->
	    N0 = N div 2,
	    %% remove the parent
	    N1 = N - N0 - 1;
	true ->
	    %% remove the parent
	    N0 = (N + 1) div 2 - 1,
	    N1 = N - 1 - N0
    end,
    Trees = tree(N0, N1),
    [{X1,X2}|_] = Trees,
    if
	length(Trees) =:= 2 ->
	    [complete_tree({x, X1, X2}, Acc),
	    complete_tree({x, X2, X1}, Acc)];
	true ->
	    [complete_tree({x, X1, X1}, Acc)]
    end.

complete_tree({Elem, 0, 0}, _) ->
    {Elem, nil, nil};
complete_tree({Elem, L, R}, Acc) ->
    NewAcc = append({L, R}, Acc, []),
    {Elem, complete_balanced_tree(L, NewAcc), complete_balanced_tree(R, NewAcc)}.


append({A, B}, [H|T], Acc) when A =:= nil, B =:= nil ->
    NewH = traverse_append({nil, nil}, H),
    io:format("~w||~w-~w~n", [A, B, NewH]),
    append({A, B}, T, lists:append(Acc, NewH));
append(_, [], Acc) ->
    Acc.

traverse_append({A, B}, Tree) ->
    Left = traverse(A, Tree, left),
    Right = traverse(A, Tree, right),
    {x, Left, Right}.

traverse(_, _, _) ->
    stub.


tree(N, N) ->
    [{N, N}];
tree(N0, N1) ->
    [{N0, N1}, {N1, N0}].


expand({Elem, L = [Hl|Tl], R = [Hr|Tr]}) ->
    A = {Elem, expand(Hl), expand(Hr)},
    io:format("~w~n", [A]),
    expand(L),
    expand(R),
    A;
expand([Z]) when is_atom(Z) ->
    Z;
expand([]) ->
    nil;
expand(Z) when is_atom(Z) ->
    Z;
expand([H|T]) ->
    expand(H),
    expand(T).
