%% P27 (**) Group the elements of a set into disjoint subsets.

%% a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a predicate that generates all the possibilities via backtracking.

%% Example:
%% ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
%% G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
%% ...

%% b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

%% Example:
%% ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
%% Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
%% ...

%% Note that we do not want permutations of the group members; i.e. [[aldo,beat],...] is the same solution as [[beat,aldo],...]. However, we make a difference between [[aldo,beat],[carla,david],...] and [[carla,david],[aldo,beat],...].

%% You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients". 

-module(p27).

-export([group/2]).

%% LOGIC:
%% group (do combination) on first condition ie N at a time and apply to the rest, grouping on next condition,
%% do this recursively

%% NOTE: not sure whether there is a better solution wrt Big-Oh! :-) (because lists:subtract is O(len(A)*len(B))

%% TODO: i skipped the first part and made the generalized solution which includes first part

group(List, Conditions) ->
    Ans = group(List, Conditions, []),
    pretty_print(Ans),
    ok.

pretty_print([H|T]) ->
    io:format("Set : ~p~n", [H]),
    pretty_print(T);
pretty_print([]) ->
    ok.

group([], _, Acc) ->
    Acc;
group(_, [], _) ->
    no_proper_distribution;
group(List, [H|T], Acc) ->
    [
     group(Rest, T, [Set | Acc]) 
     || {Set, Rest} <- combination(H, List)
    ].


combination(Count, List) ->
    %% return {combination, elements not in combination}
    [
     {Set, lists:subtract(List, Set)}
     || Set <- combination(Count, List, [none])
    ].


combination(Count, [H|T], P) ->
    combination(Count, T, lists:append(P, product(H, P)));
combination(Count, [], P) ->
    select(Count, P, []).

select(Count, [none | T], Acc) ->
    select(Count, T, Acc);
select(_, [], Acc) ->
    Acc;
select(Count, [H|T], Acc) ->
    if
	length(H) =:= Count ->
	    select(Count, T, [H|Acc]);	    
	true ->
	    select(Count, T, Acc)
    end.


product(H, P) ->
    product(H, P, []).
    

product(_, [], Acc) ->
    Acc;
product(H, [H1|T1], Acc) ->
    if 
	H1 =:= none ->
	    product(H, T1, [[H] | Acc]);
	true ->
	    %% append the new item to the already present sub-list from Product list (P)
	    product(H, T1, [lists:append(H1,[H]) | Acc])
    end.
