%% P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list

%% In how many ways can a committee of 3 be chosen from a group of 12 people? 
%% We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
%% For pure mathematicians, this result may be great. But we want to really generate all the possibilities
%% (via backtracking).

%% Example:
%% ?- combination(3,[a,b,c,d,e,f],L).
%% L = [a,b,c] ;
%% L = [a,b,d] ;
%% L = [a,b,e] ;
%% ... 

-module(p26).

-export([combination/2]).

%% LOGIC:
%% do bottom up and each level up do a product with subset of that element with already calcualted product
%% eg, 4C3 on (a, b, c, d), phi is denoted by %
%% we start with rest list and product list
%% in beginning we have rest list (R) = (a, b, c, d) and product list (P) = ( % )
%% 1. pop one element (d) from R and have a product with all in P list
%%    (d, %) X ( % ) => (d, %) = P
%%    R = (a, b, c) 
%% 2. pop c
%%    (c, %) X (d, %) => (cd, c, d, %) = P
%%    R = (a, b)
%% 3. pop b
%%    (b, %) X (cd, c, d, %) = (bcd, bc, bd, b, cd, c, d, %) = P
%%    R = (a)
%% 4. pop a
%%    (a, %) X (bcd, bc, bd, b, cd, c, d, %) = (abcd, abc, abd, ab, acd, ac, ad, a, 
%%                                                     bcd, bc, bd, b, cd, c, d, %) = P
%%    R = ()
%% 5. select all element with size 3 from P
%%    (abc, abd, acd, bcd) = Answer

combination(Count, List) ->
    combination(Count, List, [none]).

combination(Count, [H|T], P) ->
    combination(Count, T, lists:append(P, product(H, P)));
combination(Count, [], P) ->
    select(Count, P, []).

select(Count, [none | T], Acc) ->
    select(Count, T, Acc);
select(_, [], Acc) ->
    Acc;
select(Count, [H|T], Acc) ->
    Tmp = atom_to_list(H),
    if
	length(Tmp) =:= Count ->
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
