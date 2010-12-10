%% P28 (**) Sorting a list of lists according to length of sublists

%% a) We suppose that a list (InList) contains elements that are lists themselves. The objective is to sort the elements of InList according to their length. E.g. short lists first, longer lists later, or vice versa.

%% Example:
%% ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%% L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]

%% b) Again, we suppose that a list (InList) contains elements that are lists themselves. But this time the objective is to sort the elements of InList according to their length frequency; i.e. in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

%% Example:
%% ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%% L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]

%% Note that in the above example, the first two lists in the result L have length 4 and 1, both lengths appear just once. The third and forth list have length 3 which appears, there are two list of this length. And finally, the last three lists have length 2. This is the most frequent length.

-module(p28).

-export([length_sort/1, length_freq_sort/1]).


%% LOGIC:
%% a. length_sort
%% ==============
%% insert the sub_list into the nth position of a list where n = length(sub_list)
%% always make sure the list is long enough to insert the sub-list in nth postn, if not fill will 
%% empty entries. This approach will be less Big-Oh O(nlog(n)).
%% eg.
%% 392> p28:length_sort([[1,1,2], [], [1], [1,2,3], [1,2,3,4,5]]).
%% [[[]],[[1]],[[1,1,2],[1,2,3]],[[1,2,3,4,5]]]
%%
%% b. length_freq_sort
%% ===================
%% use the same logic as above, except for we will insert straight into another list the output of first answer
%% and it will sort again on the contents on sub-list of sub-list (this ensures the least occuring length will be taken
%% as the key for sorting)
%% 421> p28:length_freq_sort([[1],[1],[2], [1,2]]).
%% [[[1,2]],[[1],[1],[2]]]

length_sort(List) ->
    [ Elem || Elem <- length_sort(List, [[]]), length(Elem) > 0 ].

length_sort([], Acc) ->
    Acc;
length_sort([H|T], Acc) ->
    length_sort(T, insert(H, length(H) + 1, Acc)). %% + 1 because there can be empty sublists


insert(H, LenH, Acc) when LenH =< length(Acc) ->
    insert_at(Acc, H, LenH);
insert(H, LenH, Acc) ->
    Tmp = [ [] || _ <- lists:seq(length(Acc), LenH - 1, 1) ], %% - 1 because seq includes the stopping condn
    NewAcc = lists:append(Acc, Tmp),
    insert_at(NewAcc, H, LenH).


insert_at(Acc, H, LenH) ->
    insert_at(Acc, H, LenH, []).

insert_at([Postn|Rest], H, 1, Acc) ->
    lists:append(lists:append(lists:reverse(Acc), [lists:append(Postn, [H])]), Rest);
insert_at([H1|T], H, LenH, Acc) ->
    insert_at(T, H, LenH - 1, [H1|Acc]).


length_freq_sort(List) ->
    [ Elem || [Elem | _] <- length_freq_sort(length_sort(List), []), length(Elem) > 0 ]. %% remove the nested listing

length_freq_sort([], Acc) ->
    Acc;
length_freq_sort([H|T], Acc) ->  
    length_freq_sort(T, insert(H, length(H) + 1, Acc)). %% + 1 because there can be empty sublists
