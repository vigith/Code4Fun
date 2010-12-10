%% P50 (***) Huffman code.

%% First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!

%% We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:

%% % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs 

-module(p50).

-export([huffman_code/1]).

%% LOGIC:
%% TREE CREATION
%% =============
%% 1. sort the initial list
%% 2. extract the last two (because they will the least two)
%% 3. sum-up their values and combine the two elements and append to the orginal list
%% 4. do a sort the new list (could have done an insertion on the right position which would have been better interms of complexity, but algo won't change though)
%%    4.a. remove the last two individual elements which has been sum-up'ed
%% 5. iterate will we have only one big combined element
%% 6. create code such that left tree is 0 and right is 1
%% CODE BUILDING
%% =============
%% 1. start the tree with prefix 0
%% 2. if it is a head which is NOT a list prepend to the accumulator {prefix, elem}
%% 3. head which is a list will recurse itself with prefix appended by 0
%% 4. for tail, invert the last character (ie, replace 0 by 1) and recurse

%% eg.
%% 149> p50:huffman_code([{a,45},{b,13},{c,12},{d,16},{e,9},{f,5}]).
%% Huffman Tree: [a,[[c,b],[[f,e],d]]]
%% Huffman Code: [{'111',d},{'1101',e},{'1100',f},{'101',b},{'100',c},{'0',a}]
%% ok
%% 150>

huffman_code(FreqList) when length(FreqList) =:= 1 ->
    [{Code, _Count}|_] = FreqList,
    io:format("Huffman Tree: ~p~n", [Code]),
    Ans = huffman_code(Code, '0', []),
    io:format("Huffman Code: ~p~n", [Ans]);
huffman_code(FreqList) ->
    SortedFreqList = lists:reverse(lists:keysort(2, FreqList)),
    {{E1, V1}, Tmp1} = p20:remove_kth(SortedFreqList,length(SortedFreqList)),
    {{E2, V2}, Tmp2} = p20:remove_kth(Tmp1,length(Tmp1)),
    huffman_code(lists:append(Tmp2, [{[E1 | [E2]], V1 + V2}])).


huffman_code([H|T], Prefix, Acc) when is_list(H) ->
    NewAcc = huffman_code(H, list_to_atom(atom_to_list(Prefix) ++ "0"), Acc),
    huffman_code(T, list_to_atom((atom_to_list(Prefix) -- "0") ++ "1"), NewAcc);		 
huffman_code([H|T], Prefix, Acc) ->
    huffman_code(T, list_to_atom((atom_to_list(Prefix) -- "0") ++ "1"), [{Prefix, H} | Acc]);
huffman_code([], _, Acc) ->
    Acc.
