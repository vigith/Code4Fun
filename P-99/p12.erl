%% P12 (**) Decode a run-length encoded list.
%% Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

-module(p12).

-export([decode_rlc/1]).

%% LOGIC:
%% could have done better by lists:fold

decode_rlc(List) ->
    decode_rlc(List, []).

decode_rlc([H|T], Acc) when is_list(H) ->
    Expanded = expand(H),
    decode_rlc(T, lists:append(Acc, Expanded));
decode_rlc([H|T], Acc) ->
    decode_rlc(T, lists:append(Acc, [H]));
decode_rlc([], Acc) ->
    Acc.

expand([H|T]) ->
    expand(H, T, []).

expand(0, _, Acc) ->
    Acc;
expand(H, T, Acc) ->
    expand(H - 1, T, lists:append(Acc, T)).
