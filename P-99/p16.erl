%% P16 (**) Drop every N'th element from a list.

-module(p16).

-export([drop_every_nth/2]).


drop_every_nth(List, Nth) ->
    drop_every_nth(List, Nth, [], Nth).

drop_every_nth([_|T], Nth, Acc, 1) ->
    drop_every_nth(T, Nth, Acc, Nth);
drop_every_nth([], _, Acc, _) ->
    lists:reverse(Acc);
drop_every_nth([H|T], Nth, Acc, Cntr) ->
    drop_every_nth(T, Nth, [H | Acc], Cntr - 1).
