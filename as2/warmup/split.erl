-module(split).
-export([split/1]).


split(List) -> split(List, [], []).
split([], Acc1, Acc2) -> [Acc1, Acc2];
split([H], Acc1, Acc2) -> [[H|Acc1], Acc2];
split([H1, H2| Tail], Acc1, Acc2) ->
  split(Tail, [H1|Acc1], [H2|Acc2]).
