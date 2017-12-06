-module(mergesort).
-export([mergesort/1]).

mergesort([H]) -> [H];
mergesort(List) -> mergesort(4, List).
mergesort(D, List) ->
  Parent = self(),
  [L1, L2] = split:split(List),
  spawn_link( fun () ->
    Parent ! merge:merge(D-1, mergesort(L1), mergesort(L2))
              end),

