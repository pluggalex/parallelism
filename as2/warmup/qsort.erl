-module(qsort).

-export([benchmark/2, qsort/1, pqsort/1, pqsort2/1, pqsort3/1, pqsort4/1, random_list/1]).

qsort([]) -> [];
qsort([P|Xs]) ->
  qsort([X || X <- Xs, X =< P])
  ++ [P]  % pivot element
  ++ qsort([X || X <- Xs, P < X]).

random_list(N) ->
  [rand:uniform(12345678) || _ <- lists:seq(1,N)].
	
benchmark(Fun, L) ->
  Rs = [timer:tc(?MODULE, Fun, [L])
      || _ <- lists:seq(1, 100)],
  lists:sum([T || {T,_} <- Rs]) / (1000*length(Rs)).


pqsort([]) -> [];
pqsort([P|Xs]) ->
  Parent = self(),
  spawn_link(fun () ->
               Parent ! pqsort([X || X <- Xs, P < X])
             end),
  pqsort([X || X <- Xs, X =< P])
  ++ [P]
  ++ receive Ys -> Ys end.


pqsort2(L) -> pqsort2(5, L).

pqsort2(0, L) -> qsort(L);
pqsort2(_, []) -> [];
pqsort2(D, [P|Xs]) ->
  Par = self(),
  spawn_link(fun () ->
    Par ! pqsort2(D-1,[X || X <- Xs, P < X])
  end),
pqsort2(D-1, [X || X <- Xs, X =< P])
  ++ [P]
  ++ receive Ys -> Ys end.

pqsort3(L) -> pqsort3(5, L).

pqsort3(0, L) -> qsort(L);
pqsort3(_, []) -> [];
pqsort3(D, [P|Xs]) ->
  Par = self(),
  Ref = make_ref(),
  spawn_link(fun () ->
               Gs = [X || X <- Xs, P < X],
               Par ! {Ref, pqsort3(D-1, Gs)}
  end),
  pqsort3(D-1, [X || X <- Xs, X =< P])
    ++ [P]
    ++ receive {Ref, Ys} -> Ys end.
		
pqsort4(L) -> pqsort4(5, L).
pqsort4(0, L) -> qsort(L);
pqsort4(_, []) -> [];
pqsort4(D, [P|Xs]) ->
  Par = self(),
  Ref = make_ref(),
  Gs = [X || X <- Xs, P < X],
  spawn_link(fun () ->
               Par ! {Ref, pqsort4(D-1, Gs)}
             end),
  pqsort4(D-1, [X || X <- Xs, X =< P])
  ++ [P]
  ++ receive {Ref, Ys} -> Ys end.
	
