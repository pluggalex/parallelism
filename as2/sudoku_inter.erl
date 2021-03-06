%% -*- erlang-indent-level: 2 -*-
%% -------------------------------------------------------------------
%% This code comes from an Erlang program, originally written by John
%% Hughes, to solve the Sudoku puzzle and be used as a laboratory
%% exercise for his 2014 Parallel Functional Programming course at
%% Chalmers.
%%
%% It has been cleaned up a bit and modified by Kostis Sagonas, and 
%% then Dave Clarke, who are thus responsible for any bug or problem 
%% that might exist.
%% -------------------------------------------------------------------
-module(sudoku_inter).

-export([par_benchmarks/0,benchmarks/0, solve_all/0, solve/1]).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type elem()   :: 0..9.
-type matrix() :: [[elem()]].
-type name()   :: atom().
-type puzzle() :: {name(), matrix()}.
-type musecs() :: non_neg_integer().

-type solution()   :: matrix() | 'no_solution'.
-type bm_results() :: [{name(), float()}].

%%
%% benchmarking code
%%
-define(EXECUTIONS, 1).
-define(PROBLEMS,  "sudoku_problems.txt").
-define(SOLUTIONS, "sudoku_solutions.txt").

-spec par_benchmarks() -> {musecs(), bm_results()}.
par_benchmarks() ->
  {ok, Problems} = file:consult(?PROBLEMS),
  timer:tc(fun () -> par_benchmarks(Problems) end).

-spec par_benchmarks([puzzle()]) -> bm_results().
par_benchmarks([]) -> [];

par_benchmarks([Puzzle|Puzzles]) ->
  Par = self(),
  Ref = make_ref(),
  spawn_link(fun() ->
                 Par ! {Ref, par_benchmarks(Puzzles)}
             end),
  {Name,M} = Puzzle, 
  Result = {Name, bm(fun() -> solve(M) end)}, 
  receive {Ref, Ys} -> [Result|Ys] end.

-spec benchmarks() -> {musecs(), bm_results()}.
benchmarks() ->
  {ok, Problems} = file:consult(?PROBLEMS),
  timer:tc(fun () -> benchmarks(Problems) end).

-spec benchmarks([puzzle()]) -> bm_results().
benchmarks(Puzzles) ->
  [{Name, bm(fun() -> solve(M) end)} || {Name, M} <- Puzzles].

bm(F) ->
  {T, _} = timer:tc(fun () -> repeat(?EXECUTIONS, F) end),
  T / ?EXECUTIONS / 1000.

-spec repeat(non_neg_integer(), fun(() -> term())) -> 'ok'.
repeat(0, _) -> ok;
repeat(N, F) when N > 0 ->
  _ = F(), repeat(N-1, F).

%%
%% solve all puzzles in the (hardcoded) input file
%%
-spec solve_all() -> [{name(), solution()}].
solve_all() ->
  {ok, Puzzles} = file:consult(?PROBLEMS),
  [{Name, solve(M)} || {Name, M} <- Puzzles].

%%
%% solve a Sudoku puzzle
%%
-spec solve(matrix()) -> solution().
solve(M) ->
  Solution = solve_refined(parallel_refine(fill(M))),
  case valid_solution(Solution) of
    true ->
      Solution;
    false -> % in correct puzzles should never happen
      exit({invalid_solution, Solution})
  end.

solve_refined(M) ->
  case solved(M) of
    true ->
      M;
    false ->
      solve_one(guesses(M))
  end.

solve_one([]) ->
  no_solution;
solve_one([M]) ->
  solve_refined(M);
solve_one([M|Ms]) ->
  case solve_refined(M) of
    no_solution ->
      solve_one(Ms);
    Solution ->
      Solution
  end.

%% is a puzzle solved?

solved(no_solution) ->
  true;
solved(M) ->
  lists:all(fun solved_row/1, M).

solved_row(Row) ->
  lists:all(fun is_decided/1, Row).

is_decided(no_solution) ->
  true;
is_decided(X) ->
  1 =< X andalso X =< 9.

%% check solutions for validity

valid_solution(no_solution) ->
  true;
valid_solution(M) ->
  valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).

valid_rows(M) ->
  lists:all(fun valid_row/1, M).

-define(NINE, [1, 2, 3, 4, 5, 6, 7, 8, 9]).

valid_row(Row) ->
  lists:usort(Row) =:= ?NINE.

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
  Nine = ?NINE,
  [[case is_decided(X) of true -> X; false -> Nine end || X <- Row] || Row <- M].

inter(A, B, C) -> sets:to_list(sets:intersection(sets:from_list(A),sets:from_list([B|C]))). 

intersect([], [], [], Acc) -> []; 
intersect([AH|AT], [BH|BT], [CH|CT], Acc) -> intersect(AT,BT,CT, inter(AH, BH, CH) ++ Acc).

%% refine entries which are lists by removing numbers they are known
%% not to be

parallel_refine(M) ->
%  NewM =
%    refine_rows(
%      transpose(
%	refine_rows(
%	  transpose(
%	    unblocks(
%	      refine_rows(
%		blocks(M))))))),
  Par = self(),
%  RowRef = make_ref(),
  ColRef = make_ref(),
  BlockRef = make_ref(),
  spawn_link(fun() -> Par ! {ColRef, refine_rows(transpose(M))} end),
  spawn_link(fun() -> Par ! {BlockRef, refine_rows(blocks(M))} end),
  RowMatrix = refine_rows(M),

  receive {ColRef, Y} -> ColMatrix = transpose(Y) end,
  receive {BlockRef, Z} -> BlockMatrix = unblocks(Z) end,
 %Receive{RowRef, X} -> RowMatrix = X,

  io:format("~w~n", [RowMatrix]),
  io:format("~w~n", [ColMatrix]),
  io:format("~w~n", [BlockMatrix]),
  NewM = intersect(RowMatrix, ColMatrix, BlockMatrix, []),
  io:format("~w~n", [NewM]),
  %InterMatrix = [sets:intersection(Elem1, Elem2) || {Elem1, Elem2} <- [{Row, Col} || {Row,Col} <- lists:zip(RowMatrix,ColMatrix)]],
  %NewM = [sets:intersection(Elem1, Elem2) || {Elem1, Elem2} <- [{Inter, Block} || {Inter,Block} <- lists:zip(InterMatrix, BlockMatrix)]],

  if M =:= NewM ->
      M;
     true ->
      refine(NewM)
  end.


%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
  NewM =
    refine_rows(
      transpose(
	refine_rows(
	  transpose(
	    unblocks(
	      refine_rows(
		blocks(M))))))),
  if M =:= NewM ->
      M;
     true ->
      refine(NewM)
  end.

receive_matrix(Acc, 0) -> Acc;
receive_matrix(Acc, RN) when RN > 0 ->
  receive_matrix([receive{RN, RefinedRow} -> RefinedRow end|Acc], RN-1).

par_refine_awesome(M) ->
  Par = self(),
  RowNumbers = lists:seq(1, length(M)),
 % [spawn_link(fun() -> Par ! {RN, refine_row(R)} end) || {R,RN} <- lists:zip(M, RowNumbers)],
  receive_matrix([], 9). 
%lists:foldr(fun(X,Acc) -> receive {X, RefinedRow} -> [RefinedRow|Acc] end end, [], RowNumbers).


par_spawn_refine_rows(_, []) -> [];
par_spawn_refine_rows(0, M) -> [refine_row(R) || R <- M];
par_spawn_refine_rows(T, M) ->
  Par = self(),
  Ref = make_ref(), 
  {Matrix1, Matrix2} = lists:split(floor(length(M)/2), M),
  spawn_link(fun() ->
                 Par ! {Ref, par_spawn_refine_rows(T-1, Matrix2)}
             end),
  RefinedRow = par_spawn_refine_rows(T-1, Matrix1),
  receive {Ref, Row} -> RefinedRow ++ Row end.

par_refine_rows(no_solution) ->
  no_solution;

par_refine_rows(M) ->
  Refined = par_spawn_refine_rows(1, M),
  %io:format("~w~n\n", [Refined]),
  case lists:member(no_solution, Refined) of
    true -> no_solution;
    false -> Refined
  end.


refine_rows(no_solution) ->
  no_solution;
refine_rows(M) ->
  Refined = [refine_row(R) || R <- M],
  %io:format("~w~n\n", [Refined]),
  case lists:member(no_solution, Refined) of
    true -> no_solution;
    false -> Refined
  end.

refine_row(Row) ->
  Entries = entries(Row),
  NewRow =
    [if is_list(X) ->
	 case X -- Entries of
	   [] ->
	     no_solution;
	   [Y] ->
	     Y;
	   NewX ->
	     NewX
	 end;
	true ->
	 X
     end
     || X <- Row],
  %% check we didn't create a duplicate entry, or any entry that has
  %% no solution anymore; cheat by adding 'no_solution' to new entries
  NewEntries = [no_solution|entries(NewRow)],
  case length(lists:usort(NewEntries)) =:= length(NewEntries) of
    true ->
      NewRow;
    false ->
      no_solution
  end.

entries(Row) ->
  [X || X <- Row, is_decided(X)].

is_wrong(no_solution) ->
  true;
is_wrong(_) ->
  false.

%% how hard is the puzzle?

hard(M) ->
  lists:sum([lists:sum([hardness(X) || X <- Row]) || Row <- M]).

hardness(X) when is_list(X) -> length(X);
hardness(_) -> 0.

%% choose a position {I, J, Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
  Nine = ?NINE,
  {_, I, J, X} =
    lists:min([{length(X), I, J, X}
	       || {I, Row} <- lists:zip(Nine, M),
		  {J, X} <- lists:zip(Nine, Row),
		  is_list(X)]),
  {I, J, X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M0) ->
  {I, J, Guesses} = guess(M0),
  Ms = [refine(update_element(M0, I, J, G)) || G <- Guesses],
  SortedGuesses = lists:sort([{hard(M), M} || M <- Ms, not is_wrong(M)]),
  [G || {_, G} <- SortedGuesses].

%% -------------------------------------------------------------------
%% Matrix operations (with some of their testing code)

transpose(no_solution) ->
  no_solution;
transpose([Row]) ->
  [[X] || X <- Row];
transpose([Row|M]) ->
  [[X|Xs] || {X, Xs} <- lists:zip(Row, transpose(M))].

-ifdef(PROPER).
prop_transpose() ->
  ?FORALL({M, N}, {nat(), nat()},
	  ?FORALL(Mat, vector(M+1, vector(N+1, elem())),
		  transpose(transpose(Mat)) =:= Mat)).
-endif.

update_element(M, I, J, G) ->
  update_nth(I, update_nth(J, G, lists:nth(I, M)), M).

update_nth(I, X, Xs) ->
  {Pre, [_|Post]} = lists:split(I-1, Xs),
  Pre ++ [X|Post].

-ifdef(PROPER).
prop_update() ->
  ?FORALL(L, list(integer()),
	  ?IMPLIES(L =/= [],
		   ?FORALL(I, choose(1, length(L)),
			   update_nth(I, lists:nth(I, L), L) =:= L))).
-endif.

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
  [[A,B,C]|triples(D)];
triples([]) ->
  [].

blocks(no_solution) ->
  no_solution;
blocks(M) ->
  Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
  lists:append([[lists:append(X) || X <- B] || B <- Blocks]).

unblocks(no_solution) ->
  no_solution;
unblocks(M) ->
  case lists:member(no_solution, M) of
    true -> no_solution;
    false ->    
      [lists:append(X)
       || X <- transpose([lists:append(Y)
                          || Y <- [[triples(T) || T <- Ts] || Ts <- triples(M)]])]
  end.

-ifdef(PROPER).
prop_blocks() ->
  ?FORALL(M, vector(9, vector(9, elem())), unblocks(blocks(M)) =:= M).
-endif.

%% -------------------------------------------------------------------
%% EUnit tests below

-ifdef(TEST).
sanity_test_() ->
  [test_benchmarks()].

test_benchmarks() ->
  {ok, Problems} = file:consult(?PROBLEMS),
  {ok, Solutions} = file:consult(?SOLUTIONS),
  ZipF = fun ({Name, P}, {Name, S}) -> {P, S} end,
  Pairs = lists:zipwith(ZipF, Problems, Solutions), % assumes order is the same
  [?_assertEqual(Sol, solve(Problem)) || {Problem, Sol} <- Pairs].
-endif.
