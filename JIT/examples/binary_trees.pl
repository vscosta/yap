% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),

   MIN_DEPTH is 4, set_limits(N, MIN_DEPTH, MAX_DEPTH, STRETCH_DEPTH),

   bottom_up_tree(0, STRETCH_DEPTH, ST),

   check_tree(ST, ITS),
   format('stretch tree of depth ~w\t check: ~w~n', [STRETCH_DEPTH, ITS]),

   bottom_up_tree(0, MAX_DEPTH, LLT),

   descend_trees(MIN_DEPTH, MIN_DEPTH, MAX_DEPTH),

   check_tree(LLT, ITL),
   format('long lived tree of depth ~w\t check: ~w~n', [MAX_DEPTH, ITL]),
   
   statistics,
   statistics_jit.

% ------------------------------- %

set_limits(N, MinDepth, MaxDepth, StretchDepth) :-
   MinDepth1 is MinDepth + 2,
   (MinDepth1 > N -> MaxDepth is MinDepth1 ; MaxDepth is N),
   StretchDepth is MaxDepth + 1.

% ------------------------------- %

descend_trees(CurrentDepth, MinDepth, MaxDepth) :-
(
   CurrentDepth =< MaxDepth ->
    N is integer(2 ** (MaxDepth - CurrentDepth + MinDepth)), Iterations is 2 * N,
    sum_trees(N, CurrentDepth, 0, Sum),
    format('~w\t trees of depth ~w\t check: ~w~n', [Iterations, CurrentDepth, Sum]),
    NewDepth is CurrentDepth + 2, !, descend_trees(NewDepth, MinDepth, MaxDepth)
;
    true
).

% ------------- %

sum_trees(0, _, AccSum, AccSum) :- !.

sum_trees(N, CurrentDepth, AccSum, Sum) :-
   bottom_up_tree(N, CurrentDepth, TreeLeft),
   Nneg is -1 * N, bottom_up_tree(Nneg, CurrentDepth, TreeRight),
   check_tree(TreeLeft, ItemLeft), check_tree(TreeRight, ItemRight),
   AccSum1 is AccSum + ItemLeft + ItemRight,
   N1 is N - 1, !, sum_trees(N1, CurrentDepth, AccSum1, Sum).

% ------------------------------- %

make_tree(Item, Left, Right, tree(Item, Left, Right)).

% ------------- %

bottom_up_tree(Item, 0, tree(Item, nil, nil)) :- !.

bottom_up_tree(Item, Depth, Tree) :-
   ItemLeft is 2 * Item - 1, DepthLeft is Depth - 1,
   bottom_up_tree(ItemLeft, DepthLeft, TreeLeft),
   ItemRight is 2 * Item, DepthRight is Depth - 1,
   bottom_up_tree(ItemRight, DepthRight, TreeRight),
   make_tree(Item, TreeLeft, TreeRight, Tree).

% ------------- %

check_tree(tree(Item, nil, _), Item) :- !.
check_tree(tree(Item, _, nil), Item) :- !.

check_tree(tree(Item, Left, Right), ItemNew) :-
   check_tree(Left, ItemLeft),
   check_tree(Right, ItemRight),
   ItemNew is Item + ItemLeft - ItemRight.

% ------------------------------- %
