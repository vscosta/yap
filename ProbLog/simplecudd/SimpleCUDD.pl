:-use_module(library(system)).
%:-use_module(library(clib)).

bdd_init(FDO, FDI, PID):-
  exec('/home/theo/BDDs/SimpleCUDD/Version4/Example -online', [pipe(FDO), pipe(FDI), std], PID).
  %process_create('/home/theo/BDDs/SimpleCUDD/Version3/Example', ['-online'], [stdin(pipe(FDI)), stdout(pipe(FDO)), process(PID)]).

bdd_commit(FDO, LINE):-
  write(FDO, LINE),
  write(FDO, '\n').

bdd_kill(FDO, FDI, PID, S):-
  bdd_commit(FDO, '@e'),
  wait(PID, S),
  %process_wait(PID, S),
  close(FDO),
  close(FDI).

bdd_line([], X, _, L):-
  atomic(X),
  X \= [],
  (bdd_curinter(N) ->
    retract(bdd_curinter(N))
  ;
    N = 1
  ),
  M is N + 1,
  assert(bdd_curinter(M)),
  atomic_concat(['L', N, '=', X], L).

bdd_line(L, X, O, NL):-
  atomic(X),
  X \= [],
  atom(L),
  L \= [],
  atomic_concat([L, O, X], NL).

bdd_line(L, [], _, L):-!.

bdd_line(L, [X|T], O, R):-
  bdd_line(L, X, O, NL),
  bdd_line(NL, T, O, R).

bdd_AND(L, X, NL):-
  bdd_line(L, X, '*', NL).
bdd_OR(L, X, NL):-
  bdd_line(L, X, '+', NL).
bdd_XOR(L, X, NL):-
  bdd_line(L, X, '#', NL).
bdd_NAND(L, X, NL):-
  bdd_line(L, X, '~*', NL).
bdd_NOR(L, X, NL):-
  bdd_line(L, X, '~+', NL).
bdd_XNOR(L, X, NL):-
  bdd_line(L, X, '~#', NL).

bdd_not(X, NX):-
  atomic(X),
  atomic_concat(['~', X], NX).

bdd_laststep(L):-
  bdd_curinter(N),
  M is N - 1,
  atomic_concat(['L', M], L),
  !.

bdd_nextDFS(FDO):-
  bdd_commit(FDO, '@n').

bdd_nextBFS(FDO):-
  bdd_commit(FDO, '@n,BFS').

bdd_current(FDO, FDI, N, Qcnt):-
  bdd_commit(FDO, '@c'),
  read(FDI, F),
  assert(F),
  bdd_temp_value(N, Qcnt),
  retract(F).

bdd_highnodeof(FDO, FDI, H):-
  bdd_commit(FDO, '@h'),
  read(FDI, F),
  assert(F),
  bdd_temp_value(H),
  retract(F).

bdd_lownodeof(FDO, FDI, L):-
  bdd_commit(FDO, '@l'),
  read(FDI, F),
  assert(F),
  bdd_temp_value(L),
  retract(F).

bdd_nodevaluesof(FDO, FDI, N, V):-
  atomic_concat(['@v,', N], Q),
  bdd_commit(FDO, Q),
  read(FDI, F),
  assert(F),
  bdd_temp_value(V),
  retract(F).
/*
bdd_addnodetohis(FDO, N, [D, I, Dyn]):-
  atomic_concat(['@a,', N, ',', D, ',', I, ',', Dyn], Q),
  bdd_commit(FDO, Q).

bdd_getnodefromhis(FDO, FDI, N, V):-
  atomic_concat(['@g,', N], Q),
  bdd_commit(FDO, Q),
  read(FDI, F),
  assert(F),
  bdd_temp_value(V),
  retract(F).
*/

runme:-
  bdd_init(FDO, FDI, PID),
  bdd_AND([], ['A', 'B', 'C', 'D', 'E'], L1),
  bdd_laststep(L1S),
  bdd_commit(FDO, L1),
  bdd_AND([], ['A', 'F', 'G', '~B'], L2),
  bdd_laststep(L2S),
  bdd_commit(FDO, L2),
  bdd_AND([], ['A', 'F', 'G', '~C'], L3),
  bdd_laststep(L3S),
  bdd_commit(FDO, L3),
  bdd_OR([], [L1S, L2S, L3S], L4),
  bdd_laststep(L4S),
  bdd_commit(FDO, L4),
  bdd_commit(FDO, L4S),

  repeat,
  bdd_current(FDO, FDI, N, I),
  write(1),nl,
  bdd_nodevaluesof(FDO, FDI, N, V),
  write(N), write(' ('), write(V), write(')'), nl,
  bdd_next(FDO),
  I = 0, (N = 'TRUE' ; N = 'FALSE'),

  bdd_kill(FDO, FDI, PID, S),
  write('BDD terminated with state: '), write(S), nl.

