testpred(_1) :-
    debug(jpl, '~w', [_1]).

stats :-
    garbage_collect_atoms,
    statistics.
