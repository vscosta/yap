main :- $pp_batch.

$pp_batch_call(Goal) :-
    ( call(Goal) -> Res = yes ; Res = no ),
    format("~n~w~n",[Res]).
