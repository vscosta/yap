%%
%% sample.pl: routines for sampling execution
%%
%% <ex.>
%%     | ?- sample(bloodtype(X)).
%%  
%%     X = a ?
%%
%% Also available for Utility program.
%% <ex.>
%% go(Loc,Dir) :-
%%     ( is_wall(forward,Loc),
%%       sample(coin(X)),
%%       ( X = head,!,Dir = right
%%       ; Dir = left
%%       )
%%     ; Dir = forward
%%     ).

sample(Goal) :-
    $pp_require_probabilistic_atom(Goal,$msg(1201),sample/1),
    $trace_call(Goal).   % just calls call(Goal) if not in debug mode

%%----------------------------------------------------------------------------

msw(Sw,V) :-
    $pp_require_ground(Sw,$msg(0101),msw/2),
    $prism_sample_msw(Sw,V).

% Sw is assumed to be ground in $prism_sample_msw/{2,5}.

$prism_sample_msw(Sw,V) :-
    $pp_get_parameters(Sw,Values,Pbs),!,
    sumlist(Pbs,Sum),
    random_uniform(Sum,R),
    $pp_choose(Pbs,R,Values,V,_P).

$prism_sample_msw(Sw,V,Depth,_CP,CallNo,AR) :-
    $pp_get_parameters(Sw,Values,Pbs),!,
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Call: ',(msw(Sw,V):P),Depth,CallNo,AR),
    sumlist(Pbs,Sum),
    random_uniform(Sum,R),
    ( $pp_choose(Pbs,R,Values,V,P) ->
          $print_call(Flag,'   Exit: ',(msw(Sw,V):P),Depth,CallNo,AR)
    ; $print_call(Flag,'   Fail: ',msw(Sw,V),Depth,CallNo,AR),
      fail
    ).

$pp_choose(Pbs,R,Vs,X,P) :- $pp_choose(0,Pbs,R,Vs,X,P).
$pp_choose(CPb,[Pb|Pbs],R,[V|Vs],X,P) :-
    CPb1 is CPb+Pb,
    ( R < CPb1 -> X = V, P = Pb
    ; Pbs = [] -> X = V, P = Pb
    ; $pp_choose(CPb1,Pbs,R,Vs,X,P)
    ).

%%----------------------------------------
%%  sampling utils

get_samples(N,G,Gs) :-           % G assumed to never fail
    $pp_require_positive_integer(N,$msg(1203),get_samples/3),
    $pp_require_probabilistic_atom(G,$msg(1201),get_samples/3),	
    $pp_get_samples(0,N,G,Gs).

$pp_get_samples(N,N,_,[]) :- !.
$pp_get_samples(N0,N,G,[G1|Gs]) :-
    copy_term(G,G1),!,
    sample(G1),
    N1 is N0 + 1,!,
    $pp_get_samples(N1,N,G,Gs).

get_samples_c(N,G,Gs) :- get_samples_c(N,G,true,Gs).

get_samples_c(N,G,C,Gs) :-
    get_samples_c(N,G,C,Gs,[NS,NF]),
    format("sampling -- #success = ~w~n",[NS]), 
    format("sampling -- #failure = ~w~n",[NF]).

get_samples_c(PairN,PairG,C,Gs,[NS,NF]) :-
    ( [N,M] = PairN -> true ; N = PairN, M = PairN ),
    ( [S,G] = PairG -> true ; S = PairG, G = PairG ),
    $pp_require_positive_integer_or_infinity(N,$msg(1204),get_samples_c/5),
    $pp_require_positive_integer(M,$msg(1203),get_samples_c/5),
    $pp_require_probabilistic_atom(S,$msg(1201),get_samples_c/5),
    $pp_require_callable(C,$msg(1202),get_samples_c/5),
    $pp_get_samples_c(0,N,M,S,G,C,Gs,0,NS,0,NF).

$pp_get_samples_c(N,N,_ ,_,_,_,[],NS,NS,NF,NF) :- !.
$pp_get_samples_c(_,_,NS,_,_,_,[],NS,NS,NF,NF) :- !.

$pp_get_samples_c(N0,N,M,S,G,C,Gs,NS0,NS,NF0,NF) :-
    copy_term([S,G,C],[S1,G1,C1]),!,
    ( sample(S1),!,call(C1) ->
        Gs = [G1|Gs1], NS1 is NS0 + 1, NF1 is NF0
    ;   Gs = Gs1,      NS1 is NS0,     NF1 is NF0 + 1
    ),
    N1 is N0 + 1,!,
    $pp_get_samples_c(N1,N,M,S,G,C,Gs1,NS1,NS,NF1,NF).

%%----------------------------------------

$pp_require_positive_integer_or_infinity(X,MsgID,Source) :-
    ( ( X == inf ; integer(X), X > 0 ) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_positive_integer_or_infinity)
    ).

$pp_error_positive_integer_or_infinity(X,Error) :-
    X \== inf,
    ( $pp_error_integer(X,Error)
    ; X =< 0 -> Error = domain_error(infinity_or_greater_than_zero,X)
    ).
