%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% set_sw/1,set_sw/2: initialize the prob. of MSW

set_sw(Sw) :- set_sw(Sw,default).

set_sw(Sw,Dist) :-
    $pp_require_ground(Sw,$msg(0101),set_sw/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),set_sw/2),
    $pp_require_distribution(Dist,$msg(0200),set_sw/2),
    $pp_set_sw(Sw,Dist).

$pp_set_sw(Sw,Dist) :-
    ( $pd_fixed_parameters(Sw) -> $pp_raise_warning($msg(0109),[Sw])
    ; $pp_get_values(Sw,Values),
      length(Values,N),
      expand_probs(Dist,N,Probs),
      ( retract($pd_parameters(Sw,_,_)) -> true ; true),
      assert($pd_parameters(Sw,Values,Probs))
    ),!.

%% set_sw_all(Sw): set parameters to all switches that matches with Sw.

set_sw_all          :- $pp_set_sw_all(_,default).
set_sw_all(Sw)      :- $pp_set_sw_all(Sw,default).
set_sw_all(Sw,Dist) :- $pp_set_sw_all(Sw,Dist).

$pp_set_sw_all(Sw,Dist) :-
    findall(Sw,$pp_registered_sw(Sw),Sws),
    $pp_set_sw_list(Sws,Dist),!.

$pp_set_sw_list([],_).
$pp_set_sw_list([Sw|Sws],Dist) :-
    set_sw(Sw,Dist),!,
    $pp_set_sw_list(Sws,Dist).

% fix switches
fix_sw(Sw,Dist) :-
    $pp_require_ground(Sw,$msg(0101),fix_sw/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),fix_sw/2),
    $pp_require_distribution(Dist,$msg(0200),fix_sw/2),
    $pp_unfix_sw(Sw),
    $pp_set_sw(Sw,Dist),
    $pp_fix_sw(Sw),!.

fix_sw(Sw) :- var(Sw),!,
    ( get_sw(switch(Sw1,_,_,_)),
      fix_sw(Sw1),
      fail
    ; true
    ).
fix_sw(Sw) :- Sw = [_|_],!,
    $pp_fix_sw_list(Sw).
fix_sw(Sw) :-
    ( $pd_parameters(Sw,_,_),
      $pp_fix_sw(Sw),
      fail
    ; true
    ).

$pp_fix_sw_list([]).
$pp_fix_sw_list([Sw|Sws]) :-
    fix_sw(Sw),!,
    $pp_fix_sw_list(Sws).

$pp_fix_sw(Sw) :-
    ( $pd_fixed_parameters(Sw) -> true
    ; assert($pd_fixed_parameters(Sw))
    ).

unfix_sw(Sw) :- var(Sw),!,
    ( get_sw(switch(Sw1,_,_,_)),
      unfix_sw(Sw1),
      fail
    ; true
    ).
unfix_sw(SwList) :- SwList = [_|_],!,$pp_unfix_sw_list(SwList).
unfix_sw(Sw) :-
    ( $pd_parameters(Sw,_,_),
      $pp_unfix_sw(Sw),
      fail
    ; true
    ).

$pp_unfix_sw_list([]).
$pp_unfix_sw_list([Sw|Sws]) :-
    $pp_unfix_sw(Sw),!,
    $pp_unfix_sw_list(Sws).

$pp_unfix_sw(Sw) :-
    ( retract($pd_fixed_parameters(Sw)) -> true ; true).

% show msw
show_sw :- show_sw(_).

show_sw(Sw) :-
   findall(Sw,$pp_registered_sw(Sw),Sws0),
   sort(Sws0,Sws),
   $pp_show_sw_list(Sws).

$pp_show_sw_list([]) :- !.
$pp_show_sw_list([Sw|Sws]) :-!,
   $pp_show_sw1(Sw),!,
   $pp_show_sw_list(Sws).

% We can assume Sw is ground
$pp_show_sw1(Sw) :-
    $pp_get_parameters(Sw,Values,Probs),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_parameters(Sw) -> write('fixed_p:') ; write('unfixed_p:') ),
    $pp_show_sw_values(Values,Probs),
    nl.

$pp_show_sw_values([],_Ps).
$pp_show_sw_values([V|Vs],[P|Ps]) :-
    format(" ~w (p: ~9f)",[V,P]),!,
    $pp_show_sw_values(Vs,Ps).

get_sw(Sw) :-
    get_sw(SwName,Status,Values,Probs),
    Sw = switch(SwName,Status,Values,Probs).

get_sw(Sw,[Status,Values,Probs]) :-
    get_sw(Sw,Status,Values,Probs).

% - Inconsitency of outcome spaces are checked in advance in
%   $pp_get_parameters/3 and $pp_get_expectations/3.

get_sw(Sw,Status,Values,Probs) :-
    $pp_get_parameters(Sw,Values,Probs),
    ( $pd_fixed_parameters(Sw) -> Status = fixed ; Status = unfixed ).
    
get_sw(Sw,Status,Values,Probs,Es) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_expectations(Sw,_,Es),
    ( $pd_fixed_parameters(Sw) -> Status = fixed ; Status = unfixed ).

%% save/restore switch information

save_sw :- save_sw('Saved_SW').

save_sw(File) :-
    open(File,write,OutStream),
    ( get_sw(SwName,Status,Values,Probs),
      format(OutStream,"switch(~q,~q,~q,",[SwName,Status,Values]),
      $pp_write_distribution(OutStream,Probs,'['),
      format(OutStream,"]).~n",[]),
      fail
    ; true
    ),
    close(OutStream),!.

$pp_write_distribution(_,[],_).
$pp_write_distribution(OutStream,[Prob|Probs],C) :-
    format(OutStream,"~w~15e",[C,Prob]),!,
    $pp_write_distribution(OutStream,Probs,',').

restore_sw :- restore_sw('Saved_SW').

restore_sw(File) :-
    open(File,read,InStream),
    repeat,
    read(InStream,Switch),
    ( Switch == end_of_file
    ; Switch = switch(ID,_,_,Params),
      set_sw(ID,Params),
      fail
    ),
    close(InStream),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% set_sw_{a,d}/1-2: initialize the hyperparameters of MSW

set_sw_a(Sw) :- set_sw_a(Sw,default).

set_sw_a(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),set_sw_a/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),set_sw_a/2),
    $pp_require_hyperparameters(Spec,$msg(0208),set_sw_a/2),
    $pp_set_sw_a(Sw,Spec).

$pp_set_sw_a(Sw,Spec) :-
    ( $pd_fixed_hyperparameters(Sw) -> $pp_raise_warning($msg(0110),[Sw])
    ; $pp_get_values(Sw,Values),
      length(Values,N),
      $pp_expand_pseudo_counts(set_sw_a/2,Spec,N,Alphas,Deltas),
      ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true),
      assert($pd_hyperparameters(Sw,Values,Alphas,Deltas))
    ),!.

set_sw_d(Sw) :- set_sw_d(Sw,default).

set_sw_d(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),set_sw_d/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),set_sw_d/2),
    $pp_require_hyperparameters(Spec,$msg(0209),set_sw_d/2),
    $pp_set_sw_d(Sw,Spec).

$pp_set_sw_d(Sw,Spec) :-
    ( $pd_fixed_hyperparameters(Sw) -> $pp_raise_warning($msg(0110),[Sw])
    ; $pp_get_values(Sw,Values),
      length(Values,N),
      $pp_expand_pseudo_counts(set_sw_d/2,Spec,N,Alphas,Deltas),
      ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true),
      assert($pd_hyperparameters(Sw,Values,Alphas,Deltas))
    ),!.

% wrapper for getting alphas and deltas
$pp_expand_pseudo_counts(Caller,Spec,N,Alphas,Deltas) :-
    expand_pseudo_counts(Spec,N,Hs),
    ( Spec = default ->
      ( get_prism_flag(default_sw_a,$disabled) -> Mode = delta
      ; Mode = alpha
      )
    ; Caller = set_sw_a/2 -> Mode = alpha
    ; Mode = delta
    ),
    ( Mode = alpha ->
        Alphas = Hs,
        ( $pp_test_positive_numbers(Alphas) -> true
        ; $pp_raise_domain_error($msg(0208),[Spec],[alphas,Spec],Caller)
        ),                                        % a bit dirty
        $pp_alpha_to_delta(Alphas,Deltas)
    ; % Mode = delta
        Deltas = Hs,
        $pp_delta_to_alpha(Deltas,Alphas)
    ).

%% aliases for backward compatibility
set_sw_h(Sw)      :- set_sw_d(Sw).
set_sw_h(Sw,Spec) :- set_sw_d(Sw,Spec).

%%% set_sw_all_{a,d}(Sw):
%%%     set hyperparameters to all switches that matches with Sw.

set_sw_all_a :- set_sw_all_a(_).

set_sw_all_a(Sw) :- set_sw_all_a(Sw,default).

set_sw_all_a(Sw,Spec) :-
    findall(Sw,$pp_registered_sw(Sw),Sws),
    $pp_set_sw_a_list(Sws,Spec),!.

$pp_set_sw_a_list([],_).
$pp_set_sw_a_list([Sw|Sws],Spec) :-
    set_sw_a(Sw,Spec),!,
    $pp_set_sw_a_list(Sws,Spec).


set_sw_all_d :- set_sw_all_d(_).

set_sw_all_d(Sw) :- set_sw_all_d(Sw,default).

set_sw_all_d(Sw,Spec) :-
    findall(Sw,$pp_registered_sw(Sw),Sws),
    $pp_set_sw_d_list(Sws,Spec),!.

$pp_set_sw_d_list([],_).
$pp_set_sw_d_list([Sw|Sws],Spec) :-
    set_sw_d(Sw,Spec),!,
    $pp_set_sw_d_list(Sws,Spec).

%% aliases for backward compatibility

set_sw_all_h          :- set_sw_all_d.
set_sw_all_h(Sw)      :- set_sw_all_d(Sw).
set_sw_all_h(Sw,Spec) :- set_sw_all_d(Sw,Spec).

set_sw_a_all          :- set_sw_all_a.
set_sw_a_all(Sw)      :- set_sw_all_a(Sw).
set_sw_a_all(Sw,Spec) :- set_sw_all_a(Sw,Spec).

set_sw_d_all          :- set_sw_all_d.
set_sw_d_all(Sw)      :- set_sw_all_d(Sw).
set_sw_d_all(Sw,Spec) :- set_sw_all_d(Sw,Spec).

set_sw_h_all          :- set_sw_all_h.
set_sw_h_all(Sw)      :- set_sw_all_h(Sw).
set_sw_h_all(Sw,Spec) :- set_sw_all_h(Sw,Spec).

%%% fix_sw_h(Sw,Spec) :- fix the hyperparameters of Sw at Spec

fix_sw_a(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),fix_sw_a/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),fix_sw_a/2),
    $pp_require_hyperparameters(Spec,$msg(0208),fix_sw_a/2),
    $pp_unfix_sw_h(Sw),
    $pp_set_sw_a(Sw,Spec),
    $pp_fix_sw_h(Sw),!.

fix_sw_a(Sw) :- var(Sw),!,
    ( get_sw_a(switch(Sw1,_,_,_)),
      fix_sw_a(Sw1),
      fail
    ; true
    ).
fix_sw_a(Sw) :- Sw = [_|_],!,
    $pp_fix_sw_a_list(Sw).
fix_sw_a(Sw) :-
    ( $pd_hyperparameters(Sw,_,_,_),
      $pp_fix_sw_h(Sw),
      fail
    ; true
    ),!.

$pp_fix_sw_a_list([]).
$pp_fix_sw_a_list([Sw|Sws]) :-
    fix_sw_a(Sw),!,
    $pp_fix_sw_a_list(Sws).

fix_sw_d(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),fix_sw_d/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),fix_sw_d/2),
    $pp_require_hyperparameters(Spec,$msg(0209),fix_sw_d/2),
    $pp_unfix_sw_h(Sw),
    $pp_set_sw_d(Sw,Spec),
    $pp_fix_sw_h(Sw),!.

fix_sw_d(Sw) :- var(Sw),!,
    ( get_sw_d(switch(Sw1,_,_,_)),
      fix_sw_d(Sw1),
      fail
    ; true
    ).
fix_sw_d(Sw) :- Sw = [_|_],!,
    $pp_fix_sw_d_list(Sw).
fix_sw_d(Sw) :-
    ( $pd_hyperparameters(Sw,_,_,_),
      $pp_fix_sw_h(Sw),
      fail
    ; true
    ),!.

$pp_fix_sw_d_list([]).
$pp_fix_sw_d_list([Sw|Sws]) :-
    fix_sw_d(Sw),!,
    $pp_fix_sw_d_list(Sws).

$pp_fix_sw_h(Sw) :-
    ( clause($pd_fixed_hyperparameters(Sw),_) -> true
    ; assert($pd_fixed_hyperparameters(Sw))
    ).

%% aliases for backward compatibility

fix_sw_h(Sw,Spec) :- fix_sw_d(Sw,Spec).
fix_sw_h(Sw)      :- fix_sw_d(Sw).

%%% unfix_sw_{a,d}(Sw) :- unfix the hyperparameters of Sw

unfix_sw_d(Sw) :- var(Sw),!,
    ( get_sw_d(switch(Sw1,_,_,_)),
      unfix_sw_d(Sw1),
      fail
    ; true
    ).
unfix_sw_d(SwList) :- SwList = [_|_],!,
    $pp_unfix_sw_d_list(SwList).
unfix_sw_d(Sw) :-
    ( $pd_hyperparameters(Sw,_,_,_),
      $pp_unfix_sw_h(Sw),
      fail
    ; true
    ),!.

$pp_unfix_sw_d_list([]).
$pp_unfix_sw_d_list([Sw|Sws]) :-
    unfix_sw_d(Sw),!,
    $pp_unfix_sw_d_list(Sws).

$pp_unfix_sw_h(Sw) :-
    ( retract($pd_fixed_hyperparameters(Sw)) -> true
    ; true
    ).

%% aliases

unfix_sw_a(Sw) :- unfix_sw_d(Sw).
unfix_sw_h(Sw) :- unfix_sw_d(Sw).

%%% show hyperparameters

show_sw_a :- show_sw_a(_).

show_sw_a(Sw) :-
   findall(Sw,$pp_registered_sw(Sw),Sws0),
   sort(Sws0,Sws),
   $pp_show_sw_list_a(Sws).

$pp_show_sw_list_a([]) :- !.
$pp_show_sw_list_a([Sw|Sws]) :- !,
   $pp_show_sw1_a(Sw),!,
   $pp_show_sw_list_a(Sws).

$pp_show_sw1_a(Sw) :-
    $pp_get_hyperparameters(Sw,Values,Alphas,_),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_a_values(Values,Alphas),
    nl.

$pp_show_sw_a_values([],_).
$pp_show_sw_a_values([V|Vs],[A|As]) :-
    format(" ~w (a: ~9f)",[V,A]),!,
    $pp_show_sw_a_values(Vs,As).

show_sw_d :- show_sw_d(_).

show_sw_d(Sw) :-
   findall(Sw,$pp_registered_sw(Sw),Sws0),
   sort(Sws0,Sws),
   $pp_show_sw_list_d(Sws).

$pp_show_sw_list_d([]) :- !.
$pp_show_sw_list_d([Sw|Sws]) :- !,
   $pp_show_sw1_d(Sw),!,
   $pp_show_sw_list_d(Sws).

$pp_show_sw1_d(Sw) :-
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_d_values(Values,Deltas),
    nl.

$pp_show_sw_d_values([],_).
$pp_show_sw_d_values([V|Vs],[D|Ds]) :-
    format(" ~w (d: ~9f)",[V,D]),!,
    $pp_show_sw_d_values(Vs,Ds).

%% aliases

show_sw_h     :- show_sw_d.
show_sw_h(Sw) :- show_sw_d(Sw).

%%% show both parameters and hyperparameters

show_sw_pa :- show_sw_pa(_).

show_sw_pa(Sw) :-
    findall(Sw,$pp_registered_sw(Sw),Sws0),
    sort(Sws0,Sws),
    $pp_show_sw_list_pa(Sws).

$pp_show_sw_list_pa([]) :- !.
$pp_show_sw_list_pa([Sw|Sws]) :- !,
    $pp_show_sw1_pa(Sw),!,
    $pp_show_sw_list_pa(Sws).

$pp_show_sw1_pa(Sw) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,Alphas,_),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_parameters(Sw)      -> write('fixed_p,') ; write('unfixed_p,') ),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_pa_values(Values,Probs,Alphas),
    nl,!.

$pp_show_sw_pa_values([],_,_).

$pp_show_sw_pa_values([V|Vs],[P|Ps],[A|As]) :-
    format(" ~w (p: ~9f, a: ~9f)",[V,P,A]),!,
    $pp_show_sw_pa_values(Vs,Ps,As).

$pp_show_sw_pa_values([V|Vs],[P|Ps],$not_assigned) :-
    format(" ~w (p: ~9f, a: n/a)",[V,P]),!,
    $pp_show_sw_pa_values(Vs,Ps,$not_assigned).

$pp_show_sw_pa_values([V|Vs],$not_assigned,[A|As]) :-
    format(" ~w (p: n/a, a: ~9f)",[V,A]),!,
    $pp_show_sw_pa_values(Vs,$not_assigned,As).

show_sw_pd :- show_sw_pd(_).

show_sw_pd(Sw) :-
    findall(Sw,$pp_registered_sw(Sw),Sws0),
    sort(Sws0,Sws),
    $pp_show_sw_list_pd(Sws).

$pp_show_sw_list_pd([]) :- !.
$pp_show_sw_list_pd([Sw|Sws]) :- !,
    $pp_show_sw1_pd(Sw),!,
    $pp_show_sw_list_pd(Sws).

$pp_show_sw1_pd(Sw) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,_,Deltas),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_parameters(Sw)      -> write('fixed_p,') ; write('unfixed_p,') ),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_pd_values(Values,Probs,Deltas),
    nl,!.

$pp_show_sw_pd_values([],_,_).

$pp_show_sw_pd_values([V|Vs],[P|Ps],[D|Ds]) :-
    format(" ~w (p: ~9f, d: ~9f)",[V,P,D]),!,
    $pp_show_sw_pd_values(Vs,Ps,Ds).

$pp_show_sw_pd_values([V|Vs],[P|Ps],$not_assigned) :-
    format(" ~w (p: ~9f, d: n/a)",[V,P]),!,
    $pp_show_sw_pd_values(Vs,Ps,$not_assigned).

$pp_show_sw_pd_values([V|Vs],$not_assigned,[D|Ds]) :-
    format(" ~w (p: n/a, d: ~9f)",[V,D]),!,
    $pp_show_sw_pd_values(Vs,$not_assigned,Ds).

%% aliases

show_sw_b     :- show_sw_pd.
show_sw_b(Sw) :- show_sw_pd(Sw).

%%% get switch information including hyperparameters

get_sw_a(Sw) :-
    get_sw_a(SwName,Status,Values,Alphas),
    Sw = switch(SwName,Status,Values,Alphas).

get_sw_a(Sw,[Status,Values,Alphas]) :- get_sw_a(Sw,Status,Values,Alphas).

get_sw_a(Sw,Status,Values,Alphas) :-
    $pp_get_hyperparameters(Sw,Values,Alphas,_),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

get_sw_a(Sw,Status,Values,Alphas,Es) :-
    $pp_get_hyperparameters(Sw,Values,Alphas,_),
    $pp_get_hyperexpectations(Sw,_,Es),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

get_sw_d(Sw) :-
    get_sw_d(SwName,Status,Values,Deltas),
    Sw = switch(SwName,Status,Values,Deltas).

get_sw_d(Sw,[Status,Values,Deltas]) :- get_sw_d(Sw,Status,Values,Deltas).

get_sw_d(Sw,Status,Values,Deltas) :-
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

get_sw_d(Sw,Status,Values,Deltas,Es) :-
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    $pp_get_expectations(Sw,_,Es),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

%% aliases

get_sw_h(Sw)              :- get_sw_d(Sw).
get_sw_h(Sw,Info)         :- get_sw_d(Sw,Info).
get_sw_h(Sw,Status,Vs,Ds) :- get_sw_d(Sw,Status,Vs,Ds).

%%% get switch information including both parameters and hyperparameters

get_sw_pa(Sw) :-
    get_sw_pa(SwName,StatusPair,Values,Probs,Alphas),
    Sw = switch(SwName,StatusPair,Values,Probs,Alphas).

get_sw_pa(Sw,[StatusPair,Values,Probs,Alphas]) :-
    get_sw_pa(Sw,StatusPair,Values,Probs,Alphas).

get_sw_pa(Sw,[StatusP,StatusH],Values,Probs,Alphas) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,Alphas,_),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

get_sw_pa(Sw,[StatusP,StatusH],Values,Probs,Alphas,Es) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,Alphas,_),
    $pp_get_hyperexpectations(Sw,_,Es),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

get_sw_pd(Sw) :-
    get_sw_pd(SwName,StatusPair,Values,Probs,Deltas),
    Sw = switch(SwName,StatusPair,Values,Probs,Deltas).

get_sw_pd(Sw,[StatusPair,Values,Probs,Deltas]) :-
    get_sw_pd(Sw,StatusPair,Values,Probs,Deltas).

get_sw_pd(Sw,[StatusP,StatusH],Values,Probs,Deltas) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,_,Deltas),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

get_sw_pd(Sw,[StatusP,StatusH],Values,Probs,Deltas,Es) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,_,Deltas),
    $pp_get_expectations(Sw,_,Es),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

%% aliases

get_sw_b(Sw)                   :- get_sw_pd(Sw).
get_sw_b(Sw,Info)              :- get_sw_pd(Sw,Info).
get_sw_b(Sw,StatusPH,Vs,Ps,Ds) :- get_sw_pd(Sw,StatusPH,Vs,Ps,Ds).

%%%% save hyperparameters

save_sw_a :- save_sw_a('Saved_SW_A').

save_sw_a(File) :-
    open(File,write,OutStream),
    ( get_sw_a(SwName,Status,Values,Alphas),
      format(OutStream,"switch(~q,~q,~q,",[SwName,Status,Values]),
      $pp_write_hyperparameters(OutStream,Alphas,'['),
      format(OutStream,"]).~n",[]),
      fail
    ; true
    ),
    close(OutStream),!.

save_sw_d :- save_sw_d('Saved_SW_D').

save_sw_d(File) :-
    open(File,write,OutStream),
    ( get_sw_d(SwName,Status,Values,Deltas),
      format(OutStream,"switch(~q,~q,~q,",[SwName,Status,Values]),
      $pp_write_hyperparameters(OutStream,Deltas,'['),
      format(OutStream,"]).~n",[]),
      fail
    ; true
    ),
    close(OutStream),!.

$pp_write_hyperparameters(_,[],_).
$pp_write_hyperparameters(OutStream,[H|Hs],C) :-
    format(OutStream,"~w~15e",[C,H]),!,
    $pp_write_hyperparameters(OutStream,Hs,',').

%% aliases

save_sw_h       :- save_sw_d.
save_sw_h(File) :- save_sw_d(File).

%%%% restore hyperparameters

restore_sw_a :- restore_sw_a('Saved_SW_A').

restore_sw_a(File) :-
    open(File,read,InStream),
    repeat,
    read(InStream,Switch),
    ( Switch == end_of_file
    ; Switch = switch(ID,_,_,Alphas),
      set_sw_a(ID,Alphas),
      fail
    ),
    close(InStream),!.

restore_sw_d :- restore_sw_d('Saved_SW_D').

restore_sw_d(File) :-
    open(File,read,InStream),
    repeat,
    read(InStream,Switch),
    ( Switch == end_of_file
    ; Switch = switch(ID,_,_,Deltas),
      set_sw_d(ID,Deltas),
      fail
    ),
    close(InStream),!.

%% aliases

restore_sw_h       :- restore_sw_d.
restore_sw_h(File) :- restore_sw_d(File).

%%%% save both parameters and hyperparameters

save_sw_pa :- save_sw, save_sw_a.

save_sw_pa(FileP,FileA) :-
    save_sw(FileP),
    save_sw_a(FileA),!.

save_sw_pd :- save_sw, save_sw_d.

save_sw_pd(FileP,FileD) :-
    save_sw(FileP),
    save_sw_d(FileD),!.

%% aliases

save_sw_b              :- save_sw_pd.
save_sw_b(FileP,FileD) :- save_sw_pd(FileP,FileD).

%%%% restore both parameters and hyperparameters

restore_sw_pa :- restore_sw, restore_sw_a.

restore_sw_pa(FileP,FileA) :-
    restore_sw(FileP),
    restore_sw_a(FileA),!.

restore_sw_pd :- restore_sw, restore_sw_d.

restore_sw_pd(FileP,FileD) :-
    restore_sw(FileP),
    restore_sw_d(FileD),!.

%% aliases

restore_sw_b              :- restore_sw_pd.
restore_sw_b(FileP,FileD) :- restore_sw_pd(FileP,FileD).

%%----------------------------------------
%%  [Note]
%%  $pp_get_{values,parameters,expectations,hyperparameters}/2 do not check
%%  the groundness of switch names.

% raises a exception when there are no msw declarations
% (and can be a replacement of values/2 called in the clause bodies)
get_values(Sw,Values) :-
    $pp_require_msw_declaration($msg(0100),get_values0/2),
    $pp_get_values(Sw,Values).

% provides a simple access to value declarations
get_values0(Sw,Values) :-
    current_predicate($pu_values/2),
    $pp_get_values(Sw,Values).

% deterministically behaves and raises a exception also when there is no msw
% declaration that matches with Sw
get_values1(Sw,Values) :-
    $pp_require_ground(Sw,$msg(0101),get_values1/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),get_values1/2),
    $pp_get_values(Sw,Values),!.

% $pu_values/2 = translated values declarations
$pp_get_values(Sw,Values) :- $pu_values(Sw,Values).

%%----------------------------------------
%% Wrappers to the switch database

$pp_get_parameters(Sw,Values,Probs) :-
     ( ground(Sw) ->
	 get_values1(Sw,Values),
         ( $pd_parameters(Sw,Values0,Probs0) ->
	     ( Values0 = Values -> Probs = Probs0
	     ; $pd_fixed_parameters(Sw) ->
                 $pp_raise_runtime_error($msg(0106),[Sw],
                                         modified_switch_outcomes,
                                         $pp_get_parameters/3)
             ; set_sw(Sw,default),!,
               $pd_parameters(Sw,Values,Probs)
	     )
         ; set_sw(Sw,default),!,
           $pd_parameters(Sw,Values,Probs)
         )
     ; $pd_parameters(Sw,Values,Probs)
         % if Sw is not ground, we do not assign the default distribution
     ).

% [Note] set_sw_a(Sw,default) and set_sw_d(Sw,default) behaves in the same way
$pp_get_hyperparameters(Sw,Values,Alphas,Deltas) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         ( $pd_hyperparameters(Sw,Values0,Alphas0,Deltas0) ->
             ( Values0 = Values ->
                 Alphas = Alphas0,
                 Deltas = Deltas0
             ; $pd_fixed_hyperparameters(Sw) ->
                   $pp_raise_runtime_error($msg(0108),[Sw],
                                           modified_switch_outcomes,
                                           $pp_get_hyperparameters/4)
             ; set_sw_a(Sw,default),!,
               $pd_hyperparameters(Sw,Values,Alphas,Deltas)
             )
         ; set_sw_a(Sw,default),!,
           $pd_hyperparameters(Sw,Values,Alphas,Deltas)
         )
     ; $pd_hyperparameters(Sw,Values,Alphas,Deltas)
     ).

$pp_get_expectations(Sw,Values,Es) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         $pd_expectations(Sw,Values0,Es0),
         ( Values0 = Values -> Es = Es0
         ; $pp_raise_runtime_error($msg(0107),[Sw],modified_switch_outcomes,
                                   $pp_get_expectations/3)
         )
     ; $pd_expectations(Sw,Values,Es)
     ).

$pp_get_hyperexpectations(Sw,Values,Es) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         $pd_hyperexpectations(Sw,Values0,Es0),
         ( Values0 = Values -> Es = Es0
         ; $pp_raise_runtime_error($msg(0107),[Sw],modified_switch_outcomes,
                                   $pp_get_hyperexpectations/3)
         )
     ; $pd_hyperexpectations(Sw,Values,Es)
     ).

%%----------------------------------------

$pp_registered_sw(Sw) :-   % ground switch name will be returned
     ( $pd_parameters(Sw,_,_)
     ; $pd_hyperparameters(Sw,_,_,_)
     ).

show_reg_sw :-
     get_reg_sw_list(Sws),
     $pp_show_reg_sw(Sws).

$pp_show_reg_sw(Sws) :-
     format("Registered random switches:~n",[]),
     $pp_show_reg_sw1(Sws).

$pp_show_reg_sw1([]).
$pp_show_reg_sw1([Sw|Sws]) :-
     format("  ~w~n",[Sw]),!,
     $pp_show_reg_sw1(Sws).

get_reg_sw(Sw) :-
     get_reg_sw_list(Sws),!,
     member(Sw,Sws).

get_reg_sw_list(Sws) :-
     findall(Sw,$pp_registered_sw(Sw),Sws0),
     sort(Sws0,Sws).

%%----------------------------------------

alpha_to_delta(Alphas,Deltas) :-
     $pp_require_non_negative_numbers(Alphas,$msg(0208),alpha_to_delta/2),
     $pp_alpha_to_delta(Alphas,Deltas).

$pp_alpha_to_delta([],[]).
$pp_alpha_to_delta([Alpha|Alphas],[Delta|Deltas]) :-
     Delta is Alpha - 1,!,
     $pp_alpha_to_delta(Alphas,Deltas).

delta_to_alpha(Deltas,Alphas) :-
     $pp_require_non_negative_numbers(Deltas,$msg(0209),delta_to_alpha/2),
     $pp_delta_to_alpha(Deltas,Alphas).

$pp_delta_to_alpha([],[]).
$pp_delta_to_alpha([Delta|Deltas],[Alpha|Alphas]) :-
     Alpha is Delta + 1,!,
     $pp_delta_to_alpha(Deltas,Alphas).
