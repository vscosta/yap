%%----------------------------------------

$pp_emit_message(MsgID) :-
    $pp_emit_message(MsgID,[]).

$pp_emit_message(MsgID,Args) :-
    $pp_assert($pp_message(MsgID,Type,Format)),
    $pp_message_head(Type,Head),
    format("***  ~w: ",[Head]),
    $pp_format_message(Format,Args),
    format("~n",[]).

$pp_message_head(fatal,'PRISM FATAL ERROR').
$pp_message_head(inter,'PRISM INTERNAL ERROR').
$pp_message_head(error,'PRISM ERROR').
$pp_message_head(fail ,'PRISM WARNING').
$pp_message_head(warn ,'PRISM WARNING').
$pp_message_head(obosl,'PRISM WARNING').
$pp_message_head(info ,'PRISM INFO').

%%----------------------------------------

$pp_assert(Cond) :-
    ( call(Cond) ->
      true
    ; $pp_emit_message($msg(9900),[Cond]), halt
    ).

%%----------------------------------------

% instanciation errors
$pp_raise_instanciation_error(MsgID,Source) :-
    $pp_raise_instanciation_error(MsgID,[],Source).
$pp_raise_instanciation_error(MsgID,Args,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(instanciation_error,Source)).

% type errors
$pp_raise_type_error(MsgID,[Type,Culprit],Source) :-
    $pp_raise_type_error(MsgID,[],[Type,Culprit],Source).
$pp_raise_type_error(MsgID,Args,[Type,Culprit],Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(type_error(Type,Culprit),Source)).

% domain errors
$pp_raise_domain_error(MsgID,[Domain,Culprit],Source) :-
    $pp_raise_domain_error(MsgID,[],[Domain,Culprit],Source).
$pp_raise_domain_error(MsgID,Args,[Domain,Culprit],Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(domain_error(Domain,Culprit),Source)).

% existence errors
$pp_raise_existence_error(MsgID,[ObjType,Culprit],Source) :-
    $pp_raise_existence_error(MsgID,[],[ObjType,Culprit],Source).
$pp_raise_existence_error(MsgID,Args,[ObjType,Culprit],Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(existence_error(ObjType,Culprit),Source)).

% permission errors
$pp_raise_permission_error(MsgID,[Operation,PermissionType,Culprit],Source) :-
    $pp_raise_permission_error(MsgID,[],
                               [Operation,PermissionType,Culprit],
                               Source).
$pp_raise_permission_error(MsgID,Args,
                           [Operation,PermissionType,Culprit],
                           Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(permission_error(Operation,PermissionType,Culprit),Source)).

% evaluation errors
$pp_raise_evaluation_error(MsgID,Error,Source) :-
    $pp_raise_evaluation_error(MsgID,[],Error,Source).
$pp_raise_evaluation_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(evaluation_error(Error),Source)).

% runtime errors
$pp_raise_runtime_error(MsgID,Error,Source) :-
    $pp_raise_runtime_error(MsgID,[],Error,Source).
$pp_raise_runtime_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(prism_runtime_error(Error),Source)).

% translation errors
$pp_raise_trans_error(MsgID,Error,Source) :-
    $pp_raise_trans_error(MsgID,[],Error,Source).
$pp_raise_trans_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(prism_translation_error(Error),Source)).

% internal errors
$pp_raise_internal_error(MsgID,Error,Source) :-
    $pp_raise_internal_error(MsgID,[],Error,Source).
$pp_raise_internal_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(prism_internal_error(Error),Source)).

% warnings
$pp_raise_warning(MsgID) :- $pp_raise_warning(MsgID,[]).
$pp_raise_warning(MsgID,Args) :-
    ( get_prism_flag(warn,on) -> $pp_emit_message(MsgID,Args)
    ; true
    ).

%%----------------------------------------
%% typical internal errors

$pp_raise_unmatched_branches(Source) :-
    $pp_raise_internal_error($msg(9803),unmatched_branches,Source).
$pp_raise_unmatched_branches(Source,Position) :-
    $pp_raise_internal_error($msg(9803),unmatched_branches(Position),Source).

$pp_raise_unexpected_failure(Source) :-
    $pp_raise_internal_error($msg(9804),unexpected_failure,Source).

$pp_raise_unexpected_failure(Source,Position) :-
    $pp_raise_internal_error($msg(9804),unexpected_failure(Position),Source).

%%----------------------------------------

$pp_raise_on_require(Xs,MsgID,Source,Pred) :-
    $pp_emit_message(MsgID,Xs),
    append(Xs,[Error],Args),
    G =.. [Pred|Args],
    ( call(G) ->
      true
    ; $pp_emit_message($msg(9800)),
      Error = prism_internal_error(error_term_not_found)
    ),
    throw(error(Error,Source)).

%%----------------------------------------

$pp_require_atom(X,MsgID,Source) :-
    ( atom(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_atom)
    ).

$pp_error_atom(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_atom(X,type_error(atom,X)) :-
    \+ atom(X), !.

%%----------------------------------------

$pp_require_nonvar(X,MsgID,Source) :-
    ( nonvar(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_nonvar)
    ).

$pp_error_nonvar(X,instantiation_error) :-
    var(X), !.

%%----------------------------------------

$pp_require_nonvars(Xs,MsgID,Source) :-
    ( $pp_test_nonvars(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,$pp_error_nonvars)
    ).

$pp_test_nonvars(Xs) :-
    Xs = [_|_],
    $pp_test_nonvars1(Xs).

$pp_test_nonvars1([]).
$pp_test_nonvars1([X|Xs]) :-
    nonvar(X),!,
    $pp_test_nonvars1(Xs).

$pp_error_nonvars(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_nonvars(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_nonvars(Xs,domain_error(non_variables,Xs)) :-
    member(X,Xs),
    var(X), !.

%%----------------------------------------

$pp_require_ground(X,MsgID,Source) :-
    ( ground(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_ground)
    ).

$pp_error_ground(X,instantiation_error) :-
    \+ ground(X), !.

%%----------------------------------------

$pp_require_callable(X,MsgID,Source) :-
    ( callable(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_callable)
    ).

$pp_error_callable(X,type_error(callable,X)) :-
    \+ callable(X), !.

%%----------------------------------------

$pp_require_integer(X,MsgID,Source) :-
    ( integer(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_integer)
    ).

$pp_error_integer(X,instantiation_error) :-
    var(X), !.
$pp_error_integer(X,type_error(integer,X)) :-
    \+ integer(X), !.

%%----------------------------------------

$pp_require_positive_integer(X,MsgID,Source) :-
    ( integer(X), X > 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_positive_integer)
    ).

$pp_error_positive_integer(X,Error) :-
    $pp_error_integer(X,Error), !.
$pp_error_positive_integer(X,domain_error(greater_than_zero,X)) :-
    X =< 0, !.

%%----------------------------------------

$pp_require_non_negative_integer(X,MsgID,Source) :-
    ( integer(X), X >= 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_non_negative_integer)
    ).

$pp_error_non_negative_integer(X,Error) :-
    $pp_error_integer(X,Error), !.
$pp_error_non_negative_integer(X,domain_error(not_less_than_zero,X)) :-
    X < 0, !.

%%----------------------------------------

$pp_require_number(X,MsgID,Source) :-
    ( number(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_number)
    ).

$pp_error_number(X,instantiation_error) :-
    var(X), !.
$pp_error_number(X,type_error(number,X)) :-
    \+ number(X), !.

%%----------------------------------------

$pp_require_numbers(Xs,MsgID,Source) :-
    ( $pp_test_numbers(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,
                           $pp_error_numbers)
    ).

$pp_test_numbers(Xs) :-
    Xs = [_|_],
    $pp_test_numbers1(Xs).

$pp_test_numbers1([]).
$pp_test_numbers1([X|Xs]) :-
    number(X),!,
    $pp_test_numbers1(Xs).

$pp_error_numbers(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_numbers(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_numbers(Xs,domain_error(numbers,Xs)) :-
    member(X,Xs),
    \+ number(X), !.

%%----------------------------------------

$pp_require_positive_number(X,MsgID,Source) :-
    ( number(X), X > 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_positive_number)
    ).

$pp_error_positive_number(X,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_positive_number(X,domain_error(greater_than_zero,X)) :-
    X =< 0, !.

%%----------------------------------------

$pp_require_positive_numbers(Xs,MsgID,Source) :-
    ( $pp_test_positive_numbers(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,
                           $pp_error_positive_numbers)
    ).

$pp_test_positive_numbers(Xs) :-
    Xs = [_|_],
    $pp_test_positive_numbers1(Xs).

$pp_test_positive_numbers1([]).
$pp_test_positive_numbers1([X|Xs]) :-
    number(X),
    X > 0,!,
    $pp_test_positive_numbers1(Xs).

$pp_error_positive_numbers(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_positive_numbers(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_positive_numbers(Xs,domain_error(positive_numbers,Xs)) :-
    member(X,Xs),
    (\+ number(X) ; X =< 0), !.

%%----------------------------------------

$pp_require_number_not_less_than(X,Min,MsgID,Source) :-
    $pp_assert(number(Min)),
    ( number(X), X >= Min ->
      true
    ; $pp_raise_on_require([X,Min],MsgID,Source,$pp_error_number_not_less_than)
    ).

$pp_error_number_not_less_than(X,_,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_number_not_less_than(X,Min,domain_error(not_less_than(Min),X)) :-
    X < Min, !.

%%----------------------------------------

$pp_require_numbers_not_less_than(Xs,Min,MsgID,Source) :-
    $pp_assert(number(Min)),
    ( $pp_test_numbers_not_less_than(Min,Xs) -> true
    ; $pp_raise_on_require([Xs,Min],MsgID,Source,
                           $pp_error_numbers_not_less_than)
    ).

$pp_test_numbers_not_less_than(Min,Xs) :-
    Xs = [_|_],
    $pp_test_numbers_not_less_than1(Min,Xs).

$pp_test_numbers_not_less_than1(_,[]).
$pp_test_numbers_not_less_than1(Min,[X|Xs]) :-
    number(X),
    X >= Min,!,
    $pp_test_numbers_not_less_than1(Min,Xs).

$pp_error_numbers_not_less_than(Xs,_,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_numbers_not_less_than(Xs,_,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_numbers_not_less_than(Xs,Min,
                                domain_error(numbers_not_less_than(Min),Xs)) :-
    member(X,Xs),
    (\+ number(X) ; X < Min ), !.

%%----------------------------------------

$pp_require_non_negative_number(X,MsgID,Source) :-
    ( number(X), X >= 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_non_negative_number)
    ).

$pp_error_non_negative_number(X,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_non_negative_number(X,domain_error(not_less_than_zero,X)) :-
    X < 0, !.

%%----------------------------------------

$pp_require_non_negative_numbers(Xs,MsgID,Source) :-
    ( $pp_test_non_negative_numbers(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,$pp_error_non_negative_numbers)
    ).

$pp_test_non_negative_numbers(Xs) :-
    Xs = [_|_],
    $pp_test_non_negative_numbers1(Xs).

$pp_test_non_negative_numbers1([]).
$pp_test_non_negative_numbers1([X|Xs]) :-
    number(X),
    X >= 0.0,!,
    $pp_test_non_negative_numbers1(Xs).

$pp_error_non_negative_numbers(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_non_negative_numbers(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_non_negative_numbers(Xs,domain_error(non_negative_numbers,Xs)) :-
    member(X,Xs),
    (\+ number(X) ; X < 0 ), !.

%%----------------------------------------

$pp_require_list(X,MsgID,Source) :-
    ( nonvar(X), X = [_|_] -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_list)
    ).

$pp_error_list(X,instanciation_error) :-
    var(X), !.
$pp_error_list(X,type_error(list,X)) :-
    X \= [_|_], !.

%%----------------------------------------

$pp_require_list_or_nil(X,MsgID,Source) :-
    ( nonvar(X), (X = [_|_] ; X = []) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_list_or_nil)
    ).

$pp_error_list_or_nil(X,instanciation_error) :-
    var(X), !.
$pp_error_list_or_nil(X,type_error(list_or_nil,X)) :-
    X \= [_|_], X \= [], !.

%%----------------------------------------

$pp_require_list_not_shorter_than(X,Min,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(Min >= 0),
    ( $pp_test_list_not_shorter_than(X,Min) -> true
    ; $pp_raise_on_require([X,Min],MsgID,Source,$pp_error_list_not_shorter_than)
    ).

$pp_test_list_not_shorter_than(X,Min) :-
    nonvar(X),
    ( X = [_|_] ; X = [] ),
    length(X,L), L >= Min.

$pp_error_list_not_shorter_than(X,_Min,instanciation_error) :-
    var(X), !.
$pp_error_list_not_shorter_than(X,_Min,type_error(list,X)) :-
    X \= [_|_], X \= [], !.
$pp_error_list_not_shorter_than(X,Min,type_error(list_not_shorter_than(Min),X)) :-
    length(X,L), L < Min, !.

%%----------------------------------------

$pp_require_compound(X,MsgID,Source) :-
    ( compound(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_compound)
    ).

$pp_error_compound(X,instantiation_error) :-
    var(X), !.
$pp_error_compound(X,type_error(compound,X)) :-
    \+ compound(X), !.

%%----------------------------------------

$pp_require_integer_range(Min,Max,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(integer(Max)),
    ( Min < Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_integer_range)
    ).

$pp_error_integer_range(Min,Max,Error) :-
    Min >= Max,
    Error = domain_error(integer_range,[Min,Max]), !.

%%----------------------------------------

$pp_require_integer_range_incl(Min,Max,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(integer(Max)),
    ( Min =< Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_integer_range_incl)
    ).

$pp_error_integer_range_incl(Min,Max,Error) :-
    Min > Max,
    Error = domain_error(integer_range_inclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_integer_range_excl(Min,Max,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(integer(Max)),
    ( Min + 1 > Min, Min + 1 < Max ->   % (Min + 1 =< Min) -> overflow
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_integer_range_excl)
    ).

$pp_error_integer_range_excl(Min,Max,Error) :-
    ( Min + 1 =< Min ; Min + 1 >= Max ),
    Error = domain_error(integer_range_exclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_number_range_incl(Min,Max,MsgID,Source) :-
    $pp_assert(number(Min)),
    $pp_assert(number(Max)),
    ( Min =< Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_number_range_incl)
    ).

$pp_error_number_range_incl(Min,Max,Error) :-
    Min > Max,
    Error = domain_error(number_range_inclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_number_range_excl(Min,Max,MsgID,Source) :-
    $pp_assert(number(Min)),
    $pp_assert(number(Max)),
    ( Min < Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_number_range_excl)
    ).

$pp_error_number_range_excl(Min,Max,Error) :-
    Min >= Max,
    Error = domain_error(number_range_exclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_membership(X,Xs,MsgID,Source) :-
    $pp_assert(Xs = [_|_]),
    ( nonvar(X),membchk(X,Xs) -> true
    ; $pp_raise_on_require([X,Xs],MsgID,Source,$pp_error_membership)
    ).

$pp_error_membership(X,_Xs,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_membership(X,Xs,domain_error(Xs,X)) :-
    \+ membchk(X,Xs), !.

%%----------------------------------------

$pp_require_predicate_indicator(X,MsgID,Source) :-
    ( $pp_test_predicate_indicator(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_predicate_indicator)
    ).

$pp_test_predicate_indicator(X) :-
    X = F/N, atom(F), integer(N), N >= 0.

$pp_error_predicate_indicator(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_predicate_indicator(X,type_error(predicate_indicator,X)) :-
    \+ $pp_test_predicate_indicator(X), !.

%%----------------------------------------

$pp_require_user_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_user_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_user_probabilistic_atom)
    ).

$pp_error_user_probabilistic_atom(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_user_probabilistic_atom(X,Error) :-
    $pp_error_callable(X,Error), !.
$pp_error_user_probabilistic_atom(X,type_error(user_probabilistic_atom,X)) :-
    \+ $pp_is_user_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_probabilistic_atom)
    ).

$pp_error_probabilistic_atom(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_probabilistic_atom(X,Error) :-
    $pp_error_callable(X,Error), !.
$pp_error_probabilistic_atom(X,type_error(probabilistic_atom,X)) :-
    \+ $pp_is_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_extended_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_extended_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_extended_probabilistic_atom)
    ).

$pp_error_extended_probabilistic_atom(X,Error) :-
    $pp_error_probabilistic_atom(X,Error), !.
$pp_error_extended_probabilistic_atom(X,type_error(extended_probabilistic_atom,X)) :-
    \+ $pp_is_extended_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_probabilistic_callable(X,MsgID,Source) :-
    ( $pp_is_probabilistic_callable(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_probabilistic_callable)
    ).

$pp_error_probabilistic_callable(X,Error) :-
    $pp_error_probabilistic_atom(X,Error), !.
$pp_error_probabilistic_callable(X,type_error(probabilistic_callable,X)) :-
    \+ $pp_is_probabilistic_callable(X), !.

%%----------------------------------------

$pp_require_tabled_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_tabled_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_tabled_probabilistic_atom)
    ).

$pp_error_tabled_probabilistic_atom(X,Error) :-
    $pp_error_probabilistic_atom(X,Error), !.
$pp_error_tabled_probabilistic_atom(X,type_error(tabled_probabilistic_atom,X)) :-
    \+ $pp_is_tabled_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_msw_declaration(MsgID,Source) :-
    ( current_predicate($pu_values/2) -> true
    ; $pp_raise_on_require([],MsgID,Source,$pp_error_msw_declaration)
    ).

$pp_error_msw_declaration(msw_declaration_not_found) :-
    \+ current_predicate($pu_values/2), !.

%%----------------------------------------

$pp_require_switch_outcomes(X,MsgID,Source) :-
    $pp_assert(ground(X)),
    ( current_predicate($pu_values/2),
      $pu_values(X,_)
          -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_switch_outcomes)
    ).

$pp_error_switch_outcomes(_X,Error) :-
    $pp_error_msw_declaration(Error), !.
$pp_error_switch_outcomes(X,existence_error(outcome,X)) :-
    \+ $pu_values(X,_), !.

%%----------------------------------------

$pp_require_prism_flag(Flag,MsgID,Source) :-
    ( $pp_test_prism_flag(Flag) -> true
    ; $pp_raise_on_require([Flag],MsgID,Source,$pp_error_prism_flag)
    ).

$pp_test_prism_flag(Flag) :-
    atom(Flag),
    ( $pp_prism_flag(Flag,_,_,_)
    ; $pp_prism_flag_renamed(Flag,Flag1),
      $pp_prism_flag(Flag1,_,_,_)
    ).

$pp_error_prism_flag(Flag,Error) :-
    $pp_error_atom(Flag,Error), !.
$pp_error_prism_flag(Flag,domain_error(prism_flag,Flag)) :-
    \+ $pp_prism_flag(Flag,_,_,_), !.

%%----------------------------------------

$pp_require_prism_flag_value(Flag,Value,MsgID,Source) :-
    $pp_assert($pp_test_prism_flag(Flag)),
    ( $pp_test_prism_flag_value(Flag,Value) -> true
    ; $pp_raise_on_require([Flag,Value],MsgID,Source,$pp_error_prism_flag_value)
    ).

$pp_test_prism_flag_value(Flag,Value) :-
    ground(Value),
    ( $pp_prism_flag(Flag,Type,_,_),
      $pp_check_prism_flag(Type,Value,_,_)
    ; $pp_prism_flag_renamed(Flag,Flag1),
      $pp_prism_flag(Flag1,Type,_,_),
      $pp_check_prism_flag(Type,Value,_,_)
    ).

$pp_error_prism_flag_value(_Flag,Value,Error) :-
    $pp_error_ground(Value,Error), !.
$pp_error_prism_flag_value(Flag,Value,
                           domain_error(prism_flag_value(Flag),Value)) :-
    \+ $pp_test_prism_flag_value(Flag,Value), !.

%%----------------------------------------

$pp_require_distribution(X,MsgID,Source) :-
    ( $pp_test_distribution(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_distribution)
    ).

% we do not check each element at this moment
$pp_test_distribution(X) :-
    ( $pp_test_fixed_size_distribution(X)
    ; $pp_test_variable_size_distribution(X)
    ).

$pp_test_variable_size_distribution(X) :-
    ground(X),
    ( X = uniform
    ; X = f_geometric
    ; X = f_geometric(Base)      -> number(Base), Base > 1
    ; X = f_geometric(Base,Type) -> number(Base), Base > 1, membchk(Type,[asc,desc])
    ; X = random
    ; X = default
    ).

$pp_error_distribution(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_distribution(X,domain_error(distribution,X)) :-
    \+ $pp_test_distribution(X), !.

%%----------------------------------------

$pp_require_fixed_size_distribution(X,MsgID,Source) :-
    ( $pp_test_fixed_size_distribution(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_fixed_size_distribution)
    ).

% we do not check each element at this moment
$pp_test_fixed_size_distribution(X) :-
    ground(X),
    ( $pp_test_probabilities(X)
    ; $pp_test_probabilities_plus(X)
    ; $pp_test_ratio(X)
    ).

$pp_test_probabilities_plus(X) :-
    $pp_expr_to_list('+',X,Ps),
    length(Ps,L),
    L > 1,!,
    $pp_test_probabilities(Ps).

$pp_test_ratio(X) :-
    $pp_expr_to_list(':',X,Rs),
    length(Rs,L),
    L > 1,!,
    $pp_test_non_negative_numbers(Rs),
    \+ $pp_test_zeros(Rs).

$pp_test_zeros([]).
$pp_test_zeros([Z|Zs]):-
    -1.0e-15 < Z,
     1.0e-15 > Z,!,
    $pp_test_zeros(Zs).

$pp_error_fixed_size_distribution(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_fixed_size_distribution(X,domain_error(fixed_size_distribution,X)) :-
    \+ $pp_test_fixed_size_distribution(X), !.

%%----------------------------------------

$pp_require_probability(X,MsgID,Source) :-
    ( $pp_test_probability(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_probability)
    ).

$pp_test_probability(X) :-
    number(X),
    X >= 0.0,
    X =< 1.0.

$pp_error_probability(X,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_probability(X,domain_error(probability,X)) :-
    ( X < 0.0 ;  X > 1.0 ), !.

%%----------------------------------------

$pp_require_probabilities(Ps,MsgID,Source) :-
    ( $pp_test_probabilities(Ps) -> true
    ; $pp_raise_on_require([Ps],MsgID,Source,$pp_error_probabilities)
    ).

$pp_test_probabilities(Ps) :-
    Ps = [_|_],
    $pp_test_probabilities1(Ps),
    sumlist(Ps,Sum),
    abs(Sum - 1.0) =< 1.0e-12.

$pp_test_probabilities1([]).
$pp_test_probabilities1([P|Ps]) :-
    $pp_test_probability(P),!,
    $pp_test_probabilities1(Ps).

$pp_error_probabilities(Ps,Error) :-
    $pp_error_list(Ps,Error), !.
$pp_error_probabilities(Ps,Error) :-
    member(P,Ps),
    $pp_error_probability(P,Error), !.
$pp_error_probabilities(Ps,domain_error(probabilities,Ps)) :-
    sumlist(Ps,Sum),
    abs(Sum - 1.0) > 1.0e-12, !.

%%----------------------------------------

$pp_require_hyperparameters(X,MsgID,Source) :-
    ( $pp_test_hyperparameters(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_hyperparameters)
    ).

$pp_test_hyperparameters(X) :-
    ( $pp_test_fixed_size_hyperparameters(X)
    ; $pp_test_variable_size_hyperparameters(X)
    ).

$pp_test_variable_size_hyperparameters(X) :-
    ground(X),
    ( number(X) -> X >= 0.0
    ; X = uniform
    ; X = uniform(U) -> number(U), U >= 0
    ; X = f_geometric
    ; X = f_geometric(Base) ->
        number(Base), Base > 1
    ; X = f_geometric(Init,Base) ->
        number(Init), Init >= 0,
        number(Base), Base >  1
    ; X = f_geometric(Init,Base,Type) ->
        number(Init), Init >= 0,
        number(Base), Base >  1,
        membchk(Type,[asc,desc])
    ; X = default
    ).

$pp_error_hyperparameters(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_hyperparameters(X,domain_error(hyperparameters,X)) :-
    \+ $pp_test_hyperparameters(X), !.

%%----------------------------------------

$pp_require_fixed_size_hyperparameters(X,MsgID,Source) :-
    ( $pp_test_fixed_size_hyperparameters(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_fixed_size_hyperparameters)
    ).

$pp_test_fixed_size_hyperparameters(X) :-
    ground(X),
    $pp_test_non_negative_numbers(X).

$pp_error_fixed_size_hyperparameters(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_fixed_size_hyperparameters(X,domain_error(fixed_size_hyperparameters,X)) :-
    \+ $pp_test_fixed_size_hyperparameters(X), !.

%%----------------------------------------

$pp_require_prism_option(X,MsgID,Source) :-
    ( $pp_test_prism_option(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_prism_option)
    ).

$pp_test_prism_option(X) :-
    ground(X),
    ( X = dump
    ; X = consult
    ; X = compile
    ; X = load
    ; X = v
    ; X = verb
    ; X = nv
    ; X = noverb
    ; X = consult
    ; X = (_=_)
    ).

$pp_error_prism_option(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_prism_option(X,domain_error(prism_option,X)) :-
    \+ $pp_test_prism_opton(X), !.

%%----------------------------------------

% aggregate pattern is so flexible that we can only check if
% X is callable or not

$pp_require_hindsight_aggregate_pattern(X,MsgID,Source) :-
    ( $pp_test_hindsight_aggregate_pattern(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_hindsight_aggregate_pattern)
    ).

$pp_test_hindsight_aggregate_pattern(X) :-
    callable(X).

$pp_error_hindsight_aggregate_pattern(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_hindsight_aggregate_pattern(X,Error) :-
    $pp_error_callable(X,Error), !.

%%----------------------------------------

$pp_require_write_callable(G,MsgID,Source) :-
    ( $pp_is_write_callable(G) -> true
    ; $pp_raise_on_require([G],MsgID,Source,$pp_write_callable)
    ).

$pp_error_write_callable(G,Error) :-
    $pp_error_nonvar(G,Error), !.
$pp_error_write_callable(G,Error) :-
    $pp_error_callable(G,Error), !.
$pp_error_write_callable(G,domain_error(write_callable,G)) :-
    \+ $pp_is_write_callable(G), !.
