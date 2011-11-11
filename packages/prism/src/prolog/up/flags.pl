%% -*- Prolog -*-

%% prism_flag(Name,Type,Init,Pred) defines a new Prism flag where each
%% argument indicates:
%% 
%%   Name : the flag name
%%   Type : the domain of possible values
%%   Init : the default value
%%   Pred : the auxiliary predicate (see below) or `$none'.
%% 
%% Type should be one of the followings:
%% 
%%   bool:
%%     boolean value taking either `on' or `off'
%% 
%%   enum(Cands):
%%     atom occuring in Cands
%% 
%%   term(Cands):
%%     term matching one of patterns in Cands
%% 
%%   integer(Min,Max):
%%     integral value from Min to Max (Min/Max can be -inf/+inf)
%% 
%%   float(Min,Max):
%%     floating value from Min to Max (Min/Max can be -inf/+inf)
%% 
%% 
%% Declaring Auxiliary Predicates
%% ------------------------------
%% 
%% An auxiliary predicate is called just after a new value is set to
%% the corresponding flag.  A typical purpose of auxiliary predicates
%% is to have the new value notified to the C routines.
%% 
%% Auxiliary predicates must be of the arity one, and are called with
%% the argument indicating the new value set to the flag as described
%% below (depending on Type):
%% 
%%   bool:
%%     an integer 1 (on) or 0 (off).
%% 
%%   enum(Cands):
%%     an integer representing the index (starting at 0) at which the
%%     specified atom exists in Cands
%% 
%%   term(Cands):
%%     the specified term
%% 
%%   integer(Min,Max):
%%     the specified integral value
%% 
%%   float(Min,Max):
%%     the specified floating value
%% 
%% [TODO: describe open/half-open ranges of floating values]
%% [TODO: describe special(PredName)]
%% 
%% [Note] Make sure to declare flags in alphabetical order.

$pp_prism_flag(clean_table,bool,on,$none).
$pp_prism_flag(daem,bool,off,$pc_set_daem).
$pp_prism_flag(data_source,term([none,data/1,file(_)]),data/1,$none).
$pp_prism_flag(default_sw,special($pp_check_default_sw),uniform,$none).
$pp_prism_flag(default_sw_a,special($pp_check_default_sw_a),1,$none).
$pp_prism_flag(default_sw_d,special($pp_check_default_sw_d),0,$none).
$pp_prism_flag(em_message,bool,on,$none).
$pp_prism_flag(em_progress,integer(1,+inf),10,$pc_set_em_progress).
$pp_prism_flag(epsilon,float(@0,+inf),0.0001,$pc_set_prism_epsilon).
$pp_prism_flag(error_on_cycle,bool,on,$pc_set_error_on_cycle).
$pp_prism_flag(explicit_empty_expls,bool,on,$pc_set_explicit_empty_expls).
$pp_prism_flag(fix_init_order,bool,on,$pc_set_fix_init_order).
$pp_prism_flag(init,enum([none,noisy_u,random]),random,$pc_set_init_method).
$pp_prism_flag(itemp_init,float(@0,1),0.1,$pc_set_itemp_init).
$pp_prism_flag(itemp_rate,float(@1,+inf),1.5,$pc_set_itemp_rate).
$pp_prism_flag(learn_message,special($pp_check_learn_message),all,$none).
$pp_prism_flag(learn_mode,enum([params,hparams,both]),params,$none).
$pp_prism_flag(log_scale,bool,off,$pc_set_log_scale).
$pp_prism_flag(max_iterate,special($pp_check_max_iterate),
               default,$pc_set_max_iterate).
$pp_prism_flag(reset_hparams,bool,on,$none).
$pp_prism_flag(restart,integer(1,+inf),1,$pc_set_num_restart).
$pp_prism_flag(rerank,integer(1,+inf),10,$none).
$pp_prism_flag(search_progress,integer(1,+inf),10,$none).
$pp_prism_flag(show_itemp,bool,off,$pc_set_show_itemp).
$pp_prism_flag(sort_hindsight,enum([by_goal,by_prob]),by_goal,$none).
$pp_prism_flag(std_ratio,float(@0,+inf),0.2,$pc_set_std_ratio).
$pp_prism_flag(verb,special($pp_check_verb),none,$pp_set_verb).
$pp_prism_flag(viterbi_mode,enum([params,hparams]),params,$none).
$pp_prism_flag(warn,bool,off,$pc_set_warn).
$pp_prism_flag(write_call_events,special($pp_check_write_call_events),all,$none).

% first flag is enabled by default
$pp_prism_flag_exclusive([default_sw_d,default_sw_a]).

$pp_prism_flag_renamed(default_sw_h,default_sw_d).

$pp_prism_flag_deleted(avg_branch,'1.11').
$pp_prism_flag_deleted(layer_check,'1.11').
$pp_prism_flag_deleted(log_viterbi,'2.0').
$pp_prism_flag_deleted(dynamic_default_sw,'2.0').
$pp_prism_flag_deleted(dynamic_default_sw_h,'2.0').
$pp_prism_flag_deleted(params_after_vbem,'2.0').
$pp_prism_flag_deleted(reduce_copy,'2.0').
$pp_prism_flag_deleted(scaling,'2.0').
$pp_prism_flag_deleted(scaling_factor,'2.0').
$pp_prism_flag_deleted(smooth,'2.0').

%%----------------------------------------

get_prism_flag(Name,Value) :-
    $pp_prism_flag(Name,_,_,_),
    $pp_variable_prism_flag(Name,VarName),
    global_get(VarName,Value).
get_prism_flag(Name,Value) :-
    $pp_prism_flag_renamed(Name0,Name1),
    Name == Name0,!,
    $pp_raise_warning($msg(3102),[Name,Name1]),
    $pp_variable_prism_flag(Name1,VarName),
    global_get(VarName,Value).

%%----------------------------------------

set_prism_flag(Name,Value) :-
    $pp_require_prism_flag(Name,$msg(3100),set_prism_flag/2),
    $pp_require_prism_flag_value(Name,Value,$msg(3101),set_prism_flag/2),
    ( current_predicate($pp_prism_flag_deleted/2),
      $pp_prism_flag_deleted(Name,Version)
        -> $pp_raise_domain_error($msg(3103),[Name,Version],[prism_flag,Name],
                                  set_prism_flag/2)
    ; current_predicate($pp_prism_flag_deleted/3),
      $pp_prism_flag_deleted(Name,Value,Version)
        -> $pp_raise_domain_error($msg(3104),[Name,Value,Version],
                                  [prism_flag_value(Name),Value],set_prism_flag/2)
    ; true ),
    ( $pp_prism_flag(Name,Type,_,Pred) ->
          Name1 = Name
    ; $pp_prism_flag_renamed(Name,Name1),$pp_prism_flag(Name1,Type,_,Pred) ->
          $pp_raise_warning($msg(3102),[Name,Name1])
    ),
    $pp_check_prism_flag(Type,Value,SValue,IValue),
    $pp_disable_prism_flag(Name1),
    $pp_variable_prism_flag(Name1,VarName),
    global_set(VarName,SValue),
    ( Pred == $none -> true
    ; Term =.. [Pred,IValue], call(Term)
    ),!.

%%----------------------------------------

reset_prism_flags :-
    set_default_prism_flags,
    disable_exclusive_prism_flags.

set_default_prism_flags :-
    $pp_prism_flag(Name,_,Value,_),
    set_prism_flag(Name,Value),
    fail.
set_default_prism_flags.
    
disable_exclusive_prism_flags :-
    ( current_predicate($pp_prism_flag_exclusive/1),
      $pp_prism_flag_exclusive([_|Names]),
      $pp_disable_prism_flag1(Names),
      fail
    ; true
    ).

%%----------------------------------------

show_prism_flags :-
    get_prism_flag(Name,Value),
    ( Value = $disabled -> Value1 = '(disabled)'
    ; Value1 = Value
    ),
    format("~w~22|: ~w~n",[Name,Value1]),
    fail.
show_prism_flags.

%%----------------------------------------
%% aliases 

current_prism_flag(Name,Value) :- get_prism_flag(Name,Value).

show_prism_flag :- show_prism_flags.
show_flags      :- show_prism_flags.
show_flag       :- show_prism_flags.

$pp_variable_prism_flag(Name,VarName) :-
    atom_chars(Name,Name1),
    VarName1 = [$,p,g,'_',f,l,a,g,'_'|Name1],
    atom_chars(VarName,VarName1).

%%----------------------------------------

$pp_check_prism_flag(Type,Value,SValue,IValue), Type = bool =>
    nth0(IValue,[off,on],Value),!,
    SValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue), Type = enum(Cands) =>
    nth0(IValue,Cands,Value),!,
    SValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue), Type = term(Patts) =>
    member(Value,Patts),!,
    SValue = Value,
    IValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue), Type = integer(Min,Max) =>
    integer(Value),
    $pp_check_min_max(Value,Min,Max),!,
    SValue = Value,
    IValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue), Type = float(Min,Max) =>
    number(Value),
    $pp_check_min_max(Value,Min,Max),!,
    SValue = Value,
    IValue is float(Value).
$pp_check_prism_flag(Type,Value,SValue,IValue), Type = special(PredName) =>
    call(PredName,Value,SValue,IValue). % B-Prolog extension

$pp_check_min_max(Value,Min,Max) :-
    ( Min = -inf -> true
    ; Min = @Min0 -> Min0 < Value
    ; Min =< Value
    ),!,
    ( Max = +inf -> true
    ; Max = @Max0 -> Max0 > Value
    ; Max >= Value
    ),!.

$pp_check_max_iterate(0,inf,0) :- $pp_raise_warning($msg(3105),[0,inf]).
$pp_check_max_iterate(inf,inf,0).
$pp_check_max_iterate(default,default,-1).
$pp_check_max_iterate(N,N,N) :- integer(N), N > 0.

$pp_check_default_sw(V,V,V) :-
    ( V = f_geometric(B),   number(B), B > 1.0
    ; V = f_geometric(B,T), number(B), B > 1.0, member(T,[asc,desc])
    ; member(V,[none,uniform,f_geometric])
    ).

$pp_check_default_sw_a(V,V,V) :-
    ( number(V), V > 0.0
    ; V = uniform(U), number(U), U > 0.0
    ; member(V,[none,uniform])
    ).

$pp_check_default_sw_d(V,V,V) :-
    ( number(V), V >= 0.0
    ; V = uniform(U), number(U), U >= 0.0
    ; member(V,[none,uniform])
    ).

$pp_check_verb(none,none,[0,0]).
$pp_check_verb(em,em,[1,0]).
$pp_check_verb(graph,graph,[0,1]).
$pp_check_verb(full,full,[1,1]).
$pp_check_verb(off,none,[0,0]) :- $pp_raise_warning($msg(3105),[off,none]).
$pp_check_verb(on,full,[1,1])  :- $pp_raise_warning($msg(3105),[on,full]).

$pp_check_write_call_events(X,Y,Y) :- $pp_write_call_events(X,Y),!.
$pp_check_write_call_events(off,off,off) :- !.

$pp_check_learn_message(X,Y,Y) :- $pp_learn_message_events(X,Y),!.
$pp_check_learn_message(off,off,off) :- !.

%% disable competitors

$pp_disable_prism_flag(Name) :-
    ( current_predicate($pp_prism_flag_exclusive/1),
      $pp_prism_flag_exclusive(Competitors),
      select(Name,Competitors,Names),  % B-Prolog's built-in
      $pp_disable_prism_flag1(Names),
      fail
    ; true
    ).

$pp_disable_prism_flag1([]).
$pp_disable_prism_flag1([Name|Names]) :-
    $pp_variable_prism_flag(Name,VarName),
    global_set(VarName,$disabled),!,
    $pp_disable_prism_flag1(Names).

%% check the availability of the flag (Note: Name must be ground)
$pp_is_enabled_flag(Name) :-
    \+ get_prism_flag(Name,$disabled).

%%----------------------------------------
%% auxiliary predicates

$pp_set_verb([EM,Graph]) :-
    $pc_set_verb_em(EM),
    $pc_set_verb_graph(Graph).
