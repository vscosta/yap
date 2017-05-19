:- system_module( '$_tabling', [abolish_table/1,
        global_trie_statistics/0,
        is_tabled/1,
        show_all_local_tables/0,
        show_all_tables/0,
        show_global_trie/0,
        show_table/1,
        show_table/2,
        show_tabled_predicates/0,
        (table)/1,
        table_statistics/1,
        table_statistics/2,
        tabling_mode/2,
        tabling_statistics/0,
        tabling_statistics/2], []).

:- use_system_module( '$_errors', ['$do_error'/2]).

/** @defgroup Tabling Tabling
@ingroup extensions
@{

 *YAPTab* is the tabling engine that extends YAP's execution
model to support tabled evaluation for definite programs. YAPTab was
implemented by Ricardo Rocha and its implementation is largely based
on the ground-breaking design of the XSB Prolog system, which
implements the SLG-WAM. Tables are implemented using tries and YAPTab
supports the dynamic intermixing of batched scheduling and local
scheduling at the subgoal level. Currently, the following restrictions
are of note:

+ YAPTab does not handle tabled predicates with loops through negation (undefined behaviour).
+ YAPTab does not handle tabled predicates with cuts (undefined behaviour).
+ YAPTab does not support coroutining (configure error).
+ YAPTab does not support tabling dynamic predicates (permission error).


To experiment with YAPTab use `--enable-tabling` in the configure
script or add `-DTABLING` to `YAP_EXTRAS` in the system's
`Makefile`. We next describe the set of built-ins predicates
designed to interact with YAPTab and control tabled execution:

 
*/

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                   The YapTab/YapOr/OPTYap systems                   %%
%%                                                                     %%
%% YapTab extends the Yap Prolog engine to support sequential tabling  %%
%% YapOr extends the Yap Prolog engine to support or-parallelism       %%
%% OPTYap extends the Yap Prolog engine to support or-parallel tabling %%
%%                                                                     %%
%%                                                                     %%
%%      Yap Prolog was developed at University of Porto, Portugal      %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/**
YapTab extends the Yap Prolog engine to support sequential tabling. YapOr extends the Yap Prolog engine to support or-parallelism. YapOr extends the Yap Prolog engine to support or-parallelism.
*/


/** @pred abolish_table(+ _P_) 


Removes all the entries from the table space for predicate  _P_ (or
a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]). The predicate remains as a tabled predicate.

 
*/
/** @pred is_tabled(+ _P_) 


Succeeds if the predicate  _P_ (or a list of predicates
 _P1_,..., _Pn_ or [ _P1_,..., _Pn_]), of the form
 _name/arity_, is a tabled predicate.

 
*/
/** @pred show_table(+ _P_) 


Prints table contents (subgoals and answers) for predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]).

 
*/
/** @pred table( + _P_ )


Declares predicate  _P_ (or a list of predicates
 _P1_,..., _Pn_ or [ _P1_,..., _Pn_]) as a tabled
predicate.  _P_ must be written in the form
 _name/arity_. Examples:

~~~~~
:- table son/3.
:- table father/2.
:- table mother/2.
~~~~~
 or

~~~~~
:- table son/3, father/2, mother/2.
~~~~~
 or

~~~~~
:- table [son/3, father/2, mother/2].
~~~~~

 
*/
/** @pred table_statistics(+ _P_) 


Prints table statistics (subgoals and answers) for predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]).

 
*/
/** @pred tabling_mode(+ _P_,? _Mode_) 


Sets or reads the default tabling mode for a tabled predicate  _P_
(or a list of predicates  _P1_,..., _Pn_ or
[ _P1_,..., _Pn_]). The list of  _Mode_ options includes:

+ `batched`

    Defines that, by default, batched scheduling is the scheduling
strategy to be used to evaluated calls to predicate  _P_.

+ `local`

    Defines that, by default, local scheduling is the scheduling
strategy to be used to evaluated calls to predicate  _P_.

+ `exec_answers`

    Defines that, by default, when a call to predicate  _P_ is
already evaluated (completed), answers are obtained by executing
compiled WAM-like code directly from the trie data
structure. This reduces the loading time when backtracking, but
the order in which answers are obtained is undefined.

+ `load_answers`
  
   Defines that, by default, when a call to predicate  _P_ is
already evaluated (completed), answers are obtained (as a
consumer) by loading them from the trie data structure. This
guarantees that answers are obtained in the same order as they
were found. Somewhat less efficient but creates less choice-points.

The default tabling mode for a new tabled predicate is `batched`
and `exec_answers`. To set the tabling mode for all predicates at
once you can use the yap_flag/2 predicate as described next.
 
*/
:- meta_predicate 
   table(:), 
   is_tabled(:), 
   tabling_mode(:,?), 
   abolish_table(:), 
   show_table(:), 
   show_table(?,:), 
   table_statistics(:),
   table_statistics(?,:).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      show_tabled_predicates/0                       %%
%%                         show_global_trie/0                          %%
%%                          show_all_tables/0                          %%
%%                       show_all_local_tables/0                       %%
%%                      global_trie_statistics/0                       %%
%%                        tabling_statistics/0                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_tabled_predicates :- 
   current_output(Stream),
   show_tabled_predicates(Stream).

show_global_trie :-
   current_output(Stream),
   show_global_trie(Stream).

show_all_tables :-
   current_output(Stream),
   show_all_tables(Stream).

show_all_local_tables :-
   current_output(Stream),
   show_all_local_tables(Stream).

global_trie_statistics :-
   current_output(Stream),
   global_trie_statistics(Stream).

/** @pred tabling_statistics/0 


Prints statistics on space used by all tables.



 */
tabling_statistics :-
   current_output(Stream),
   tabling_statistics(Stream).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        tabling_statistics/2                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% should match with code in OPTYap/opt.preds.c
tabling_statistics(total_memory,[BytesInUse,BytesAllocated]) :-
   '$c_get_optyap_statistics'(0,BytesInUse,BytesAllocated).
tabling_statistics(table_entries,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(1,BytesInUse,StructsInUse).
tabling_statistics(subgoal_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(2,BytesInUse,StructsInUse).
tabling_statistics(dependency_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(3,BytesInUse,StructsInUse).
tabling_statistics(subgoal_trie_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(6,BytesInUse,StructsInUse).
tabling_statistics(answer_trie_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(7,BytesInUse,StructsInUse).
tabling_statistics(subgoal_trie_hashes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(8,BytesInUse,StructsInUse).
tabling_statistics(answer_trie_hashes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(9,BytesInUse,StructsInUse).
tabling_statistics(global_trie_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(10,BytesInUse,StructsInUse).
tabling_statistics(global_trie_hashes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(11,BytesInUse,StructsInUse).
tabling_statistics(subgoal_entries,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(16,BytesInUse,StructsInUse).
tabling_statistics(answer_ref_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(17,BytesInUse,StructsInUse).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               table/1                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table(Pred) :-
   '$current_module'(Mod),
   '$do_table'(Mod,Pred).

'$do_table'(Mod,Pred) :-
    var(Pred), !,
   '$do_error'(instantiation_error,table(Mod:Pred)).
'$do_table'(_,Mod:Pred) :- !,
   '$do_table'(Mod,Pred).
'$do_table'(_,[]) :- !.
'$do_table'(Mod,[HPred|TPred]) :- !,
   '$do_table'(Mod,HPred),
   '$do_table'(Mod,TPred).
'$do_table'(Mod,(Pred1,Pred2)) :- !,
   '$do_table'(Mod,Pred1),
   '$do_table'(Mod,Pred2).
'$do_table'(Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity), !,
   '$set_table'(Mod,PredFunctor,[]).
'$do_table'(Mod,PredDeclaration) :- 
    PredDeclaration=..[PredName|PredList],
    '$transl_to_mode_list'(PredList,PredModeList,PredArity),
    functor(PredFunctor,PredName,PredArity), !,
    '$set_table'(Mod,PredFunctor,PredModeList).
'$do_table'(Mod,Pred) :-
   '$do_pi_error'(type_error(callable,Pred),table(Mod:Pred)).

'$set_table'(Mod,PredFunctor,_PredModeList) :-
   '$undefined'('$c_table'(_,_,_),prolog), !,
   functor(PredFunctor, PredName, PredArity),
   '$do_error'(resource_error(tabling,Mod:PredName/PredArity),table(Mod:PredName/PredArity)).
'$set_table'(Mod,PredFunctor,PredModeList) :-
   '$undefined'(PredFunctor,Mod), !,
   '$c_table'(Mod,PredFunctor,PredModeList).
'$set_table'(Mod,PredFunctor,_PredModeList) :-
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags),
   Flags /\ 0x00000040 =:= 0x00000040, !.
'$set_table'(Mod,PredFunctor,PredModeList) :-
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags),
   Flags /\ 0x1991F8C0 =:= 0,
   '$c_table'(Mod,PredFunctor,PredModeList), !.
'$set_table'(Mod,PredFunctor,_PredModeList) :-
   functor(PredFunctor,PredName,PredArity), 
   '$do_error'(permission_error(modify,table,Mod:PredName/PredArity),table(Mod:PredName/PredArity)).

'$transl_to_mode_list'([],[],0) :- !.
'$transl_to_mode_list'([TextualMode|L],[Mode|ModeList],Arity) :-
    '$transl_to_mode_directed_tabling'(TextualMode,Mode),
    '$transl_to_mode_list'(L,ModeList,ListArity),
    Arity is ListArity + 1.

%% should match with code in OPTYap/tab.macros.h
'$transl_to_mode_directed_tabling'(index,1).
'$transl_to_mode_directed_tabling'(min,2).
'$transl_to_mode_directed_tabling'(max,3).
'$transl_to_mode_directed_tabling'(all,4).
'$transl_to_mode_directed_tabling'(sum,5).
'$transl_to_mode_directed_tabling'(last,6).
'$transl_to_mode_directed_tabling'(first,7).
%% B-Prolog compatibility
'$transl_to_mode_directed_tabling'(+,1).
'$transl_to_mode_directed_tabling'(@,4).
'$transl_to_mode_directed_tabling'(-,7).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             is_tabled/1                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_tabled(Pred) :- 
   '$current_module'(Mod), 
   '$do_is_tabled'(Mod,Pred).

'$do_is_tabled'(Mod,Pred) :- 
   var(Pred), !, 
   '$do_error'(instantiation_error,is_tabled(Mod:Pred)).
'$do_is_tabled'(_,Mod:Pred) :- !, 
   '$do_is_tabled'(Mod,Pred).
'$do_is_tabled'(_,[]) :- !.
'$do_is_tabled'(Mod,[HPred|TPred]) :- !,
   '$do_is_tabled'(Mod,HPred),
   '$do_is_tabled'(Mod,TPred).
'$do_is_tabled'(Mod,(Pred1,Pred2)) :- !,
   '$do_is_tabled'(Mod,Pred1),
   '$do_is_tabled'(Mod,Pred2).
'$do_is_tabled'(Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags), !,
   Flags /\ 0x000040 =\= 0.
'$do_is_tabled'(Mod,Pred) :- 
   '$do_pi_error'(type_error(callable,Pred),is_tabled(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            tabling_mode/2                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tabling_mode(Pred,Options) :- 
   '$current_module'(Mod), 
   '$do_tabling_mode'(Mod,Pred,Options).

'$do_tabling_mode'(Mod,Pred,Options) :- 
   var(Pred), !, 
   '$do_error'(instantiation_error,tabling_mode(Mod:Pred,Options)).
'$do_tabling_mode'(_,Mod:Pred,Options) :- !, 
   '$do_tabling_mode'(Mod,Pred,Options).
'$do_tabling_mode'(_,[],_) :- !.
'$do_tabling_mode'(Mod,[HPred|TPred],Options) :- !,
   '$do_tabling_mode'(Mod,HPred,Options),
   '$do_tabling_mode'(Mod,TPred,Options).
'$do_tabling_mode'(Mod,(Pred1,Pred2),Options) :- !,
   '$do_tabling_mode'(Mod,Pred1,Options),
   '$do_tabling_mode'(Mod,Pred2,Options).
'$do_tabling_mode'(Mod,PredName/PredArity,Options) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$set_tabling_mode'(Mod,PredFunctor,Options)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),tabling_mode(Mod:PredName/PredArity,Options))
   ).
'$do_tabling_mode'(Mod,Pred,Options) :- 
   '$do_pi_error'(type_error(callable,Pred),tabling_mode(Mod:Pred,Options)).

'$set_tabling_mode'(Mod,PredFunctor,Options) :-
   var(Options), !,
   '$c_tabling_mode'(Mod,PredFunctor,Options).
'$set_tabling_mode'(_,_,[]) :- !.
'$set_tabling_mode'(Mod,PredFunctor,[HOption|TOption]) :- !,
   '$set_tabling_mode'(Mod,PredFunctor,HOption),
   '$set_tabling_mode'(Mod,PredFunctor,TOption).
'$set_tabling_mode'(Mod,PredFunctor,(Option1,Option2)) :- !,
   '$set_tabling_mode'(Mod,PredFunctor,Option1),
   '$set_tabling_mode'(Mod,PredFunctor,Option2).
'$set_tabling_mode'(Mod,PredFunctor,Option) :- 
   '$transl_to_pred_flag_tabling_mode'(Flag,Option), !,
   '$c_tabling_mode'(Mod,PredFunctor,Flag).
'$set_tabling_mode'(Mod,PredFunctor,Options) :- 
   functor(PredFunctor,PredName,PredArity), 
   '$do_error'(domain_error(flag_value,tabling_mode+Options),tabling_mode(Mod:PredName/PredArity,Options)).

%% should match with code in OPTYap/opt.preds.c
'$transl_to_pred_flag_tabling_mode'(1,batched).
'$transl_to_pred_flag_tabling_mode'(2,local).
'$transl_to_pred_flag_tabling_mode'(3,exec_answers).
'$transl_to_pred_flag_tabling_mode'(4,load_answers).
'$transl_to_pred_flag_tabling_mode'(5,local_trie).
'$transl_to_pred_flag_tabling_mode'(6,global_trie).
'$transl_to_pred_flag_tabling_mode'(7,coinductive).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           abolish_table/1                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abolish_table(Pred) :-
   '$current_module'(Mod),
   '$do_abolish_table'(Mod,Pred).

'$do_abolish_table'(Mod,Pred) :-
   var(Pred), !,
   '$do_error'(instantiation_error,abolish_table(Mod:Pred)).
'$do_abolish_table'(_,Mod:Pred) :- !,
   '$do_abolish_table'(Mod,Pred).
'$do_abolish_table'(_,[]) :- !.
'$do_abolish_table'(Mod,[HPred|TPred]) :- !,
   '$do_abolish_table'(Mod,HPred),
   '$do_abolish_table'(Mod,TPred).
'$do_abolish_table'(Mod,(Pred1,Pred2)) :- !,
   '$do_abolish_table'(Mod,Pred1),
   '$do_abolish_table'(Mod,Pred2).
'$do_abolish_table'(Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$c_abolish_table'(Mod,PredFunctor)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),abolish_table(Mod:PredName/PredArity))
   ).
'$do_abolish_table'(Mod,Pred) :-
   '$do_pi_error'(type_error(callable,Pred),abolish_table(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             show_table/1                            %%
%%                             show_table/2                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_table(Pred) :-
   current_output(Stream),
   show_table(Stream,Pred).

show_table(Stream,Pred) :-
   '$current_module'(Mod),
   '$do_show_table'(Stream,Mod,Pred).

'$do_show_table'(_,Mod,Pred) :-
   var(Pred), !,
   '$do_error'(instantiation_error,show_table(Mod:Pred)).
'$do_show_table'(Stream,_,Mod:Pred) :- !,
   '$do_show_table'(Stream,Mod,Pred).
'$do_show_table'(_,_,[]) :- !.
'$do_show_table'(Stream,Mod,[HPred|TPred]) :- !,
   '$do_show_table'(Stream,Mod,HPred),
   '$do_show_table'(Stream,Mod,TPred).
'$do_show_table'(Stream,Mod,(Pred1,Pred2)) :- !,
   '$do_show_table'(Stream,Mod,Pred1),
   '$do_show_table'(Stream,Mod,Pred2).
'$do_show_table'(Stream,Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$c_show_table'(Stream,Mod,PredFunctor)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),show_table(Mod:PredName/PredArity))
   ).
'$do_show_table'(_,Mod,Pred) :-
   '$do_pi_error'(type_error(callable,Pred),show_table(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         table_statistics/1                          %%
%%                         table_statistics/2                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table_statistics(Pred) :-
   current_output(Stream),
   table_statistics(Stream,Pred).

table_statistics(Stream,Pred) :-
   '$current_module'(Mod),
   '$do_table_statistics'(Stream,Mod,Pred).

'$do_table_statistics'(_,Mod,Pred) :-
   var(Pred), !,
   '$do_error'(instantiation_error,table_statistics(Mod:Pred)).
'$do_table_statistics'(Stream,_,Mod:Pred) :- !,
   '$do_table_statistics'(Stream,Mod,Pred).
'$do_table_statistics'(_,_,[]) :- !.
'$do_table_statistics'(Stream,Mod,[HPred|TPred]) :- !,
   '$do_table_statistics'(Stream,Mod,HPred),
   '$do_table_statistics'(Stream,Mod,TPred).
'$do_table_statistics'(Stream,Mod,(Pred1,Pred2)) :- !,
   '$do_table_statistics'(Stream,Mod,Pred1),
   '$do_table_statistics'(Stream,Mod,Pred2).
'$do_table_statistics'(Stream,Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$predicate_flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$c_table_statistics'(Stream,Mod,PredFunctor)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),table_statistics(Mod:PredName/PredArity))
   ).
'$do_table_statistics'(_,Mod,Pred) :-
   '$do_pi_error'(type_error(callable,Pred),table_statistics(Mod:Pred)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
@}
*/
