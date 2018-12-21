
/**
  * @file   yapor.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:23:00 2017
  * 
  * @brief Or-Parallelism
  *
  * @defgroup YapOR Or-Parallelism
  @ @ingroup extensions
  * 
  * 
*/
:- system_module( '$_utils', [callable/1,
        current_op/3,
        nb_current/2,
        nth_instance/3,
        nth_instance/4,
        op/3,
        prolog/0,
        recordaifnot/3,
        recordzifnot/3,
        simple/1,
        subsumes_term/2], ['$getval_exception'/3]).

:- use_system_module( '$_boot', ['$live'/0]).

:- use_system_module( '$_errors', ['$do_error'/2]).

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

:- meta_predicate 
   parallel(0),
   parallel_findall(?,0,?),
   parallel_findfirst(?,0,?),
   parallel_once(0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           or_statistics/0                           %%
%%                          opt_statistics/0                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

or_statistics :-
   current_output(Stream),
   or_statistics(Stream).

opt_statistics :-
   current_output(Stream),
   opt_statistics(Stream).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           or_statistics/2                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% should match with code in OPTYap/opt.preds.c
or_statistics(total_memory,[BytesInUse,BytesAllocated]) :-
   '$c_get_optyap_statistics'(0,BytesInUse,BytesAllocated).
or_statistics(or_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(4,BytesInUse,StructsInUse).
or_statistics(query_goal_solution_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(12,BytesInUse,StructsInUse).
or_statistics(query_goal_answer_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(13,BytesInUse,StructsInUse).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          opt_statistics/2                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% should match with code in OPTYap/opt.preds.c
opt_statistics(total_memory,[BytesInUse,BytesAllocated]) :-
   '$c_get_optyap_statistics'(0,BytesInUse,BytesAllocated).
opt_statistics(table_entries,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(1,BytesInUse,StructsInUse).
opt_statistics(subgoal_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(2,BytesInUse,StructsInUse).
opt_statistics(dependency_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(3,BytesInUse,StructsInUse).
opt_statistics(or_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(4,BytesInUse,StructsInUse).
opt_statistics(suspension_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(5,BytesInUse,StructsInUse).
opt_statistics(subgoal_trie_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(6,BytesInUse,StructsInUse).
opt_statistics(answer_trie_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(7,BytesInUse,StructsInUse).
opt_statistics(subgoal_trie_hashes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(8,BytesInUse,StructsInUse).
opt_statistics(answer_trie_hashes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(9,BytesInUse,StructsInUse).
opt_statistics(global_trie_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(10,BytesInUse,StructsInUse).
opt_statistics(global_trie_hashes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(11,BytesInUse,StructsInUse).
opt_statistics(query_goal_solution_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(12,BytesInUse,StructsInUse).
opt_statistics(query_goal_answer_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(13,BytesInUse,StructsInUse).
opt_statistics(table_subgoal_solution_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(14,BytesInUse,StructsInUse).
opt_statistics(table_subgoal_answer_frames,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(15,BytesInUse,StructsInUse).
opt_statistics(subgoal_entries,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(16,BytesInUse,StructsInUse).
opt_statistics(answer_ref_nodes,[BytesInUse,StructsInUse]) :-
   '$c_get_optyap_statistics'(17,BytesInUse,StructsInUse).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             parallel/1                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parallel(Goal) :-
   parallel_mode(Mode), Mode = on, !,
   (
      '$parallel_query'(Goal)
   ;
      true
   ).
parallel(Goal) :-
   (
      '$execute'(Goal),
      fail
   ;
      true
   ).

'$parallel_query'(Goal) :-
   '$c_yapor_start', 
   '$execute'(Goal),
   fail.
'$parallel_query'(_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          parallel_findall/3                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parallel_findall(Template,Goal,Answers) :- 
   parallel_mode(Mode), Mode = on, !,
   (
      '$parallel_findall_query'(Template,Goal)
   ;
      '$c_parallel_get_answers'(Refs),
      '$parallel_findall_recorded'(Refs,Answers),     
      eraseall(parallel_findall)
   ).
parallel_findall(Template,Goal,Answers) :-
   findall(Template,Goal,Answers).

'$parallel_findall_query'(Template,Goal) :-
   '$c_yapor_start', 
   '$execute'(Goal),
   recordz(parallel_findall,Template,Ref),
   '$c_parallel_new_answer'(Ref),
   fail.
'$parallel_findall_query'(_,_).

'$parallel_findall_recorded'([],[]) :- !.
'$parallel_findall_recorded'([Ref|Refs],[Template|Answers]):-
   recorded(parallel_findall,Template,Ref),
   '$parallel_findall_recorded'(Refs,Answers).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      parallel_findfirst/3                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parallel_findfirst(Template,Goal,Answer) :- 
   parallel_findall(Template,(Goal,!),Answer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         parallel_once/1                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parallel_once(Goal) :-
   parallel_mode(Mode), Mode = on, !,
   (
      '$parallel_once_query'(Goal)
   ;
      recorded(parallel_once,Goal,Ref),
      erase(Ref)
   ).
parallel_once(Goal) :-
   once(Goal).

'$parallel_once_query'(Goal) :-
   '$c_yapor_start', 
   '$execute'(once(Goal)),
    recordz(parallel_once,Goal,_),
    fail.
'$parallel_once_query'(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
