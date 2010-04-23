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
   or_statistics(:,:),
   opt_statistics(:,:),
   default_sequential(:).



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        default_sequential/1                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_sequential(X) :-
   '$c_default_sequential'(X), !.
default_sequential(_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          $parallel_query/2                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'$parallel_query'(G,[]) :- !, 
   '$c_start_yapor', 
   '$execute'(G), !,
   '$c_parallel_yes_answer'.
'$parallel_query'(G,V) :- 
   '$c_start_yapor', 
   '$execute'(G), 
   '$c_parallel_new_answer'(V).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            $sequential/0                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'$sequential' :-
   '$c_default_sequential'(X),
   '$initialization'('$c_default_sequential'(X)),
   '$c_default_sequential'(on).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             $parallel/0                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'$parallel' :-
   '$c_default_sequential'(X),
   '$initialization'('$c_default_sequential'(X)),
   '$c_default_sequential'(off).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       $sequential_directive/2                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'$sequential_directive'(Pred,Mod) :-
    var(Pred), !,
   '$do_error'(instantiation_error,sequential(Mod:Pred)).
'$sequential_directive'(Mod:Pred,_) :- !,
   '$sequential_directive'(Pred,Mod).
'$sequential_directive'((Pred1,Pred2),Mod) :- !,
   '$sequential_directive'(Pred1,Mod), 
   '$sequential_directive'(Pred2,Mod).
'$sequential_directive'(PredName/PredArity,Mod) :- 
   atom(PredName), integer(PredArity),
   functor(PredFunctor,PredName,PredArity), !,
   '$flags'(PredFunctor,Mod,Flags,Flags),
   (
       Flags /\ 0x1991F880 =:= 0, !, 
       (
          Flags /\ 0x00000020 =\= 0, !,
          write(user_error, '[ Warning: '),
          write(user_error, Mod:PredName/PredArity),
          write(user_error, ' is already declared as sequential ]'),
          nl(user_error)
       ;  
          NewFlags is Flags \/ 0x00000020,
          '$flags'(PredFunctor,Mod,Flags,NewFlags)
       )
   ;
       write(user_error, '[ Error: '),
       write(user_error, Mod:PredName/PredArity),
       write(user_error, ' cannot be declared as sequential ]'),
       nl(user_error),
       fail
   ).
'$sequential_directive'(Pred,Mod) :- 
   '$do_error'(type_error(callable,Mod:Pred),sequential(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        $parallel_directive/2                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'$parallel_directive'(Pred,Mod) :-
    var(Pred), !,
   '$do_error'(instantiation_error,parallel(Mod:Pred)).
'$parallel_directive'((Pred1,Pred2),Mod) :- !,
   '$parallel_directive'(Pred1,Mod),
   '$parallel_directive'(Pred2,Mod).
'$parallel_directive'(Mod:Pred,_) :- !,
   '$parallel_directive'(Pred,Mod).
'$parallel_directive'(PredName/PredArity,Mod) :- 
   atom(PredName), integer(PredArity),
   functor(PredFunctor,PredName,PredArity), !,
   '$flags'(PredFunctor,Mod,Flags,Flags),
   (
       Flags /\ 0x1991F880 =:= 0, !, 
       (
          Flags /\ 0x00000020 =:= 0, !,
          write(user_error, '[ Warning: '),
          write(user_error, Mod:PredName/PredArity),
          write(user_error, ' is already declared as parallel ]'),
          nl(user_error)
       ;
          NewFlags is Flags /\ 0xffffffdf, 
          '$flags'(PredFunctor,Mod,Flags,NewFlags)
       )
   ;
       write(user_error, '[ Error: '),
       write(user_error, Mod:PredName/PredArity),
       write(user_error, ' cannot be declared as parallel ]'),
       nl(user_error),
       fail
   ).
'$parallel_directive'(Pred,Mod) :- 
   '$do_error'(type_error(callable,Mod:Pred),parallel(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          $parallelizable/1                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'$parallelizable'(_) :-
   nb_getval('$consulting_file',S), S\=[], !, fail.
'$parallelizable'((G1,G2)) :- !,
   '$parallelizable'(G1),
   '$parallelizable'(G2).
'$parallelizable'((G1;G2)) :- !,
   '$parallelizable'(G1),
   '$parallelizable'(G2).
'$parallelizable'((G1|G2)) :- !,
   '$parallelizable'(G1),
   '$parallelizable'(G2).
'$parallelizable'((G1->G2)) :- !,
   '$parallelizable'(G1),
   '$parallelizable'(G2).
'$parallelizable'([]) :- !, fail.
'$parallelizable'([_|_]) :- !, fail.
'$parallelizable'(consult(_)) :- !, fail.
'$parallelizable'(reconsult(_)) :- !, fail.
'$parallelizable'(compile(_)) :- !, fail.
'$parallelizable'(use_module(_)) :- !, fail.
'$parallelizable'(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
