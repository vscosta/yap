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
   table(:), 
   is_tabled(:), 
   tabling_mode(:,?), 
   abolish_table(:), 
   show_table(:), 
   table_statistics(:),
   table_statistics(:,:).



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
   '$set_table'(Mod,PredFunctor).
'$do_table'(Mod,Pred) :-
   '$do_error'(type_error(callable,Mod:Pred),table(Mod:Pred)).

'$set_table'(Mod,PredFunctor) :-
   '$undefined'('$c_table'(_,_),prolog), !,
   functor(PredFunctor, PredName, PredArity),
   '$do_error'(resource_error(tabling,Mod:PredName/PredArity),table(Mod:PredName/PredArity)).
'$set_table'(Mod,PredFunctor) :-
   '$undefined'(PredFunctor,Mod), !,
   '$c_table'(Mod,PredFunctor).
'$set_table'(Mod,PredFunctor) :-
   '$flags'(PredFunctor,Mod,Flags,Flags),
   Flags /\ 0x1991F880 =:= 0,
   '$c_table'(Mod,PredFunctor), !.
'$set_table'(Mod,PredFunctor) :-
   functor(PredFunctor,PredName,PredArity), 
   '$do_error'(permission_error(modify,table,Mod:PredName/PredArity),table(Mod:PredName/PredArity)).



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
   '$flags'(PredFunctor,Mod,Flags,Flags), !,
   Flags /\ 0x000040 =\= 0.
'$do_is_tabled'(Mod,Pred) :- 
   '$do_error'(type_error(callable,Mod:Pred),is_tabled(Mod:Pred)).



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
   '$flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$set_tabling_mode'(Mod,PredFunctor,Options)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),tabling_mode(Mod:PredName/PredArity,Options))
   ).
'$do_tabling_mode'(Mod,Pred,Options) :- 
   '$do_error'(type_error(callable,Mod:Pred),tabling_mode(Mod:Pred,Options)).

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
   '$flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$c_abolish_table'(Mod,PredFunctor)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),abolish_table(Mod:PredName/PredArity))
   ).
'$do_abolish_table'(Mod,Pred) :-
   '$do_error'(type_error(callable,Mod:Pred),abolish_table(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             show_table/1                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_table(Pred) :-
   '$current_module'(Mod),
   '$do_show_table'(Mod,Pred).

'$do_show_table'(Mod,Pred) :-
   var(Pred), !,
   '$do_error'(instantiation_error,show_table(Mod:Pred)).
'$do_show_table'(_,Mod:Pred) :- !,
   '$do_show_table'(Mod,Pred).
'$do_show_table'(_,[]) :- !.
'$do_show_table'(Mod,[HPred|TPred]) :- !,
   '$do_show_table'(Mod,HPred),
   '$do_show_table'(Mod,TPred).
'$do_show_table'(Mod,(Pred1,Pred2)) :- !,
   '$do_show_table'(Mod,Pred1),
   '$do_show_table'(Mod,Pred2).
'$do_show_table'(Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$c_show_table'(Mod,PredFunctor)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),show_table(Mod:PredName/PredArity))
   ).
'$do_show_table'(Mod,Pred) :-
   '$do_error'(type_error(callable,Mod:Pred),show_table(Mod:Pred)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         table_statistics/1                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table_statistics(Pred) :-
   '$current_module'(Mod),
   '$do_table_statistics'(Mod,Pred).

'$do_table_statistics'(Mod,Pred) :-
   var(Pred), !,
   '$do_error'(instantiation_error,table_statistics(Mod:Pred)).
'$do_table_statistics'(_,Mod:Pred) :- !,
   '$do_table_statistics'(Mod,Pred).
'$do_table_statistics'(_,[]) :- !.
'$do_table_statistics'(Mod,[HPred|TPred]) :- !,
   '$do_table_statistics'(Mod,HPred),
   '$do_table_statistics'(Mod,TPred).
'$do_table_statistics'(Mod,(Pred1,Pred2)) :- !,
   '$do_table_statistics'(Mod,Pred1),
   '$do_table_statistics'(Mod,Pred2).
'$do_table_statistics'(Mod,PredName/PredArity) :- 
   atom(PredName), 
   integer(PredArity),
   functor(PredFunctor,PredName,PredArity),
   '$flags'(PredFunctor,Mod,Flags,Flags), !,
   (
       Flags /\ 0x000040 =\= 0, !, '$c_table_statistics'(Mod,PredFunctor)
   ;
       '$do_error'(domain_error(table,Mod:PredName/PredArity),table_statistics(Mod:PredName/PredArity))
   ).
'$do_table_statistics'(Mod,Pred) :-
   '$do_error'(type_error(callable,Mod:Pred),table_statistics(Mod:Pred)).



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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
