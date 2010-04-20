/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		directives.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	directing system execution				 *
*									 *
*************************************************************************/

'$all_directives'(_:G1) :- !,
	'$all_directives'(G1).
'$all_directives'((G1,G2)) :- !,
	'$all_directives'(G1),
	'$all_directives'(G2).
'$all_directives'(G) :- !,
	'$directive'(G).

'$directive'(block(_)).
'$directive'(char_conversion(_,_)).
'$directive'(compile(_)).
'$directive'(consult(_)).
'$directive'(discontiguous(_)).
'$directive'(dynamic(_)).
'$directive'(elif(_)).
'$directive'(else).
'$directive'(encoding(_)).
'$directive'(endif).
'$directive'(ensure_loaded(_)).
'$directive'(expects_dialect(_)).
'$directive'(if(_)).
'$directive'(include(_)).
'$directive'(initialization(_)).
'$directive'(initialization(_,_)).
'$directive'(meta_predicate(_)).
'$directive'(module(_,_)).
'$directive'(module(_,_,_)).
'$directive'(module_transparent(_)).
'$directive'(multifile(_)).
'$directive'(noprofile(_)).
'$directive'(parallel).
'$directive'(public(_)).
'$directive'(op(_,_,_)).
'$directive'(set_prolog_flag(_,_)).
'$directive'(reconsult(_)).
'$directive'(reexport(_)).
'$directive'(reexport(_,_)).
'$directive'(sequential).
'$directive'(sequential(_)).
'$directive'(thread_initialization(_)).
'$directive'(thread_local(_)).
'$directive'(uncutable(_)).
'$directive'(use_module(_)).
'$directive'(use_module(_,_)).
'$directive'(use_module(_,_,_)).
'$directive'(wait(_)).

'$exec_directives'((G1,G2), Mode, M) :- !,
	'$exec_directives'(G1, Mode, M),
	'$exec_directives'(G2, Mode, M).
'$exec_directives'(G, Mode, M) :-
	'$exec_directive'(G, Mode, M).

'$exec_directive'(multifile(D), _, M) :-
	'$system_catch'('$multifile'(D, M), M,
	      Error,
	      user:'$LoopError'(Error, top)).
'$exec_directive'(discontiguous(D), _, M) :-
	'$discontiguous'(D,M).
'$exec_directive'(initialization(D), _, M) :-
	'$initialization'(M:D).
'$exec_directive'(initialization(D,OPT), _, M) :-
	'$initialization'(M:D, OPT).
'$exec_directive'(thread_initialization(D), _, M) :-
	'$thread_initialization'(M:D).
'$exec_directive'(expects_dialect(D), _, _) :-
	'$expects_dialect'(D).
'$exec_directive'(encoding(Enc), _, _) :-
        '$set_encoding'(Enc).
'$exec_directive'(parallel, _, _) :-
	'$parallel'.
'$exec_directive'(sequential, _, _) :-
	'$sequential'.
'$exec_directive'(sequential(G), _, M) :-
	'$sequential_directive'(G, M).
'$exec_directive'(parallel(G), _, M) :-
	'$parallel_directive'(G, M).
'$exec_directive'(include(F), Status, _) :-
	'$include'(F, Status).
'$exec_directive'(module(N,P), Status, _) :-
	'$module'(Status,N,P).
'$exec_directive'(module(N,P,Op), Status, _) :-
	'$module'(Status,N,P,Op).
'$exec_directive'(meta_predicate(P), _, M) :-
	'$meta_predicate'(P, M).
'$exec_directive'(module_transparent(P), _, M) :-
	'$module_transparent'(P, M).
'$exec_directive'(noprofile(P), _, M) :-
	'$noprofile'(P, M).
'$exec_directive'(dynamic(P), _, M) :-
	'$dynamic'(P, M).
'$exec_directive'(thread_local(P), _, M) :-
	'$thread_local'(P, M).
'$exec_directive'(op(P,OPSEC,OP), _, _) :-
	'$current_module'(M),
	op(P,OPSEC,M:OP).
'$exec_directive'(set_prolog_flag(F,V), _, _) :-
	set_prolog_flag(F,V).
'$exec_directive'(ensure_loaded(Fs), _, M) :-
	'$load_files'(M:Fs, [if(changed)], ensure_loaded(Fs)).
'$exec_directive'(char_conversion(IN,OUT), _, _) :-
	char_conversion(IN,OUT).
'$exec_directive'(public(P), _, M) :-
	'$public'(P, M).
'$exec_directive'(compile(Fs), _, M) :-
	'$load_files'(M:Fs, [], compile(Fs)).
'$exec_directive'(reconsult(Fs), _, M) :-
	'$load_files'(M:Fs, [], reconsult(Fs)).
'$exec_directive'(consult(Fs), _, M) :-
	'$consult'(Fs, M).
'$exec_directive'(use_module(F), _, M) :-
	'$load_files'(M:F, [if(not_loaded)],use_module(F)).
'$exec_directive'(reexport(F), _, M) :-
	'$reexport'(F, all, M).
'$exec_directive'(reexport(F,Spec), _, M) :-
	'$reexport'(F, Spec, M).
'$exec_directive'(use_module(F,Is), _, M) :-
	'$load_files'(M:F, [if(not_loaded),imports(Is)],use_module(F,Is)).
'$exec_directive'(use_module(Mod,F,Is), _, _) :-
	'$use_module'(Mod,F,Is).
'$exec_directive'(block(BlockSpec), _, _) :-
	'$block'(BlockSpec).
'$exec_directive'(wait(BlockSpec), _, _) :-
	'$wait'(BlockSpec).
'$exec_directive'(table(PredSpec), _, M) :-
	'$table'(PredSpec, M).
'$exec_directive'(uncutable(PredSpec), _, M) :-
	'$uncutable'(PredSpec, M).
'$exec_directive'(if(Goal), Context, M) :-
	'$if'(M:Goal, Context).
'$exec_directive'(else, Context, _) :-
	'$else'(Context).
'$exec_directive'(elif(Goal), Context, M) :-
	'$elif'(M:Goal, Context).
'$exec_directive'(endif, Context, _) :-
	'$endif'(Context).

%                                                                                  
% allow users to define their own directives.                                      
%                                                                                  
user_defined_directive(Dir,_) :-
        '$directive'(Dir), !.
user_defined_directive(Dir,Action) :-
        functor(Dir,Na,Ar),
        functor(NDir,Na,Ar),
        '$current_module'(M, prolog),
	assert_static('$directive'(NDir)),
	assert_static(('$exec_directive'(Dir, _, _) :- Action)),
        '$current_module'(_, M).

'$thread_initialization'(M:D) :-
	eraseall('$thread_initialization'),
	recorda('$thread_initialization',M:D,_),
	fail.
'$thread_initialization'(M:D) :-
	'$initialization'(M:D).


