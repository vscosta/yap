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
* File:		statistics.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	statistics on Prolog status				 *
*									 *
*************************************************************************/

%%% User interface for statistics

statistics :-
	'$runtime'(Runtime,_), 
	'$cputime'(CPUtime,_), 
	'$systime'(SYStime,_), 
	'$walltime'(Walltime,_), 
	'$statistics_heap_info'(HpSpa, HpInUse), 
	'$statistics_heap_max'(HpMax), 
	'$statistics_trail_info'(TrlSpa, TrlInUse), 
	'$statistics_trail_max'(TrlMax), 
	'$statistics_stacks_info'(StkSpa, GlobInU, LocInU), 
	'$statistics_global_max'(GlobMax), 
	'$statistics_local_max'(LocMax), 
	'$inform_heap_overflows'(NOfHO,TotHOTime),
	'$inform_stack_overflows'(NOfSO,TotSOTime),
	'$inform_trail_overflows'(NOfTO,TotTOTime),
	'$inform_gc'(NOfGC,TotGCTime,TotGCSize),
	'$inform_agc'(NOfAGC,TotAGCTime,TotAGCSize),
	'$statistics'(Runtime,CPUtime,SYStime,Walltime,HpSpa,HpInUse,HpMax,TrlSpa, TrlInUse,TrlMax,StkSpa, GlobInU, LocInU,GlobMax,LocMax,NOfHO,TotHOTime,NOfSO,TotSOTime,NOfTO,TotTOTime,NOfGC,TotGCTime,TotGCSize,NOfAGC,TotAGCTime,TotAGCSize).

'$statistics'(Runtime,CPUtime,SYStime,Walltime,HpSpa,HpInUse,HpMax,TrlSpa, TrlInUse,_TrlMax,StkSpa, GlobInU, LocInU,GlobMax,LocMax,NOfHO,TotHOTime,NOfSO,TotSOTime,NOfTO,TotTOTime,NOfGC,TotGCTime,TotGCSize,NOfAGC,TotAGCTime,TotAGCSize) :-
	TotalMemory is HpSpa+StkSpa+TrlSpa,
	format(user_error,'memory (total)~t~d bytes~35+~n', [TotalMemory]),
	format(user_error,'   program space~t~d bytes~35+', [HpSpa]),
	format(user_error,':~t  ~d in use~19+', [HpInUse]),
	HpFree is HpSpa-HpInUse,
	format(user_error,',~t  ~d free~19+~n', [HpFree]),
	format(user_error,'~t  ~d  max~73+~n', [HpMax]),
	format(user_error,'   stack space~t~d bytes~35+', [StkSpa]),
	StackInUse is GlobInU+LocInU,
	format(user_error,':~t  ~d in use~19+', [StackInUse]),
	StackFree is StkSpa-StackInUse,
	format(user_error,',~t  ~d free~19+~n', [StackFree]),
	format(user_error,'     global stack:~t~35+', []),
	format(user_error,' ~t  ~d in use~19+', [GlobInU]),
	format(user_error,',~t  ~d  max~19+~n', [GlobMax]),
	format(user_error,'      local stack:~t~35+', []),
	format(user_error,' ~t  ~d in use~19+', [LocInU]),
	format(user_error,',~t  ~d  max~19+~n', [LocMax]),
	format(user_error,'   trail stack~t~d bytes~35+', [TrlSpa]),
	format(user_error,':~t  ~d in use~19+', [TrlInUse]),
	TrlFree is TrlSpa-TrlInUse,
	format(user_error,',~t  ~d free~19+~n', [TrlFree]),
	OvfTime is (TotHOTime+TotSOTime+TotTOTime)/1000,
	format(user_error,'~n~t~3f~12+ sec. for ~w code, ~w stack, and ~w trail space overflows~n',
	       [OvfTime,NOfHO,NOfSO,NOfTO]),
	TotGCTimeF is float(TotGCTime)/1000,
	format(user_error,'~t~3f~12+ sec. for ~w garbage collections which collected ~d bytes~n',
	       [TotGCTimeF,NOfGC,TotGCSize]),
	TotAGCTimeF is float(TotAGCTime)/1000,
	format(user_error,'~t~3f~12+ sec. for ~w atom garbage collections which collected ~d bytes~n',
	       [TotAGCTimeF,NOfAGC,TotAGCSize]),
	RTime is float(Runtime)/1000,
	format(user_error,'~t~3f~12+ sec. runtime~n', [RTime]),
	CPUTime is float(CPUtime)/1000,
	format(user_error,'~t~3f~12+ sec. cputime~n', [CPUTime]),
	SYSTime is float(SYStime)/1000,
	format(user_error,'~t~3f~12+ sec. systime~n', [SYSTime]),
	WallTime is float(Walltime)/1000,
	format(user_error,'~t~3f~12+ sec. elapsed time~n~n', [WallTime]),
	fail.
'$statistics'(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

statistics(runtime,[T,L]) :-
	'$runtime'(T,L).
statistics(cputime,[T,L]) :-
	'$cputime'(T,L).
statistics(walltime,[T,L]) :-
	'$walltime'(T,L).
statistics(threads,NT) :-
	'$nof_threads'(NT).
statistics(threads_created,TC) :-
	'$nof_threads_created'(TC).
statistics(thread_cputime,TR) :-
	'$thread_runtime'(TR).
%statistics(core,[_]).
%statistics(memory,[_]).
statistics(heap,[Hp,HpF]) :-
	'$statistics_heap_info'(HpM, Hp),
	HpF is HpM-Hp.
statistics(program,Info) :-
	statistics(heap,Info).
statistics(global_stack,[GlobInU,GlobFree]) :-
	'$statistics_stacks_info'(StkSpa, GlobInU, LocInU),
	GlobFree is StkSpa-GlobInU-LocInU.
statistics(local_stack,[LocInU,LocFree]) :-
	'$statistics_stacks_info'(StkSpa, GlobInU, LocInU),
	LocFree is StkSpa-GlobInU-LocInU.
statistics(trail,[TrlInUse,TrlFree]) :-
	'$statistics_trail_info'(TrlSpa, TrlInUse),
	TrlFree is TrlSpa-TrlInUse.
statistics(garbage_collection,[NOfGC,TotGCSize,TotGCTime]) :-
	'$inform_gc'(NOfGC,TotGCTime,TotGCSize).
statistics(stack_shifts,[NOfHO,NOfSO,NOfTO]) :-
	'$inform_heap_overflows'(NOfHO,_),
	'$inform_stack_overflows'(NOfSO,_),
	'$inform_trail_overflows'(NOfTO,_).
statistics(atoms,[NOf,SizeOf]) :-
	'$statistics_atom_info'(NOf,SizeOf),
	'$inform_stack_overflows'(NOfSO,_),
	'$inform_trail_overflows'(NOfTO,_).
statistics(static_code,[ClauseSize, IndexSize, TreeIndexSize, ExtIndexSize, SWIndexSize]) :-
	'$statistics_db_size'(ClauseSize, TreeIndexSize, ExtIndexSize, SWIndexSize),
	IndexSize is TreeIndexSize+ ExtIndexSize+ SWIndexSize.
statistics(dynamic_code,[ClauseSize,IndexSize, TreeIndexSize, CPIndexSize, ExtIndexSize, SWIndexSize]) :-
	'$statistics_lu_db_size'(ClauseSize, TreeIndexSize, CPIndexSize, ExtIndexSize, SWIndexSize),
	IndexSize is TreeIndexSize+CPIndexSize+ ExtIndexSize+ SWIndexSize.

key_statistics(Key, NOfEntries, TotalSize) :-
	key_statistics(Key, NOfEntries, ClSize, IndxSize),
	TotalSize is ClSize+IndxSize.


%%	time(:Goal)
%
%	Time the execution of Goal.  Possible choice-points of Goal are removed.
%	Based on the SWI-Prolog definition minus reporting the number of inferences,
%	which YAP does not currently supports

:- meta_predicate time(:).

time(Goal) :-
	var(Goal),
	'$do_error'(instantiation_error,time(Goal)).
time(_:Goal) :-
	var(Goal),
	'$do_error'(instantiation_error,time(Goal)).
time(Goal) :- \+ callable(Goal), !,
	'$do_error'(type_error(callable,Goal),time(Goal)).
time(Goal) :-
	statistics(walltime, _),
	statistics(cputime, _), 
	(   catch(Goal, E, true)
	->  Result = yes
	;   Result = no
	),
	statistics(cputime, [_, Time]),
	statistics(walltime, [_, Wall]),
	(   Time =:= 0
	->  CPU = 'Inf'
	;   CPU is truncate(Time/Wall*100)
	),
	TimeSecs is Time/1000,
	WallSecs is Wall/1000,
	format(user_error,'% ~3f CPU in ~3f seconds (~|~t~w~3+% CPU)~n', [TimeSecs, WallSecs, CPU]),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).
