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

/**
  * @file   statistics.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:13:51 2017
  * 
  * @brief  System Status
  *
  * @defgroup Statistics System Status
  * @ingroup builtins
  * 
  * @{
*/

:- system_module( '$_statistics', [key_statistics/3,
        statistics/0,
        statistics/2,
        time/1], []).

:- use_system_module( '$_errors', ['$do_error'/2]).

%%% User interface for statistics

/** @pred  statistics/0 


Send to the current user error stream general information on space used and time
spent by the system.

~~~~~
?- statistics.
memory (total)        4784124 bytes
   program space      3055616 bytes:    1392224 in use,      1663392 free
                                                             2228132  max
   stack space        1531904 bytes:        464 in use,      1531440 free
     global stack:                           96 in use,       616684  max
      local stack:                          368 in use,       546208  max
   trail stack         196604 bytes:          8 in use,       196596 free

       0.010 sec. for 5 code, 2 stack, and 1 trail space overflows
       0.130 sec. for 3 garbage collections which collected 421000 bytes
       0.000 sec. for 0 atom garbage collections which collected 0 bytes
       0.880 sec. runtime
       1.020 sec. cputime
      25.055 sec. elapsed time

~~~~~
The example shows how much memory the system spends. Memory is divided
into Program Space, Stack Space and Trail. In the example we have 3MB
allocated for program spaces, with less than half being actually
used. YAP also shows the maximum amount of heap space having been used
which was over 2MB.

The stack space is divided into two stacks which grow against each
other. We are in the top level so very little stack is being used. On
the other hand, the system did use a lot of global and local stack
during the previous execution (we refer the reader to a WAM tutorial in
order to understand what are the global and local stacks).

YAP also shows information on how many memory overflows and garbage
collections the system executed, and statistics on total execution
time. Cputime includes all running time, runtime excludes garbage
collection and stack overflow time.

 
*/
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

/** @pred  statistics(? _Param_,- _Info_)

Gives statistical information on the system parameter given by first
argument:



+ atoms `[ _NumberOfAtoms_, _SpaceUsedBy Atoms_]`

This gives the total number of atoms `NumberOfAtoms` and how much
space they require in bytes,  _SpaceUsedBy Atoms_.

+ cputime `[ _Time since Boot_, _Time From Last Call to Cputime_]`

This gives the total cputime in milliseconds spent executing Prolog code,
garbage collection and stack shifts time included.

+ dynamic_code `[ _Clause Size_, _Index Size_, _Tree Index Size_, _Choice Point Instructions Size_, _Expansion Nodes Size_, _Index Switch Size_]`


Size of static code in YAP in bytes:  _Clause Size_, the number of
bytes allocated for clauses, plus
 _Index Size_, the number of bytes spent in the indexing code. The
indexing code is divided into main tree,  _Tree Index Size_, 
tables that implement choice-point manipulation,  _Choice Point Size_, tables that cache clauses for future expansion of the index
tree,  _Expansion Nodes Size_, and 
tables such as hash tables that select according to value,   _Index Switch Size_.

+ garbage_collection `[ _Number of GCs_, _Total Global Recovered_, _Total Time Spent_]`


Number of garbage collections, amount of space recovered in kbytes, and
total time spent doing garbage collection in milliseconds. More detailed
information is available using `yap_flag(gc_trace,verbose)`.

+ global_stack `[ _Global Stack Used_, _Execution Stack Free_]`


Space in kbytes currently used in the global stack, and space available for
expansion by the local and global stacks.

+ local_stack `[ _Local Stack Used_, _Execution Stack Free_]`


Space in kbytes currently used in the local stack, and space available for
expansion by the local and global stacks.

+ heap `[ _Heap Used_, _Heap Free_]`


Total space in kbytes not recoverable
in backtracking. It includes the program code, internal data base, and,
atom symbol table.

+ program `[ _Program Space Used_, _Program Space Free_]`


Equivalent to heap.

+ runtime `[ _Time since Boot_, _Time From Last Call to Runtime_]`


This gives the total cputime in milliseconds spent executing Prolog
code, not including garbage collections and stack shifts. Note that
until YAP4.1.2 the runtime statistics would return time spent on
garbage collection and stack shifting.

+ stack_shifts `[ _Number of Heap Shifts_, _Number of Stack Shifts_, _Number of Trail Shifts_]`


Number of times YAP had to
expand the heap, the stacks, or the trail. More detailed information is
available using `yap_flag(gc_trace,verbose)`.

+ static_code `[ _Clause Size_, _Index Size_, _Tree Index Size_, _Expansion Nodes Size_, _Index Switch Size_]`


Size of static code in YAP in bytes:  _Clause Size_, the number of
bytes allocated for clauses, plus
 _Index Size_, the number of bytes spent in the indexing code. The
indexing code is divided into a main tree,  _Tree Index Size_, table that cache clauses for future expansion of the index
tree,  _Expansion Nodes Size_, and and 
tables such as hash tables that select according to value,   _Index Switch Size_.

+ trail `[ _Trail Used_, _Trail Free_]`


Space in kbytes currently being used and still available for the trail.

+ walltime `[ _Time since Boot_, _Time From Last Call to Walltime_]`


This gives the clock time in milliseconds since starting Prolog.



 
*/
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
	'$statistics_atom_info'(NOf,SizeOf).
statistics(static_code,[ClauseSize, IndexSize, TreeIndexSize, ExtIndexSize, SWIndexSize]) :-
	'$statistics_db_size'(ClauseSize, TreeIndexSize, ExtIndexSize, SWIndexSize),
	IndexSize is TreeIndexSize+ ExtIndexSize+ SWIndexSize.
statistics(dynamic_code,[ClauseSize,IndexSize, TreeIndexSize, CPIndexSize, ExtIndexSize, SWIndexSize]) :-
	'$statistics_lu_db_size'(ClauseSize, TreeIndexSize, CPIndexSize, ExtIndexSize, SWIndexSize),
	IndexSize is TreeIndexSize+CPIndexSize+ ExtIndexSize+ SWIndexSize.

/** @pred  key_statistics(+ _K_,- _Entries_,- _TotalSize_)

Returns several statistics for a key  _K_. Currently, it says how
many entries we have for that key,  _Entries_, what is the
total size spent on this key.

 
*/
key_statistics(Key, NOfEntries, TotalSize) :-
	key_statistics(Key, NOfEntries, ClSize, IndxSize),
	TotalSize is ClSize+IndxSize.


%%	time(:Goal)
%
%	Time the execution of Goal.  Possible choice-points of Goal are removed.
%	Based on the SWI-Prolog definition minus reporting the number of inferences,
%	which YAP does not currently supports

/** @pred  time(: _Goal_) 


Prints the CPU time and the wall time for the execution of  _Goal_.
Possible choice-points of  _Goal_ are removed. Based on the SWI-Prolog 
definition (minus reporting the number of inferences, which YAP currently
does not support).

 
*/
:- meta_predicate time(0).

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

%% @}

