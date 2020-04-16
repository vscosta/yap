
/**
 * @file   dbusage.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 15:04:52 2015
 * 
 * @brief Useful statistics on memory usage
 * 
 * 
*/

:- module(dbusage, [
	db_usage/0,
	db_static/0,
	db_static/1,
	db_dynamic/0,
	db_dynamic/1
    ]).

/**
  * @defgroup dbusage Memory Usage in Prolog Data-Base
  * @ingroup library
  @{

  This library provides a set of utilities for studying memory usage in YAP.
  The following routines are available once included with the
  `use_module(library(dbusage))` command.
*/

/** @pred db_usage
  
  Give general overview of data-base usage in the system.
*/
db_usage :-
	statistics(heap,[HeapUsed,HeapFree]),
	statistics(local_stack,[GInU,FreeS]),
	statistics(global_stack,[SInU,_]),
	statistics(trail,[TInU,FreeT]),
	HeapUsedK is HeapUsed//1024,
	HeapFreeK is HeapFree//1024,
	StackSpace is (GInU+SInU+FreeS+TInU+FreeT)//1024,
	format(user_error, 'Heap Space = ~D KB (+ ~D KB free)~n',[HeapUsedK,HeapFreeK]),	
	format(user_error, 'Stack Space = ~D KB~n',[StackSpace]),
	findall(p(Cls,CSz,ISz),
		(current_module(M),
		 current_predicate(_,M:P),
		 predicate_statistics(M:P,Cls,CSz,ISz)),LAll),
	sumall(LAll, TCls, TCSz, TISz),
	statistics(atoms,[AtomN,AtomS]),
	AtomSK is AtomS//1024,
	format(user_error, '~D Atoms taking ~D KB~n',[AtomN,AtomSK]),
	TSz is TCSz+TISz,
	TSzK is TSz//1024,
	TCSzK is TCSz//1024,
	TISzK is TISz//1024,
	format(user_error, 'Total User Code~n    ~D clauses taking ~D KB~n    ~D KB in clauses + ~D KB in indices~n',
	       [TCls,TSzK,TCSzK,TISzK]),
	statistics(static_code,[SCl,SI,SI1,SI2,SI3]),
	SClK is SCl//1024,
	SIK is SI//1024,
	SI1K is SI1//1024,
	SI2K is SI2//1024,
	SI3K is SI3//1024,
	ST is SCl+SI,
	STK is ST//1024,
	format(user_error, 'Total Static code=~D KB~n    ~D KB in clauses + ~D KB in indices (~D+~D+~D)~n',
	       [STK,SClK,SIK,SI1K,SI2K,SI3K]),
	statistics(dynamic_code,[DCl,DI,DI1,DI2,DI3,DI4]),
	DClK is DCl//1024,
	DIK is DI//1024,
	DI1K is DI1//1024,
	DI2K is DI2//1024,
	DI3K is DI3//1024,
	DI4K is DI4//1024,
	DT is DCl+DI,
	DTK is DT//1024,
	format(user_error, 'Total Dynamic code=~D KB~n    ~D KB in clauses + ~D KB in indices (~D+~D+~D+~D)~n',
	       [DTK,DClK,DIK,DI1K,DI2K,DI3K,DI4K]),
	total_erased(DCls,DSZ,ICls,ISZ),
	(DCls =:= 0 ->
	 true
	;
	 DSZK is DSZ//1024,
	 format(user_error, '    ~D erased clauses not reclaimed (~D KB)~n',[DCls,DSZK])
	),
	(ICls =:= 0 ->
	 true
	;
	 ISZK is ISZ//1024,
	 format(user_error, '    ~D erased indices not reclaimed (~D KB)~n',[ICls,ISZK])
	),
	!.

db_usage:-
	write(mem_dump_error),nl.


/** @pred db_static 


List memory usage for every static predicate.

 
*/
db_static :-
    db_static(-1).

/** @pred db_static(+ _Threshold_)

List memory usage for every static predicate. Predicate must use more
than  _Threshold_ bytes.

 
*/
db_static(Min) :-
	setof(p(Sz,M:P,Cls,CSz,ISz),
	      (statics(M,P),
	       predicate_statistics(M:P,Cls,CSz,ISz),
		  Sz is (CSz+ISz),
		  Sz > Min),All),
	format(user_error,' Static user code~n===========================~n',[]),
	display_preds(All).

statics(M,P) :-
    current_module(M),
    M \= idb,
    current_predicate(_PN,M:P),
		  \+predicate_property(M:P,dynamic),
		  \+predicate_property(M:P,imported_from(_)).


/** @pred db_dynamic 


List memory usage for every dynamic predicate.

 
*/
db_dynamic :-
    db_dynamic(-1).

/** @pred db_dynamic(+ _Threshold_)

List memory usage for every dynamic predicate. Predicate must use more
than  _Threshold_ bytes.




 */
db_dynamic(Min) :-
	setof(p(Sz,M:P,Cls,CSz,ISz,ECls,ECSz,EISz),
	      (dynamics(M,P),
		  predicate_statistics(M:P,Cls,CSz,ISz),
		  predicate_erased_statistics(M:P,ECls,ECSz,EISz),
		  Sz is (CSz+ISz+ECSz+EISz),
		  Sz > Min),
	      All),
	format(user_error,' Dynamic user code~n===========================~n',[]),
	display_dpreds(All).

dynamics(M,P) :-
    current_module(M),
    M \= idb,
		  current_predicate(_PN,M:P),
		  predicate_property(M:P,dynamic),
		  \+predicate_property(M:P,imported_from(_)).
dynamics(idb,P) :-
    current_key(_,P).

display_preds([]).
display_preds([p(Sz,M:P,Cls,CSz,ISz)|_]) :-
	functor(P,A,N),
	KSz is Sz//1024,
	KCSz is CSz//1024,
	KISz is ISz//1024,
	(M = user -> Name = A/N ; Name = M:A/N),
	format(user_error,'~w~t~36+:~t~D~7+ clauses using~|~t~D~8+ KB (~D + ~D)~n',[Name,Cls,KSz,KCSz,KISz]),
	fail.
display_preds([_|All]) :-
	display_preds(All).


display_dpreds([]).
display_dpreds([p(Sz,M:P,Cls,CSz,ISz,ECls,ECSz,EISz)|_]) :-
	(integer(P) -> A=P, N=0 ; functor(P,A,N) ),
	KSz is Sz//1024,
	KCSz is CSz//1024,
	KISz is ISz//1024,
	(M = user -> Name = A/N ; Name = M:A/N),
	format(user_error,'~w~t~36+:~t~D~7+ clauses using~|~t~D~8+ KB (~D + ~D)~n',[Name,Cls,KSz,KCSz,KISz]),
	(ECls =:= 0
	->
	 true
	;
	 ECSzK is ECSz//1024,
	 format(user_error,'                        ~D erased clauses: ~D KB~n',[ECls,ECSzK])
	),
	(EISz =:= 0
	->
	 true
	;
	 EISzK is EISz//1024,
	 format(user_error,'                        ~D KB erased indices~n',[EISzK])
	),
	fail.
display_dpreds([_|All]) :-
	display_dpreds(All).


sumall(LEDAll, TEDCls, TEDCSz, TEDISz) :-
	sumall(LEDAll, 0, TEDCls, 0, TEDCSz, 0, TEDISz).

sumall([], TEDCls, TEDCls, TEDCSz, TEDCSz, TEDISz, TEDISz).
sumall([p(Cls,CSz,ISz)|LEDAll], TEDCls0, TEDCls, TEDCSz0, TEDCSz, TEDISz0, TEDISz) :-
	TEDClsI is Cls+TEDCls0,
	TEDCSzI is CSz+TEDCSz0,
	TEDISzI is ISz+TEDISz0,
	sumall(LEDAll, TEDClsI, TEDCls, TEDCSzI, TEDCSz, TEDISzI, TEDISz).

/**
  @}
*/
