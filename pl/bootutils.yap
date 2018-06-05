/**
  @file bootutils.yap
  @short utilities

  @addtogroup Internal_Database
 @{
  */

/** @pred  recordaifnot(+ _K_, _T_,- _R_) 


If a term equal to  _T_ up to variable renaming is stored under key
 _K_ fail. Otherwise, make term  _T_ the first record under key
 _K_ and unify  _R_ with its reference.

 
*/
recordaifnot(K,T,R) :-
        recorded(K,T,R), % force non-det binding to R.
        '$still_variant'(R,T),
        !,
        fail.
recordaifnot(K,T,R) :-
        recorda(K,T,R).

/** @pred  recordzifnot(+ _K_, _T_,- _R_) 


If a term equal to  _T_ up to variable renaming is stored under key
 _K_ fail. Otherwise, make term  _T_ the first record under key
 _K_ and unify  _R_ with its reference.

This predicate is YAP specific.

 
*/
recordzifnot(K,T,R) :-
        recorded(K,T,R),
        '$still_variant'(R,T),
        !,
        fail.
recordzifnot(K,T,R) :-
        recordz(K,T,R).

%% @}
