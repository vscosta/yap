:- module(dbqueue, [
	db_enqueue/2,
	db_dequeue/2,
	db_clean_queue/1	  
		 ]).


:- unhide('$init_db_queue').
:- unhide('$db_enqueue').
:- unhide('$db_dequeue').


db_enqueue(Name,El) :- var(Name),
	throw(error(instantiation_error(Name),db_enqueue(Name,El))).
db_enqueue(Name,El) :- \+ atom(Name), !,
	throw(error(type_error_atom(Name),db_enqueue(Name,El))).
db_enqueue(Name,El) :-
	recorded('$db_queue',[Name|Ref],_), !,
	prolog:'$db_enqueue'(Ref, El).
db_enqueue(Name,El) :-
	prolog:'$init_db_queue'(Ref),
	recorda('$db_queue',[Name|Ref],_),
	prolog:'$db_enqueue'(Ref,El).


db_dequeue(Name,El) :- var(Name),
	throw(error(instantiation_error(Name),db_dequeue(Name,El))).
db_dequeue(Name,El) :- \+ atom(Name), !,
	throw(error(type_error_atom(Name),db_dequeue(Name,El))).
db_dequeue(Name,El) :-
	recorded('$db_queue',[Name|Ref],R),
	( prolog:'$db_dequeue'(Ref, El) ->
	    true
	;
	    erase(R),
	    fail
	).

db_clean_queue(Name) :-
	recorded('$db_queue',[Name|Ref],R), !,
	erase(R),
	db_dequeue_all(Ref).
db_clean_queue(_).

db_dequeue_all(Ref) :-
	( prolog:'$db_dequeue'(Ref, _) -> db_dequeue_all(Ref) ; true ).
