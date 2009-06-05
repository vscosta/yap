% A library to implement queues of NB Terms

:- module(nbqueue, [
		    nb_enqueue/2,
		    nb_dequeue/2,
		    nb_clean_queue/1,	  
		    nb_size/2	  
		   ]).


:- unhide('$init_nb_queue').
:- unhide('$nb_enqueue').
:- unhide('$nb_dequeue').


nb_enqueue(Name,El) :- var(Name),
	throw(error(instantiation_error(Name),nb_enqueue(Name,El))).
nb_enqueue(Name,El) :- \+ atom(Name), !,
	throw(error(type_error_atom(Name),nb_enqueue(Name,El))).
nb_enqueue(Name,El) :-
	recorded('$nb_queue',[Name|Ref],_), !,
	prolog:'$nb_enqueue'(Ref, El).
nb_enqueue(Name,El) :-
	prolog:'$init_nb_queue'(Ref),
	recorda('$nb_queue',[Name|Ref],_),
	prolog:'$nb_enqueue'(Ref,El).


nb_dequeue(Name,El) :- var(Name),
	throw(error(instantiation_error(Name),nb_dequeue(Name,El))).
nb_dequeue(Name,El) :- \+ atom(Name), !,
	throw(error(type_error_atom(Name),nb_dequeue(Name,El))).
nb_dequeue(Name,El) :-
	recorded('$nb_queue',[Name|Ref],R),
	( prolog:'$nb_dequeue'(Ref, El) ->
	    true
	;
	    erase(R),
	    fail
	).

nb_clean_queue(Name) :-
	recorded('$nb_queue',[Name|Ref],R), !,
	erase(R),
	nb_dequeue_all(Ref).
nb_clean_queue(_).

nb_dequeue_all(Ref) :-
	( prolog:'$nb_dequeue'(Ref, _) -> nb_dequeue_all(Ref) ; true ).

nb_dequeue_size(Ref, Size) :-
	prolog:'$nb_size'(Ref, Size).
