
:- module(android,
	[text_to_query/2]).
	
:- initialization(yap_flag(verbose,_,normal)).

:- meta_predicate( text_to_query( :, - ) ).

text_to_query( MString, Status ) :-
	strip_module( MString, Mod, String ),

    top_query(query_( MString, Mod, Status ) ).

query_( Mtring, Mod, Status ) :-
  	atomic_to_term( String, Goal, VarNames ),
	(
	is_list(Goal) -> G = ensure_loaded( Goal ) ; G = Goal ),
 	catch(query_to_answer( Mod:G, VarNames, Status, Bindings).


