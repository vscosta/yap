
:- module(android,
	[text_to_query/2]).
	
:- initialization(yap_flag(verbose,_,normal)).

:- meta_predicate( text_to_query( :, - ) ).

text_to_query( MString, Status ) :-
	strip_module( MString, Mod, String ),
  	atomic_to_term( String, Goal, VarNames ),
	(
	is_list(Goal) -> G = ensure_loaded( Goal ) ; G = Goal ),
 	catch(query_to_answer( Mod:G, VarNames, Status, Bindings),
	      H,error_handler(H,error)
	     ), 
	write_query_answer( Bindings ),
	nl(user_error).

%:- [sqlitest].
user:file_search_path(data, Home) :-
    user:file_search_path(library, Home).
