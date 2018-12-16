%:- start_low_level_trace.

:- module(user).
:- yap_flag(verbose,normal).

query( String ) :-
 	yap_flag(typein_module, Mod),
  	atomic_to_term( String, Goal, VarNames ),
 	query_to_answer( Mod:Goal, VarNames, Status, Bindings),
 	output( Bindings, Status) .

output( Bindings, Status) :-
	(Status == answer -> true ;
	 Status == exit ->true
	),
	write_query_answer( Bindings ),
	nl(user_error).

%:- [sqlitest].