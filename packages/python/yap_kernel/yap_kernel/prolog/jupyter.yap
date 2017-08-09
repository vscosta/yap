

:-	 use_module(library(python)).

:-	initialization
       ensure_loaded(library(yapi) ),
		python_import(sys).

%% @pred yap_query(0:Goalc, - Dictionary)
%%
%% dictionary, Examples
%%
%%
jupyter_query( Self, String  ) :-
	set_python_output( Self,  Output , Error),
	python_query( Self, String ),
	close( Output ),
	cloe( Error ).



set_python_output(_Self,Output,Error) :-
	open('//python/sys.stdout', append, Output, [alias(output)]),
	open('//python/sys.stderr', append, Error, [alias(error)]),
	yap_flag(user_output, output),
	yap_flag(user_error, error).


