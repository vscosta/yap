%%% -*- Mode: Prolog; -*-


:- module(logger,[logger_define_variable/2,
	          logger_define_variables/2,
	          logger_set_filename/1,
		  logger_set_delimiter/1,
		  logger_set_variable/2,
		  logger_set_variable_again/2,
		  logger_get_variable/2,
		  logger_start_timer/1,
		  logger_stop_timer/1,
		  logger_write_data/0,
		  logger_write_header/0]).

:- use_module(library(system),[datime/1,mktime/2]).
:- use_module(library(lists),[append/3,member/2]).

:- yap_flag(unknown,error).
:- style_check(single_var).

:- bb_put(logger_filename,'out.dat').
:- bb_put(logger_delimiter,';').
:- bb_put(logger_variables,[]).


%========================================================================
%= Defines a new variable, possible types are: int, float and time
%=
%= +Name, +Type
%========================================================================

logger_define_variable(Name,int) :-
	!,
	is_variable_already_defined(Name),
	bb_delete(logger_variables,OldVariables),
	append(OldVariables,[(Name,int)],NewVariables),
	bb_put(logger_variables,NewVariables),
	atom_concat(logger_data_,Name,Key),
	bb_put(Key,null).
logger_define_variable(Name,float) :-
	!,
	is_variable_already_defined(Name),
	bb_delete(logger_variables,OldVariables),
	append(OldVariables,[(Name,float)],NewVariables),
	bb_put(logger_variables,NewVariables),
	atom_concat(logger_data_,Name,Key),
	bb_put(Key,null).
logger_define_variable(Name,time) :-
	!,
	is_variable_already_defined(Name),
	bb_delete(logger_variables,OldVariables),
	append(OldVariables,[(Name,time)],NewVariables),
	bb_put(logger_variables,NewVariables),
	atom_concat(logger_data_,Name,Key),
	atom_concat(logger_start_time_,Name,Key2),
	bb_put(Key,null),
	bb_put(Key2,null).
logger_define_variable(Name,Unknown) :-
	is_variable_already_defined(Name),
	write('logger_define_variable, unknown type '),
	write(Unknown),
	write(' for variable '),
	write(Name),
	nl,
	fail.

is_variable_already_defined(Name) :-
	bb_get(logger_variables,Variables),
	member((Name,_),Variables),!,
	write('logger_define_variable, Variable '),
	write(Name),
	write(' is already defined!\n'),
	fail;
	true.

%========================================================================
%= 
%=
%= +ListOfNames, +Type
%========================================================================

logger_define_variables([],_).
logger_define_variables([H|T],Type) :-
	logger_define_variable(H,Type),
	logger_define_variables(T,Type).

%========================================================================
%= Set the filename, to which the output should be appended
%=
%= +Name
%========================================================================

logger_set_filename(Name) :-
	bb_put(logger_filename,Name).

%========================================================================
%= Set the delimiter for the fields
%= 
%= +Delimiter
%========================================================================

logger_set_delimiter(Delimiter) :-
	bb_put(logger_delimiter,Delimiter).
%========================================================================
%= Set the value of the variable name. If the value is already set or
%= if the variable does not exists, an error will be displayed and the
%= Prolog will be halted. 
%=
%= +Name, +Value
%========================================================================

logger_set_variable(Name,Value) :-
	atom_concat(logger_data_,Name,Key),
	(
	    bb_get(Key,null)
	->
	    (
		bb_put(Key,Value)
	    );(
	         bb_get(Key,_)
	      ->
	         (
		     write('logger_set_variable, Variable '),
		     write(Name),
		     write(' is already set'),
		     nl,
		     fail
		 ) ; (
		     write('logger_set_variable, unknown variable '),
		     write(Name),
		     nl,
		     fail
		     )
		 )	 
	),!.

%========================================================================
%= Set the value of the variable name. If the value is already set or
%= the old value is overwritten. If the variable does not exists, an
%= error will be displayed and the Prolog will be halted. 
%=
%= +Name, +Value
%========================================================================

logger_set_variable_again(Name,Value) :-
	atom_concat(logger_data_,Name,Key),
	(
	    bb_get(Key,_)
	->
	    (
		bb_put(Key,Value)
	    );(
	         write('logger_set_variable, unknown variable '),
                 write(Name),
     		 nl,
		 fail
               )
	),!.


logger_variable_is_set(Name) :-
	atom_concat(logger_data_,Name,Key),
	bb_get(Key,X),
	X \= null.

%========================================================================
%= Get the value of the variable name. If the value is not yet set or
%= if the variable does not exists, an error will be displayed and the
%= Prolog will be halted. 
%=
%= +Name, +Value
%========================================================================

logger_get_variable(Name,Value) :-
	atom_concat(logger_data_,Name,Key),
	(
	    bb_get(Key,null)
	->
	    (
		write('logger_get_variable, Variable '),
		write(Name),
		write(' is not yet set'),
		nl,
		fail
	    );(
	         bb_get(Key,Value)
	         ; 
		  (
		      write('logger_set_variable, unknown variable '),
		      write(Name),
		      nl,
		      fail
		   )	 
	)
        ),!.
%========================================================================
%= 
%= 
%= +Name
%========================================================================

logger_start_timer(Name) :-
	atom_concat(logger_start_time_,Name,Key),
	(
	    bb_get(Key,null)
	->
	    (
		statistics(walltime,[StartTime,_]),
		bb_put(Key,StartTime)
	    );(
	        bb_get(Key,_)
	    ->
	       (
		   write('logger_start_timer, timer '),
		   write(Name),
		   write(' is already started'),
		   nl,
		   fail
	       );(
	           write('logger_start_timer, timer '),
		   write(Name),
		   write(' is not defined'),
		   nl,
		   fail
	         )
	      )
	  ),!.
	

logger_stop_timer(Name) :-
	atom_concat(logger_start_time_,Name,Key),

	bb_delete(Key,StartTime),
	statistics(walltime,[StopTime,_]),

	bb_put(Key,null),

	Duration is StopTime-StartTime,

	(
	    logger_variable_is_set(Name)
        ->
	    (
		logger_get_variable(Name,OldDuration),
		NewDuration is Duration+OldDuration,
	        logger_set_variable_again(Name,NewDuration)
	    ); logger_set_variable(Name,Duration)
	),!.

%========================================================================
%= write a new line to the log file, which contains all the
%= values of the variables. afterwards, reset all variables to null.
%= 
%========================================================================

logger_write_data :-
	bb_get(logger_filename,FName),
	bb_get(logger_variables,Variables),
	open(FName,'append',Handle),
	logger_write_data_intern(Variables,Handle),
	close(Handle),

	% reset variables
	findall(_,(member((Name,_),Variables),atom_concat(logger_data_,Name,Key),bb_put(Key,null)),_),
	findall(_,(member((Name,time),Variables),atom_concat(logger_start_time_,Name,Key2),bb_put(Key2,null)),_).
	
logger_write_data_intern([],_).
logger_write_data_intern([(Name,_Type)],Handle) :-
	variablevalue_with_nullcheck(Name,Value),
	write(Handle,Value),
	write(Handle,'\n').
logger_write_data_intern([(Name,_Type),Next|T],Handle) :-
	variablevalue_with_nullcheck(Name,Value),
	bb_get(logger_delimiter,D),
	write(Handle,Value),
	write(Handle,D),
	logger_write_data_intern([Next|T],Handle).

variablevalue_with_nullcheck(Name,Result) :-
	atom_concat(logger_data_,Name,Key),
	bb_get(Key,Value),
	(
	    Value=null
	-> 
	    Result = '' ; 
	    Result=Value
	).
%========================================================================
%= 
%= 
%= 
%========================================================================

logger_write_header :-
	bb_get(logger_filename,FName),
	bb_get(logger_variables,Variables),
	open(FName,'append',Handle),
	write(Handle,'# '),
	logger_write_header_intern(Variables,Handle),
	write(Handle,'\n'),
	close(Handle).
	
logger_write_header_intern([],_).
logger_write_header_intern([(Name,_Type)],Handle) :-
	write(Handle,Name).
logger_write_header_intern([(Name,_Type),Next|T],Handle) :-
	bb_get(logger_delimiter,D),
	write(Handle,Name),
	write(Handle,D),
	logger_write_header_intern([Next|T],Handle).
