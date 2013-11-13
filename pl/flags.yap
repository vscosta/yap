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
* File:		flags.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	controlling YAP						 *
*									 *
*************************************************************************/


yap_flag(V,Out) :-
	'$user_defined_flag'(V,_,_,_),
	(nonvar(V) ->
	 !
	;
	 true
	),
	'$user_flag_value'(V, Out).

yap_flag(V,Out) :-
	( var(V) ->
	  '$swi_current_prolog_flag'(V, Out)
        ;
	  '$swi_current_prolog_flag'(V, Current)
        ->
	  (var(Out) ->
	      Current = Out
	  ;
	      '$swi_set_prolog_flag'(V, Out)
	  )
        ).
yap_flag(V,Out) :-
	var(V), !,
	'$show_yap_flag_opts'(V,Out).

% do or do not machine code
yap_flag(fast,on) :- set_value('$fast',true).
yap_flag(fast,off) :- !, set_value('$fast',[]).

% do or do not machine code
yap_flag(argv,L) :- '$argv'(L).

% hide/unhide atoms
yap_flag(hide,Atom) :- !, hide(Atom).
yap_flag(unhide,Atom) :- !, unhide(Atom).

% control garbage collection
yap_flag(gc,V) :-
	var(V), !,
	( get_value('$gc',[]) -> V = off ; V = on).
yap_flag(gc,on) :- !, set_value('$gc',true).
yap_flag(gc,off) :- !, set_value('$gc',[]).

yap_flag(gc_margin,N) :- 
	( var(N) -> 
	    get_value('$gc_margin',N)
	;
	  integer(N), N >0  ->
	    set_value('$gc_margin',N)
	;
	    '$do_error'(domain_error(flag_value,gc_margin+X),yap_flag(gc_margin,X))
	).
yap_flag(gc_trace,V) :-
	var(V), !,
	get_value('$gc_trace',N1),
	get_value('$gc_verbose',N2),
	get_value('$gc_very_verbose',N3),
	'$yap_flag_show_gc_tracing'(N1, N2, N3, V).
yap_flag(gc_trace,on) :- !,
	set_value('$gc_trace',true),
	set_value('$gc_verbose',[]),
	set_value('$gc_very_verbose',[]).
yap_flag(gc_trace,verbose) :- !,
	set_value('$gc_trace',[]),
	set_value('$gc_verbose',true),
	set_value('$gc_very_verbose',[]).
yap_flag(gc_trace,very_verbose) :- !,
	set_value('$gc_trace',[]),
	set_value('$gc_verbose',true),
	set_value('$gc_very_verbose',true).
yap_flag(gc_trace,off) :-
	set_value('$gc_trace',[]),
	set_value('$gc_verbose',[]),
	set_value('$gc_very_verbose',[]).
yap_flag(syntax_errors, V) :- var(V), !,
	'$get_read_error_handler'(V).
yap_flag(syntax_errors, Option) :-
	'$set_read_error_handler'(Option).
% compatibility flag
yap_flag(enhanced,on) :- !, set_value('$enhanced',true).
yap_flag(enhanced,off) :- set_value('$enhanced',[]).

% compatibility flag
yap_flag(agc_margin,Margin) :-
	'$agc_threshold'(Margin).


%
% show state of $
%
yap_flag(dollar_as_lower_case,V) :-
	var(V), !,
	'$type_of_char'(36,T),
	(T = 3 -> V = on ; V = off).
%
% make $a a legit atom
%
yap_flag(dollar_as_lower_case,on) :- !,
	'$change_type_of_char'(36,3).
%
% force quoting of '$a'
%
yap_flag(dollar_as_lower_case,off) :- 
	'$change_type_of_char'(36,7).

yap_flag(call_counting,X) :- (var(X); X = on; X = off), !,
	'$is_call_counted'(X).

:- set_value('$associate',yap).

yap_flag(associate,X) :-
	var(X), !,
	get_value('$associate',X).
yap_flag(associate,X) :-
	atom(X), !,
	set_value('$associate',X).
yap_flag(associate,X) :-
	'$do_error'(type_error(atom,X),associate(X)).

% do or do not indexation
yap_flag(index,X) :- var(X),
	'$access_yap_flags'(19, X1),
	'$transl_to_index_mode'(X1,X), !.
yap_flag(index,X)  :-
	'$transl_to_index_mode'(X1,X), !,
	'$set_yap_flags'(19,X1).
yap_flag(index,X) :-
	'$do_error'(domain_error(flag_value,index+X),yap_flag(index,X)).

% do or do not indexation
yap_flag(index_sub_term_search_depth,X) :- var(X),
	'$access_yap_flags'(23, X), !.
yap_flag(index_sub_term_search_depth,X,X)  :-
	integer(X), X > 0,
	'$set_yap_flags'(23,X1).
yap_flag(index_sub_term_search_depth,X,X)  :-
	\+ integer(X),
	'$do_error'(type_error(integer,X),yap_flag(index_sub_term_search_depth,X)).
yap_flag(index_sub_term_search_depth,X,X) :-
	'$do_error'(domain_error(out_of_range,index_sub_term_search_depth+X),yap_flag(index_sub_term_search_depth,X)).

% should match definitions in Yap.h
'$transl_to_index_mode'(0, off).
'$transl_to_index_mode'(1, single).
'$transl_to_index_mode'(2, compact).
'$transl_to_index_mode'(3, multi).
'$transl_to_index_mode'(3, on). % default is multi argument indexing
'$transl_to_index_mode'(4, max).

% tabling mode
yap_flag(tabling_mode,Options) :- 
   var(Options), !,
   '$access_yap_flags'(20,Options).
yap_flag(tabling_mode,[]) :- !,
   yap_flag(tabling_mode,default).
yap_flag(tabling_mode,[HOption|TOption]) :- !,
   yap_flag(tabling_mode,TOption),
   yap_flag(tabling_mode,HOption).
yap_flag(tabling_mode,Option) :-
   '$transl_to_yap_flag_tabling_mode'(Flag,Option), !,
   '$set_yap_flags'(20,Flag).
yap_flag(tabling_mode,Options) :-
   '$do_error'(domain_error(flag_value,tabling_mode+Options),yap_flag(tabling_mode,Options)).

% should match with code in stdpreds.c
'$transl_to_yap_flag_tabling_mode'(0,default).
'$transl_to_yap_flag_tabling_mode'(1,batched).
'$transl_to_yap_flag_tabling_mode'(2,local).
'$transl_to_yap_flag_tabling_mode'(3,exec_answers).
'$transl_to_yap_flag_tabling_mode'(4,load_answers).
'$transl_to_yap_flag_tabling_mode'(5,local_trie).
'$transl_to_yap_flag_tabling_mode'(6,global_trie).

yap_flag(informational_messages,X) :- var(X), !,
	 yap_flag(verbose, X).

yap_flag(version,X) :-
	var(X), !,
	get_value('$version_name',X).
yap_flag(version,X) :-
	'$do_error'(permission_error(modify,flag,version),yap_flag(version,X)).

/* ISO Core Revision DTR: new float flags

yap_flag(float_mantissa_digits,X) :-
	var(X), !,
	?????
yap_flag(float_mantissa_digits,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_mantissa_digits),yap_flag(float_mantissa_digits,X)).
yap_flag(float_mantissa_digits,X) :-
	'$do_error'(domain_error(flag_value,float_mantissa_digits+X),yap_flag(float_mantissa_digits,X)).

yap_flag(float_epsilon,X) :-
	var(X), !,
	?????
yap_flag(float_epsilon,X) :-
	float(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_epsilon),yap_flag(float_epsilon,X)).
yap_flag(float_epsilon,X) :-
	'$do_error'(domain_error(flag_value,float_epsilon+X),yap_flag(float_epsilon,X)).

yap_flag(float_min_exponent,X) :-
	var(X), !,
	?????
yap_flag(float_min_exponent,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_min_exponent),yap_flag(float_min_exponent,X)).
yap_flag(float_epsilon,X) :-
	'$do_error'(domain_error(flag_value,float_min_exponent+X),yap_flag(float_min_exponent,X)).

yap_flag(float_max_exponent,X) :-
	var(X), !,
	?????
yap_flag(float_max_exponent,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_max_exponent),yap_flag(flo
									     at_max_exponent,X)).
yap_flag(float_max_exponent,X) :-
	'$do_error'(domain_error(flag_value,float_max_exponent+X),yap_flag(float_max_exponent,X)).
*/

yap_flag(char_conversion,X) :-
	var(X), !,
	'$access_yap_flags'(5, X1),
	'$transl_to_on_off'(X1,X).
yap_flag(char_conversion,X) :-
	'$transl_to_on_off'(X1,X), !,
	'$set_yap_flags'(5,X1),
	( X1 = 1 ->
	    '$force_char_conversion'
	    ;
	    '$disable_char_conversion'
	).
yap_flag(char_conversion,X) :-
	'$do_error'(domain_error(flag_value,char_conversion+X),yap_flag(char_conversion,X)).

yap_flag(n_of_integer_keys_in_db,X) :-
	var(X), !,
	'$resize_int_keys'(X).
yap_flag(n_of_integer_keys_in_db,X) :- integer(X), X > 0, !,
	'$resize_int_keys'(X).
yap_flag(n_of_integer_keys_in_db,X) :-
	'$do_error'(domain_error(flag_value,n_of_integer_keys_in_db+X),yap_flag(n_of_integer_keys_in_db,X)).

yap_flag(n_of_integer_keys_in_bb,X) :-
	var(X), !,
	'$resize_bb_int_keys'(X).
yap_flag(n_of_integer_keys_in_bb,X) :- integer(X), X > 0, !,
	'$resize_bb_int_keys'(X).
yap_flag(n_of_integer_keys_in_bb,X) :-
	'$do_error'(domain_error(flag_value,n_of_integer_keys_in_bb+X),yap_flag(n_of_integer_keys_in_bb,X)).

yap_flag(profiling,X) :- (var(X); X = on; X = off), !,
	'$is_profiled'(X).

yap_flag(strict_iso,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(9,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(strict_iso,on) :- !,
	yap_flag(language,iso),
	'$transl_to_on_off'(X,on),
	'$set_yap_flags'(9,X).
yap_flag(strict_iso,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(9,X).
yap_flag(strict_iso,X) :-
	'$do_error'(domain_error(flag_value,strict_iso+X),yap_flag(strict_iso,X)).

yap_flag(variable_names_may_end_with_quotes,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(21,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(variable_names_may_end_with_quotes,on) :- !,
	'$transl_to_on_off'(X,on),
	'$set_yap_flags'(21,X).
yap_flag(variable_names_may_end_with_quotes,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(21,X).
yap_flag(variable_names_may_end_with_quotes,X) :-
	'$do_error'(domain_error(flag_value,strict_iso+X),yap_flag(strict_iso,X)).

yap_flag(language,X) :-
	var(X), !,
	'$access_yap_flags'(8, X1),
	'$trans_to_lang_flag'(X1,X).
yap_flag(language,X) :-
	'$trans_to_lang_flag'(N,X), !,
	'$set_yap_flags'(8,N),
	'$adjust_language'(X).
yap_flag(language,X) :-
	'$do_error'(domain_error(flag_value,language+X),yap_flag(language,X)).

yap_flag(discontiguous_warnings,X) :-
	var(X), !,
	'$syntax_check_discontiguous'(on,_).
yap_flag(discontiguous_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X == on -> 
	    style_check(discontiguous)
	;
	    style_check(-discontiguous)
	).
yap_flag(discontiguous_warnings,X) :-
	'$do_error'(domain_error(flag_value,discontiguous_warnings+X),yap_flag(discontiguous_warnings,X)).

yap_flag(redefine_warnings,X) :-
	var(X), !,
	'$syntax_check_multiple'(X,X).
yap_flag(redefine_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X == on -> 
	    style_check(multiple)
	;
	    style_check(-multiple)
	).
yap_flag(redefine_warnings,X) :-
	'$do_error'(domain_error(flag_value,redefine_warnings+X),yap_flag(redefine_warnings,X)).

yap_flag(chr_toplevel_show_store,X) :-
	var(X), !,
	'$nb_getval'('$chr_toplevel_show_store', X, fail).
yap_flag(chr_toplevel_show_store,X) :-
	(X = true ; X = false), !,
	nb_setval('$chr_toplevel_show_store',X).
yap_flag(chr_toplevel_show_store,X) :-
	'$do_error'(domain_error(flag_value,chr_toplevel_show_store+X),yap_flag(chr_toplevel_show_store,X)).

yap_flag(source,X) :-
	var(X), !,
	source_mode( X, X ).
yap_flag(source,X) :-
	(X == off -> true ; X == on), !,
	source_mode( _, X ).
yap_flag(chr_toplevel_show_store,X) :-
	'$do_error'(domain_error(flag_value,source+X),yap_flag(source,X)).

yap_flag(open_expands_filename,Expand) :-
	var(Expand), !,
	'$default_expand'(Expand).
yap_flag(open_expands_filename,Expand) :-
	'$set_default_expand'(Expand).

yap_flag(single_var_warnings,X) :-
	var(X), !,
	'$syntax_check_single_var'(X,X).
yap_flag(single_var_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X == on ->
	     style_check(single_var)
	;
	     style_check(-single_var)
	).
yap_flag(single_var_warnings,X) :-
	'$do_error'(domain_error(flag_value,single_var_warnings+X),yap_flag(single_var_warnings,X)).

yap_flag(system_options,X) :-
	'$system_options'(X).

'$system_options'(big_numbers) :-
	'$has_bignums'.
'$system_options'(coroutining) :-
	'$yap_has_coroutining'.
'$system_options'(depth_limit) :-
	\+ '$undefined'(get_depth_limit(_), prolog).
'$system_options'(low_level_tracer) :-
	\+ '$undefined'(start_low_level_trace, prolog).
'$system_options'(or_parallelism) :-
	\+ '$undefined'('$c_yapor_start', prolog).
'$system_options'(rational_trees) :-
	'$yap_has_rational_trees'.
'$system_options'(readline) :-
	'$swi_current_prolog_flag'(readline, true).
'$system_options'(tabling) :-
	\+ '$undefined'('$c_table'(_,_,_), prolog).
'$system_options'(threads) :-
	\+ '$undefined'('$thread_join'(_), prolog).
'$system_options'(wam_profiler) :-
	\+ '$undefined'(reset_op_counters, prolog).
	
yap_flag(update_semantics,X) :-
	var(X), !,
	( '$log_upd'(I) -> '$convert_upd_sem'(I,X) ).
yap_flag(update_semantics,logical) :- !,
	'$switch_log_upd'(1).
yap_flag(update_semantics,logical_assert) :- !,
	'$switch_log_upd'(2).
yap_flag(update_semantics,immediate) :- !,
	'$switch_log_upd'(0).
yap_flag(update_semantics,X) :-
	'$do_error'(domain_error(flag_value,update_semantics+X),yap_flag(update_semantics,X)).

yap_flag(toplevel_hook,G) :-
	var(G), !,
	( recorded('$toplevel_hooks',G,_) -> G ; G = fail ).
yap_flag(toplevel_hook,G) :- !,
	'$set_toplevel_hook'(G).

yap_flag(shared_object_search_path,P) :-
	'$ld_path'(P).

yap_flag(typein_module,X) :-
	var(X), !,
	'$current_module'(X).
yap_flag(typein_module,X) :-
	module(X).

yap_flag(write_strings,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(13,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(write_strings,on) :- !,
	'$transl_to_on_off'(X,on),
	'$set_yap_flags'(13,X).
yap_flag(write_strings,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(13,X).
yap_flag(write_strings,X) :-
	'$do_error'(domain_error(flag_value,write_strings+X),yap_flag(write_strings,X)).

yap_flag(prompt_alternatives_on,OUT) :-
	var(OUT), !,
	'$prompt_alternatives_on'(OUT).
yap_flag(prompt_alternatives_on,determinism) :- !,
	'$purge_clauses'('$prompt_alternatives_on'(_),prolog),
	'$compile'('$prompt_alternatives_on'(determinism),0,'$prompt_alternatives_on'(determinism),prolog).
yap_flag(prompt_alternatives_on,groundness) :- !,
	'$purge_clauses'('$prompt_alternatives_on'(_),prolog),
	'$compile'('$prompt_alternatives_on'(groundness),0,'$prompt_alternatives_on'(groundness),prolog).
yap_flag(prompt_alternatives_on,X) :-
	'$do_error'(domain_error(flag_value,prompt_alternatives_on+X),yap_flag(prompt_alternatives_on,X)).

yap_flag(stack_dump_on_error,OUT) :-
	var(OUT), !,
	'$access_yap_flags'(17,X),
	'$transl_to_on_off'(X,OUT).
yap_flag(stack_dump_on_error,on) :- !,
'$transl_to_on_off'(X,on),
	'$set_yap_flags'(17,X).
yap_flag(stack_dump_on_error,off) :- !,
	'$transl_to_on_off'(X,off),
	'$set_yap_flags'(17,X).
yap_flag(stack_dump_on_error,X) :-
	'$do_error'(domain_error(flag_value,stack_dump_on_error+X),yap_flag(stack_dump_on_error,X)).

yap_flag(user_input,OUT) :-
	var(OUT), !,
	stream_property(OUT,alias(user_input)).
yap_flag(user_input,Stream) :-
	set_stream(Stream, alias(user_input)).

yap_flag(user_output,OUT) :-
	var(OUT), !,
	stream_property(OUT,alias(user_output)).
yap_flag(user_output,Stream) :-
	set_stream(Stream, alias(user_output)).

yap_flag(user_error,OUT) :-
	var(OUT), !,
	stream_property(OUT,alias(user_error)).
yap_flag(user_error,Stream) :-
	set_stream(Stream, alias(user_error)).

yap_flag(debugger_print_options,OUT) :-
	var(OUT),
	recorded('$print_options','$debugger'(OUT),_), !.
yap_flag(debugger_print_options,Opts) :-
	recorda('$print_options','$debugger'(Opts),_).

:- recorda('$print_options','$debugger'([quoted(true),numbervars(true),portrayed(true),max_depth(10)]),_).

yap_flag(toplevel_print_options,OUT) :-
	var(OUT),
	recorded('$print_options','$toplevel'(OUT),_), !.
yap_flag(toplevel_print_options,Opts) :-
	recorda('$print_options','$toplevel'(Opts),_).

:- recorda('$print_options','$toplevel'([quoted(true),numbervars(true),portrayed(true)]),_).

yap_flag(host_type,X) :-
	'$host_type'(X).

yap_flag(float_format,X) :-
	var(X), !,
	'$float_format'(X).
yap_flag(float_format,X) :-
	atom(X), !,
	'$float_format'(X).
yap_flag(float_format,X) :-
	'$do_error'(type_error(atom,X),yap_flag(float_format,X)).

yap_flag(max_workers,X) :-
	var(X), !,
	'$max_workers'(X).
yap_flag(max_workers,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_workers),yap_flag(max_workers,X)).
yap_flag(max_workers,X) :-
	'$do_error'(domain_error(flag_value,max_workers+X),yap_flag(max_workers,X)).

yap_flag(max_threads,X) :-
	var(X), !,
	'$max_threads'(X).
yap_flag(max_threads,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_threads),yap_flag(max_threads,X)).
yap_flag(max_threads,X) :-
	'$do_error'(domain_error(flag_value,max_threads+X),yap_flag(max_threads,X)).

'$yap_system_flag'(agc_margin).
'$yap_system_flag'(answer_format).
'$yap_system_flag'(argv).
'$yap_system_flag'(char_conversion).
'$yap_system_flag'(chr_toplevel_show_store).
'$yap_system_flag'(debugger_print_options).
'$yap_system_flag'(discontiguous_warnings).
'$yap_system_flag'(dollar_as_lower_case).
'$yap_system_flag'(double_quotes).
%		V = fast  ;
% '$yap_system_flag'(file_name_variables).
% '$yap_system_flag'(fileerrors ).
'$yap_system_flag'(float_format).
%		V = float_mantissa_digits ;
%		V = float_epsilon ;
%		V = float_min_exponent ;
%		V = float_max_exponent ;
'$yap_system_flag'(gc   ).
'$yap_system_flag'(gc_margin   ).
'$yap_system_flag'(gc_trace    ).
%	    V = hide  ;
'$yap_system_flag'(host_type ).
'$yap_system_flag'(index).
'$yap_system_flag'(index_sub_term_search_depth).
'$yap_system_flag'(tabling_mode).
'$yap_system_flag'(informational_messages).
'$yap_system_flag'(language).
'$yap_system_flag'(max_workers).
'$yap_system_flag'(max_threads).
'$yap_system_flag'(n_of_integer_keys_in_db).
'$yap_system_flag'(open_expands_filename).
'$yap_system_flag'(profiling).
'$yap_system_flag'(prompt_alternatives_on).
'$yap_system_flag'(redefine_warnings).
'$yap_system_flag'(shared_object_search_path).
'$yap_system_flag'(single_var_warnings).
'$yap_system_flag'(source).
'$yap_system_flag'(stack_dump_on_error).
'$yap_system_flag'(strict_iso).
'$yap_system_flag'(syntax_errors).
'$yap_system_flag'(system_options).
'$yap_system_flag'(toplevel_hook).
'$yap_system_flag'(toplevel_print_options).
'$yap_system_flag'(typein_module).
'$yap_system_flag'(update_semantics).
'$yap_system_flag'(user_error).
'$yap_system_flag'(user_input).
'$yap_system_flag'(user_output).
'$yap_system_flag'(variable_names_may_end_with_quotes).
'$yap_system_flag'(version).
'$yap_system_flag'(write_strings).

'$show_yap_flag_opts'(V,Out) :-
	'$yap_system_flag'(V),
	yap_flag(V, Out).

'$trans_to_lang_flag'(0,cprolog).
'$trans_to_lang_flag'(1,iso).
'$trans_to_lang_flag'(2,sicstus).

'$adjust_language'(cprolog) :-
%	'$switch_log_upd'(0),
	'$syntax_check_mode'(_,off),
	'$syntax_check_single_var'(_,off),
	'$syntax_check_discontiguous'(_,off),
	'$syntax_check_multiple'(_,off),
	'$swi_set_prolog_flag'(character_escapes, false), % disable character escapes.
	'$set_yap_flags'(14,1),
	'$set_fpu_exceptions',
	unknown(_,fail).
'$adjust_language'(sicstus) :-
	'$switch_log_upd'(1),
	leash(full),
	'$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on),
	'$syntax_check_discontiguous'(_,on),
	'$syntax_check_multiple'(_,on),
	'$transl_to_on_off'(X1,on),
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$swi_set_prolog_flag'(character_escapes, true), % disable character escapes.
	'$set_fpu_exceptions',
	'$swi_set_prolog_flag'(fileerrors, true),
	unknown(_,error).
'$adjust_language'(iso) :-
	'$switch_log_upd'(1),
	'$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on),
	'$syntax_check_discontiguous'(_,on),
	'$syntax_check_multiple'(_,on),
	fileerrors,
	'$transl_to_on_off'(X1,on),
	% CHAR_CONVERSION
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	% ALLOW_ASSERTING_STATIC
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$swi_set_prolog_flag'(character_escapes, true), % disable character escapes.
	'$set_fpu_exceptions',
	unknown(_,error).


'$convert_upd_sem'(0,immediate).
'$convert_upd_sem'(1,logical).
'$convert_upd_sem'(2,logical_assert).

'$transl_to_true_false'(0,false).
'$transl_to_true_false'(1,true).

'$transl_to_on_off'(0,off).
'$transl_to_on_off'(1,on).

'$transl_to_rounding_function'(0,toward_zero).
'$transl_to_rounding_function'(1,down).

'$yap_flag_show_gc_tracing'(true, _, _, on) :- !.
'$yap_flag_show_gc_tracing'(_, true, _, verbose) :- !.
'$yap_flag_show_gc_tracing'(_, _, on, very_verbose) :- !.
'$yap_flag_show_gc_tracing'(_, _, _, off).

'$flag_check_alias'(OUT, Alias) :-
	stream_property(OUT,alias(Alias)), !.
	
current_prolog_flag(V,Out) :-
	var(V), !,
	'$yap_flag'(V,NOut),
	NOut = Out.
current_prolog_flag(V,Out) :-
	atom(V), !,
	yap_flag(V,NOut),
	NOut = Out.
current_prolog_flag(M:V,Out) :-
	current_module(M), atom(V), !,
	yap_flag(M:V,NOut),
	NOut = Out.
current_prolog_flag(V,Out) :-
	'$do_error'(type_error(atom,V),current_prolog_flag(V,Out)).

set_prolog_flag(F,V) :-
	var(F), !,
	'$do_error'(instantiation_error,set_prolog_flag(F,V)).
set_prolog_flag(F,V) :-
	var(V), !,
	'$do_error'(instantiation_error,set_prolog_flag(F,V)).
set_prolog_flag(M:V,Out) :-
	current_module(M), atom(V), !,
	'$swi_set_prolog_flag'(M:V,Out).
set_prolog_flag(F,V) :-
	\+ atom(F), !,
	'$do_error'(type_error(atom,F),set_prolog_flag(F,V)).
set_prolog_flag(F, Val) :-
	'$swi_current_prolog_flag'(F, _), !,
	'$swi_set_prolog_flag'(F, Val).
set_prolog_flag(F,V) :-
	'$yap_system_flag'(F), !,
	yap_flag(F,V).
set_prolog_flag(F,V) :-
	'$swi_current_prolog_flag'(user_flags, UFlag),
	(
	 UFlag = silent ->
	 create_prolog_flag(F, V, [])
	;
	 UFlag = warning ->
	 print_message(warning,existence_error(prolog_flag, F)),
	 create_prolog_flag(F, V, [])
	;
	 UFlag = error ->
	 '$do_error'(existence_error(prolog_flag, F),set_prolog_flag(F,V))
	).

prolog_flag(F, Old, New) :-
	var(F), !,
	'$do_error'(instantiation_error,prolog_flag(F,Old,New)).
prolog_flag(F, Old, New) :-
	current_prolog_flag(F, Old),
	set_prolog_flag(F, New).

prolog_flag(F, Old) :-
	current_prolog_flag(F, Old).

% if source_mode is on, then the source for the predicates
% is stored with the code
source_mode(Old,New) :-
	'$access_yap_flags'(11,X),
	'$transl_to_on_off'(X,Old),
	'$transl_to_on_off'(XN,New),
	'$set_yap_flags'(11,XN).

source :- '$set_yap_flags'(11,1).
no_source :- '$set_yap_flags'(11,0).

create_prolog_flag(Name, Value, Options) :-
	'$check_flag_name'(Name, create_prolog_flag(Name, Value, Options)),
	'$check_flag_options'(Options, Domain, RW, create_prolog_flag(Name, Value, Options)),
	'$check_flag_value'(Value, Domain, create_prolog_flag(Name, Value, Options)),
	retractall(prolog:'$user_defined_flag'(Name,_,_,_)),
	assert(prolog:'$user_defined_flag'(Name,Domain,RW,Value)).

'$check_flag_name'(V, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_name'(Name, _) :-
	atom(Name), !.
'$check_flag_name'(Name, G) :-
	'$do_error'(type_error(atom),G).
	
'$check_flag_options'(O, _, _, G) :-
	var(O),
	'$do_error'(instantiation_error,G).
'$check_flag_options'([], _, read_write, _) :- !.
'$check_flag_options'([O1|Os], Domain, RW, G) :- !,
	'$check_flag_optionsl'([O1|Os], Domain, RW, G).
'$check_flag_options'(O, _, _, G) :-
	'$do_error'(type_error(list),G).


'$check_flag_optionsl'([], _, read_write, G).
'$check_flag_optionsl'([V|Os], Domain, RW, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_optionsl'([type(Type)|Os], Domain, RW, G) :- !,
	'$check_flag_type'(Type, Domain, G),
	'$check_flag_optionsl'(Os, _, RW, G).
'$check_flag_optionsl'([access(Access)|Os], Domain, RW, G) :- !,
	'$check_flag_access'(Access, RW, G),
	'$check_flag_optionsl'(Os, Domain, _, G).
'$check_flag_optionsl'(Os, Domain, RW, G) :-
	'$do_error'(domain_error(create_prolog_flag,Os),G).

'$check_flag_type'(V, _, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_type'(boolean, boolean, _) :- !.
'$check_flag_type'(integer, integer, _) :- !.
'$check_flag_type'(float, float, _) :- !.
'$check_flag_type'(atom, atom, _) :- !.
'$check_flag_type'(term, term, _) :- !.
'$check_flag_type'(Atom, _, G) :-
	'$do_error'(domain_error(create_prolog_flag_option(type),Atom),G).

'$check_flag_access'(V, _, G) :-
	var(V),
	'$do_error'(instantiation_error,G).
'$check_flag_access'(read_write, read_write, _) :- !.
'$check_flag_access'(read_only, read_only, _) :- !.
'$check_flag_type'(Atom, _, G) :-
	'$do_error'(domain_error(create_prolog_flag_option(access),Atom),G).

'$user_flag_value'(F, Val) :-
	var(Val), !,
	'$user_defined_flag'(F,_,_,Val).
'$user_flag_value'(F, Val) :-
	atomic(Val), !,
	prolog:'$user_defined_flag'(F,Domain,RW,V0),
	(
	 Val == V0
	->
	 true
	;
	 RW = read_only
	->
	 '$do_error'(permission_error(modify,flag,F),yap_flag(F,Val))
	;
	  '$check_flag_value'(Val, Domain, yap_flag(F,Val)),
	  retractall(prolog:'$user_defined_flag'(F,_,_,_)),
	  assert(prolog:'$user_defined_flag'(F,Domain,RW,Val))
	).
'$user_flag_value'(F, Val) :-
	'$do_error'(type_error(atomic,Val),yap_flag(F,Val)).

'$check_flag_value'(Value, _, G) :-
	\+ ground(Value), !,
	'$do_error'(instantiation_error,G).
'$check_flag_value'(Value, Domain, G) :-
	var(Domain), !,
	'$flag_domain_from_value'(Value, Domain).
'$check_flag_value'(_, term, _) :- !.
'$check_flag_value'(Value, atom, _) :-
	atom(Value), !.
'$check_flag_value'(Value, integer, _) :-
	integer(Value), !.
'$check_flag_value'(Value, float, _) :-
	float(Value), !.
'$check_flag_value'(true, boolean, _) :- !.
'$check_flag_value'(false, boolean, _) :- !.
'$check_flag_value'(Value, Domain, G) :-
	'$do_error'(domain_error(Domain,Value),G).

'$flag_domain_from_value'(true, boolean) :- !.
'$flag_domain_from_value'(false, boolean) :- !.
'$flag_domain_from_value'(Value, integer) :- integer(Value), !.
'$flag_domain_from_value'(Value, float) :- float(Value), !.
'$flag_domain_from_value'(Value, atom) :- atom(Value), !.
'$flag_domain_from_value'(_, term).


