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
* File:		directives.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	directing system execution				 *
*									 *
*************************************************************************/

'$all_directives'(_:G1) :- !,
	'$all_directives'(G1).
'$all_directives'((G1,G2)) :- !,
	'$all_directives'(G1),
	'$all_directives'(G2).
'$all_directives'(G) :- !,
	'$directive'(G).

'$directive'(block(_)).
'$directive'(char_conversion(_,_)).
'$directive'(compile(_)).
'$directive'(consult(_)).
'$directive'(discontiguous(_)).
'$directive'(dynamic(_)).
'$directive'(elif(_)).
'$directive'(else).
'$directive'(encoding(_)).
'$directive'(endif).
'$directive'(ensure_loaded(_)).
'$directive'(expects_dialect(_)).
'$directive'(if(_)).
'$directive'(include(_)).
'$directive'(initialization(_)).
'$directive'(meta_predicate(_)).
'$directive'(module(_,_)).
'$directive'(module(_,_,_)).
'$directive'(module_transparent(_)).
'$directive'(multifile(_)).
'$directive'(noprofile(_)).
'$directive'(parallel).
'$directive'(public(_)).
'$directive'(op(_,_,_)).
'$directive'(set_prolog_flag(_,_)).
'$directive'(reconsult(_)).
'$directive'(reexport(_)).
'$directive'(reexport(_,_)).
'$directive'(sequential).
'$directive'(sequential(_)).
'$directive'(thread_initialization(_)).
'$directive'(thread_local(_)).
'$directive'(uncutable(_)).
'$directive'(use_module(_)).
'$directive'(use_module(_,_)).
'$directive'(use_module(_,_,_)).
'$directive'(wait(_)).

'$exec_directives'((G1,G2), Mode, M) :- !,
	'$exec_directives'(G1, Mode, M),
	'$exec_directives'(G2, Mode, M).
'$exec_directives'(G, Mode, M) :-
	'$exec_directive'(G, Mode, M).

'$exec_directive'(multifile(D), _, M) :-
	'$system_catch'('$multifile'(D, M), M,
	      Error,
	      user:'$LoopError'(Error, top)).
'$exec_directive'(discontiguous(D), _, M) :-
	'$discontiguous'(D,M).
'$exec_directive'(initialization(D), _, M) :-
	'$initialization'(M:D).
'$exec_directive'(thread_initialization(D), _, M) :-
	'$thread_initialization'(M:D).
'$exec_directive'(expects_dialect(D), _, _) :-
	'$expects_dialect'(D).
'$exec_directive'(encoding(Enc), _, _) :-
        '$set_encoding'(Enc).
'$exec_directive'(parallel, _, _) :-
	'$parallel'.
'$exec_directive'(sequential, _, _) :-
	'$sequential'.
'$exec_directive'(sequential(G), _, M) :-
	'$sequential_directive'(G, M).
'$exec_directive'(parallel(G), _, M) :-
	'$parallel_directive'(G, M).
'$exec_directive'(include(F), Status, _) :-
	'$include'(F, Status).
'$exec_directive'(module(N,P), Status, _) :-
	'$module'(Status,N,P).
'$exec_directive'(module(N,P,Op), Status, _) :-
	'$module'(Status,N,P,Op).
'$exec_directive'(meta_predicate(P), _, M) :-
	'$meta_predicate'(P, M).
'$exec_directive'(module_transparent(P), _, M) :-
	'$module_transparent'(P, M).
'$exec_directive'(noprofile(P), _, M) :-
	'$noprofile'(P, M).
'$exec_directive'(dynamic(P), _, M) :-
	'$dynamic'(P, M).
'$exec_directive'(thread_local(P), _, M) :-
	'$thread_local'(P, M).
'$exec_directive'(op(P,OPSEC,OP), _, _) :-
	'$current_module'(M),
	op(P,OPSEC,M:OP).
'$exec_directive'(set_prolog_flag(F,V), _, _) :-
	set_prolog_flag(F,V).
'$exec_directive'(ensure_loaded(Fs), _, M) :-
	'$load_files'(M:Fs, [if(changed)], ensure_loaded(Fs)).
'$exec_directive'(char_conversion(IN,OUT), _, _) :-
	char_conversion(IN,OUT).
'$exec_directive'(public(P), _, M) :-
	'$public'(P, M).
'$exec_directive'(compile(Fs), _, M) :-
	'$load_files'(M:Fs, [], compile(Fs)).
'$exec_directive'(reconsult(Fs), _, M) :-
	'$load_files'(M:Fs, [], reconsult(Fs)).
'$exec_directive'(consult(Fs), _, M) :-
	'$consult'(Fs, M).
'$exec_directive'(use_module(F), _, M) :-
	'$load_files'(M:F, [if(not_loaded)],use_module(F)).
'$exec_directive'(reexport(F), _, M) :-
	'$reexport'(F, all, M).
'$exec_directive'(reexport(F,Spec), _, M) :-
	'$reexport'(F, Spec, M).
'$exec_directive'(use_module(F,Is), _, M) :-
	'$load_files'(M:F, [if(not_loaded),imports(Is)],use_module(F,Is)).
'$exec_directive'(use_module(Mod,F,Is), _, _) :-
	'$use_module'(Mod,F,Is).
'$exec_directive'(block(BlockSpec), _, _) :-
	'$block'(BlockSpec).
'$exec_directive'(wait(BlockSpec), _, _) :-
	'$wait'(BlockSpec).
'$exec_directive'(table(PredSpec), _, M) :-
	'$table'(PredSpec, M).
'$exec_directive'(uncutable(PredSpec), _, M) :-
	'$uncutable'(PredSpec, M).
'$exec_directive'(if(Goal), Context, M) :-
	'$if'(M:Goal, Context).
'$exec_directive'(else, Context, _) :-
	'$else'(Context).
'$exec_directive'(elif(Goal), Context, M) :-
	'$elif'(M:Goal, Context).
'$exec_directive'(endif, Context, _) :-
	'$endif'(Context).

%                                                                                  
% allow users to define their own directives.                                      
%                                                                                  
user_defined_directive(Dir,_) :-
        '$directive'(Dir), !.
user_defined_directive(Dir,Action) :-
        functor(Dir,Na,Ar),
        functor(NDir,Na,Ar),
        '$current_module'(M, prolog),
	assert_static('$directive'(NDir)),
	assert_static(('$exec_directive'(Dir, _, _) :- Action)),
        '$current_module'(_, M).


yap_flag(V,Out) :-
	'$user_defined_flag'(V,_,_,_),
	(nonvar(V) ->
	 !
	;
	 true
	),
	'$user_flag_value'(V, Out).

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

% hide/unhide atoms
yap_flag(encoding,DefaultEncoding) :- var(DefaultEncoding), !,
	'$default_encoding'(DefCode),
	'$valid_encoding'(DefaultEncoding, DefCode).
yap_flag(encoding,Encoding) :-
	'$valid_encoding'(Encoding, EncCode), !,
	'$default_encoding'(EncCode).
yap_flag(encoding,Encoding) :-
	'$do_error'(domain_error(io_mode,encoding(Encoding)),yap_flag(encoding,Encoding)).

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

%
% SWI compatibility flag
%
yap_flag(generate_debug_info,X) :-
	var(X), !,
        '$access_yap_flags'(18,Options),
	(Options =:= 0 -> X = false ; X = true ).
yap_flag(generate_debug_info,true) :- !,
	'$enable_restore_flag_info'(generate_debug_info),
	'$set_yap_flags'(18,1),
	source.
yap_flag(generate_debug_info,false) :- !,
	'$enable_restore_flag_info'(generate_debug_info),
	'$set_yap_flags'(18,0),
	no_source.
yap_flag(generate_debug_info,X) :-
	'$do_error'(domain_error(flag_value,generate_debug_info+X),yap_flag(generate_debug_info,X)).

'$enable_restore_flag_info'(_) :-
	nb_getval('$consulting_file',[]), !.
'$enable_restore_flag_info'(_) :-
	nb_getval('$initialization_goals',on), !.
'$enable_restore_flag_info'(Flag) :-
	'$show_consult_level'(Level1),
	yap_flag(Flag, Info),
	% it will be done after we leave the current consult level.
	Level is Level1-1,
	recorda('$initialisation',do(Level,yap_flag(Flag,Info)),_),
	fail.
'$enable_restore_flag_info'(_).

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

yap_flag(bounded,X) :-
	var(X), !,
	'$access_yap_flags'(0, X1),
	'$transl_to_true_false'(X1,X).
yap_flag(bounded,X) :- !,
	(X = true ; X = false), !,
	'$do_error'(permission_error(modify,flag,bounded),yap_flag(bounded,X)).
yap_flag(bounded,X) :-
	'$do_error'(domain_error(flag_value,bounded+X),yap_flag(bounded,X)).

% do or do not indexation
yap_flag(index,X) :- var(X),
	'$access_yap_flags'(19, X1),
	'$transl_to_index_mode'(X1,X), !.
yap_flag(index,X)  :-
	'$transl_to_index_mode'(X1,X), !,
	'$set_yap_flags'(19,X1).
yap_flag(index,X) :-
	'$do_error'(domain_error(flag_value,index+X),yap_flag(index,X)).

yap_flag(home,X) :-
	'$yap_home'(X).

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
yap_flag(tabling_mode,[]) :- !.
yap_flag(tabling_mode,[HOption|TOption]) :- !,
   yap_flag(tabling_mode,HOption),
   yap_flag(tabling_mode,TOption).
yap_flag(tabling_mode,(Option1,Option2)) :- !,
   yap_flag(tabling_mode,Option1),
   yap_flag(tabling_mode,Option2).
yap_flag(tabling_mode,Option) :-
   '$transl_to_tabling_mode'(Flag,Option),
   '$set_yap_flags'(20,Flag).
yap_flag(tabling_mode,Options) :-
   '$do_error'(domain_error(flag_value,tabling_mode+Options),yap_flag(tabling_mode,Options)).

% should match with code in stdpreds.c
'$transl_to_tabling_mode'(0,default).
'$transl_to_tabling_mode'(1,batched).
'$transl_to_tabling_mode'(2,local).
'$transl_to_tabling_mode'(3,exec_answers).
'$transl_to_tabling_mode'(4,load_answers).

yap_flag(informational_messages,X) :- var(X), !,
	 get_value('$verbose',X).
yap_flag(informational_messages,on)  :- !,
	set_value('$verbose',on),
	'$set_yap_flags'(22,0).
yap_flag(informational_messages,off) :- !,
	set_value('$verbose',off),
	'$set_yap_flags'(22,1).
yap_flag(informational_messages,X) :-
	'$do_error'(domain_error(flag_value,informational_messages+X),yap_flag(informational_messages,X)).

yap_flag(verbose,X) :- var(X), !,
	 get_value('$verbose',X0),
	 (X0 == on -> X = normal ; X = silent).
yap_flag(verbose,normal)  :- !,
	set_value('$verbose',on),
	'$set_yap_flags'(22,0).
yap_flag(verbose,silent) :- !,
	set_value('$verbose',off),
	'$set_yap_flags'(22,1).
yap_flag(verbose,X) :-
	'$do_error'(domain_error(flag_value,verbose+X),yap_flag(verbose,X)).

yap_flag(integer_rounding_function,X) :-
	var(X), !,
	'$access_yap_flags'(2, X1),
	'$transl_to_rounding_function'(X1,X).
yap_flag(integer_rounding_function,X) :-
	(X = down; X = toward_zero), !,
	'$do_error'(permission_error(modify,flag,integer_rounding_function),yap_flag(integer_rounding_function,X)).
yap_flag(integer_rounding_function,X) :-
	'$do_error'(domain_error(flag_value,integer_rounding_function+X),yap_flag(integer_rounding_function,X)).

yap_flag(max_arity,X) :-
	var(X), !,
	'$access_yap_flags'(1, X1),
	'$transl_to_arity'(X1,X).
yap_flag(max_arity,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_arity),yap_flag(max_arity,X)).
yap_flag(max_arity,X) :-
	'$do_error'(domain_error(flag_value,max_arity+X),yap_flag(max_arity,X)).

yap_flag(version,X) :-
	var(X), !,
	get_value('$version_name',X).
yap_flag(version,X) :-
	'$do_error'(permission_error(modify,flag,version),yap_flag(version,X)).

yap_flag(version_data,X) :-
	var(X), !,
	'$get_version_codes'(Major,Minor,Patch),
	X = yap(Major, Minor, Patch, _).
yap_flag(version_data,X) :-
	'$do_error'(permission_error(modify,flag,version),yap_flag(version_data,X)).

'$get_version_codes'(Major,Minor,Patch) :-
	get_value('$version_name',X),
	atom_codes(X,VersionTag), %'
	'$fetch_num_code'(VersionTag,0,Major,L1),
	'$fetch_num_code'(L1,0,Minor,L2),
	'$fetch_num_code'(L2,0,Patch,[]).

'$fetch_num_code'([],Code,Code,[]).
'$fetch_num_code'([C|Cs],Code0,CodeF,L) :-
        C >= 0'0, C =< 0'9, !,
	CodeI is Code0*10+(C-0'0), %'
	'$fetch_num_code'(Cs,CodeI,CodeF,L).
'$fetch_num_code'([_|Cs],Code,Code,Cs).

yap_flag(max_integer,X) :-
	var(X), !,
	'$access_yap_flags'(0, 1),
	'$access_yap_flags'(3, X).
yap_flag(max_integer,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_integer),yap_flag(max_integer,X)).
yap_flag(max_integer,X) :-
	'$do_error'(domain_error(flag_value,max_integer+X),yap_flag(max_integer,X)).

yap_flag(max_tagged_integer,X) :-
	'$max_tagged_integer'(X), !.
yap_flag(max_tagged_integer,X) :-			       
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,max_tagged_integer),yap_flag(max_tagged_integer,X)).
yap_flag(max_tagged_integer,X) :-
	'$do_error'(domain_error(flag_value,max_tagged_integer+X),yap_flag(max_tagged_integer,X)).

yap_flag(min_integer,X) :-
	var(X), !,
	'$access_yap_flags'(0, 1),
	'$access_yap_flags'(4, X).
yap_flag(min_integer,X) :-
	integer(X), X < 0, !,
	'$do_error'(permission_error(modify,flag,min_integer),yap_flag(min_integer,X)).
yap_flag(min_integer,X) :-
	'$do_error'(domain_error(flag_value,min_integer+X),yap_flag(min_integer,X)).

yap_flag(min_tagged_integer,X) :-
	'$min_tagged_integer'( X), !.
yap_flag(min_tagged_integer,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,min_tagged_integer),yap_flag(min_tagged_integer,X)).
yap_flag(min_tagged_integer,X) :-
	'$do_error'(domain_error(flag_value,min_tagged_integer+X),yap_flag(min_tagged_integer,X)).

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
	'$do_error'(permission_error(modify,flag,float_min_exponent),yap_flag(ﬂoat_min_exponent,X)).
yap_flag(float_epsilon,X) :-
	'$do_error'(domain_error(flag_value,float_min_exponent+X),yap_flag(ﬂoat_min_exponent,X)).

yap_flag(float_max_exponent,X) :-
	var(X), !,
	?????
yap_flag(float_max_exponent,X) :-
	integer(X), X > 0, !,
	'$do_error'(permission_error(modify,flag,float_max_exponent),yap_flag(ﬂoat_max_exponent,X)).
yap_flag(float_max_exponent,X) :-
	'$do_error'(domain_error(flag_value,float_max_exponent+X),yap_flag(ﬂoat_max_exponent,X)).
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

yap_flag(double_quotes,X) :-
	var(X), !,
	'$access_yap_flags'(6, X1),
	'$transl_to_trl_types'(X1,X).
yap_flag(double_quotes,X) :-
	'$transl_to_trl_types'(X1,X), !,
	'$set_yap_flags'(6,X1).
yap_flag(double_quotes,X) :-
	'$do_error'(domain_error(flag_value,double_quotes+X),yap_flag(double_quotes,X)).

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

yap_flag(debug,X) :-
	var(X), !,
	('$debug_on'(true)
	->
	 X = on
	;
	 X = true
	).
yap_flag(debug,X) :-
	'$transl_to_on_off'(_,X), !,
	(X = on -> debug ; nodebug).
yap_flag(debug,X) :-
	'$do_error'(domain_error(flag_value,debug+X),yap_flag(debug,X)).

yap_flag(discontiguous_warnings,X) :-
	var(X), !,
	('$syntax_check_mode'(on,_), '$syntax_check_discontiguous'(on,_) ->
	    X = on
	;
	    X = off
	).
yap_flag(discontiguous_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X = on -> 
	    '$syntax_check_mode'(_,on),
	    '$syntax_check_discontiguous'(_,on)
	;
	    '$syntax_check_discontiguous'(_,off)).
yap_flag(discontiguous_warnings,X) :-
	'$do_error'(domain_error(flag_value,discontiguous_warnings+X),yap_flag(discontiguous_warnings,X)).

yap_flag(redefine_warnings,X) :-
	var(X), !,
	('$syntax_check_mode'(on,_), '$syntax_check_multiple'(on,_) ->
	    X = on
	;
	    X = off
	).
yap_flag(redefine_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X = on -> 
	    '$syntax_check_mode'(_,on),
	    '$syntax_check_multiple'(_,on)
	;
	    '$syntax_check_multiple'(_,off)).
yap_flag(redefine_warnings,X) :-
	'$do_error'(domain_error(flag_value,redefine_warnings+X),yap_flag(redefine_warnings,X)).

yap_flag(chr_toplevel_show_store,X) :-
	var(X), !,
	nb_getval('$chr_toplevel_show_store',X).
yap_flag(chr_toplevel_show_store,X) :-
	(X = true ; X = false), !,
	nb_setval('$chr_toplevel_show_store',X).
yap_flag(chr_toplevel_show_store,X) :-
	'$do_error'(domain_error(flag_value,chr_toplevel_show_store+X),yap_flag(chr_toplevel_show_store,X)).

yap_flag(open_expands_filename,Expand) :-
	var(Expand), !,
	'$default_expand'(Expand).
yap_flag(open_expands_filename,Expand) :-
	'$set_default_expand'(Expand).

yap_flag(single_var_warnings,X) :-
	var(X), !,
	('$syntax_check_mode'(on,_), '$syntax_check_single_var'(on,_) ->
	    X = on
	;
	    X = off
	).
yap_flag(single_var_warnings,X) :-
	'$transl_to_on_off'(_,X), !,
	(X = on -> 
	    '$syntax_check_mode'(_,on),
	    '$syntax_check_single_var'(_,on)
	;
	    '$syntax_check_single_var'(_,off)).
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
	\+ '$undefined'('$yapor_on', prolog).
'$system_options'(rational_trees) :-
	'$yap_has_rational_trees'.
'$system_options'(readline) :-
	'$has_readline'.
'$system_options'(tabling) :-
	\+ '$undefined'('$c_table'(_,_), prolog).
'$system_options'(threads) :-
	\+ '$no_threads'.
'$system_options'(wam_profiler) :-
	\+ '$undefined'(reset_op_counters, prolog).
	
yap_flag(unknown,X) :-
	var(X), !,
	unknown(X,_).
yap_flag(unknown,N) :-
	unknown(_,N).

yap_flag(to_chars_mode,X) :-
	var(X), !,
	( '$access_yap_flags'(7,0) -> X = quintus ; X = iso ).
yap_flag(to_chars_mode,quintus) :- !,
	'$set_yap_flags'(7,0).
yap_flag(to_chars_mode,iso) :- !,
	'$set_yap_flags'(7,1).
yap_flag(to_chars_mode,X) :-
	'$do_error'(domain_error(flag_value,to_chars_mode+X),yap_flag(to_chars_mode,X)).

yap_flag(character_escapes,X) :-
	var(X), !,
	'$access_yap_flags'(12,Y),	
	'$transl_to_character_escape_modes'(Y,X).
yap_flag(character_escapes,X) :- !,
	'$transl_to_character_escape_modes'(Y,X), !,
	'$set_yap_flags'(12,Y).
yap_flag(character_escapes,X) :-
	'$do_error'(domain_error(flag_value,character_escapes+X),yap_flag(to_chars_mode,X)).

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

yap_flag(toplevel_hook,X) :-
	var(X), !,
	( recorded('$toplevel_hooks',G,_) -> G ; true ).
yap_flag(toplevel_hook,G) :- !,
	'$set_toplevel_hook'(G).

yap_flag(unix,true) :-
	'$unix', !.
yap_flag(unix,false).

yap_flag(windows,true) :-
	'$win32', !.
yap_flag(windows,false).

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
	'$flag_check_alias'(OUT, user_input).
	
yap_flag(user_input,Stream) :-
	'$change_alias_to_stream'(user_input,Stream).

yap_flag(user_output,OUT) :-
	var(OUT), !,
	'$flag_check_alias'(OUT, user_output).
yap_flag(user_output,Stream) :-
	'$change_alias_to_stream'(user_output,Stream).


yap_flag(user_error,OUT) :-
	var(OUT), !,
	'$flag_check_alias'(OUT, user_error).
yap_flag(user_error,Stream) :-
	'$change_alias_to_stream'(user_error,Stream).

yap_flag(debugger_print_options,OUT) :-
	var(OUT),
	recorded('$print_options','$debugger'(OUT),_), !.
yap_flag(debugger_print_options,Opts) :- !,
	'$check_io_opts'(Opts, yap_flag(debugger_print_options,Opts)),
	recorda('$print_options','$debugger'(Opts),_).

:- recorda('$print_options','$debugger'([quoted(true),numbervars(true),portrayed(true),max_depth(10)]),_).

yap_flag(toplevel_print_options,OUT) :-
	var(OUT),
	recorded('$print_options','$toplevel'(OUT),_), !.
yap_flag(toplevel_print_options,Opts) :- !,
	'$check_io_opts'(Opts, yap_flag(toplevel_print_options,Opts)),
	recorda('$print_options','$toplevel'(Opts),_).

yap_flag(fileerrors,OUT) :-
	var(OUT), !,
	get_value(fileerrors,X0),
	(X0 = [] -> X= 0 ; X = X0),
	'$transl_to_on_off'(X,OUT).
yap_flag(fileerrors,on) :- !,
	set_value(fileerrors,1).
yap_flag(fileerrors,off) :- !,
	set_value(fileerrors,0).
yap_flag(fileerrors,X) :-
	'$do_error'(domain_error(flag_value,fileerrors+X),yap_flag(fileerrors,X)).

:- recorda('$print_options','$toplevel'([quoted(true),numbervars(true),portrayed(true)]),_).

yap_flag(host_type,X) :-
	'$host_type'(X).

yap_flag(verbose_load,X) :-
	var(X), !,
	( get_value('$lf_verbose',silent) -> X = false ; X = true ).
yap_flag(verbose_load,true) :- !,
	set_value('$lf_verbose',informational).
yap_flag(verbose_load,false) :- !,
	set_value('$lf_verbose',silent),
	'$set_yap_flags'(7,1).
yap_flag(verbose_load,X) :-
	'$do_error'(domain_error(flag_value,verbose_auto_load+X),yap_flag(verbose_auto_load,X)).

yap_flag(verbose_auto_load,X) :-
	var(X), !,
	( get_value('$verbose_auto_load',true) -> X = true ; X = false ).
yap_flag(verbose_auto_load,true) :- !,
	set_value('$verbose_auto_load',true).
yap_flag(verbose_auto_load,false) :- !,
	set_value('$verbose_auto_load',false),
	'$set_yap_flags'(7,1).
yap_flag(verbose_auto_load,X) :-
	'$do_error'(domain_error(flag_value,verbose_auto_load+X),yap_flag(verbose_auto_load,X)).

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

yap_flag(dialect,yap).

'$show_yap_flag_opts'(V,Out) :-
	(
		V = answer_format ;
		V = argv ;
		V = bounded ;
		V = char_conversion ;
		V = character_escapes ;
		    V = chr_toplevel_show_store ;
		V = debug ;
		V = debugger_print_options ;
		V = dialect ;
		V = discontiguous_warnings ;
		V = dollar_as_lower_case ;
		V = double_quotes ;
		V = encoding ;
%		V = fast  ;
		V = fileerrors  ;
		V = float_format ;
%		V = float_mantissa_digits ;
%		V = float_epsilon ;
%		V = float_min_exponent ;
%		V = float_max_exponent ;
		V = gc    ;
		V = gc_margin    ;
		V = gc_trace     ;
		V = generate_debug_info     ;
%	    V = hide  ;
		V = home  ;
		V = host_type  ;
		V = index ;
		V = tabling_mode ;
		V = informational_messages ;
		V = integer_rounding_function ;
		V = language ;
		V = max_arity ;
		V = max_integer ;
		V = max_tagged_integer ;
		V = max_workers ;
		V = max_threads ;
		V = min_integer ;
		V = min_tagged_integer ;
		V = n_of_integer_keys_in_db ;
		V = open_expands_filename ;
		V = profiling ;
		V = redefine_warnings ;
		V = shared_object_search_path ;
		V = single_var_warnings ;
		V = stack_dump_on_error ;
		V = strict_iso ;
		V = syntax_errors ;
		V = system_options ;
		V = to_chars_mode ;
		V = toplevel_hook ;
		V = toplevel_print_options ;
		V = typein_module ;
		V = unix ;
		V = unknown ;
		V = update_semantics ;
		V = user_error ;
		V = user_input ;
		V = user_output ;
		V = variable_names_may_end_with_quotes ;
		V = verbose ;
		V = verbose_auto_load ;
		V = version ;
		V = version_data ;
		V = windows ;
		V = write_strings
	),
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
	'$set_yap_flags'(12,0), % disable character escapes.
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
	'$set_yap_flags'(12,1),
	'$set_fpu_exceptions',
	fileerrors,
	unknown(_,error).
'$adjust_language'(iso) :-
	'$switch_log_upd'(1),
	'$syntax_check_mode'(_,on),
	'$syntax_check_single_var'(_,on),
	'$syntax_check_discontiguous'(_,on),
	'$syntax_check_multiple'(_,on),
	% YAP_TO_CHARS
	'$set_yap_flags'(7,1),
	fileerrors,
	'$transl_to_on_off'(X1,on),
	% CHAR_CONVERSION
	'$set_yap_flags'(5,X1),
	'$force_char_conversion',
	% ALLOW_ASSERTING_STATIC
	'$set_yap_flags'(14,0),
	% CHARACTER_ESCAPE
	'$set_yap_flags'(12,1),
	'$set_fpu_exceptions',
	unknown(_,error).

'$transl_to_character_escape_modes'(0,off) :- !.
'$transl_to_character_escape_modes'(0,cprolog).
'$transl_to_character_escape_modes'(2,on) :- !.
'$transl_to_character_escape_modes'(1,iso).
'$transl_to_character_escape_modes'(2,sicstus).

'$convert_upd_sem'(0,immediate).
'$convert_upd_sem'(1,logical).
'$convert_upd_sem'(2,logical_assert).

'$transl_to_true_false'(0,false).
'$transl_to_true_false'(1,true).

'$transl_to_on_off'(0,off).
'$transl_to_on_off'(1,on).

'$transl_to_arity'(X1,X) :- X1 < 0, !, X = unbounded.
'$transl_to_arity'(X,X).

'$transl_to_rounding_function'(0,down).
'$transl_to_rounding_function'(1,toward_zero).

'$transl_to_trl_types'(0,chars).
'$transl_to_trl_types'(1,codes).
'$transl_to_trl_types'(2,atom).

'$yap_flag_show_gc_tracing'(true, _, _, on) :- !.
'$yap_flag_show_gc_tracing'(_, true, _, verbose) :- !.
'$yap_flag_show_gc_tracing'(_, _, on, very_verbose) :- !.
'$yap_flag_show_gc_tracing'(_, _, _, off).

'$flag_check_alias'(OUT, Alias) :-
	stream_property(OUT,[alias(Alias)]), !.
	
current_prolog_flag(V,Out) :-
	var(V), !,
	'$show_yap_flag_opts'(V,NOut),
	NOut = Out.
current_prolog_flag(V,Out) :-
	atom(V), !,
	yap_flag(V,NOut),
	NOut = Out.
current_prolog_flag(V,Out) :-
	'$do_error'(type_error(atom,V),current_prolog_flag(V,Out)).

set_prolog_flag(F,V) :-
	var(F), !,
	'$do_error'(instantiation_error,set_prolog_flag(F,V)).
set_prolog_flag(F,V) :-
	var(V), !,
	'$do_error'(instantiation_error,set_prolog_flag(F,V)).
set_prolog_flag(F, Val) :-
	prolog:'$user_defined_flag'(F,_,_,_), !,
	yap_flag(F, Val).
set_prolog_flag(F,V) :-
	\+ atom(F), !,
	'$do_error'(type_error(atom,F),set_prolog_flag(F,V)).
set_prolog_flag(F,V) :-
	yap_flag(F,V).

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
'$check_flag_options'([], term, read_write, _) :- !.
'$check_flag_options'([O1|Os], Domain, RW, G) :- !,
	'$check_flag_optionsl'([O1|Os], Domain, RW, G).
'$check_flag_options'(O, _, _, G) :-
	'$do_error'(type_error(list),G).


'$check_flag_optionsl'([], term, read_write, G).
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

'$expects_dialect'(swi) :-
	eraseall('$dialect'),
	recorda('$dialect',swi,_),
	load_files(library('dialect/swi'),[silent(true),if(not_loaded)]).
'$expects_dialect'(yap) :-
	eraseall('$dialect'),
	recorda('$dialect',yap,_).

'$thread_initialization'(M:D) :-
	eraseall('$thread_initialization'),
	recorda('$thread_initialization',M:D,_),
	fail.
'$thread_initialization'(M:D) :-
	'$initialization'(M:D).


