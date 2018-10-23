/**
  * @file   strict_iso.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:15:33 2017
  * 
  * @brief  StrictISO Mode
  *
  * @addtogroup YAPCompilerSettings
  * 
  * 
*/
:- system_module( '$_strict_iso', [], ['$check_iso_strict_clause'/1,
        '$iso_check_goal'/2]).

:- use_system_module( '$_errors', ['$do_error'/2]).

'$iso_check_goal'(V,G) :-
	var(V), !,
	'$do_error'(instantiation_error,call(G)).
'$iso_check_goal'(V,G) :-
	number(V), !,
	'$do_error'(type_error(callable,V),G).
'$iso_check_goal'(_:G,G0) :- !,
	'$iso_check_goal'(G,G0).
'$iso_check_goal'((G1,G2),G0) :- !,
	'$iso_check_a_goal'(G1,(G1,G2),G0),
	'$iso_check_a_goal'(G2,(G1,G2),G0).
'$iso_check_goal'((G1;G2),G0) :- !,
	'$iso_check_a_goal'(G1,(G1;G2),G0),
	'$iso_check_a_goal'(G2,(G1;G2),G0).
'$iso_check_goal'((G1->G2),G0) :- !,
	'$iso_check_a_goal'(G1,(G1->G2),G0),
	'$iso_check_a_goal'(G2,(G1->G2),G0).
'$iso_check_goal'(!,_) :- !.
'$iso_check_goal'((G1|G2),G0) :-
	current_prolog_flag(language, iso), !, 
	'$do_error'(domain_error(builtin_procedure,(G1|G2)), call(G0)).
'$iso_check_goal'((G1|G2),G0) :- !,
	'$iso_check_a_goal'(G1,(G1|G2),G0),
	'$iso_check_a_goal'(G2,(G1|G2),G0).
'$iso_check_goal'(G,G0) :- 
	current_prolog_flag(language, iso),
	'$system_predicate'(G,prolog),
	(
            '$iso_builtin'(G)
	->
	    true
	;
	    '$do_error'(domain_error(builtin_procedure,G), call(G0))
	).
'$iso_check_goal'(_,_).

'$iso_check_a_goal'(V,_,G) :-
	var(V), !,
	'$do_error'(instantiation_error,call(G)).
'$iso_check_a_goal'(V,E,G) :-
	number(V), !,
	'$do_error'(type_error(callable,E),call(G)).
'$iso_check_a_goal'(_:G,E,G0) :- !,
	'$iso_check_a_goal'(G,E,G0).
'$iso_check_a_goal'((G1,G2),E,G0) :- !,
        '$iso_check_a_goal'(G1,E,G0),
        '$iso_check_a_goal'(G2,E,G0).
'$iso_check_a_goal'((G1;G2),E,G0) :- !,
        '$iso_check_a_goal'(G1,E,G0),
        '$iso_check_a_goal'(G2,E,G0).
'$iso_check_a_goal'((G1->G2),E,G0) :- !,
        '$iso_check_a_goal'(G1,E,G0),
        '$iso_check_a_goal'(G2,E,G0).
'$iso_check_a_goal'(!,_,_) :- !.
'$iso_check_a_goal'((_|_),E,G0) :-
	current_prolog_flag(language, iso), !,
	'$do_error'(domain_error(builtin_procedure,E), call(G0)).
'$iso_check_a_goal'((_|_),_,_) :- !.
'$iso_check_a_goal'(G,_,G0) :- 
	current_prolog_flag(language, iso),
	'$is+system_predicate'(G,prolog),
	(
            '$iso_builtin'(G)
	->
	    true
	;
	    '$do_error'(domain_error(builtin_procedure,G), call(G0))
	).
'$iso_check_a_goal'(_,_,_).

'$check_iso_strict_clause'((_:-B)) :- !,
	'$check_iso_strict_body'(B).
'$check_iso_strict_clause'(_).

'$check_iso_strict_body'((B1,B2)) :- !,
	'$check_iso_strict_body'(B1),
	'$check_iso_strict_body'(B2).
'$check_iso_strict_body'((B1;B2)) :- !,
	'$check_iso_strict_body'(B1),
	'$check_iso_strict_body'(B2).
'$check_iso_strict_body'((B1->B2)) :- !,
	'$check_iso_strict_body'(B1),
	'$check_iso_strict_body'(B2).
'$check_iso_strict_body'(B) :-
	'$check_iso_strict_goal'(B).

'$check_iso_strict_goal'(G) :-
	'$is_system_predicate'(G,prolog), !,
	'$check_iso_system_goal'(G).
'$check_iso_strict_goal'(_).


'$check_iso_system_goal'(G) :-
	'$iso_builtin'(G), !.
'$check_iso_system_goal'(G) :-
	'$do_error'(domain_error(builtin_procedure,G), G).

'$iso_builtin'(abolish(_)).
'$iso_builtin'(acylic_term(_)).
'$iso_builtin'(arg(_,_,_)).
'$iso_builtin'(_=:=_).
'$iso_builtin'(_=\=_).
'$iso_builtin'(_>_).
'$iso_builtin'(_>=_).
'$iso_builtin'(_<_).
'$iso_builtin'(_=<_).
'$iso_builtin'(asserta(_)).
'$iso_builtin'(assertz(_)).
'$iso_builtin'(at_end_of_stream).
'$iso_builtin'(at_end_of_stream(_)).
'$iso_builtin'(atom(_)).
'$iso_builtin'(atom_chars(_,_)).
'$iso_builtin'(atom_codes(_,_)).
'$iso_builtin'(atom_concat(_,_,_)).
'$iso_builtin'(atom_length(_,_)).
'$iso_builtin'(atomic(_)).
'$iso_builtin'(bagof(_,_,_)).
'$iso_builtin'(call(_)).
'$iso_builtin'(call(_,_)).
'$iso_builtin'(call(_,_,_)).
'$iso_builtin'(call(_,_,_,_)).
'$iso_builtin'(call(_,_,_,_,_)).
'$iso_builtin'(call(_,_,_,_,_,_)).
'$iso_builtin'(call(_,_,_,_,_,_,_)).
'$iso_builtin'(call(_,_,_,_,_,_,_,_)).
'$iso_builtin'(callable(_)).
'$iso_builtin'(catch(_,_,_)).
'$iso_builtin'(char_code(_,_)).
'$iso_builtin'(char_conversion(_,_)).
'$iso_builtin'(clause(_,_)).
'$iso_builtin'(close(_)).
'$iso_builtin'(close(_,_)).
'$iso_builtin'(compare(_,_,_)).
'$iso_builtin'(compound(_)).
'$iso_builtin'((_,_)).
'$iso_builtin'(copy_term(_,_)).
'$iso_builtin'(current_char_conversion(_,_)).
'$iso_builtin'(current_input(_)).
'$iso_builtin'(current_op(_,_,_)).
'$iso_builtin'(current_output(_)).
'$iso_builtin'(current_predicate(_)).
'$iso_builtin'(current_prolog_flag(_,_)).
'$iso_builtin'(!).
'$iso_builtin'((_;_)).
'$iso_builtin'(fail).
'$iso_builtin'(false).
'$iso_builtin'(findall(_,_,_)).
'$iso_builtin'(float(_)).
'$iso_builtin'(abort).
'$iso_builtin'(flush_output).
'$iso_builtin'(flush_output(_)).
'$iso_builtin'(functor(_,_,_)).
'$iso_builtin'(get_byte(_)).
'$iso_builtin'(get_byte(_,_)).
'$iso_builtin'(get_char(_)).
'$iso_builtin'(get_char(_,_)).
'$iso_builtin'(get_code(_)).
'$iso_builtin'(get_code(_,_)).
'$iso_builtin'(ground(_)).
'$iso_builtin'(halt).
'$iso_builtin'(halt(_)).
'$iso_builtin'((_->_)).
'$iso_builtin'(integer(_)).
'$iso_builtin'(_ is _).
'$iso_builtin'(keysort(_,_)).
'$iso_builtin'(nl).
'$iso_builtin'(nl(_)).
'$iso_builtin'(nonvar(_)).
'$iso_builtin'(\+(_)).
'$iso_builtin'(number(_)).
'$iso_builtin'(number_chars(_,_)).
'$iso_builtin'(number_codes(_,_)).
'$iso_builtin'(once(_)).
'$iso_builtin'(op(_,_,_)).
'$iso_builtin'(open(_,_,_)).
'$iso_builtin'(open(_,_,_,_)).
'$iso_builtin'(peek_byte(_)).
'$iso_builtin'(peek_byte(_,_)).
'$iso_builtin'(peek_char(_)).
'$iso_builtin'(peek_char(_,_)).
'$iso_builtin'(peek_code(_)).
'$iso_builtin'(peek_code(_,_)).
'$iso_builtin'(put_byte(_)).
'$iso_builtin'(put_byte(_,_)).
'$iso_builtin'(put_char(_)).
'$iso_builtin'(put_char(_,_)).
'$iso_builtin'(put_code(_)).
'$iso_builtin'(put_code(_,_)).
'$iso_builtin'(read(_)).
'$iso_builtin'(read(_,_)).
'$iso_builtin'(read_term(_,_)).
'$iso_builtin'(read_term(_,_,_)).
'$iso_builtin'(repeat).
'$iso_builtin'(retract(_)).
'$iso_builtin'(retractall(_)).
'$iso_builtin'(set_input(_)).
'$iso_builtin'(set_output(_)).
'$iso_builtin'(set_prolog_flag(_,_)).
'$iso_builtin'(set_stream_position(_,_)).
'$iso_builtin'(setof(_,_,_)).
'$iso_builtin'(sort(_,_)).
'$iso_builtin'(stream_property(_,_)).
'$iso_builtin'(sub_atom(_,_,_,_,_)).
'$iso_builtin'(subsumes_term(_,_)).
'$iso_builtin'(_@>_).
'$iso_builtin'(_@>=_).
'$iso_builtin'(_==_).
'$iso_builtin'(_@<_).
'$iso_builtin'(_@=<_).
'$iso_builtin'(_\==_).
'$iso_builtin'(term_variables(_,_)).
'$iso_builtin'(throw(_)).
'$iso_builtin'(true).
'$iso_builtin'(_\=_).
'$iso_builtin'(_=_).
'$iso_builtin'(unify_with_occurs_check(_,_)).
'$iso_builtin'(_384=.._385).
'$iso_builtin'(var(_)).
'$iso_builtin'(write(_)).
'$iso_builtin'(write(_,_)).
'$iso_builtin'(write_canonical(_)).
'$iso_builtin'(write_canonical(_,_)).
'$iso_builtin'(write_term(_,_)).
'$iso_builtin'(write_term(_,_,_)).
'$iso_builtin'(writeq(_)).
'$iso_builtin'(writeq(_,_)).

