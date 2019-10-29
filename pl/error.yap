/**
  @file pl/error.yap

 @author Jan Wielemaker
 @author Richard O'Keefe
@author adapted to YAP by Vitor Santos Costa
*/

:- module(system(error,
	  [ must_be_of_type/2,		% +Type, +Term
	    must_be_of_type/3,		% +Type, +Term, +Comment
		must_be/2,		% +Type, +Term
		must_be_callable/1,		% +Type, +Term
  	    must_be/3,		% +Type, +Term, +Comment
  	    type_error/2,		% +Type, +Term
%	    must_be_in_domain/2,	% +Domain, +Term
%	    must_be_in_domain/3,	% +Domain, +Term, +Comment
	    domain_error/3,		% +Domain, +Values, +Term
	    existence_error/2,		% +Type, +Term
	    permission_error/3,		% +Action, +Type, +Term
            must_be_instantiated/1,        % +Term
            must_bind_to_type/2,        % +Type, ?Term
	    instantiation_error/1,	% +Term
	    representation_error/1, 	% +Reason
	    is_of_type/2,		% +Type, +Term
	    must_be_callable/1,
	    	    must_be_callable/2
	  ]), [])  .
/**
 @defgroup SWI-error High-level error testing.
@ingroup Deb_Interaction
@{

This  SWI module  provides  predicates  to  simplify  error  generation  and
checking. Adapted to use YAP built-ins.

Its implementation is based on a discussion on the SWI-Prolog
mailinglist on best practices in error   handling. The utility predicate
must_be/2  provides  simple  run-time  type    validation.  The  *_error
predicates are simple wrappers around throw/1   to simplify throwing the
most common ISO error terms.

YAP reuses the code with some extensions, and supports interfacing to some C-builtins.

*/

:- multifile
	has_type/2.

%%	@pred type_error(+Type, +Term).
%%	@pred domain_error(+Type, +Value, +Term).
%%	@pred existence_error(+Type, +Term).
%%	@pred permission_error(+Action, +Type, +Term).
%%	@pred instantiation_error(+Term).
%%	@pred representation_error(+Reason).
%
%	Throw ISO compliant error messages.

type_error(Type, Term) :-
	throw(error(type_error(Type, Term), _)).
domain_error(Type, Term) :-
	throw(error(domain_error(Type, Term), _)).
existence_error(Type, Term) :-
	throw(error(existence_error(Type, Term), _)).
permission_error(Action, Type, Term) :-
	throw(error(permission_error(Action, Type, Term), _)).
instantiation_error(_Term) :-
	throw(error(instantiation_error, _)).
representation_error(Reason) :-
	throw(error(representation_error(Reason), _)).

%%	must_be_of_type(+Type, @Term) is det.
%
%	True if Term satisfies the type constraints for Type. Defined
%	types are =atom=, =atomic=, =between=, =boolean=, =callable=,
%	=chars=, =codes=, =text=, =compound=, =constant=, =float=,
%	=integer=, =nonneg=, =positive_integer=, =negative_integer=,
%	=nonvar=, =number=, =oneof=, =list=, =list_or_partial_list=,
%	=symbol=, =var=, =rational= and =string=.
%
%	Most of these types are defined by an arity-1 built-in predicate
%	of the same name. Below  is  a   brief  definition  of the other
%	types.
%
%	| boolean | one of =true= or =false= |
%	| chars | Proper list of 1-character atoms |
%	| codes | Proper list of Unicode character codes |
%	| text | One of =atom=, =string=, =chars= or =codes= |
%	| between(L,U) | Number between L and U (including L and U) |
%	| nonneg | Integer >= 0 |
%	| positive_integer | Integer > 0 |
%	| negative_integer | Integer < 0 |
%	| oneof(L) | Ground term that is member of L |
%	| list(Type) | Proper list with elements of Type |
%	| list_or_partial_list | A list or an open list (ending in a variable) |
%	| predicate_indicator | a predicate indicator of the form M:N/A or M:N//A |
%
%	@throws instantiation_error if Term is insufficiently
%	instantiated and type_error(Type, Term) if Term is not of Type.

must_be(Type, X) :-
	must_be_of_type(Type, X).

must_be(Type, X, Comment) :-
	must_be_of_type(Type, X, Comment).

must_be_of_type(callable, X) :-
	!,
	must_be_callable(X).
must_be_of_type(atom, X) :-
	!,
	is_atom(X).
must_be_of_type(module, X) :-
	!,
	is_atom(X).
must_be_of_type(predicate_indicator, X) :-
	!,
	is_predicate_indicator(X, _).
must_be_of_type(Type, X) :-
	(   has_type(Type, X)
	->  true
	;   is_not(Type, X)
	).

must_be_of_type(predicate_indicator, X, Comment) :-
	!,
	is_predicate_indicator(X, Comment).
must_be_of_type(callable, X, _Comment) :-
	!,
	must_be_callable(X).
must_be_of_type(Type, X, _Comment) :-
	(   has_type(Type, X)
	->  true
	;   is_not(Type, X)
	).

must_bind_to_type(Type, X) :-
	(   may_bind_to_type(Type, X)
	->  true
	;   is_not(Type, X)
	).

%%	@predicate is_not(+Type, +Term)
%
%	Throws appropriate error. It is _known_ that Term is not of type
%	Type.
%
%	@throws type_error(Type, Term)
%	@throws instantiation_error

is_not(list, X) :- !,
	not_a_list(list, X).
is_not(list(_), X) :- !,
	not_a_list(list, X).
is_not(list_or_partial_list, X) :- !,
	type_error(list, X).
is_not(chars, X) :- !,
	not_a_list(chars, X).
is_not(codes, X) :- !,
	not_a_list(codes, X).
is_not(var,_X) :- !,
	representation_error(variable).
is_not(rational, X) :- !,
	not_a_rational(X).
is_not(Type, X) :-
	(   var(X)
	->  instantiation_error(X)
	;   ground_type(Type), \+ ground(X)
	->  instantiation_error(X)
	;   type_error(Type, X)
	).

ground_type(ground).
ground_type(oneof(_)).
ground_type(stream).
ground_type(text).
ground_type(string).

not_a_list(Type, X) :-
	'$skip_list'(_, X, Rest),
	(   var(Rest)
	->  instantiation_error(X)
	;   type_error(Type, X)
	).

not_a_rational(X) :-
	(   var(X)
	->  instantiation_error(X)
	;   X = rdiv(N,D)
	->  must_be(integer, N), must_be(integer, D),
	    type_error(rational,X)
	;   type_error(rational,X)
	).

%%	is_of_type(+Type, @Term) is semidet.
%
%	True if Term satisfies Type.

is_of_type(Type, Term) :-
	has_type(Type, Term).


%%	has_type(+Type, @Term) is semidet.
%
%	True if Term satisfies Type.

has_type(impossible, _) :-	instantiation_error(_).
has_type(any, _).
has_type(atom, X)	  :- atom(X).
has_type(atomic, X)	  :- atomic(X).
has_type(between(L,U), X) :- (   integer(L)
			     ->  integer(X), between(L,U,X)
			     ;   number(X), X >= L, X =< U
			     ).
has_type(boolean, X) 	  :- (X==true;X==false), !.
has_type(callable, X)	  :- callable(X).
has_type(chars,	X)	  :- chars(X).
has_type(codes,	X)	  :- codes(X).
has_type(text, X)	  :- text(X).
has_type(compound, X)	  :- compound(X).
has_type(constant, X)	  :- atomic(X).
has_type(float, X)	  :- float(X).
has_type(ground, X)	  :- ground(X).
has_type(integer, X)	  :- integer(X).
has_type(nonneg, X)	  :- integer(X), X >= 0.
has_type(positive_integer, X)	  :- integer(X), X > 0.
has_type(negative_integer, X)	  :- integer(X), X < 0.
has_type(nonvar, X)	  :- nonvar(X).
has_type(number, X)	  :- number(X).
has_type(oneof(L), X)	  :- ground(X), lists:memberchk(X, L).
has_type(proper_list, X)  :- is_list(X).
has_type(list, X)  	  :- is_list(X).
has_type(list_or_partial_list, X)  :- is_list_or_partial_list(X).
has_type(symbol, X)	  :- atom(X).
has_type(var, X)	  :- var(X).
has_type(rational, X)	  :- rational(X).
has_type(string, X)	  :- string(X).
has_type(stream, X)	  :- is_stream(X).
has_type(list(Type), X)	  :- is_list(X), element_types(X, Type).

%%	may_bind_to_type(+Type, @Term) is semidet.
%
%	True if _Term_ or term _Term\theta_ satisfies _Type_.

may_bind_to_type(_, X ) :- var(X), !.
may_bind_to_type(impossible, _) :-	instantiation_error(_).
may_bind_to_type(any, _).
may_bind_to_type(atom, X)	  :- atom(X).
may_bind_to_type(atomic, X)	  :- atomic(X).
may_bind_to_type(between(L,U), X) :- (   integer(L)
			     ->  integer(X), between(L,U,X)
			     ;   number(X), X >= L, X =< U
			     ).
may_bind_to_type(boolean, X) 	  :- (X==true;X==false), !.
may_bind_to_type(callable, X)	  :- callable(X).
may_bind_to_type(chars,	X)	  :- chars(X).
may_bind_to_type(codes,	X)	  :- codes(X).
may_bind_to_type(text, X)	  :- text(X).
may_bind_to_type(compound, X)	  :- compound(X).
may_bind_to_type(constant, X)	  :- atomic(X).
may_bind_to_type(float, X)	  :- float(X).
may_bind_to_type(ground, X)	  :- ground(X).
may_bind_to_type(integer, X)	  :- integer(X).
may_bind_to_type(nonneg, X)	  :- integer(X), X >= 0.
may_bind_to_type(positive_integer, X)	  :- integer(X), X > 0.
may_bind_to_type(negative_integer, X)	  :- integer(X), X < 0.
may_bind_to_type(predicate_indicator, X)  :-
	(
	 X = M:PI
	->
	 may_bind_to_type( atom, M),
	 may_bind_to_type(predicate_indicator, PI)
	;
	 X = N/A
	->
	 may_bind_to_type( atom, N),
	 may_bind_to_type(integer, A)
	;
	 X = N//A
	->
	 may_bind_to_type( atom, N),
	 may_bind_to_type(integer, A)
	).


may_bind_to_type(nonvar, _X).
may_bind_to_type(number, X)	  :- number(X).
may_bind_to_type(oneof(L), X)	  :- ground(X), lists:memberchk(X, L).
may_bind_to_type(proper_list, X)  :- is_list(X).
may_bind_to_type(list, X)  	  :- is_list(X).
may_bind_to_type(list_or_partial_list, X)  :- is_list_or_partial_list(X).
may_bind_to_type(symbol, X)	  :- atom(X).
may_bind_to_type(var, X)	  :- var(X).
may_bind_to_type(rational, X)	  :- rational(X).
may_bind_to_type(string, X)	  :- string(X).
may_bind_to_type(stream, X)	  :- is_stream(X).
may_bind_to_type(list(Type), X)	  :- is_list(X), element_types(X, Type).

chars(0) :- !, fail.
chars([]).
chars([H|T]) :-
	atom(H), atom_length(H, 1),
	chars(T).

codes(x) :- !, fail.
codes([]).
codes([H|T]) :-
	integer(H), between(1, 0x10ffff, H),
	codes(T).

text(X) :-
	(   atom(X)
	;   string(X)
	;   chars(X)
	;   codes(X)
	), !.

element_types([], _).
element_types([H|T], Type) :-
	must_be(Type, H),
	element_types(T, Type).

is_list_or_partial_list(L0) :-
	'$skip_list'(_, L0,L),
	( var(L) -> true ; L == [] ).

must_be_instantiated(X) :-
  ( var(X) -> instantiation_error(X) ; true).

must_be_instantiated(X, Comment) :-
  ( var(X) -> instantiation_error(X, Comment) ; true).


inline(must_be_of_type( atom, X ), is_atom(X) ).
inline(must_be_of_type( module, X ), is_atom(X) ).
inline(must_be_atom( X ), is_atom(X) ).
inline(must_be_module( X ), is_atom(X) ).

%% @}
