%
% Preliminary support for some CHR handlers
%
% Define a stable ordering on variables
% (Term/Var ordering changes under put_atts, delay, etc.)
%
% Bindings still brake our ordering!
%
%

:- module( ordering,
	[
	    globalize/1,
	    unglobalize/1,
	    var_compare/3
	]).

:- use_module( library(terms), [term_variables/2]).
:- use_module( library(atts)).

:- attribute id/1.

%
% The exception mechanism copies the thrown term.
% Thus we cannot pass the variable to the catcher ...
%
verify_attributes( X, Y, []) :-
	get_atts( X, id(Id)),
	!,
	( var(Y) ->
	    ( get_atts( Y, id(_)) ->
		true % raise_exception( binding_globalized_var)
	    ;
		put_atts( Y, id(Id))
	    )
	;
	    true % raise_exception( binding_globalized_var)
	).
verify_attributes( _, _, []).

globalize( Term) :-
	term_variables( Term, Vars),
	var_globalize( Vars).

var_globalize( X) :- var( X), !,		% indexing only
	( get_atts( X, id(_)) ->
	    true
	;
	    put_atts( X, id(_))
	).
var_globalize( []).
var_globalize( [X|Xs]) :-
	var_globalize( X),
	var_globalize( Xs).

unglobalize( Term) :-
	term_variables( Term, Vars),
	var_unglobalize( Vars).

var_unglobalize( X) :- var( X), !,		% indexing only
	put_atts( X, -id(_)).
var_unglobalize( []).
var_unglobalize( [X|Xs]) :-
	var_unglobalize( X),
	var_unglobalize( Xs).

var_compare( Rel, X, Y) :-
	(var(X),get_atts( X, id(IdX)) ->
	    true
	;
	    raise_exception( not_globalized)
	),
	(var(Y),get_atts( Y, id(IdY)) ->
	    true
	;
	    raise_exception( not_globalized)
	),
	compare( Rel, IdX, IdY).



