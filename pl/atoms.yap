/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
 *									 *
 *************************************************************************/

/**
 * @file atoms.yap
 *
 */

:- system_module_( '$_atoms', [
			      atom_concat/
			      string_concat/2,
        atomic_list_concat/2,
        atomic_list_concat/3,
        atom_list_concat/2,
        atom_list_concat/3,
        string_list_concat/2,
        string_list_concat/3,
        current_atom/1], []).

:- use_system_module( '$_errors', [throw_error/2]).


sub_string(Text,Below, Mid, Above,  Out) :-
%     writeln(sub_string(Text,Below, Mid, Above,  Out) ),
    deterministic_sub_string(Text,Below, Mid, Above,  Out, Sz),
    (
      var(Sz)
      ->
      true
      ;
      nonvar(Below)
      ->
      Max is Sz-Below,
      between(0,Max, Mid),
      sub_string_fetch(Text,Below, Mid, Out),
      Above is Sz-Below-Mid
      ;
      nonvar(Mid)
      ->
      Max is Sz-Mid,
      between(0,Max, Below),
      sub_string_fetch(Text,Below, Mid, Out),
      Above is Sz-Below-Mid
      ;
      nonvar(Above)
      ->
      Max is Sz-Above,
      between(0,Max,Below),
      Mid is Sz-Above-Below,
      sub_string_fetch(Text,Below,Mid, Out)
      ;
      between(0,Sz,Below),
      Max is Sz-Below,
      between(0,Max,Mid),
      sub_string_fetch(Text,Below, Mid, Out),
      Above is Sz-Below-Mid
    ).
      


sub_atom(Text,Below, Mid, Above,  Out) :-
    deterministic_sub_atom(Text,Below, Mid, Above,  Out, Sz),
    (
      var(Sz)
      ->
      true
      ;
      nonvar(Below)
      ->
      Max is Sz-Below,
      between(0,Max, Mid),
      sub_atom_fetch(Text,Below, Mid, Out),
      Above is Sz-Below-Mid
      ;
      nonvar(Mid)
      ->
      Max is Sz-Mid,
      between(0,Max, Below),
      sub_atom_fetch(Text,Below, Mid, Out),
      Above is Sz-Below-Mid
      ;
      nonvar(Above)
      ->
      Max is Sz-Above,
      between(0,Max,Below),
      Mid is Sz-Above-Below,
      sub_atom_fetch(Text,Below,Mid, Out)
      ;
      between(0,Sz,Below),
      Max is Sz-Below,
      between(0,Max,Mid),
      sub_atom_fetch(Text,Below, Mid, Out),
      Above is Sz-Below-Mid
    ).
      
      

/** @pred  atom_concat(+ As, ? A)


The predicate holds when the first argument is a list of atoms, and the
second unifies with the atom obtained by concatenating all the atoms in
the first list.


*/
atom_concat(Xs,At) :-
	( var(At) ->
	   '$atom_concat'(Xs, At )
        ;
	 '$atom_concat_constraints'(Xs, 0, At, Unbound),
	 '$process_atom_holes'(Unbound)
        ).

'$atom_concat_constraints'([],  ``).
    '$atom_concat_constraints'([V],  V) :-
!.
'$atom_concat_constraints'([V|Vs],  At) :-
    sub_atom(At,0,_,Extra, V),
    sub_atom(At,_,Extra,0, More),
    '$atom_concat_constraints'(Vs,  More).

'$atom_concat_constraints'([],  '').
    '$atom_concat_constraints'([V],  V) :-
!.
'$atom_concat_constraints'([V|Vs],  At) :-
    sub_atom(At,0,_,Extra, V),
    sub_atom(At,_,Extra,0, More),
    '$atom_concat_constraints'(Vs,  More).

/** @pred  atomic_list_concat(+ _As_,? _A_)


The predicate holds when the first argument is a list of atomic terms, and
the second unifies with the atom obtained by concatenating all the
atomic terms in the first list. The first argument thus may contain
atoms or numbers.


*/
atomic_list_concat(L,At) :-
	atomic_concat(L, At).

/** @pred  atomic_list_concat(? _As_,+ _Separator_,? _A_)

Creates an atom just like atomic_list_concat/2, but inserts
 _Separator_ between each pair of atoms. For example:

```{.prolog}
?- atomic_list_concat([gnu, gnat], `, `, A).

A = `gnu, gnat`
```

YAP emulates the SWI-Prolog version of this predicate that can also be
used to split atoms by instantiating  _Separator_ and  _Atom_ as
shown below.

```{.prolog}
?- atomic_list_concat(L, -, 'gnu-gnat').

L = [gnu, gnat]
```


*/
atomic_list_concat(L, El, At) :-
	var(El), !,
	throw_error(instantiation_error,atomic_list_concat(L,El,At)).
atomic_list_concat(L, El, At) :-
	ground(L), !,
	'$add_els'(L,El,LEl),
	atomic_concat(LEl, At).
atomic_list_concat(L, El, At) :-
	nonvar(At), !,
	'$atomic_list_concat_all'( At, El, L).

'$atomic_list_concat_all'( At, El, [A|L]) :-
	sub_atom(At, Pos, 1, Left, El), !,
        sub_atom(At, 0, Pos, _, A),
        sub_atom(At, _, Left, 0, At1),
	'$atomic_list_concat_all'( At1, El, L).
'$atomic_list_concat_all'( At, _El, [At]).

'$add_els'([A,B|L],El,[A,El|NL]) :- !,
	'$add_els'([B|L],El,NL).
'$add_els'(L,_,L).

/**
  @pred atom_concat(_A1_,_A2,_A3_)

  The concatenation of _A1_ and _A2_ should be _A3_. The
  predicate can have multiple solutions.

  */
atom_concat(A,B,At) :-
    (
      nonvar(A), nonvar(B)
   ->
	      !,
	      '$atom_concat'(A,B,At)
		  ).
atom_concat(A,B,At) :-
    (    A==B
      -> 
      sub_atom(At,Len,Len,0,B),
    sub_atom(At,0,Len,Len,A)
      ;     
nonvar(A)
      ->
    sub_atom(At,0,Len,Sz,A),
      sub_atom(At,Len,Sz,0,B)
      ;
      sub_atom(At,Len,Sz,0,B),
    sub_atom(At,0,Len,Sz,A)



    ).


/** @pred  atom_list_concat(+ _As_,? _A_)

Alias for atom_cooncat/2.

The predicate holds when the first argument is a list of atomic terms,
and the second unifies with the atom obtained by concatenating all the
atoms in the list. The first argument may only contain atoms.


*/
atom_list_concat(L,At) :-
	atom_concat(L, At).

/** @pred  atom_list_concat(? _As_,+ _Separator_,? _A_)

Creates an atom just like atomic_list_concat/2, but inserts
 _Separator_ between each pair of atoms. For example:

```{.prolog}
?- atom_list_concat([gnu,gnat], '-', A).

A = 'gnu-gnat'
```

YAP emulates the SWI-Prolog version of this predicate that can also be
used to split atoms by instantiating  _Separator_ and  _Atom_ as
shown below.

```{.prolog}
?- atomic_list_concat(L, -, 'gnu-gnat').

L = [gnu, gnat]
```


*/
atom_list_concat(L, El, At) :-
	var(El), !,
	throw_error(instantiation_error,atom_list_concat(L,El,At)).
atom_list_concat(L, El, At) :-
	ground(L), !,
	'$add_els'(L,El,LEl),
	atom_concat(LEl, At).
atom_list_concat(L, El, At) :-
	atom(At), !,
	'$atom_list_concat_all'( At, El, L).

'$atom_list_concat_all'( At, El, [A|L]) :-
	sub_atom(At, Pos, 1, Left, El), !,
        sub_atom(At, 0, Pos, _, A),
        sub_atom(At, _, Left, 0, At1),
	'$atom_list_concat_all'( At1, El, L).
'$atom_list_concat_all'( At, _El, [At]).


/**
@pred string_concat(_T1_,_T2_,_T3_)

Similar to atom_concat/3 but it expects strings  as arguments.
  The concatenation of _A1_ and _A2_ should be _A3_. The
  predicate can have multiple solutions.

  */
string_concat(A,B,At) :-
    var(At),
    !,
    '$string_concat'(A,B,At).
string_concat(A,B,At) :-
    sub_string(At,0,Len,M,A),
    sub_string(At,Len,M,0,B).



/** @pred  string_list_concat(+ _As_,? _A_)

Alias for string_cooncat/2.

The predicate holds when the first argument is a list of stringic terms,
and the second unifies with the string obtained by concatenating all the
strings in the list. The first argument may only contain strings.


*/
string_list_concat(L,At) :-
	string_concat(L, At).

/** @pred  string_list_concat(? _As_,+ _Separator_,? _A_)

Creates an string just like stringic_list_concat/2, but inserts
 _Separator_ between each pair of strings. For example:

```{.prolog}
?- string_list_concat([gnu,gnat], '-', A).

A = 'gnu-gnat'
```

YAP emulates the SWI-Prolog version of this predicate that can also be
used to split strings by instantiating  _Separator_ and  _String_ as
shown below.

```{.prolog}
?- stringic_list_concat(L, -, 'gnu-gnat').

L = [gnu, gnat]
```


*/
string_list_concat(L, El, At) :-
	var(El), !,
	throw_error(instantiation_error,stringc_list_concat(L,El,At)).
string_list_concat(L, El, At) :-
	ground(L), !,
	'$add_els'(L,El,LEl),
	string_concat(LEl, At).
string_list_concat(L, El, At) :-
	string(At), !,
	'$string_list_concat_all'( At, El, L).

'$string_list_concat_all'( At, El, [A|L]) :-
	sub_string(At, Pos, _, Left, El),
        sub_string(At, 0, Pos, _, A),
        sub_string(At, _, Left, 0, At1),
	'$string_list_concat_all'( At1, El, L).
'$string_list_concat_all'( At, _El, [At]).
'$string_list_concat_all'( '' , _El, []).

/** @pred  current_atom( _A_)


Checks whether  _A_ is a currently defined atom. It is used to find all
currently defined atoms by backtracking.


*/
current_atom(A) :-				% check
	atom(A), !.
current_atom(A) :-				% generate
	'$current_atom'(A).

string_concat(Xs,At) :-
	( var(At) ->
	   '$string_concat'(Xs, At )
          ;
	 '$string_concat_constraints'(Xs, At )
	).

'$string_concat_constraints'([],  ``).
    '$string_concat_constraints'([V],  V) :-
!.
'$string_concat_constraints'([V|Vs],  At) :-
    sub_string(At,0,_,Extra, V),
    sub_string(At,_,Extra,0, More),
    '$string_concat_constraints'(Vs,  More).


/**
 * @pred string_char( ?_I_, +_S_+, ?-_C_)
 *
 * Given string _S_, _C_                                                    * represents the character at position _I_. Arguments are enumerated  so that position 1 corresponds to the first character in string _S_.
 *
 *
 * The string _S_ must be known when the predicate is called. _I_ and _C_ may be unbound. If they are, YAP will enumerate all characters from the string.                                                                        
 *
*/
string_char(I,S,C) :-
    must_be_string(S),
    (var(I) ->
     string_length(S,L),
     between(1,L,I)
     ;
     true
    ),
    '$get_string_char'(I,S,C).

/**
 * @pred string_code( ?_I_, +_S_+, ?-_C_)
 *
 * Given string _S_, _C_                                                    * represents the character at position _I_. Arguments are enumerated  so that position 1 corresponds to the first character in string _S_.
 *
 *
 * The string _S_ must be known when the predicate is called. _I_ and _C_ may be unbound. If they are, YAP will enumerate all characters from the string.                                                                        
 *
*/
string_code(I,S,C) :-
    must_be_string(S),
    (var(I) ->
     string_length(S,L),
     between(1,L,I)
     ;
     true
    ),
    '$get_string_code'(I,S,C).
o

/**
@}
*/

