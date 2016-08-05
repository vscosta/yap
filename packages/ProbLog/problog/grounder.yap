%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-11-28 14:41:26 +0100 (Mon, 28 Nov 2011) $
%  $Revision: 6764 $
%
%  Main author of this file:
%  Bernd Gutmann
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(grounder, [grounder_reset/0,
               	     grounder_compute_reachable_atoms/3,
		     grounder_reachable_atom/1,
		     grounder_ground_term_with_reachable_atoms/2,
		     grounder_completion_for_atom/3
		    ]).


:- style_check(all).
:- yap_flag(unknown,error).

:- use_module('../problog',[probabilistic_fact/3]).
:- use_module(termhandling).


%========================================================================
%=
%========================================================================

:- multifile user:myclause/3.

user:myclause(_InterpretationID,Head,Body) :-
	current_predicate(user:myclause/2),
	user:myclause(Head,Body).


%========================================================================
%= reset the internal state, that is, forget all reachable atoms
%========================================================================

grounder_reset :-
	eraseall(reachable).


%========================================================================
%= grounder_reachable_atom(-Atom)
%========================================================================

grounder_reachable_atom(Atom) :-
	recorded(reachable,Atom,_Key).


%========================================================================
%= grounder_compute_reachable_atoms(+A,+ID,-Success)
%=   A   is a ground atom
%=   ID  is an interpretation ID
%=   Success is "true" if there is a proof for A, otherwise "false"
%=
%= The predicate always succeeds exactly once
%=
%= This is basically a vanilla meta-interpreter, that follows all
%= paths in the SLD tree and records which atoms can be reached
%= while proving A.
%= the only "speciality" is that the negation of a probilistic
%= fact always succeeds
%=
%= the reachable atoms are stored in the internal database
%= under the key "reachable"
%========================================================================

grounder_compute_reachable_atoms(A,ID,Success) :-
	bb_put(dep_proven,false),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all proofs for A in interpretation ID
      tabled_meta_interpreter(A,ID),
	 bb_put(dep_proven,true),

	 fail; % go to next proof
	 true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	bb_delete(dep_proven,Success).


%========================================================================
%= tabled_meta_interpreter(+E, +ID)
%=   E is a valid Prolog expression
%=   ID is an interpretation ID
%=
%= the predicate succeeds if there is a proof for E
%= upon backtracking all possible proofs are generated
%= the atoms visited while proving E are added to the internal database
%= using the key "reachable"
%=
%= if a ground atom is revisited, it is not proven again
%========================================================================


tabled_meta_interpreter(X,ID) :-
    writeln(ID:X), fail.
tabled_meta_interpreter((X,Y),ID) :-
	!,
	tabled_meta_interpreter(X,ID),
	tabled_meta_interpreter(Y,ID).
tabled_meta_interpreter((X;Y),ID) :-
	!,
	(
	 tabled_meta_interpreter(X,ID);
	 tabled_meta_interpreter(Y,ID)
	).
tabled_meta_interpreter(\+ X,ID) :-
	!,
	(
	    probabilistic_fact(_, X, _)
	->
	    tabled_meta_interpreter(X,ID)   % prob. facts can be true/false
	;
	    \+ tabled_meta_interpreter(X,ID)
	).
tabled_meta_interpreter(X,_) :-
	predicate_property(X,built_in),
	!,
	call(X).
tabled_meta_interpreter( Atom,ID ) :-
	ground(Atom),
	!,

	(
	    recorded(reachable,Atom,_) % did we see this atom before?
	->
	    true   % nothing to do
	;
	           % nope, we have to continue proving
	    recorda(reachable,Atom,_),
	    tabled_meta_interpreter_aux_ground_atom(Atom,ID)
	).
tabled_meta_interpreter(Atom,ID) :-
	% at this point we know, Atom is non-ground
	% hence we need to be carefull not to ignore any path in the SLD tree
	%
	% we can ignore probabilistic facts and only look for myclauses
	% since in ProbLog the requirement is that non-ground facts have to be
	% ground at query time
	current_predicate(user:myclause/3),
	user:myclause(ID,Atom,Body),
	tabled_meta_interpreter(Body,ID),

	% check whether Atom got grounded now,
	% if not, complain and give up

	(
	    ground(Atom)
	->
	    recorda(reachable,Atom,_)
	;

	  format(user_error,'Error at running the meta interpreter.~n',[]),
	  format(user_error,'The clauses defined by myclause/2 have to be written in a way such that~n',[]),
	  format(user_error,'each atom in the body of a clause gets fully grounded when it is called.~n',[]),
	  format(user_error,'    This is not the case for the atom ~w~3n',[Atom]),
	  throw(meta_interpreter_error(Atom))
        ).
        % note, that on backtracking all alternative proofs will
        % be followed as well

%========================================================================
%= tabled_meta_interpreter_aux_ground_atom(+E, +ID)
%=   E is a valid Prolog expression
%=   ID is an interpretation ID
%=
%= the predicate succeeds if there is a proof for E
%= upon backtracking all possible proofs are generated
%= the atoms visited while proving E are added to the internal database
%= using the key "reachable"
%=
%= if a ground atom is revisited, it is not proven again
%=
%= DON'T call this predicate directly, it is a helper predicate for
%= tabled_meta_interpreter/2
%========================================================================

tabled_meta_interpreter_aux_ground_atom(Atom,_ID) :-
	probabilistic_fact(_, Atom, _),
	!.
        % probabilistic facts and background knowledge must not have
        % an atom in common. hence we can savely put that cut above.
tabled_meta_interpreter_aux_ground_atom(Atom,ID) :-
	current_predicate(user:myclause/3),
	user:myclause(ID,Atom,Body),
	% find a suitable clause and continue proving
	% on backtracking we will try all suitable clauses
	tabled_meta_interpreter(Body,ID).


%========================================================================
%= grounder_ground_term_with_reachable_atoms(+T1,-T2)
%=   T1 is a (possible non-ground) term
%=   T2 is ground term
%=
%= generates on backtracking all possible ground instances of T1
%= where atoms are grounded with reachable atoms that have
%= been found before by grounder_compute_reachable_atoms/3
%========================================================================

grounder_ground_term_with_reachable_atoms( (X,Y),  (X2,Y2)) :-
	!,
	grounder_ground_term_with_reachable_atoms(X,X2),
	grounder_ground_term_with_reachable_atoms(Y,Y2).
grounder_ground_term_with_reachable_atoms( (X;Y),  (X2;Y2)) :-
	!,
	grounder_ground_term_with_reachable_atoms(X,X2),
	grounder_ground_term_with_reachable_atoms(Y,Y2).
grounder_ground_term_with_reachable_atoms( \+X,  \+X2) :-
	!,
	grounder_ground_term_with_reachable_atoms(X,X2).
grounder_ground_term_with_reachable_atoms( false,  false) :-
	!.
grounder_ground_term_with_reachable_atoms(X, true) :-
	predicate_property(X,built_in),
	!,
	call(X).
grounder_ground_term_with_reachable_atoms(X,'$atom'(X)) :-
	!,
	recorded(reachable,X,_).

%========================================================================
%= grounder_completion_for_atom(+A,+ID,-X)
%=   A is
%=   X is
%=   ID is
%=
%=
%=
%=
%========================================================================


grounder_completion_for_atom(Head,InterpretationID,'$atom'(Head)<=>Disjunction) :-
	% find all clauses
	findall(Body2,(
		       user:myclause(InterpretationID,Head,Body),
		       grounder_ground_term_with_reachable_atoms(Body,Body2)
		      ),Bodies),
	Bodies\==[],
	list_to_disjunction(Bodies,Disjunction).
