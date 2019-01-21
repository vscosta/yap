/*  $Id$

    Part of CLP(R) (Constraint Logic Programming over Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2004, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is part of Leslie De Koninck's master thesis, supervised
    by Bart Demoen and daily advisor Tom Schrijvers.  It is based on CLP(Q,R)
    by Christian Holzbaur for SICStus Prolog and distributed under the
    license details below with permission from all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/** @addtogroup clpqr  CLP(QR)
  @ingroup packages
  @{
  */

/** @defgroup clpr_implementation  CLP(QR) Predicates
  @ingroup clpqr
  @{
  */

/** @pred bb_inf(+ _Ints_,+ _Expression_,- _Inf_)
The same as bb_inf/5 but without returning the values of the integers
and with an eps of 0.001.


*/
/** @pred bb_inf(+ _Ints_,+ _Expression_,- _Inf_,- _Vertext_,+ _Eps_)
Computes the infimum of  _Expression_ within the current constraint
store, with the additional constraint that in that infimum, all
variables in  _Ints_ have integral values.  _Vertex_ will contain
the values of  _Ints_ in the infimum.  _Eps_ denotes how much a
value may differ from an integer to be considered an integer. E.g. when
 _Eps_ = 0.001, then X = 4.999 will be considered as an integer (5 in
this case).  _Eps_ should be between 0 and 0.5.


*/
/** @pred dump(+ _Target_,+ _Newvars_,- _CodedAnswer_)
Returns the constraints on  _Target_ in the list  _CodedAnswer_
where all variables of  _Target_ have veen replaced by  _NewVars_.
This operation does not change the constraint store. E.g. in

~~~~~
dump([X,Y,Z],[x,y,z],Cons)
~~~~~

 _Cons_ will contain the constraints on  _X_,  _Y_ and
 _Z_ where these variables have been replaced by atoms `x`, `y` and `z`.




 */
/** @pred entailed(+ _Constraint_)
Succeeds if  _Constraint_ is necessarily true within the current
constraint store. This means that adding the negation of the constraint
to the store results in failure.


*/
/** @pred inf(+ _Expression_,- _Inf_)
Computes the infimum of  _Expression_ within the current state of the
constraint store and returns that infimum in  _Inf_. This predicate
does not change the constraint store.


*/
/** @pred inf(+ _Expression_,- _Sup_)
Computes the supremum of  _Expression_ within the current state of
the constraint store and returns that supremum in  _Sup_. This
predicate does not change the constraint store.


*/
/** @pred maximize( _V_)
maximise variable  _V_


*/
/** @pred minimize(<tt>V</tt>)
minimise variable  _V_




 */
:- module(clpr,
	[
	    {}/1,
	    maximize/1,
	    minimize/1,
	    inf/2, inf/4, sup/2, sup/4,
	    bb_inf/3,
	    bb_inf/5,
	    ordering/1,
	    entailed/1,
	    clp_type/2,
	    dump/3%, projecting_assert/1
	]).

%:- expects_dialect(swi).

%
% Don't report export of private predicates from clpr
%
:- multifile
	user:portray_message/2.

:- dynamic
	user:portray_message/2.
%
user:portray_message(warning,import(_,_,clpr,private)).

:- load_files(
	[
	    'clpr/bb_r',
	    'clpr/bv_r',
	    'clpr/fourmotz_r',
	    'clpr/ineq_r',
	    'clpr/itf_r',
	    'clpr/nf_r',
	    'clpr/store_r',
	    'clpqr/class',
	    'clpqr/dump',
	    'clpqr/geler',
	    'clpqr/itf',
	    'clpqr/ordering',
	    'clpqr/project',
	    'clpqr/redund',
	    library(ugraphs)
	],
	[
	    if(not_loaded),
	    silent(true)
	]).

		 /*******************************
		 *	 TOPLEVEL PRINTING	*
		 *******************************/

:- multifile
	prolog:message/3.

% prolog:message(query(YesNo)) --> !,
% 	['~@'-[chr:print_all_stores]],
%         '$messages':prolog_message(query(YesNo)).

prolog:message(query(YesNo,Bindings)) --> !,
	{dump_toplevel_bindings(Bindings,Constraints)},
	{dump_format(Constraints,Format)},
	Format,
        '$messages':prolog_message(query(YesNo,Bindings)).

dump_toplevel_bindings(Bindings,Constraints) :-
	dump_vars_names(Bindings,[],Vars,Names),
	dump(Vars,Names,Constraints).

dump_vars_names([],_,[],[]).
dump_vars_names([Name=Term|Rest],Seen,Vars,Names) :-
	(   var(Term),
	    (   get_attr(Term,itf,_)
	    ;   get_attr(Term,geler,_)
	    ),
	    \+ memberchk_eq(Term,Seen)
	->  Vars = [Term|RVars],
	    Names = [Name|RNames],
	    NSeen = [Term|Seen]
	;   Vars = RVars,
	    Names = RNames,
	    Seen = NSeen
	),
	dump_vars_names(Rest,NSeen,RVars,RNames).

dump_format([],[]).
dump_format([X|Xs],['{~w}'-[X],nl|Rest]) :-
	dump_format(Xs,Rest).

memberchk_eq(X,[Y|Ys]) :-
	(   X == Y
	->  true
	;   memberchk_eq(X,Ys)
	).

%% @}

%% @}
