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
* File:		terms.yap						 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	Term manipulation operations				 *
*									 *
*************************************************************************/
/** @defgroup Terms Utilities On Terms
@ingroup library
@{

The next routines provide a set of commonly used utilities to manipulate
terms. Most of these utilities have been implemented in `C` for
efficiency. They are available through the
`use_module(library(terms))` command.

*/
/**
 @pred cyclic_term(? _Term_) 


Succeed if the argument  _Term_ is not a cyclic term.

 
*/


/** @pred  term_subsumer(? _T1_, ? _T2_, ? _Subsumer_) 



Succeed if  _Subsumer_ unifies with the least general
generalization over  _T1_ and
 _T2_.

 
*/
/** @pred new_variables_in_term(+ _Variables_,? _Term_, - _OutputVariables_) 



Unify  _OutputVariables_ with all variables occurring in  _Term_ that are not in the list  _Variables_.

 
*/
/** @pred subsumes(? _Term1_, ? _Term2_) 



Succeed if  _Term1_ subsumes  _Term2_.  Variables in term
 _Term1_ are bound so that the two terms become equal.

 
*/
/** @pred subsumes_chk(? _Term1_, ? _Term2_) 



Succeed if  _Term1_ subsumes  _Term2_ but does not bind any
variable in  _Term1_.

 
*/
/** @pred term_hash(+ _Term_, + _Depth_, + _Range_, ? _Hash_)


Unify  _Hash_ with a positive integer calculated from the structure
of the term.  The range of the positive integer is from `0` to, but
not including,  _Range_. If  _Depth_ is `-1` the whole term
is considered. Otherwise, the term is considered only up to depth
`1`, where the constants and the principal functor have depth
`1`, and an argument of a term with depth  _I_ has depth  _I+1_. 

 
*/
/** @pred term_hash(+ _Term_, ? _Hash_) 



If  _Term_ is ground unify  _Hash_ with a positive integer
calculated from the structure of the term. Otherwise the argument
 _Hash_ is left unbound. The range of the positive integer is from
`0` to, but not including, `33554432`.

 
*/
/** @pred unifiable(? _Term1_, ? _Term2_, - _Bindings_) 



Succeed if  _Term1_ and  _Term2_ are unifiable with substitution
 _Bindings_.




 */
/** @pred variables_within_term(+ _Variables_,? _Term_, - _OutputVariables_) 



Unify  _OutputVariables_ with the subset of the variables  _Variables_ that occurs in  _Term_.

 
*/
/** @pred variant(? _Term1_, ? _Term2_) 



Succeed if  _Term1_ and  _Term2_ are variant terms.

 
*/
:- module(terms, [
		  term_hash/2,
		  term_hash/4,
		  term_subsumer/3,
		  instantiated_term_hash/4,
		  variant/2,
		  unifiable/3,
		  subsumes/2,

		  subsumes_chk/2,
		  cyclic_term/1,
		  variable_in_term/2,
		  variables_within_term/3,
		  new_variables_in_term/3
		 ]).

term_hash(T,H) :-
	term_hash(T, -1, 33554432, H).

%term_hash(X,Y) :-
%	term_hash(X,-1,16'1000000,Y).

subsumes_chk(X,Y) :-
	\+ \+ subsumes(X,Y).



/** @} */

