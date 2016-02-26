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
* File:		regexp.yap						 *
* Last rev:	5/15/2000						 *
* mods:									 *
* comments:	pseudo random numbers in YAP (from code by Van Gelder)	 *
*									 *
*************************************************************************/

/**
 * @file   prandom.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 23:43:18 2015
 * 
 * @brief  Van Gelder Random Number Generator
 * 
 * 
*/

:- module(prandom, [
	ranstart/0,
	ranstart/1,
	rannum/1,
	ranunif/2]).


%%
% @groupdef prandom Van Gelder Random Number Generator
% @ingroup builtins
% @{
%
%
% The following code produces the same random numbers as my previous 
% ranpkg.pl, but is more accurately documented and slightly more 
% efficient.  
% 
% ranpkg.pl	random number package	Allen Van Gelder, Stanford
%
% rannum produces a random non-negative integer whose low bits are not
% all that random, so it should be scaled to a smaller range in general.
% The integer is in the range 0 .. 2^(w-1) - 1,
% where w is the word size available for integers, e.g., 18 for DEC-10,
% and 16 or 32 for VAX and most IBM.
%
% ranunif produces a uniformly distributed non-negative random integer over
% a caller-specified range.  If range is R, the result is in 0 .. R-1.
%
% ranstart must be called before the first use of rannum or ranunif,
% and may be called later to redefine the seed.
% ranstart/0 causes a built-in seed to be used.
% ranstart(N), N an integer, varies this, but the same N always
% produces the same sequence of numbers.
%
% According to my reading of Knuth, Vol. 2, this generator has period
% 2^(w-1) and potency w/2, i.e., 8, 9, or 16 in practice.  Knuth says
% potency should be at least 5, so this looks more than adequate.
% Its drawback is the lack of randomness of low-order bits.


/** @pred rannum(- _I_) 


Produces a random non-negative integer  _I_ whose low bits are not
all that random, so it should be scaled to a smaller range in general.
The integer  _I_ is in the range 0 .. 2^(w-1) - 1. You can use:

~~~~~
rannum(X) :- yap_flag(max_integer,MI), rannum(R), X is R/MI.
~~~~~
to obtain a floating point number uniformly distributed between 0 and 1.

 
*/
/** @pred ranstart 


Initialize the random number generator using a built-in seed. The
ranstart/0 built-in is always called by the system when loading
the package.

 
*/
/** @pred ranstart(+ _Seed_)

Initialize the random number generator with user-defined  _Seed_. The
same  _Seed_ always produces the same sequence of numbers.

 
*/
/** @pred ranunif(+ _Range_,- _I_) 


ranunif/2 produces a uniformly distributed non-negative random
integer  _I_ over a caller-specified range  _R_.  If range is  _R_,
the result is in 0 ..  _R_-1.




 */
:- initialization(ranstart).

:- dynamic ranState/5.

%
% vsc: dangerous code, to change.
%
%
wsize(32) :-
	yap_flag(max_tagged_integer,I), I >> 32 =:= 0, !.
wsize(64).

ranstart :- ranstart(8'365). %
 
ranstart(N) :-
	wsize(Wsize),				% bits available for int.
	MaxInt is \(1 << (Wsize - 1)),		% all bits but sign bit are 1.
	Incr is (8'154 << (Wsize - 9)) + 1,	% per Knuth, v.2 p.78
	Mult is 8'3655,				% OK for 16-18 Wsize
	Prev is Mult * (8 * N + 5) + Incr,
	assert(ranState(Mult, Prev, Wsize, MaxInt, Incr) ).
 
rannum(Raw) :-
	retract(ranState(Mult, Prev, Wsize, MaxInt, Incr)),
	Curr is Mult * Prev + Incr,
	assert(ranState(Mult, Curr, Wsize, MaxInt, Incr)),
	(	Curr > 0,
		Raw is Curr
	;
		Curr < 0,
		Raw is Curr /\ MaxInt		% force positive sign bit
	).
 
ranunif(Range, Unif) :-
	Range > 0,
	retract( ranState(Mult, Prev, Wsize, MaxInt, Incr) ),
	Curr is Mult * Prev + Incr,
	assert(ranState(Mult, Curr, Wsize, MaxInt, Incr)),
	(	Curr > 0,
		Raw is Curr
	;
		Curr < 0,
		Raw is Curr /\ MaxInt		% force positive sign bit
	),
	Unif is (Raw * Range) >> (Wsize-1).


/%%! @}
