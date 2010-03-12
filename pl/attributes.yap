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
* File:		atts.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	attribute support for Prolog				 *
*									 *
*************************************************************************/

:- module('$attributes', [get_attr/3,
			   put_attr/3,
			   del_attr/2,
			   del_attrs/1,
			   get_attrs/2,
			   put_attrs/2
			  ]).


get_attr(Var, Mod, Att) :-
	functor(AttTerm, Mod, 2),
	arg(2, AttTerm, Att),
	attributes:get_module_atts(Var, AttTerm).

put_attr(Var, Mod, Att) :-
	functor(AttTerm, Mod, 2),
	arg(2, AttTerm, Att),
	attributes:put_module_atts(Var, AttTerm).

del_attr(Var, Mod) :-
	functor(AttTerm, Mod, 2),
	attributes:del_all_module_atts(Var, AttTerm).

del_attrs(Var) :-
	attributes:del_all_atts(Var).

get_attrs(AttVar, SWIAtts) :-
	attributes:get_all_swi_atts(AttVar,SWIAtts).

put_attrs(_, []).
put_attrs(V, Atts) :-
	cvt_to_swi_atts(Atts, YapAtts),
	attributes:put_att_term(V, YapAtts).

cvt_to_swi_atts([], _).
cvt_to_swi_atts(att(Mod,Attribute,Atts), ModAttribute) :-
	ModAttribute =.. [Mod, YapAtts, Attribute],
	cvt_to_swi_atts(Atts, YapAtts).

