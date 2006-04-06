/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*       BEAM extends the YAP Prolog system to support the EAM            *
*									 *
*    Copyright Ricardo Lopes and Universidade do Porto 2000-2006	 *
*									 *
**************************************************************************
*									 *
* File:		eam.yap						         *
* Last rev:	6/4/2006						 *
* mods:									 *
* comments:	Some utility predicates needed by BEAM		         *
*									 *
*************************************************************************/

'$_eamtrans'((A,B),(C,D)):- !, '$_eamtrans'(A,C),'$_eamtrans'(B,D).
'$_eamtrans'((X is Y) ,(skip_while_var(Vars), X is Y  )):- !, '$variables_in_term'(Y,[],Vars).
'$_eamtrans'((X =\= Y),(skip_while_var(Vars), X =\= Y )):- !, '$variables_in_term'(X + Y,[],Vars).
'$_eamtrans'((X =\= Y),(skip_while_var(Vars), X =:= Y )):- !, '$variables_in_term'(X + Y,[],Vars).
'$_eamtrans'((X >= Y) ,(skip_while_var(Vars), X >= Y  )):- !, '$variables_in_term'(X + Y,[],Vars).
'$_eamtrans'((X > Y)  ,(skip_while_var(Vars), X > Y   )):- !, '$variables_in_term'(X + Y,[],Vars).
'$_eamtrans'((X < Y)  ,(skip_while_var(Vars), X < Y   )):- !, '$variables_in_term'(X + Y,[],Vars).
'$_eamtrans'((X =< Y) ,(skip_while_var(Vars), X =< Y  )):- !, '$variables_in_term'(X + Y,[],Vars).
'$_eamtrans'(B,B).

eamconsult(File):- eam, eam,                    %fails if eam is disable
                assert((term_expansion((A :- B),(A :- C)):- '$_eamtrans'(B,C))),
                eam, ( consult(File) ; true), eam,
                retract((term_expansion((A :- B),(A :- C)):- '$_eamtrans'(B,C))).

