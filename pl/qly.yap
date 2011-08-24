
/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2011	 *
*									 *
**************************************************************************
*									 *
* File:		qly.yap							 *
* Last rev:								 *
* mods:									 *
* comments:	fast save/restore					 *
*									 *
*************************************************************************/

save_module(Mod) :-
	atom_concat(Mod,'.qly',F),
	open(F, write, S, [type(binary)]),
	'$save_module_preds'(S, Mod),
	close(S).

read_module(Mod) :-
	atom_concat(Mod,'.qly',F),
	open(F, read, S, [type(binary)]),
	'$read_module_preds'(S),
	close(S).

