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
* File:		listing.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	listing a prolog program				 *
*									 *
*************************************************************************/

% just reexport standard listing predicates.

:- module( listing, [listing/0,
        listing/1,
        portray_clause/1,
        portray_clause/2], []).
