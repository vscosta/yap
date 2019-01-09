/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2010	 *
*									 *
**************************************************************************
*									 *
* File:		udi.yap							 *
* Last rev:	17/12/2012						 *
* mods:									 *
* comments:	support user defined indexing				 *
*									 *
*************************************************************************/
/**
  * @file   udi.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:19:04 2017
  * 
  * @brief  User Defined Extensions
  *
  * @addtogroup UDI User Defined Extensions
  * @ingroup extensions
  * 
  * 
*/

:- system_module( '$_udi', [udi/1], []).

:- meta_predicate udi(:).

/******************
*     udi/1     *
******************/

udi(Pred) :-
   '$udi_init'(Pred).

