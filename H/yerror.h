/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		error.h							 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	Error handling header file				 *
*									 *
*************************************************************************/

/* The behaviour of ErrorHandler is set by these flags :

   CONTINUE_AFTER_ERROR: return to the caller, who should take the necessary
   	measures.
   ABORT_AFTER_ERROR: output the message, and then abort, back to the
	top level.
   EXIT_AFTER_ERROR: output the message, and exit Prolog

*/

#define CONTINUE_AFTER_ERROR	0
#define ABORT_AFTER_ERROR	1
#define EXIT_AFTER_ERROR	2


