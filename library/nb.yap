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
* File:		nb.yap							 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	non-backtrackable data-structures			 *
*									 *
*************************************************************************/

:- module(nb, [
	       nb_create_accumulator/2,
	       nb_add_to_accumulator/2,
	       nb_accumulator_value/2,
	       nb_queue/1,
	       nb_queue/2,
	       nb_queue_close/3,
	       nb_queue_enqueue/2,
	       nb_queue_dequeue/2,
	       nb_queue_peek/2,
	       nb_queue_empty/1,
	       nb_queue_size/2,
	       nb_queue_replace/3,
	       nb_heap/2,
	       nb_heap_close/1,
	       nb_heap_add/3,
	       nb_heap_del/3,
	       nb_heap_peek/3,
	       nb_heap_empty/1,
	       nb_heap_size/2,
	       nb_beam/2,
	       nb_beam_close/1,
	       nb_beam_add/3,
	       nb_beam_del/3,
	       nb_beam_peek/3,
	       nb_beam_empty/1,
%	       nb_beam_check/1,
	       nb_beam_size/2]).
