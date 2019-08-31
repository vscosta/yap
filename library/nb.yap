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

/**
 * @file   nb.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 23:18:13 2015
 * 
 * @brief  stub for global (non-backtrackable) variables.
 * 
 * 
*/


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
	       nb_heap_reset/1,
	       nb_heap_size/2,
	       nb_beam/2,
	       nb_beam_close/1,
	       nb_beam_add/3,
	       nb_beam_del/3,
	       nb_beam_peek/3,
	       nb_beam_empty/1,
%	       nb_beam_check/1,
	       nb_beam_size/2]).

/** @defgroup nonback Non-Backtrackable Data Structures
@ingroup library
@{

The following routines implement well-known data-structures using global
non-backtrackable variables (implemented on the Prolog stack). The
data-structures currently supported are Queues, Heaps, and Beam for Beam
search. They are allowed through `library(nb)`. 

 
*/

/** @pred nb_beam(+ _DefaultSize_,- _Beam_) 


Create a  _Beam_ with default size  _DefaultSize_. Note that size
is fixed throughout.

 
*/
/** @pred nb_beam_add(+ _Beam_, + _Key_, + _Value_) 


Add  _Key_- _Value_ to the beam  _Beam_. The key is sorted on
 _Key_ only.

 
*/
/** @pred nb_beam_close(+ _Beam_) 


Close the beam  _Beam_: no further elements can be added.

 
*/
/** @pred nb_beam_del(+ _Beam_, - _Key_, - _Value_) 


Remove element  _Key_- _Value_ with smallest  _Value_ in beam
 _Beam_. Fail if the beam is empty.

 
*/
/** @pred nb_beam_empty(+ _Beam_) 


Succeeds if   _Beam_ is empty.




 */
/** @pred nb_beam_peek(+ _Beam_, - _Key_, - _Value_)) 


 _Key_- _Value_ is the element with smallest  _Key_ in the beam
 _Beam_. Fail if the beam is empty.

 
*/
/** @pred nb_beam_size(+ _Beam_, - _Size_) 


Unify  _Size_ with the number of elements in the beam   _Beam_.

 
*/
/** @pred nb_heap(+ _DefaultSize_,- _Heap_) 


Create a  _Heap_ with default size  _DefaultSize_. Note that size
will expand as needed.

 
*/
/** @pred nb_heap_add(+ _Heap_, + _Key_, + _Value_) 


Add  _Key_- _Value_ to the heap  _Heap_. The key is sorted on
 _Key_ only.

 
*/
/** @pred nb_heap_close(+ _Heap_) 


Close the heap  _Heap_: no further elements can be added.

 
*/
/** @pred nb_heap_del(+ _Heap_, - _Key_, - _Value_) 


Remove element  _Key_- _Value_ with smallest  _Value_ in heap
 _Heap_. Fail if the heap is empty.

 
*/
/** @pred nb_heap_empty(+ _Heap_) 


Succeeds if   _Heap_ is empty.

 
*/
/** @pred nb_heap_peek(+ _Heap_, - _Key_, - _Value_)) 


 _Key_- _Value_ is the element with smallest  _Key_ in the heap
 _Heap_. Fail if the heap is empty.

 
*/
/** @pred nb_heap_size(+ _Heap_, - _Size_) 


Unify  _Size_ with the number of elements in the heap   _Heap_.

 
*/
/** @pred nb_queue(- _Queue_) 


Create a  _Queue_.

 
*/
/** @pred nb_queue_close(+ _Queue_, - _Head_, ? _Tail_) 


Unify the queue   _Queue_ with a difference list
 _Head_- _Tail_. The queue will now be empty and no further
elements can be added.

 
*/
/** @pred nb_queue_dequeue(+ _Queue_, - _Element_) 


Remove  _Element_ from the front of the queue   _Queue_. Fail if
the queue is empty.

 
*/
/** @pred nb_queue_empty(+ _Queue_) 


Succeeds if   _Queue_ is empty.

 
*/
/** @pred nb_queue_enqueue(+ _Queue_, + _Element_) 


Add  _Element_ to the front of the queue   _Queue_.

 
*/
/** @pred nb_queue_peek(+ _Queue_, - _Element_) 


 _Element_ is the front of the queue   _Queue_. Fail if
the queue is empty.

 
*/
/** @pred nb_queue_size(+ _Queue_, - _Size_) 


Unify  _Size_ with the number of elements in the queue   _Queue_.

 
*/
/** @} */

