/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright S. Konstantopoulos and Universidade do Porto 2002     	 *
*									 *
**************************************************************************
*									 *
* File:		mpe.c  							 *
* Last rev:	$Date: 2002-02-26 15:33:16 $				 *
* mods:									 *
* comments:	Interface to an MPE library                              *
*									 *
*************************************************************************/

#ifndef lint
static char *rcsid = "$Header: ";
#endif

#include "Yap.h"

#if HAVE_MPE

#if 0
/* for AtomEof */
#include "Heap.h"

#include "yapio.h"

#include <stdlib.h>
#include <string.h>
#endif

#include "Yatom.h"
#include <mpe.h>

STATIC_PROTO (Int p_init, (void));
STATIC_PROTO (Int p_start, (void));
STATIC_PROTO (Int p_close, (void));
STATIC_PROTO (Int p_create_event, (void));
STATIC_PROTO (Int p_create_state, (void));
STATIC_PROTO (Int p_log, (void));


/*
 * Auxiliary Data and Functions
 */



/*
 * C Predicates
 */


static Int
p_init()                 /* mpe_open */
{
  return (MPE_Init_log() == 0);
}


static Int
p_start()                /* mpe_start */
{
  return (MPE_Start_log() == 0);
}


static Int               /* mpe_close(+FileName) */
p_close()
{
  Term t_str = Deref(ARG1);
  char *str;

  /* The arg must be bound to an atom. */
  if (IsVarTerm(t_str)) {
    Error(INSTANTIATION_ERROR, t_str, "mpe_close");
    return (FALSE);
  } else if( !IsAtomTerm(t_str) ) {
    Error(TYPE_ERROR_ATOM, t_str, "mpe_close");
    return (FALSE);
  } else {
    str = RepAtom(AtomOfTerm(t_str))->StrOfAE;
  }

  return (MPE_Finish_log(str) == 0);
}


static Int               /* mpe_create_event(?Event) */
p_create_event()
{
  Int event_id;

  event_id = MPE_Log_get_event_number();
  return unify(ARG1, MkIntegerTerm(event_id));
}

static Int               /* mpe_create_state(+Event,+Event,+Text,+Colour) */
p_create_state()
{
  Int start_id, end_id;
  char *descr, *colour;
  int retv;

  /* The first and second args must be bount to integer event IDs. */
  if (IsVarTerm(ARG1)) {
    Error(INSTANTIATION_ERROR, ARG1, "mpe_create_state");
    return (FALSE);
  } else if( !IsIntegerTerm(ARG1) ) {
    Error(TYPE_ERROR_INTEGER, ARG1, "mpe_create_state");
    return (FALSE);
  } else {
    start_id = IntOfTerm(ARG1);
  }
  if (IsVarTerm(ARG2)) {
    Error(INSTANTIATION_ERROR, ARG2, "mpe_create_state");
    return (FALSE);
  } else if( !IsIntegerTerm(ARG2) ) {
    Error(TYPE_ERROR_INTEGER, ARG2, "mpe_create_state");
    return (FALSE);
  } else {
    end_id = IntOfTerm(ARG2);
  }

  /* The third and fourth args must be bound to atoms. */
  if (IsVarTerm(ARG3)) {
    Error(INSTANTIATION_ERROR, ARG3, "mpe_create_state");
    return (FALSE);
  } else if( !IsAtomTerm(ARG3) ) {
    Error(TYPE_ERROR_ATOM, ARG3, "mpe_create_state");
    return (FALSE);
  } else {
    descr = RepAtom(AtomOfTerm(ARG3))->StrOfAE;
  }
  if (IsVarTerm(ARG4)) {
    Error(INSTANTIATION_ERROR, ARG4, "mpe_create_state");
    return (FALSE);
  } else if( !IsAtomTerm(ARG4) ) {
    Error(TYPE_ERROR_ATOM, ARG4, "mpe_create_state");
    return (FALSE);
  } else {
    colour = RepAtom(AtomOfTerm(ARG4))->StrOfAE;
  }

  retv = MPE_Describe_state( (int)start_id, (int)end_id, descr, colour );

  return (retv == 0);
}


static Int
p_log()                  /* mpe_log(+EventType, +EventNum, +EventStr) */
{
  Term t_type = Deref(ARG1), t_num = Deref(ARG2), t_str = Deref(ARG3);
  Int event_id, event;
  char *descr;

  /* The first arg must be bount to integer event type ID. */
  if (IsVarTerm(t_type)) {
    Error(INSTANTIATION_ERROR, t_type, "mpe_log");
    return (FALSE);
  } else if( !IsIntegerTerm(t_type) ) {
    Error(TYPE_ERROR_INTEGER, t_type, "mpe_log");
    return (FALSE);
  } else {
    event_id = IntOfTerm(t_type);
  }

  /* The second arg must be bount to integer event number. */
  if (IsVarTerm(t_num)) {
    Error(INSTANTIATION_ERROR, t_num, "mpe_log");
    return (FALSE);
  } else if( !IsIntegerTerm(t_num) ) {
    Error(TYPE_ERROR_INTEGER, t_num, "mpe_log");
    return (FALSE);
  } else {
    event = IntOfTerm(t_num);
  }

  /* The third arg must be bound to an atom. */
  if (IsVarTerm(t_str)) {
    Error(INSTANTIATION_ERROR, t_str, "mpe_log");
    return (FALSE);
  } else if( !IsAtomTerm(t_str) ) {
    Error(TYPE_ERROR_ATOM, t_str, "mpe_log");
    return (FALSE);
  } else {
    descr = RepAtom(AtomOfTerm(t_str))->StrOfAE;
  }

  return ( MPE_Log_event((int)event_id, (int)event, descr) == 0 );
}


/*
 * Init
 */


void
InitMPE(void)
{
  MPE_Init_log();

  InitCPred( "mpe_open", 0, p_init, SafePredFlag );
  InitCPred( "mpe_start", 0, p_start, SafePredFlag );
  InitCPred( "mpe_close", 1, p_close, SafePredFlag );
  InitCPred( "mpe_create_event", 1, p_create_event, SafePredFlag );
  InitCPred( "mpe_create_state", 4, p_create_state, SafePredFlag );
  InitCPred( "mpe_log", 3, p_log, SafePredFlag );
}

#endif /* HAVE_MPE */
