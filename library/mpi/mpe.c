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
* Last rev:	$Date: 2003-07-03 15:46:10 $				 *
* mods:									 *
* comments:	Interface to an MPE library                              *
*									 *
*************************************************************************/

#ifndef lint
// static char *rcsid = "$Header: /Users/vitor/Yap/yap-cvsbackup/library/mpi/mpe.c,v 1.6 2003-07-03 15:46:10 stasinos Exp $";
#endif

#include "Yap.h"

/* Should we use MPI ? */
#if defined(HAVE_MPI_H) && (defined(HAVE_LIBMPI) || defined(HAVE_LIBMPICH))
 #define HAVE_MPI 1
#else
 #define HAVE_MPI 0
#endif

/* Should we use MPE ? */
#if defined(HAVE_MPI_H) && HAVE_LIBMPE &&  HAVE_MPI
 #define HAVE_MPE 1
#else
 #define HAVE_MPE 0
#endif

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

static Int p_init(void);
static Int p_start(void);
static Int p_close(void);
static Int p_create_event(void);
static Int p_create_state(void);
static Int p_log(void);


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
    Yap_Error(INSTANTIATION_ERROR, t_str, "mpe_close");
    return (FALSE);
  } else if( !IsAtomTerm(t_str) ) {
    Yap_Error(TYPE_ERROR_ATOM, t_str, "mpe_close");
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
  return Yap_unify(ARG1, MkIntegerTerm(event_id));
}

static Int               /* mpe_create_state(+Event,+Event,+Text,+Colour) */
p_create_state()
{
  Term t_start = Deref(ARG1), t_end = Deref(ARG2),
    t_descr = Deref(ARG3), t_colour = Deref(ARG4);
  Int start_id, end_id;
  char *descr, *colour;
  int retv;

  /* The first and second args must be bount to integer event IDs. */
  if (IsVarTerm(t_start)) {
    Yap_Error(INSTANTIATION_ERROR, t_start, "mpe_create_state");
    return (FALSE);
  } else if( !IsIntegerTerm(t_start) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_start, "mpe_create_state");
    return (FALSE);
  } else {
    start_id = IntOfTerm(t_start);
  }
  if (IsVarTerm(t_end)) {
    Yap_Error(INSTANTIATION_ERROR, t_end, "mpe_create_state");
    return (FALSE);
  } else if( !IsIntegerTerm(t_end) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_end, "mpe_create_state");
    return (FALSE);
  } else {
    end_id = IntOfTerm(t_end);
  }

  /* The third and fourth args must be bound to atoms. */
  if (IsVarTerm(t_descr)) {
    Yap_Error(INSTANTIATION_ERROR, t_descr, "mpe_create_state");
    return (FALSE);
  } else if( !IsAtomTerm(t_descr) ) {
    Yap_Error(TYPE_ERROR_ATOM, t_descr, "mpe_create_state");
    return (FALSE);
  } else {
    descr = RepAtom(AtomOfTerm(t_descr))->StrOfAE;
  }
  if (IsVarTerm(t_colour)) {
    Yap_Error(INSTANTIATION_ERROR, t_colour, "mpe_create_state");
    return (FALSE);
  } else if( !IsAtomTerm(t_colour) ) {
    Yap_Error(TYPE_ERROR_ATOM, t_colour, "mpe_create_state");
    return (FALSE);
  } else {
    colour = RepAtom(AtomOfTerm(t_colour))->StrOfAE;
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
    Yap_Error(INSTANTIATION_ERROR, t_type, "mpe_log");
    return (FALSE);
  } else if( !IsIntegerTerm(t_type) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_type, "mpe_log");
    return (FALSE);
  } else {
    event_id = IntOfTerm(t_type);
  }

  /* The second arg must be bount to integer event number. */
  if (IsVarTerm(t_num)) {
    Yap_Error(INSTANTIATION_ERROR, t_num, "mpe_log");
    return (FALSE);
  } else if( !IsIntegerTerm(t_num) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_num, "mpe_log");
    return (FALSE);
  } else {
    event = IntOfTerm(t_num);
  }

  /* The third arg must be bound to an atom. */
  if (IsVarTerm(t_str)) {
    Yap_Error(INSTANTIATION_ERROR, t_str, "mpe_log");
    return (FALSE);
  } else if( !IsAtomTerm(t_str) ) {
    Yap_Error(TYPE_ERROR_ATOM, t_str, "mpe_log");
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
Yap_InitMPE(void)
{
  Yap_InitCPred( "mpe_open", 0, p_init, SafePredFlag );
  Yap_InitCPred( "mpe_start", 0, p_start, SafePredFlag );
  Yap_InitCPred( "mpe_close", 1, p_close, SafePredFlag );
  Yap_InitCPred( "mpe_create_event", 1, p_create_event, SafePredFlag );
  Yap_InitCPred( "mpe_create_state", 4, p_create_state, SafePredFlag );
  Yap_InitCPred( "mpe_log", 3, p_log, SafePredFlag );
}

#endif /* HAVE_MPE */
