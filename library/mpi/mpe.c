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
* Last rev:	$Date: 2002-02-22 14:31:45 $				 *
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
  int retv;

  retv = MPE_Init_log();
  printf( "MPE_Init_log(): %d\n", retv );
  return TRUE;
}


static Int
p_start()                /* mpe_start */
{
  int retv;

  retv = MPE_Start_log();
  printf( "MPE_Start_log(): %d\n", retv );
  return TRUE;
}


static Int               /* mpe_close(+FileName) */
p_close()
{
  int retv;

  retv = MPE_Finish_log( "test.log" );
  printf( "MPE_Finish_log(): %d\n", retv );
  return TRUE;
}


static Int               /* mpe_create_event(-Event) */
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

  start_id = IntOfTerm(ARG1);
  end_id = IntOfTerm(ARG2);

  descr = "State";
  colour = "red";

  MPE_Describe_state( start_id, end_id, descr, colour );

  return TRUE;
}


static Int
p_log()                  /* mpe_log(+Event) */
{
  Int event_id;

  event_id = IntOfTerm(ARG1);
  MPE_Log_event( event_id, 0, "Event" );

  return TRUE;
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
  InitCPred( "mpe_close", 0, p_close, SafePredFlag );
  InitCPred( "mpe_create_event", 1, p_create_event, SafePredFlag );
  InitCPred( "mpe_create_state", 2, p_create_state, SafePredFlag );
  InitCPred( "mpe_log", 1, p_log, SafePredFlag );
}

#endif /* HAVE_MPE */
