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
* File:		mpi.c  							 *
* Last rev:	$Date: 2002-02-11 20:40:09 $						 *
* mods:									 *
* comments:	Interface to an MPI library                              *
*									 *
*************************************************************************/

#ifndef lint
static char *rcsid = "$Header: /Users/vitor/Yap/yap-cvsbackup/library/mpi/mpi.c,v 1.1 2002-02-11 20:40:09 stasinos Exp $";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "yapio.h"

/* for AtomEof */
#include "Heap.h"

#if HAVE_MPI

#include <stdlib.h>
#include <string.h>
#include <mpi.h>

STATIC_PROTO (Int p_mpi_open, (void));
STATIC_PROTO (Int p_mpi_close, (void));
STATIC_PROTO (Int p_mpi_send, (void));
STATIC_PROTO (Int p_mpi_receive, (void));


/*
 * Auxiliary Data and Functions
 */

static Int rank, numprocs, namelen;
static char processor_name[MPI_MAX_PROCESSOR_NAME];

/* mini-stream */

static char buf[255];
static int bufptr, buflen;

static int
mpi_putc(Int stream, Int ch)
{
  if( ch > 0 )
    buf[bufptr++] = ch;
  return ch;
}

static Int
mpi_getc(Int stream)
{
  return buf[bufptr++];
}

static Int
mpi_eob(void)
{
  return (bufptr<buflen) && (buf[bufptr] != EOF);
}


/*
 * C Predicates
 */


static Int
p_mpi_open(void)         /* mpi_open(?rank, ?num_procs, ?proc_name) */
{
  Term t_rank = ARG1, t_numprocs = ARG2, t_name = ARG3;

  unify( MkIntTerm(rank), t_rank );
  unify( MkIntTerm(numprocs), t_numprocs );
  unify( MkAtomTerm(LookupAtom(processor_name)), t_name );

  return TRUE;
}


static Int               /* mpi_close */
p_mpi_close()
{
  MPI_Finalize();
  return TRUE;
}



static Int
p_mpi_send()             /* mpi_send(+data, +destination, +tag) */
{
  Term t_data = Deref(ARG1), t_dest = Deref(ARG2), t_tag = Deref(ARG3);
  char *data;
  int tag, dest, retv;

  /* The first argument (data) must be bound */
  if (IsVarTerm(t_data)) {
    Error(INSTANTIATION_ERROR, t_data, "mpi_send");
    return (FALSE);
  }

  /* The second and third args must be bount to integers */
  if (IsVarTerm(t_dest)) {
    Error(INSTANTIATION_ERROR, t_dest, "mpi_send");
    return (FALSE);
  } else if( !IsIntegerTerm(t_dest) ) {
    Error(TYPE_ERROR_INTEGER, t_dest, "mpi_send");
    return (FALSE);
  } else {
    dest = IntOfTerm( t_dest );
  }
  if (IsVarTerm(t_tag)) {
    Error(INSTANTIATION_ERROR, t_tag, "mpi_send");
    return (FALSE);
  } else if( !IsIntegerTerm(t_tag) ) {
    Error(TYPE_ERROR_INTEGER, t_tag, "mpi_send");
    return (FALSE);
  } else {
    tag  = IntOfTerm( t_tag );
  }

  bufptr = 0;
  /* Turn the term into its ASCII representation */
  plwrite( t_data, mpi_putc, 5 );
  buf[bufptr] = 0;
  strcat( buf, ". " );

  retv = MPI_Send( &buf, 255, MPI_CHAR, dest, tag, MPI_COMM_WORLD );

  if( retv == 0 ) return TRUE;
  else return FALSE;
}



static Int
p_mpi_receive()          /* mpi_receive(-data, ?orig, ?tag) */
{
  Term t_orig = Deref(ARG2), t_tag = Deref(ARG3), t;
  int tag, orig, retv;
  MPI_Status status;

#if 0
  /* The second argument (source) must be bound to an integer
     (the rank of the source) or the atom mpi_any_source */
  if (IsVarTerm(t_orig)) {
    Error(INSTANTIATION_ERROR, t_orig, "mpi_receive");
    return (FALSE);
  } else if( IsAtomTerm(t_orig) ) {
    if( AtomOfTerm(t_orig) == LookupAtom("mpi_any_source") )
      orig = MPI_ANY_SOURCE;
    else {
      Error(TYPE_ERROR_ATOM, t_orig, "mpi_receive");
      return (FALSE);
    }
  } else if( !IsIntegerTerm(t_orig) ) {
    Error(TYPE_ERROR_INTEGER, t_orig, "mpi_receive");
    return (FALSE);
  } else {
    orig = IntOfTerm( t_orig );
  }
#else
  /* The second argument (source) must be bound to an integer
     (the rank of the source) or left unbound (i.e. any source
     is OK) */
  if (IsVarTerm(t_orig)) {
    orig = MPI_ANY_SOURCE;
  } else if( !IsIntegerTerm(t_orig) ) {
    Error(TYPE_ERROR_INTEGER, t_orig, "mpi_receive");
    return (FALSE);
  } else {
    orig = IntOfTerm( t_orig );
  }
#endif

  /* The third argument must be bound to an integer (the tag)
     or left unbound (i.e. any source is OK) */
  if (IsVarTerm(t_tag)) {
    tag = MPI_ANY_TAG;
  } else if( !IsIntegerTerm(t_tag) ) {
    Error(TYPE_ERROR_INTEGER, t_tag, "mpi_receive");
    return (FALSE);
  } else
    tag  = IntOfTerm( t_tag );

  /* Receive the message as a C string */
  retv = MPI_Recv( buf, 255, MPI_CHAR, orig, tag, MPI_COMM_WORLD, &status );
  if( retv != 0 ) return FALSE;
  if( orig == MPI_ANY_SOURCE ) unify(t_orig, MkIntTerm(status.MPI_SOURCE));
  if( tag == MPI_ANY_TAG ) unify(t_tag, MkIntTerm(status.MPI_TAG));

  buflen = strlen( buf );
  bufptr = 0;

  /* parse received string into a Prolog term */
  {
    Term v;
    TokEntry *tokstart, *fast_tokenizer (void);
    tr_fr_ptr old_TR;
    
    old_TR = TR;
    while( TRUE ) {
      CELL *old_H = H;

      /* Scans the term using stack space */
      eot_before_eof = FALSE;

      /* the first arg is the getc_for_read, diff only if CharConv is on */
      tokstart = tokptr = toktide = tokenizer( mpi_getc, mpi_getc );

      if ( mpi_eob() && !eot_before_eof) {
	if (tokstart != NIL && tokstart->Tok != Ord (eot_tok)) {
	  /* we got the end of file from an abort */
	  if (ErrorMessage == "Abort") {
	    TR = old_TR;
	    return(FALSE);
	  }
	  /* we need to force the next reading to also give end of file.*/
	  buf[bufptr] = EOF;
	  ErrorMessage = "[ Error: end of file found before end of term ]";
	} else {
	  /* restore TR */
	  TR = old_TR;
	  return (unify_constant (ARG2, MkAtomTerm (AtomEof)));
	}
      }
    repeat_cycle:
      if (ErrorMessage || (t = Parse ()) == 0) {
	if (ErrorMessage && (strcmp(ErrorMessage,"Stack Overflow") == 0)) {
	  /* ignore term we just built */
	  H = old_H;
	  if (growstack_in_parser(&old_TR, &tokstart, &VarTable)) {
	    tokptr = toktide = tokstart;
	    ErrorMessage = NULL;
	    goto repeat_cycle;
	  }
	}
	TR = old_TR;
	if (ErrorMessage)
	  YP_fprintf (YP_stderr, "%s", ErrorMessage);
	else
	  syntax_error (tokstart);
	YP_fprintf (YP_stderr, " ]\n");
	
	Error(SYSTEM_ERROR,TermNil,NULL);
	return(FALSE);

      } else {
	/* parsing succeeded */
	break;
      }
    }
    
    while (TRUE) {
      CELL *old_H = H;

      if (setjmp(IOBotch) == 0) {
	v = VarNames(VarTable, TermNil);
	TR = old_TR;
	break;
      } else {
	/* don't need to recheck tokens */
	tokstart = NULL;
	/* restart global */
	H = old_H;
	growstack_in_parser(&old_TR, &tokstart, &VarTable);
	old_H = H;
      }
    }
  }
    
  return unify(t, ARG1);
}


/*
 * Init
 */


void
InitMPI(void)
{
  int i,j;
  Int mpi_argc;
  char **mpi_argv;

  mpi_argv = malloc( yap_argc * sizeof(char *) );
  mpi_argv[0] = strdup( yap_args[0] );

  for( i=1; i<yap_argc; ++i ) {
    if( !strcmp(yap_args[i], "--") ) { ++i; break; }
  }
  for( j=1; i<yap_argc; ++i, ++j ) {
    mpi_argv[j] = strdup( yap_args[i] );
  }
  mpi_argc = j;

  mpi_argv[0] = strdup( yap_args[0] );

#if 0
  /* DEBUG */
  printf( "yap_argc = %d\n", yap_argc );
  for( i=0; i<yap_argc; ++i ) {
    printf( "%d %s\n", i, yap_args[i] );
  }
#endif

#if 0
  /* DEBUG */
  printf( "mpi_argc = %d\n", mpi_argc );
  for( i=0; i<mpi_argc; ++i ) {
    printf( "%d %s\n", i, mpi_argv[i] );
  }
#endif  

  MPI_Init( &mpi_argc, &mpi_argv );
  /* MPI_Init( &yap_argc, &yap_args ); */
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );
  MPI_Get_processor_name( processor_name, &namelen );

  InitCPred( "mpi_open", 3, p_mpi_open, SafePredFlag );
  InitCPred( "mpi_close", 0, p_mpi_close, SafePredFlag );
  InitCPred( "mpi_send", 3, p_mpi_send, SafePredFlag );
  InitCPred( "mpi_receive", 3, p_mpi_receive, SafePredFlag );
}

#endif /* HAVE_MPI */
