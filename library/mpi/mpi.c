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
* Last rev:	$Date: 2002-03-13 09:01:39 $				 *
* mods:									 *
* comments:	Interface to an MPI library                              *
*									 *
*************************************************************************/

#ifndef lint
static char *rcsid = "$Header: /Users/vitor/Yap/yap-cvsbackup/library/mpi/mpi.c,v 1.7 2002-03-13 09:01:39 stasinos Exp $";
#endif

#include "Yap.h"

#if HAVE_MPI

#include "Yatom.h"
#include "yapio.h"

/* for AtomEof */
#include "Heap.h"

#include <stdlib.h>
#include <string.h>
#include <mpi.h>

STATIC_PROTO (Int p_mpi_open, (void));
STATIC_PROTO (Int p_mpi_close, (void));
STATIC_PROTO (Int p_mpi_send, (void));
STATIC_PROTO (Int p_mpi_receive, (void));
STATIC_PROTO (Int p_mpi_bcast3, (void));
STATIC_PROTO (Int p_mpi_bcast2, (void));
STATIC_PROTO (Int p_mpi_barrier, (void));


/*
 * Auxiliary Data and Functions
 */

static Int rank, numprocs, namelen;
static char processor_name[MPI_MAX_PROCESSOR_NAME];

static Int mpi_argc;
static char **mpi_argv;

/* mini-stream */

#define RECV_BUF_SIZE 4*1024

static size_t bufsize, bufstrlen;
static char *buf;
static int bufptr;

static void
expand_buffer( int space )
{

#if 1
  /* realloc has been SIGSEGV'ing on HP-UX 10.20.
     do i need to look into arcane allignment issues? */

  char *tmp;

#if 0
  printf( "expanding by %d...", space );
#endif

  tmp = malloc( bufsize + space );
  if( tmp == NULL ) {
    Error(SYSTEM_ERROR, TermNil, "out of memory" );
    exit_yap( EXIT_FAILURE );
  }
  memcpy( tmp, buf, bufsize );
  free( buf );
  buf = tmp;
#else
  buf = realloc( buf, bufsize + space );
  if( buf == NULL ) {
    Error(SYSTEM_ERROR, TermNil, "out of memory" );
    exit_yap( EXIT_FAILURE );
  }
#endif

  bufsize += space;

#if 0
  printf("SUCCESS\n");
  printf( "New bufsize: %d\n", bufsize );
#endif
}

static int
mpi_putc(Int stream, Int ch)
{
  if( ch > 0 ) {
    if( bufptr >= bufsize ) expand_buffer( RECV_BUF_SIZE );
    buf[bufptr++] = ch;
  }
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
  return (bufptr<bufstrlen) && (buf[bufptr] != EOF);
}


/* Term parser */

static Term
mpi_parse(void)
{
  Term v, t;
  TokEntry *tokstart;
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
	  return TermNil;
	}
	/* we need to force the next reading to also give end of file.*/
	buf[bufptr] = EOF;
	ErrorMessage = "[ Error: end of file found before end of term ]";
      } else {
	/* restore TR */
	TR = old_TR;
	if( unify_constant (ARG2, MkAtomTerm (AtomEof)) ) {
	  /* this might be a reasonable place to reach, but i don't know when */
	  puts("1XXXXXXXXXXXXXXXXXX");
	  return TermNil ;
	}
	else {
	  puts("2XXXXXXXXXXXXXXXXXX");
	  return TermNil;
	}
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
      return TermNil;

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
  return t;
}
    


/*
 * C Predicates
 */


static Int
p_mpi_open(void)         /* mpi_open(?rank, ?num_procs, ?proc_name) */
{
  Term t_rank = Deref(ARG1), t_numprocs = Deref(ARG2), t_procname = Deref(ARG3);
  Int retv;

  MPI_Init( &mpi_argc, &mpi_argv );
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );
  MPI_Get_processor_name( processor_name, &namelen );

  retv = unify(t_rank, MkIntTerm(rank));
  retv = retv && unify(t_numprocs, MkIntTerm(numprocs));
  retv = retv && unify(t_procname, MkAtomTerm(LookupAtom(processor_name)));

  return retv;
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
  bufstrlen = bufptr;
  bufptr = 0;

  while( bufstrlen-bufptr > 0 ) {
    int n;

    n = (bufstrlen-bufptr < RECV_BUF_SIZE)? (bufstrlen-bufptr) : RECV_BUF_SIZE;

    /* Careful: the buf is not NULL-terminated and does not have the
       trailing ". " required by the parser */
    retv = MPI_Send( &buf[bufptr], n, MPI_CHAR, dest, tag, MPI_COMM_WORLD );
    if( retv != 0 ) return FALSE;
    bufptr += n;
  }

  return TRUE;
}


static Int
p_mpi_receive()          /* mpi_receive(-data, ?orig, ?tag) */
{
  Term t_data = Deref(ARG1), t_orig = Deref(ARG2), t_tag = Deref(ARG3);
  int tag, orig, retv;
  MPI_Status status;

  /* The first argument (data) must be unbound */
  if(!IsVarTerm(t_data)) {
    Error(INSTANTIATION_ERROR, t_data, "mpi_receive");
    return FALSE;
  }

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

  /* The third argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  if (IsVarTerm(t_tag)) {
    tag = MPI_ANY_TAG;
  } else if( !IsIntegerTerm(t_tag) ) {
    Error(TYPE_ERROR_INTEGER, t_tag, "mpi_receive");
    return (FALSE);
  } else
    tag  = IntOfTerm( t_tag );

  bufptr = 0;
  while(TRUE) {
    int n;

    /* Receive the message as a C string */
    retv = MPI_Recv( &buf[bufptr], RECV_BUF_SIZE, MPI_CHAR, orig, tag,
		     MPI_COMM_WORLD, &status );
    if( retv != 0 ) return FALSE;
    MPI_Get_count( &status, MPI_CHAR, &n );
    bufptr += n;

    if( n == RECV_BUF_SIZE ) {
      /* if not enough space, expand buffer */
      if( bufsize - bufptr <= RECV_BUF_SIZE ) expand_buffer(RECV_BUF_SIZE);
    }
    else {
      /* we have gotten everything */
      break;
    }
  }

  if( bufsize - bufptr <= 3 ) expand_buffer(3);
  /* NULL-terminate the string and add the ". " termination
     required by the parser. */
  buf[bufptr] = 0;
  strcat( buf, ". " );
  bufstrlen = bufptr + 2;
  bufptr = 0;

  if( orig == MPI_ANY_SOURCE ) unify(t_orig, MkIntTerm(status.MPI_SOURCE));
  if( tag == MPI_ANY_TAG ) unify(t_tag, MkIntTerm(status.MPI_TAG));

  /* parse received string into a Prolog term */
  return unify(ARG1, mpi_parse());
}


static Int
p_mpi_bcast3()           /* mpi_bcast( ?data, +root, +max_size ) */
{
  Term t_data = Deref(ARG1), t_root = Deref(ARG2), t_max_size = Deref(ARG3);
  int root, retv, max_size;

  /* The second argument must be bound to an integer (the rank of
     root processor */
  if (IsVarTerm(t_root)) {
    Error(INSTANTIATION_ERROR, t_root, "mpi_bcast");
    return FALSE;
  }
  root = IntOfTerm( t_root );

  /*  If this is the root processor, then the first argument must
      be bound to the term to be sent. */
  if( root == rank ) {
    if( IsVarTerm(t_data) ) {
      Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }
    bufptr = 0;
    /* Turn the term into its ASCII representation */
    plwrite( t_data, mpi_putc, 5 );
    /* NULL-terminate the string and add the ". " termination
       required by the parser. */
    buf[bufptr] = 0;
    strcat( buf, ". " );
    bufstrlen = bufptr + 2;
  }

  /* The third argument must be bound to an integer (the maximum length
     of the broadcast term's ASCII representation */
  if (IsVarTerm(t_max_size)) {
    Error(INSTANTIATION_ERROR, t_max_size, "mpi_bcast");
    return FALSE;
  }
  /* allow for the ". " bit and the NULL at the end */
  max_size = IntOfTerm( t_max_size ) + 3;

#if 0
  if( (rank == root) && (max_size < bufstrlen) )
    /* issue a warning? explode? bcast s'thing unparsable? */
    printf( "MAYDAY: max_size == %d, bufstrlen == %d\n", max_size, bufstrlen );
    return FALSE;
  }
#endif
  printf( "%d: About to Bcast(): max_size == %d, bufstrlen == %d\n",
	  rank, max_size, bufstrlen );

  /* adjust the buffer size, if necessary */
  if( max_size > bufsize ) {
    expand_buffer( max_size - bufsize );
  }

  retv = MPI_Bcast( buf, max_size, MPI_CHAR, root, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) {
    printf( "OOOPS! MPI_Bcast() returned %d.\n", retv );
    return FALSE;
  }

  printf( "%d: I'm just after Bcast()ing. strlen(buf) == %d\n",
	  rank, strlen(buf) );

  if( root == rank ) return TRUE;
  else {
    /* ARG1 must be unbound so that it can receive data */
    if( !IsVarTerm(t_data) ) {
      Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }

    bufstrlen = strlen(buf);
    bufptr = 0;

    /* parse received string into a Prolog term */
    return unify(mpi_parse(), ARG1);
  }    
}


/* This is the same as above, but for dynamic data size.
   It is implemented as two broadcasts, the first being the size
   and the second the actual data.
*/
static Int
p_mpi_bcast2()           /* mpi_bcast( ?data, +root ) */
{
  Term t_data = Deref(ARG1), t_root = Deref(ARG2);
  int root, retv;

  /* The second argument must be bound to an integer (the rank of
     root processor */
  if (IsVarTerm(t_root)) {
    Error(INSTANTIATION_ERROR, t_root, "mpi_bcast");
    return FALSE;
  }
  root = IntOfTerm( t_root );

  /*  If this is the root processor, then the first argument must
      be bound to the term to be sent. */
  if( root == rank ) {
    if( IsVarTerm(t_data) ) {
      Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }
    bufptr = 0;
    /* Turn the term into its ASCII representation */
    plwrite( t_data, mpi_putc, 5 );
    /* NULL-terminate the string and add the ". " termination
       required by the parser. */
    buf[bufptr] = 0;
    strcat( buf, ". " );
    bufstrlen = bufptr + 2;
  }

  /* Broadcast the data size */
  retv = MPI_Bcast( &bufstrlen, sizeof bufstrlen, MPI_INT, root, MPI_COMM_WORLD );
  if( retv != 0 ) return FALSE;

  /* adjust the buffer size, if necessary */
  if( bufstrlen > bufsize ) {
    printf("expanding by %d\n", (bufstrlen-bufsize) );
    expand_buffer( bufstrlen - bufsize );
  }

  /* Broadcast the data */
  retv = MPI_Bcast( buf, bufstrlen, MPI_CHAR, root, MPI_COMM_WORLD );
  if( retv != 0 ) return FALSE;

  if( root == rank ) return TRUE;
  else {
    /* ARG1 must be unbound so that it can receive data */
    if( !IsVarTerm(t_data) ) {
      Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }

    bufstrlen = strlen(buf);
    bufptr = 0;

    /* parse received string into a Prolog term */
    return unify(mpi_parse(), ARG1);
  }    
}


static Int
p_mpi_barrier()            /* mpi_barrier/0 */
{
  int retv;

  retv = MPI_Barrier( MPI_COMM_WORLD );

  return (retv == 0);
}



/*
 * Init
 */


void
InitMPI(void)
{
  int i,j;

  mpi_argv = malloc( yap_argc * sizeof(char *) );
  mpi_argv[0] = strdup( yap_args[0] );

  bufsize = RECV_BUF_SIZE;
  buf = malloc(bufsize * sizeof(char));

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

  InitCPred( "mpi_open", 3, p_mpi_open, /*SafePredFlag|SyncPredFlag*/ 0 );
  InitCPred( "mpi_close", 0, p_mpi_close, SafePredFlag );
  InitCPred( "mpi_send", 3, p_mpi_send, SafePredFlag );
  InitCPred( "mpi_receive", 3, p_mpi_receive, SyncPredFlag );
  InitCPred( "mpi_bcast", 3, p_mpi_bcast3, SyncPredFlag );
  InitCPred( "mpi_bcast", 2, p_mpi_bcast2, SyncPredFlag );
  InitCPred( "mpi_barrier", 0, p_mpi_barrier, 0 );
}

#endif /* HAVE_MPI */
