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
* Last rev:	$Date: 2002-11-18 18:16:51 $				 *
* mods:									 *
* comments:	Interface to an MPI library                              *
*									 *
*************************************************************************/

#ifndef lint
static char *rcsid = "$Header: /Users/vitor/Yap/yap-cvsbackup/library/mpi/mpi.c,v 1.16 2002-11-18 18:16:51 vsc Exp $";
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

/* used by the parser */
static int StartLine;

static Int mpi_argc;
static char **mpi_argv;

/* mini-stream */

#define RECV_BUF_SIZE 1024*32

static size_t bufsize, bufstrlen;
static char *buf;
static int bufptr;

static void
expand_buffer( int space )
{
#if MPI_AVOID_REALLOC
  /*
    realloc() has been SIGSEGV'ing on HP-UX 10.20, but there is
    no problem in HP-UX 11.0. We can remove this bit here as soon
    as Yap stops compiling on 10.20 anyway. If removed, also remove
    the MPI_AVOID_REALLOC bits from configure.in and config.h.in
  */

  char *tmp;

#if 0
  printf( "expanding by %d (to %d)...", space, (bufsize+space));
#endif

  tmp = malloc( bufsize + space );
  if( tmp == NULL ) {
    Yap_Error(SYSTEM_ERROR, TermNil, "out of memory" );
    Yap_exit( EXIT_FAILURE );
  }
  memcpy( tmp, buf, bufsize );
#if 0
  printf("memcpy'd...");
#endif
  free( buf );
#if 0
  printf("free'd...");
#endif
  buf = tmp;
#else /* use realloc */
  buf = realloc( buf, bufsize + space );
#if 0
  printf("realloc'ed space...");
#endif
  if( buf == NULL ) {
    Yap_Error(SYSTEM_ERROR, TermNil, "out of memory");
    Yap_exit( EXIT_FAILURE );
  }
#endif

  bufsize += space;

#if 0
  printf("SUCCESS\n");
  printf( "New bufsize: %d\n", bufsize );
  buf[bufsize-space] = 0;
  printf("Buffer contents: %s\n", buf);
#endif
}

static int
mpi_putc(Int stream, Int ch)
{
#if 0
  printf("%d: PUTC %d a.k.a. %c at %d\n", rank, ch, (char)ch, bufptr);
#endif
  if( ch > 0 ) {
    if( bufptr >= bufsize ) expand_buffer( RECV_BUF_SIZE );
    buf[bufptr++] = ch;
  }
  return ch;
}

static Int
mpi_getc(Int stream)
{
#if 0
  printf("%d: GETC %c at %d\n", rank, buf[bufptr], bufptr);
#endif
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
  Term t;
  TokEntry *tokstart;
  tr_fr_ptr old_TR, TR_before_parse;
    
  old_TR = TR;
  while( TRUE ) {
    CELL *old_H;

    /* Scans the term using stack space */
    Yap_eot_before_eof = FALSE;

    /* the first arg is the getc_for_read, diff only if CharConv is on */
    tokstart = Yap_tokptr = Yap_toktide = Yap_tokenizer(mpi_getc, mpi_getc);

    /* preserve value of H after scanning: otherwise we may lose strings
       and floats */
    old_H = H;

    if ( mpi_eob() && !Yap_eot_before_eof) {
      if (tokstart != NIL && tokstart->Tok != Ord (eot_tok)) {
	/* we got the end of file from an abort */
	if (Yap_ErrorMessage == "Abort") {
	  TR = old_TR;
	  return TermNil;
	}
	/* we need to force the next reading to also give end of file.*/
	buf[bufptr] = EOF;
	Yap_ErrorMessage = "[ Error: end of file found before end of term ]";
      } else {
	/* restore TR */
	TR = old_TR;

	return (Yap_unify_constant(t, MkAtomTerm(AtomEof)));
      }
    }
  repeat_cycle:
    TR_before_parse = TR;
    if( Yap_ErrorMessage || (t = Yap_Parse())==0 ) {
      if (Yap_ErrorMessage && (strcmp(Yap_ErrorMessage,"Stack Overflow") == 0)) {
	/* ignore term we just built */
	TR = TR_before_parse;
	H = old_H;
	if (Yap_growstack_in_parser(&old_TR, &tokstart, &Yap_VarTable)) {
	  old_H = H;
	  Yap_tokptr = Yap_toktide = tokstart;
	  Yap_ErrorMessage = NULL;
	  goto repeat_cycle;
	}
      }
      TR = old_TR;

      /*
	behave as if ParserErrorStyle were QUIET_ON_PARSER_ERROR,
	(see iopreds.c), except with bombing Yap instead of simply
	failing the predicate: the parse cannot fail unless there
	is a problem with MPI or the pretty printer.
      */
      Yap_Error(SYSTEM_ERROR, TermNil, "Failed to parse MPI_Recv()'ed term" );
      Yap_exit( EXIT_FAILURE );

    } else {
      /* parsing succeeded */
      break;
    }
  }
    
  TR = old_TR;
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

/*
With MPICH MPI_Init() must be called during initialisation,
but with LAM it can be called from Prolog (mpi_open/3)

The symptoms match a known RedHat bug, see
http://email.osc.edu/pipermail/mpiexec/2002-July/000067.html
for a suggested workaround:
  Redhat have somehow broken their sem.h and ipc.h. If you use your own
  kernel, copy from ../src/kernel/include/asm & ../src/kernel/include/linux
  the file ipc.h and sem.h to /usr/include/sys, recompile your mpich and
  everything might start working.  (it did for us)
*/

/*
Note that if MPI_Init() fails, Yap/MPICH and Yap/LAM bahave differently:
in Yap/MPICH we are still at the Yap initialisation phase, so we let
Yap exit(FAILURE), whereas in Yap/LAM mpi_open/3 simply fails.
*/

#if ! HAVE_LIBMPICH
  retv = MPI_Init( &mpi_argc, &mpi_argv );
  if( retv ) {
    Term t;

    t = MkIntegerTerm(retv);
    Yap_Error( SYSTEM_ERROR, t, "MPI_Init() returned non-zero" );
    return FALSE;
  }
#endif
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );
  MPI_Get_processor_name( processor_name, &namelen );

  retv = Yap_unify(t_rank, MkIntTerm(rank));
  retv = retv && Yap_unify(t_numprocs, MkIntTerm(numprocs));
  retv = retv && Yap_unify(t_procname, MkAtomTerm(Yap_LookupAtom(processor_name)));

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
    Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_send");
    return (FALSE);
  }

  /* The second and third args must be bount to integers */
  if (IsVarTerm(t_dest)) {
    Yap_Error(INSTANTIATION_ERROR, t_dest, "mpi_send");
    return (FALSE);
  } else if( !IsIntegerTerm(t_dest) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_dest, "mpi_send");
    return (FALSE);
  } else {
    dest = IntOfTerm( t_dest );
  }
  if (IsVarTerm(t_tag)) {
    Yap_Error(INSTANTIATION_ERROR, t_tag, "mpi_send");
    return (FALSE);
  } else if( !IsIntegerTerm(t_tag) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_tag, "mpi_send");
    return (FALSE);
  } else {
    tag  = IntOfTerm( t_tag );
  }

  bufptr = 0;
  /* Turn the term into its ASCII representation */
  Yap_plwrite( t_data, mpi_putc, 5 );
  bufstrlen = (size_t)bufptr;

  /* The buf is not NULL-terminated and does not have the
     trailing ". " required by the parser */
  mpi_putc( 0, '.' );
  mpi_putc( 0, ' ' );

  buf[bufptr] = 0;
  bufstrlen = bufptr + 1;
  bufptr = 0;

#if 0
  {
    FILE *debug_out;
    debug_out = fopen("debug.out", "a");
    fprintf(debug_out, "%d: About to send %d chars to %d\n", 
	    rank, bufstrlen, dest);
    fclose(debug_out);
  }
#endif

  /* send the data */
  retv = MPI_Send( &buf[bufptr], bufstrlen, MPI_CHAR, dest, tag, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) return FALSE;

#if 0
  {
    FILE *debug_out;
    debug_out = fopen("debug.out", "a");
    fprintf(debug_out, "%d: Sent %s to %d\n", rank, &buf[bufptr], dest);
    fclose(debug_out);
  }
#endif

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
    Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_receive");
    return FALSE;
  }

  /* The second argument (source) must be bound to an integer
     (the rank of the source) or left unbound (i.e. any source
     is OK) */
  if (IsVarTerm(t_orig)) {
    orig = MPI_ANY_SOURCE;
  } else if( !IsIntegerTerm(t_orig) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_orig, "mpi_receive");
    return (FALSE);
  } else {
    orig = IntOfTerm( t_orig );
  }

  /* The third argument must be bound to an integer (the tag)
     or left unbound (i.e. any tag is OK) */
  if (IsVarTerm(t_tag)) {
    tag = MPI_ANY_TAG;
  } else if( !IsIntegerTerm(t_tag) ) {
    Yap_Error(TYPE_ERROR_INTEGER, t_tag, "mpi_receive");
    return (FALSE);
  } else
    tag  = IntOfTerm( t_tag );

  /* probe for the size of the term */
  retv = MPI_Probe( orig, tag, MPI_COMM_WORLD, &status );
  if( retv != MPI_SUCCESS ) {
    return FALSE;
  }
  MPI_Get_count( &status, MPI_CHAR, &bufstrlen );

#if 0
  {
    FILE *debug_out;
    debug_out = fopen("debug.out", "a");
    fprintf(debug_out, "%d: About to receive %d chars from %d\n", 
	    rank, bufstrlen, orig);
    fclose(debug_out);
  }
#endif

  /* adjust the buffer */
  if( bufsize < bufstrlen ) expand_buffer(bufstrlen-bufsize);

  /* Already know the source from MPI_Probe() */
  if( orig == MPI_ANY_SOURCE ) {
    orig = status.MPI_SOURCE;
    retv = Yap_unify(t_orig, MkIntTerm(orig));
    if( retv == FALSE ) {
      printf("PROBLEM: file %s, line %d\n", __FILE__, __LINE__);
    }
  }

  /* Already know the tag from MPI_Probe() */
  if( tag == MPI_ANY_TAG ) {
    tag = status.MPI_TAG;
    retv = Yap_unify(t_tag, MkIntTerm(status.MPI_TAG));
    if( retv == FALSE ) {
      printf("PROBLEM: file %s, line %d\n", __FILE__, __LINE__);
    } 
  }

  /* Receive the message as a C string */
  retv = MPI_Recv( &buf[bufptr], bufstrlen, MPI_CHAR, orig, tag,
		   MPI_COMM_WORLD, &status );
  if( retv != MPI_SUCCESS ) {
    /* Getting in here would be weird; it means the first package
       (size) was sent properly, but there was a glitch with
       the actual content! */
    return FALSE;
  }

#if 0
  {
    int aa;
    FILE *debug_out;
    MPI_Get_count( &status, MPI_CHAR, &aa );
    debug_out = fopen("debug.out", "a");
    fprintf(debug_out, "%d: Received %d chars from %d\n\
%d: The message was: %s\n", rank, aa, orig, rank, &buf[bufptr]);
    fclose(debug_out);
  }
#endif

  /* parse received string into a Prolog term */
  bufptr = 0;
  retv = Yap_unify(ARG1, mpi_parse());

#if 0
  /* check up on mpi_parse():
     convert the newly-parsed term back to text and print */
  bufptr = 0;
  Yap_plwrite( t_data, mpi_putc, 5 );
  mpi_putc( 0, '.' );
  mpi_putc( 0, ' ' );
  buf[bufptr] = 0;
  bufptr = 0;
  {
    FILE *debug_out;
    debug_out = fopen("debug.out", "a");
    fprintf(debug_out, "%d: mpi_receive: t_data == %d, retv == %d term == %s\n",
	    rank, t_data, retv, buf);
    fclose(debug_out);
  }
#endif

  return retv;
}


static Int
p_mpi_bcast3()           /* mpi_bcast( ?data, +root, +max_size ) */
{
  Term t_data = Deref(ARG1), t_root = Deref(ARG2), t_max_size = Deref(ARG3);
  int root, retv, max_size;

  /* The second argument must be bound to an integer (the rank of
     root processor */
  if (IsVarTerm(t_root)) {
    Yap_Error(INSTANTIATION_ERROR, t_root, "mpi_bcast");
    return FALSE;
  }
  root = IntOfTerm( t_root );

  /*  If this is the root processor, then the first argument must
      be bound to the term to be sent. */
  if( root == rank ) {
    if( IsVarTerm(t_data) ) {
      Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }
    bufptr = 0;
    /* Turn the term into its ASCII representation */
    Yap_plwrite( t_data, mpi_putc, 5 );
    /* NULL-terminate the string and add the ". " termination
       required by the parser. */
    buf[bufptr] = 0;
    strcat( buf, ". " );
    bufstrlen = bufptr + 2;
  }

  /* The third argument must be bound to an integer (the maximum length
     of the broadcast term's ASCII representation */
  if (IsVarTerm(t_max_size)) {
    Yap_Error(INSTANTIATION_ERROR, t_max_size, "mpi_bcast");
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
      Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }

    bufstrlen = strlen(buf);
    bufptr = 0;

    /* parse received string into a Prolog term */
    return Yap_unify(mpi_parse(), ARG1);
  }    
}


/*
  This is the same as above, but for dynamic data size.
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
    Yap_Error(INSTANTIATION_ERROR, t_root, "mpi_bcast");
    return FALSE;
  }
  root = IntOfTerm( t_root );


  /*  If this is the root processor, then the first argument must
      be bound to the term to be sent. */
  if( root == rank ) {
    if( IsVarTerm(t_data) ) {
      Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }
    bufptr = 0;
    /* Turn the term into its ASCII representation */
    Yap_plwrite( t_data, mpi_putc, 5 );
    /* NULL-terminate the string and add the ". " termination
       required by the parser. */
    buf[bufptr] = 0;
    strcat( buf, ". " );
    bufstrlen = bufptr + 2;
  }
  /* Otherwise, it must a variable */
  else {
    if( !IsVarTerm(t_data) ) {
      Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }
  }


  /* Broadcast the data size */
  retv = MPI_Bcast( &bufstrlen, sizeof bufstrlen, MPI_INT, root, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) {
    printf("PROBLEM: file %s, line %d\n", __FILE__, __LINE__);
    return FALSE;
  }

  /* adjust the buffer size, if necessary */
  if( bufstrlen > bufsize ) {
#if 1
    printf("expanding by %d\n", (bufstrlen-bufsize) );
#endif
    expand_buffer( bufstrlen - bufsize );
  }
#if 1
  else {
    printf("bufstrlen: %d, bufsize %d: not expanding\n",bufstrlen,bufsize);
  }
#endif
  /* Broadcast the data */
  retv = MPI_Bcast( buf, bufstrlen, MPI_CHAR, root, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) {
    printf("PROBLEM: file %s, line %d\n", __FILE__, __LINE__);
    return FALSE;
  }

  if( root == rank ) return TRUE;
  else {
    /* ARG1 must be unbound so that it can receive data */
    if( !IsVarTerm(t_data) ) {
      Yap_Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
      return FALSE;
    }

    bufstrlen = strlen(buf);
    bufptr = 0;

    /* parse received string into a Prolog term */
    return Yap_unify(ARG1, mpi_parse());
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
Yap_InitMPI(void)
{
  int i,j;

  mpi_argv = malloc( Yap_argc * sizeof(char *) );
  mpi_argv[0] = strdup( Yap_argv[0] );

  bufsize = RECV_BUF_SIZE;
  buf = malloc(bufsize * sizeof(char));

  for( i=1; i<Yap_argc; ++i ) {
    if( !strcmp(Yap_argv[i], "--") ) { ++i; break; }
  }
  for( j=1; i<Yap_argc; ++i, ++j ) {
    mpi_argv[j] = strdup( Yap_argv[i] );
  }
  mpi_argc = j;

  mpi_argv[0] = strdup( Yap_argv[0] );

#if 0
  /* DEBUG */
  printf( "Yap_argc = %d\n", Yap_argc );
  for( i=0; i<Yap_argc; ++i ) {
    printf( "%d %s\n", i, Yap_argv[i] );
  }
#endif

#if 0
  /* DEBUG */
  printf( "mpi_argc = %d\n", mpi_argc );
  for( i=0; i<mpi_argc; ++i ) {
    printf( "%d %s\n", i, mpi_argv[i] );
  }
#endif  

  /* With MPICH MPI_Yap_Init() must be called during initialisation,
     but with LAM it can be called from Prolog (mpi_open/3).
     See also the comment at "if ! HAVE_LIBMPICH" above!
  */
#if HAVE_LIBMPICH
  {
    int retv;

    retv = MPI_Init(&mpi_argc, &mpi_argv);
    if( retv ) {
      Term t;

      t = MkIntegerTerm(retv);
      Yap_Error(SYSTEM_ERROR, t, "MPI_Init() returned non-zero");
      Yap_exit( EXIT_FAILURE );
    }
#if 0
    /* DEBUG */
    else {
      puts("MPI_Init() is happy!");
    }
#endif
  }
#endif

  Yap_InitCPred( "mpi_open", 3, p_mpi_open, SyncPredFlag );
  Yap_InitCPred( "mpi_close", 0, p_mpi_close, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_send", 3, p_mpi_send, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_receive", 3, p_mpi_receive, SyncPredFlag );
  Yap_InitCPred( "mpi_bcast", 3, p_mpi_bcast3, SyncPredFlag );
  Yap_InitCPred( "mpi_bcast", 2, p_mpi_bcast2, SyncPredFlag );
  Yap_InitCPred( "mpi_barrier", 0, p_mpi_barrier, SyncPredFlag );
}

#endif /* HAVE_MPI */
