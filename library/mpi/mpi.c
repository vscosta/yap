/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright S. Konstantopoulos and Universidade do Porto 2002-2003     	 *
*									 *
**************************************************************************
*									 *
* File:		mpi.c  							 *
* Last rev:	$Date: 2003-07-03 15:01:18 $				 *
* mods:									 *
* comments:	Interface to MPI libraries                               *
*									 *
*************************************************************************/

#ifndef lint
// static char *rcsid = "$Header: /Users/vitor/Yap/yap-cvsbackup/library/mpi/mpi.c,v 1.20 2003-07-03 15:01:18 stasinos Exp $";
#endif

#include "Yap.h"

/* Should we use MPI ? */
#if defined(HAVE_MPI_H) && (defined(HAVE_LIBMPI) || defined(HAVE_LIBMPICH))
 #define HAVE_MPI 1
#else
 #define HAVE_MPI 0
#endif

#if HAVE_MPI

#include "Yatom.h"
#include "yapio.h"

#include <stdlib.h>
#include <string.h>
#include <mpi.h>

void    YAP_Write(Term, void (*)(int), int);

static Int p_mpi_open( USES_REGS1 );
static Int p_mpi_close( USES_REGS1 );
static Int p_mpi_send( USES_REGS1 );
static Int p_mpi_receive( USES_REGS1 );
static Int p_mpi_bcast3( USES_REGS1 );
static Int p_mpi_bcast2( USES_REGS1 );
static Int p_mpi_barrier( USES_REGS1 );


/*
 * Auxiliary Data
 */

static int rank, numprocs, namelen;
static char processor_name[MPI_MAX_PROCESSOR_NAME];

static int mpi_argc;
static char **mpi_argv;

/* this should eventually be moved to config.h */
#define RECV_BUF_SIZE 1024*32


/* 
 * A simple stream
 */

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

  tmp = malloc( bufsize + space );
  if( tmp == NULL ) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "out of memory" );
    Yap_exit( EXIT_FAILURE );
  }
  memmove( tmp, buf, bufsize );
  free( buf );
  buf = tmp;
#else /* use realloc */
  buf = realloc( buf, bufsize + space );
  if( buf == NULL ) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "out of memory");
    Yap_exit( EXIT_FAILURE );
  }
#endif

  bufsize += space;
}

static void
mpi_putc(Int ch)
{
  if( ch > 0 ) {
    if( bufptr >= bufsize ) expand_buffer( RECV_BUF_SIZE );
    buf[bufptr++] = ch;
  }
}


/*
 * C Predicates
 */


static Int
p_mpi_open( USES_REGS1 )         /* mpi_open(?rank, ?num_procs, ?proc_name) */
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
Note that if MPI_Init() fails, Yap/MPICH and Yap/LAM behave differently:
in Yap/MPICH we are still at the Yap initialisation phase, so we get
Yap exit(FAILURE), whereas in Yap/LAM mpi_open/3 simply fails.
*/

  retv = MPI_Init( &mpi_argc, &mpi_argv );
  if( retv ) {
    Term t;

    t = MkIntegerTerm(retv);
    Yap_Error( SYSTEM_ERROR_INTERNAL, t, "MPI_Init() returned non-zero" );
    return FALSE;
  }
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );
  MPI_Get_processor_name( processor_name, &namelen );

  retv = Yap_unify(t_rank, MkIntTerm(rank));
  retv = retv && Yap_unify(t_numprocs, MkIntTerm(numprocs));
  retv = retv && Yap_unify(t_procname, MkAtomTerm(Yap_LookupAtom(processor_name)));

  return retv;
}


static Int               /* mpi_close */
p_mpi_close( USES_REGS1 )
{
  MPI_Finalize();
  return TRUE;
}


static Int
p_mpi_send( USES_REGS1 )             /* mpi_send(+data, +destination, +tag) */
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

  /* Turn the term into its ASCII representation */
  bufptr = 0;
  YAP_Write( t_data, mpi_putc, Quote_illegal_f|Handle_vars_f );

  /* The buf is not NULL-terminated and does not have the
     trailing ". " required by the parser */
  mpi_putc( '.' );
  mpi_putc( ' ' );
  mpi_putc( 0 );
  bufstrlen = strlen(buf);

  /* send the data */
  bufptr = 0;
  retv = MPI_Send( &buf[bufptr], bufstrlen, MPI_CHAR, dest, tag, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) return FALSE;

  return TRUE;
}


static Int
p_mpi_receive( USES_REGS1 )          /* mpi_receive(-data, ?orig, ?tag) */
{
  Term t, t_data = Deref(ARG1), t_orig = Deref(ARG2), t_tag = Deref(ARG3);
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
  retv = MPI_Recv( buf, bufstrlen, MPI_CHAR, orig, tag,
		   MPI_COMM_WORLD, &status );
  if( retv != MPI_SUCCESS ) {
    /* Getting in here would be weird; it means the first package
       (size) was sent properly, but there was a glitch with
       the actual content! */
    return FALSE;
  }

  /* parse received string into a Prolog term */

  bufptr = 0;
  t = YAP_ReadBuffer( buf, NULL );

  if( t == TermNil ) {
    retv = FALSE;
  }
  else {
    retv = Yap_unify(t, t_data);
  }

  return retv;
}


static Int
p_mpi_bcast3( USES_REGS1 )           /* mpi_bcast( ?data, +root, +max_size ) */
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
    /* Turn the term into its ASCII representation */
    bufptr = 0;
    YAP_Write( t_data, mpi_putc, Quote_illegal_f|Handle_vars_f );
    /* NULL-terminate the string and add the ". " termination
       required by the parser. */
    mpi_putc( '.' );
    mpi_putc( ' ' );
    mpi_putc( 0 );
    bufstrlen = strlen(buf);
  }

  /* The third argument must be bound to an integer (the maximum length
     of the broadcast term's ASCII representation */
  if (IsVarTerm(t_max_size)) {
    Yap_Error(INSTANTIATION_ERROR, t_max_size, "mpi_bcast");
    return FALSE;
  }
  /* allow for the ". " bit and the NULL at the end */
  max_size = IntOfTerm( t_max_size ) + 3;

  if( max_size < bufstrlen ) {
    /* issue a warning? explode? bcast s'thing unparsable? */
    printf( "MAYDAY: max_size == %d, bufstrlen == %d\n ", max_size, bufstrlen);
    return FALSE;
  }

  /* adjust the buffer size, if necessary */
  if( max_size > bufsize ) {
    expand_buffer( max_size-bufsize );
  }

  retv = MPI_Bcast( buf, max_size, MPI_CHAR, root, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) {
    printf( "OOOPS! MPI_Bcast() returned %d.\n", retv );
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
    return Yap_unify( YAP_ReadBuffer( buf, NULL ), ARG1 );
  }    
}


/*
  This is the same as above, but for dynamic data size.
  It is implemented as two broadcasts, the first being the size
  and the second the actual data.
*/

static Int
p_mpi_bcast2( USES_REGS1 )           /* mpi_bcast( ?data, +root ) */
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
    YAP_Write( t_data, mpi_putc, Quote_illegal_f|Handle_vars_f );
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
    expand_buffer( bufstrlen - bufsize );
  }
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

    return Yap_unify(YAP_ReadBuffer( buf, NULL ), ARG1);
  }
}


static Int
p_mpi_barrier( USES_REGS1 )            /* mpi_barrier/0 */
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

  mpi_argv = malloc( GLOBAL_argc * sizeof(char *) );
  mpi_argv[0] = strdup( GLOBAL_argv[0] );

  bufsize = RECV_BUF_SIZE;
  buf = malloc(bufsize * sizeof(char));

  for( i=1; i<GLOBAL_argc; ++i ) {
    if( !strcmp(GLOBAL_argv[i], "--") ) { ++i; break; }
  }
  for( j=1; i<GLOBAL_argc; ++i, ++j ) {
    mpi_argv[j] = strdup( GLOBAL_argv[i] );
  }
  mpi_argc = j;

  mpi_argv[0] = strdup( GLOBAL_argv[0] );

  Yap_InitCPred( "mpi_open", 3, p_mpi_open, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_close", 0, p_mpi_close, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_send", 3, p_mpi_send, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_receive", 3, p_mpi_receive, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_bcast", 3, p_mpi_bcast3, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_bcast", 2, p_mpi_bcast2, SafePredFlag|SyncPredFlag );
  Yap_InitCPred( "mpi_barrier", 0, p_mpi_barrier, SafePredFlag|SyncPredFlag );
}

#endif /* HAVE_MPI */
