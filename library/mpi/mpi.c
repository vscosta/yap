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
* Last rev:	$Date: 2002-10-03 17:28:37 $				 *
* mods:									 *
* comments:	Interface to an MPI library                              *
*									 *
*************************************************************************/

#ifndef lint
static char *rcsid = "$Header: /Users/vitor/Yap/yap-cvsbackup/library/mpi/mpi.c,v 1.8 2002-10-03 17:28:37 stasinos Exp $";
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

  tmp = malloc( bufsize + space );
  if( tmp == NULL ) {
    Error(SYSTEM_ERROR, TermNil, "out of memory" );
    exit_yap( EXIT_FAILURE );
  }
  memcpy( tmp, buf, bufsize );
  free( buf );
  buf = tmp;
#else /* use realloc */
  buf = realloc( buf, bufsize + space );
  if( buf == NULL ) {
    Error(SYSTEM_ERROR, TermNil, "out of memory");
    exit_yap( EXIT_FAILURE );
  }
#endif

  bufsize += space;
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

static void
clean_vars(VarEntry *p)
{
  if (p == NULL) return;
  p->VarAdr = TermNil;
  clean_vars(p->VarLeft);
  clean_vars(p->VarRight);
}

static Term
syntax_error (TokEntry * tokptr)
{
  Term info;
  int count = 0, out = 0;
  Int start, err = 0, end;
  Term tf[6];
  Term *error = tf+3;
  CELL *Hi = H;

  start = tokptr->TokPos;
  clean_vars(VarTable);
  clean_vars(AnonVarTable);
  while (1) {
    Term ts[2];

    if (H > ASP-1024) {
      H = Hi;
      tf[3] = TermNil;
      err = 0;
      end = 0;
      break;
    }
    if (tokptr == toktide) {
      err = tokptr->TokPos;
      out = count;
    }
    info = tokptr->TokInfo;
    switch (tokptr->Tok) {
    case Name_tok:
      {
	Term t0 = MkAtomTerm((Atom)info);
	ts[0] = MkApplTerm(MkFunctor(LookupAtom("atom"),1),1,&t0);
      }
      break;
    case Number_tok:
      ts[0] = MkApplTerm(MkFunctor(LookupAtom("number"),1),1,&(tokptr->TokInfo));
      break;
    case Var_tok:
      {
	Term t[3];
	VarEntry *varinfo = (VarEntry *)info;

	t[0] = MkIntTerm(0);
	t[1] = StringToList(varinfo->VarRep);
	if (varinfo->VarAdr == TermNil) {
	  t[2] = varinfo->VarAdr = MkVarTerm();
	} else {
	  t[2] = varinfo->VarAdr;
	}
	ts[0] = MkApplTerm(MkFunctor(LookupAtom("var"),3),3,t);
      }
      break;
    case String_tok:
      {
	Term t0 = StringToList((char *)info);
	ts[0] = MkApplTerm(MkFunctor(LookupAtom("string"),1),1,&t0);
      }
      break;
    case Ponctuation_tok:
      {
	char s[2];
	s[1] = '\0';
	if (Ord (info) == 'l') {
	  s[0] = '(';
	} else  {
	  s[0] = (char)info;
	}
	ts[0] = MkAtomTerm(LookupAtom(s));
      }
    }
    if (tokptr->Tok == Ord (eot_tok)) {
      *error = TermNil;
      end = tokptr->TokPos;
      break;
    }
    ts[1] = MkIntegerTerm(tokptr->TokPos);
    *error =
      MkPairTerm(MkApplTerm(MkFunctor(LookupAtom("-"),2),2,ts),TermNil);
    error = RepPair(*error)+1;
    count++;
    tokptr = tokptr->TokNext;
  }
  tf[0] = MkApplTerm(MkFunctor(LookupAtom("read"),1),1,&ARG2);
  {
    Term t[3];
    t[0] = MkIntegerTerm(start);
    t[1] = MkIntegerTerm(err);
    t[2] = MkIntegerTerm(end);
    tf[1] = MkApplTerm(MkFunctor(LookupAtom("between"),3),3,t);
  }
  tf[2] = MkAtomTerm(LookupAtom("\n<==== HERE ====>\n"));
  tf[4] = MkIntegerTerm(out);
  tf[5] = MkIntegerTerm(err);
  return(MkApplTerm(MkFunctor(LookupAtom("syntax_error"),6),6,tf));
}

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
      if (ParserErrorStyle == QUIET_ON_PARSER_ERROR) {
	/* just fail */
	return(FALSE);
      } else if (ParserErrorStyle == CONTINUE_ON_PARSER_ERROR) {
	ErrorMessage = NULL;
	/* try again */
	goto repeat_cycle;
      } else {
	Term terr = syntax_error(tokstart);
	if (ErrorMessage == NULL)
	  ErrorMessage = "SYNTAX ERROR";
	
	if (ParserErrorStyle == EXCEPTION_ON_PARSER_ERROR) {
	  Error(SYNTAX_ERROR,terr,ErrorMessage);
	  return(FALSE);
	} else /* FAIL ON PARSER ERROR */ {
	  Term t[2];
	  t[0] = terr;
	  t[1] = MkAtomTerm(LookupAtom(ErrorMessage));
	  return(unify(MkIntTerm(StartLine = tokstart->TokPos),ARG4) &&
		 unify(ARG5,MkApplTerm(MkFunctor(LookupAtom("error"),2),2,t)));
	}
      }

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
  bufstrlen = (size_t)bufptr;

  /* The buf is not NULL-terminated and does not have the
     trailing ". " required by the parser */
  mpi_putc( 0, '.' );
  mpi_putc( 0, ' ' );

  buf[bufptr] = 0;
  bufstrlen = bufptr + 1;
  bufptr = 0;

  /* first send the size */
  retv = MPI_Send( &bufstrlen, 1, MPI_INT, dest, tag, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) return FALSE;

  /* and then the data */
  retv = MPI_Send( &buf[bufptr], bufstrlen, MPI_CHAR, dest, tag, MPI_COMM_WORLD );
  if( retv != MPI_SUCCESS ) return FALSE;

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

  /* receive the size of the term */
  retv = MPI_Recv( &bufstrlen, 1, MPI_INT, orig, tag,
		   MPI_COMM_WORLD, &status );
  if( retv != MPI_SUCCESS ) {
    printf("BOOOOOOOM! retv == %d\n", retv);
    return FALSE;
  }

#if 1
  printf("About to receive %d chars from %d\n", bufstrlen, orig);
#endif

  /* adjust the buffer */
  if( bufsize < bufstrlen ) expand_buffer(bufstrlen-bufsize);

  /* Only the first packet can be from MPI_ANY_SOURCE */
  if( orig == MPI_ANY_SOURCE ) {
    orig = status.MPI_SOURCE;
    retv = unify(t_orig, MkIntTerm(orig));
    if( retv == FALSE ) puts( "PROBLEM1" );
  }

  /* Only the first packet can be of MPI_ANY_TAG */
  if( tag == MPI_ANY_TAG ) {
    tag = status.MPI_TAG;
    retv = unify(t_tag, MkIntTerm(status.MPI_TAG));
    if( retv == FALSE ) puts( "PROBLEM2" );
  }

  /* Receive the message as a C string */
  retv = MPI_Recv( &buf[bufptr], bufstrlen, MPI_CHAR, orig, tag,
		   MPI_COMM_WORLD, &status );
  if( retv != MPI_SUCCESS ) {
    printf("BOOOOOOOM! retv == %d\n", retv);
    return FALSE;
  }

#if 1
  {
    int aa;
    MPI_Get_count( &status, MPI_CHAR, &aa );
    printf("Received %d chars from %d\n", aa, orig);
  }
#endif

  /* parse received string into a Prolog term */
  bufptr = 0;
  retv = unify(t_data, mpi_parse());
#if 1
  printf("mpi_receive: t_data == %d, retv == %d\n", t_data, retv);
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
puts("1");
    plwrite( t_data, mpi_putc, 5 );
    /* NULL-terminate the string and add the ". " termination
       required by the parser. */
puts("1");
    buf[bufptr] = 0;
    strcat( buf, ". " );
    bufstrlen = bufptr + 2;
  }
  /* Otherwise, it must a variable */
  else {
    if( !IsVarTerm(t_data) ) {
      Error(INSTANTIATION_ERROR, t_data, "mpi_bcast");
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
    printf("expanding by %d\n", (bufstrlen-bufsize) );
    expand_buffer( bufstrlen - bufsize );
  }
  else {
    printf("bufstrlen: %d, bufsize %d: not expanding\n",bufstrlen,bufsize);
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

  InitCPred( "mpi_open", 3, p_mpi_open, SafePredFlag|SyncPredFlag );
  InitCPred( "mpi_close", 0, p_mpi_close, SyncPredFlag );
  InitCPred( "mpi_send", 3, p_mpi_send, SafePredFlag|SyncPredFlag );
  InitCPred( "mpi_receive", 3, p_mpi_receive, SafePredFlag|SyncPredFlag );
  InitCPred( "mpi_bcast", 3, p_mpi_bcast3, SafePredFlag|SyncPredFlag );
  InitCPred( "mpi_bcast", 2, p_mpi_bcast2, SafePredFlag|SyncPredFlag );
  InitCPred( "mpi_barrier", 0, p_mpi_barrier, SyncPredFlag );
}

#endif /* HAVE_MPI */
