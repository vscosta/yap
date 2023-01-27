/*************************************************************************
 *									 *
 *	 YAP Prolog * * Yap Prolog was developed at NCCUP -
 *	 Universidade do Porto * * Copyright L.Damas, V.S.Costa and
 *	 Universidade do Porto 1985-1997 * *
 **************************************************************************
 *									 *
 * File:		iopreds.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Input/Output C implemented predicates			 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file writeterm.c
 *
 * @brief I/O support for writing terms.
 *
 */


/**
 * @defgroup WriteTerm Term Writing in Prolog.
 * @ingroup InputOutput
 * @{
 */


#include "Yap.h"
#include "YapEval.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_SYS_SELECT_H && !_MSC_VER && !defined(__MINGW32__)
#include <sys/select.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#if HAVE_FCNTL_H
/* for O_BINARY and O_TEXT in WIN32 */
#include <fcntl.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif
#endif
#if !HAVE_STRNCAT
#define strncat(X, Y, Z) strcat(X, Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X, Y, Z) strcpy(X, Y)
#endif
#if _MSC_VER || defined(__MINGW32__)
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif
#include "iopreds.h"
#include "clause.h"

static Term readFromBuffer(const char *s, Term opts) {
  Term rval;
  int sno;
  encoding_t enc = ENC_ISO_UTF8;
  sno = Yap_open_buf_read_stream( NULL,
      (char *)s, strlen_utf8((unsigned char *)s), &enc, MEM_BUF_USER,
      Yap_LookupAtom(Yap_StrPrefix((char *)s, 16)), TermNone);

  rval = Yap_read_term(sno, opts, 3);
  Yap_CloseStream(sno);
  return rval;
}

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

static bool write_term(int output_stream, Term t, bool b, yap_error_number *errp, xarg *args USES_REGS) {
  bool rc;
  Term cm = CurrentModule;
  yhandle_t ynames=0;		
      int depths[3], flags = 0;


      Yap_plwrite(t, GLOBAL_Stream + output_stream, depths, HR, ynames, flags, args)
	      ;
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  rc = true;
  CurrentModule = cm;
       return rc;
}


#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t write_defs[] = {WRITE_DEFS()};
#undef PAR

/**
 *
 */
bool Yap_WriteTerm(int output_stream, Term t, Term opts USES_REGS) {

  bool o = true;
  yap_error_number err = YAP_NO_ERROR;
  int lvl = push_text_stack();
  xarg *args = NULL;
        yhandle_t y0 = Yap_StartHandles(),
	  yt = Yap_InitHandle(t),
	  yargs = Yap_InitHandle(opts);
  LOCK(GLOBAL_Stream[output_stream].streamlock);
args = Yap_ArgListToVector(opts, write_defs, WRITE_END,NULL,
                                   DOMAIN_ERROR_WRITE_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE)
      Yap_ThrowError(LOCAL_Error_TYPE, opts, NULL);
    CLOSE_LOCAL_STACKS_AND_RETURN(y0,lvl) false;
 }
  while (true) {
    if (err != YAP_NO_ERROR) {
      //      HR = VarOfTerm(Yap_GetFromHandle(ylow));
      //HB = B->cp_h;
      //      clean_tr(B->cp_tr+mytr PASS_REGS);
      Yap_CloseHandles(y0);

      if (err == RESOURCE_ERROR_TRAIL) {

	if (!Yap_growtrail(0, false)) {
	  Yap_ThrowError(RESOURCE_ERROR_TRAIL, TermNil, "while visiting terms");
	}
      } else if (err == RESOURCE_ERROR_STACK) {
	//    printf("In H0=%p Hb=%ld H=%ld G0=%ld GF=%ld ASP=%ld\n",H0, cs->oHB-H0,
	//     cs->oH-H0, ArenaPt(*arenap)-H0,ArenaLimit(*arenap)-H0,(LCL0-cs->oASP)-H0)  ;
	if (!Yap_dogcl(0 PASS_REGS)) {
	  Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	}
    //     printf("In H0=%p Hb=%ld H=%ld G0=%ld GF=%ld ASP=%ld\n",H0, cs->oHB-H0,
     //      cs->oH-H0, ArenaPt(*arenap)-H0,ArenaLimit(*arenap)-H0,LCL0-cs->oASP-H0)  ;
  }
      t = Yap_GetFromHandle(yt);
      opts = Yap_GetFromHandle(yargs);
      
    args = Yap_ArgListToVector(opts, write_defs, WRITE_END,NULL,
                                   DOMAIN_ERROR_WRITE_OPTION);
    if (args == NULL) {
      CLOSE_LOCAL_STACKS_AND_RETURN(y0,lvl) false;
    }
    }
    o = write_term(output_stream, t, false,&err, args PASS_REGS);
    if (o)
      break;
  }
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  CLOSE_LOCAL_STACKS_AND_RETURN(y0,lvl) o;
}

/** @pred  write_term(+ _T_, + _Opts_) is iso


Displays term  _T_ on the current output stream, according to the
following options:

+ quoted(+ _Bool_) is iso
If `true`, quote atoms if this would be necessary for the atom to
be recognized as an atom by YAP's parser. The default value is
`false`.

+ ignore_ops(+ _Bool_) is iso
If `true`, ignore operator declarations when writing the term. The
default value is `false`.

+ numbervars(+ _Bool_) is iso
If `true`, output terms of the form
`$VAR(N)`, where  _N_ is an integer, as a sequence of capital
letters. The default value is `false`.

+ variable_names(+ _List_) is iso
If `List` is a list of bindings
`Name = Var`, output `Var` as if bound to `Name`. If not all variables are
named, you can use the name_vars option to generate the missing variable names.

+ name_variables(+ _Bool_ )
This YAP option generates user readable names for
the term variables. It complements `variable_names`. 

+ portrayed(+ _Bool_)
If `true`, use <tt>portray/1</tt> to portray bound terms. The default
value is `false`.

+ portray(+ _Bool_)
If `true`, use <tt>portray/1</tt> to portray bound terms. The default
value is `false`.

+ max_depth(+ _Depth_)
If `Depth` is a positive integer, use <tt>Depth</tt> as
the maximum depth to portray a term. The default is `0`, that is,
unlimited depth.

+ priority(+ _Piority_)
If `Priority` is a positive integer smaller than `1200`,
give the context priority. The default is `1200`.

+ cycles(+ _Bool_)
Do not loop in rational trees (default).

+ (+ _Bool_)
Do not loop in rational trees (default).



*/
static Int write_term2(USES_REGS1) {

  /* '$write'(+Flags,?Term) */
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  return Yap_WriteTerm(LOCAL_c_output_stream, ARG1, ARG2 PASS_REGS);
}

/** @pred  write_term(+ _S_, + _T_, + _Opts_) is iso

Displays term  _T_ on the current output stream, according to the same
options used by `write_term/3`.


*/
static Int write_term3(USES_REGS1) {

  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    return false;
  }
	  return Yap_WriteTerm(output_stream, ARG2, ARG3 PASS_REGS);
}

/** @pred  write(+ _S_, _T_) is iso

vWrites term  _T_ to stream  _S_ instead of to the current output
stream. Singletons variables are written as underscores.


*/
static Int write2(USES_REGS1) {

  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */

  Term t = TermTrue;
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0)
    return false;
  Term opts  = 	  MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
			     MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t), TermNil));
  return Yap_WriteTerm(output_stream, ARG2, opts PASS_REGS);
}


/** @pred  write(+ _S_, _T_) is iso

Writes term  _T_ to the current output
stream. Singletons variables are written as underscores.
v

*/
static Int write1(USES_REGS1) {

  
  Term t = TermTrue;
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  Term opts  =
    MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t), TermNil));
  return Yap_WriteTerm(output_stream, ARG1, opts PASS_REGS);
}

/** @pred  write_canonical(+ _T_) is iso


Displays term  _T_ on the current output stream. Atoms are quoted
when necessary, and operators are ignored, that is, the term is written
in standard parenthesized prefix notation. Singles are written as underscores, and loops are broken.
*/
static Int write_canonical1(USES_REGS1) {

  Term t = TermTrue;
  Term t0 = MkIntTerm(0);
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorNameVariables,1,&t0),
			  MkPairTerm(Yap_MkApplTerm(FunctorIgnoreOps,1,&t),
				    MkPairTerm(Yap_MkApplTerm(FunctorQuoted,1,&t),
					      MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
							TermNil))));
  Int f = Yap_WriteTerm(output_stream, ARG1, opts PASS_REGS);
    return f;

}

/** @pred  write_canonical(+ _S_,+ _T_) is iso

Displays term  _T_ on the stream  _S_. Atoms are quoted when
necessary, and operators are ignored.


*/
static Int write_canonical(USES_REGS1) {
  Term t = TermTrue;
  Term t0 = MkIntTerm(0);
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    return false;
  }
  Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorNameVariables,1,&t0),
			  MkPairTerm(Yap_MkApplTerm(FunctorIgnoreOps,1,&t),
			     MkPairTerm(Yap_MkApplTerm(FunctorQuoted,1,&t),
			     MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t),
					      MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
							 TermNil)))));
 Int f = Yap_WriteTerm(output_stream, ARG2, opts PASS_REGS);
    return f;
}

static Int writeq1(USES_REGS1) {

  Term t = TermTrue, tf = TermFalse;
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  Term opts =
      MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
	       MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&tf),
			  MkPairTerm(Yap_MkApplTerm(FunctorQuoted,1,&t),
				     TermNil)));
  return Yap_WriteTerm(output_stream, ARG1, opts PASS_REGS);
}

/** @pred  writeq(+ _S_, ? _T_) is iso

As writeq/1, but the output is sent to the stream  _S_.


*/
static Int writeq(USES_REGS1) {

  Term t = TermTrue;
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    return false;
  }
  Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorQuoted,1,&t),

				     MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
				     TermNil));
  return Yap_WriteTerm(output_stream, ARG2, opts PASS_REGS);
}

static Int print1(USES_REGS1) {
  Term t = TermTrue;
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorPortray,1,&t),
			  MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t),
		  MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t),
			     TermNil)));
  return Yap_WriteTerm(output_stream, ARG1, opts PASS_REGS);
}

/** @pred  print(+ _S_, _T_)

Prints term  _T_ to the stream  _S_ instead of to the current output
stream.


*/
static Int print(USES_REGS1) {
  Term t = TermTrue;
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "write/2");
  if (output_stream < 0) {
    return false;
  }
  Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorPortray,1,&t),
			  MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t),
							TermNil));
  return Yap_WriteTerm(output_stream, ARG2, opts PASS_REGS);
}

/** @pred  display(+ _T_)

4
Displays term  _T_ on the current output stream. All Prolog terms are
written in standard parenthesized prefix notation.


*/
static Int display1(USES_REGS1) {
  Term t = TermTrue;
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  Term opts  =  MkPairTerm(Yap_MkApplTerm(FunctorIgnoreOps,1,&t),TermNil);
  return Yap_WriteTerm(output_stream, ARG1, opts PASS_REGS);
}

/** @pred  display(+ _S_, _T_)

Like display/1, but using stream  _S_ to display the term.



*/
static Int display(USES_REGS1) {
  Term t = TermTrue;
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "writeln/2");
  if (output_stream < 0) {
    return false;
  }
  Term opts  =  MkPairTerm(Yap_MkApplTerm(FunctorIgnoreOps,1,&t),TermNil);
  return Yap_WriteTerm(output_stream, ARG2, opts PASS_REGS);
}


static Int writeln1(USES_REGS1) {
  Term t = TermTrue;
  int output_stream = LOCAL_c_output_stream;
  if (output_stream == -1)
    output_stream = 1;
  Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorNl,1,&t),
			  MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t),
				     MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
						TermNil)));
  return Yap_WriteTerm(output_stream, ARG1, opts PASS_REGS);
}

static Int writeln(USES_REGS1) {
  Term t = TermTrue;
  int output_stream = Yap_CheckTextStream(ARG1, Output_Stream_f, "writeln/2");
  if (output_stream < 0) {
    return false;
  }
   Term opts  = MkPairTerm(Yap_MkApplTerm(FunctorNl,1,&t),

			   MkPairTerm(Yap_MkApplTerm(FunctorNumberVars,1,&t),
				     MkPairTerm(Yap_MkApplTerm(FunctorCycles,1,&t),
						TermNil)));
  return Yap_WriteTerm(output_stream, ARG2, opts PASS_REGS);
}

/**
 *
 * @pred write_depth( ?TermDepth, ?ListDepth, ?TermArity )
 *
 * Fine control over when to stop writing a term.
 * - number of terms above;
 * - number of previous items in a list;
 * - maximum arity.
 *
 */
static Int p_write_depth(USES_REGS1) { /* write_depth(Old,New)          */
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);


  if (IsVarTerm(t1)) {
    Yap_unify(ARG1, MkIntegerTerm(LOCAL_max_depth));    
  } else if (nat(t1)) {
    LOCAL_max_depth = IntegerOfTerm(t1);
  } else {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t1, "bad max term depth in write_depth/3");
    return false;
  }
  if (IsVarTerm(t2)) {
    Yap_unify(ARG2, MkIntegerTerm(LOCAL_max_list));    
  } else if (nat(t1)) {
    LOCAL_max_list = IntegerOfTerm(t2);
  } else {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t2, "bsd max list depth in write_depth/3");
    return false;
  }
  if (IsVarTerm(t3)) {
    Yap_unify(ARG3, MkIntegerTerm(LOCAL_max_args));    
  } else if (nat(t3)) {
    LOCAL_max_args = IntegerOfTerm(t3);
  } else {
    Yap_ThrowError(TYPE_ERROR_INTEGER, t3, "bad max_term arity in write_depth/3");
    return false;
  }
  return false;

}

static Int dollar_var(USES_REGS1) {
    Term t2;


   Term tv = Yap_MkNewApplTerm(FunctorDollarVar, 1);
   if(!Yap_unify(tv, ARG1)) return false;
    if (!IsVarTerm((t2 = Deref(ARG2)))) {
      if (IsApplTerm(t2) && ArityOfFunctor(FunctorOfTerm(t2)) == 1) {
	FunctorDollarVar= FunctorOfTerm(t2);
        return true;
      }
            Yap_ThrowError(TYPE_ERROR_COMPOUND, ARG2, "");
      return false;
    } else {
      Yap_ThrowError(INSTANTIATION_ERROR, ARG2, "");
    }
    return true;
}



static Int term_to_string(USES_REGS1) {
  Term t2 = Deref(ARG2), t1 = Deref(ARG1);
  const char *s;
  if (IsVarTerm(t2)) {
    s = Yap_TermToBuffer(t1, Quote_illegal_f | Number_vars_f);
    if (!s || !MkStringTerm(s)) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, t1,
                "Could not get memory from the operating system");
      return false;
    }
    return Yap_unify(ARG2, MkStringTerm(s));
  } else if (!IsStringTerm(t2)) {
    Yap_ThrowError(TYPE_ERROR_STRING, t2, "term_to_string/3");
    return false;
  } else {
    s = StringOfTerm(t2);
  }
  yhandle_t y0 = Yap_StartHandles();
  yhandle_t y1 = Yap_InitHandle( t1 );
  Term tf = readFromBuffer(s, TermNil);
  Int rc  = Yap_unify(tf, Yap_PopHandle(y1));
   Yap_CloseHandles(y0);
   return rc;
}

/**
 *
 * @return
 */
static Int term_to_atom(USES_REGS1) {
  Term t2 = Deref(ARG2), ctl, rc = false;
  Atom at;
  if (IsVarTerm(t2)) {
    const char *s =
        Yap_TermToBuffer(Deref(ARG1), Quote_illegal_f | Number_vars_f);
    if (!s || !(at = Yap_UTF8ToAtom((const unsigned char *)s PASS_REGS))) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, t2,
                "Could not get memory from the operating system");
      return false;
    }
    return Yap_unify(ARG2, MkAtomTerm(at));
  } else if (!IsAtomTerm(t2)) {
    Yap_ThrowError(TYPE_ERROR_ATOM, t2, "atom_to_term/2");
    return (FALSE);
  } else {
    at = AtomOfTerm(t2);
  }
  ctl = TermNil;
  return ((rc = Yap_UBufferToTerm(RepAtom(at)->UStrOfAE, ctl))) &&
         Yap_unify(rc, ARG1);
}

/**
 * @pred term_to_codes(Term, ListOfCodes)
 *
 * @arg A term
 * @arg A list of character codes
 * @return If _Term_ bound. generate a list of codes with the textual representation of the ternm;
 *         else, if _List_  is given, parse the text and unify with  _Term_ .
                                                                                                                                                                                                                                                                                                                                                                                                                                             *         else return an error.
 */
static Int term_to_codes(USES_REGS1) {
  Term t2 = Deref(ARG2), ctl, rc = false;
  if (IsVarTerm(t2)) {
    const char *s =
        Yap_TermToBuffer(Deref(ARG1), Quote_illegal_f | Number_vars_f);
    if (!s ) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, t2,
                "Could not get memory from the operating system");
      return false;
    }
      return Yap_unify(ARG2, Yap_CharsToListOfCodes(s, LOCAL_encoding));

  } else if (!Yap_IsListTerm(t2)) {
    Yap_ThrowError(TYPE_ERROR_LIST, t2, "atom_to_term/2");
    return (FALSE);
  }
seq_tv_t inp;
 inp.val.t  = t2;
 inp.type = YAP_STRING_ATOMS_CODES;
  unsigned char * buf = Yap_ListOfCodesToBuffer(NULL, t2, &inp PASS_REGS);
  ctl = TermNil;
  return (rc = Yap_UBufferToTerm(buf, ctl)) != 0L &&
    Yap_unify(rc, ARG1);
}



char *Yap_TermToBuffer(Term t, int flags) {
  CACHE_REGS
  int sno = Yap_open_buf_write_stream(LOCAL_Flags[ ENCODING_FLAG].at , flags);

  if (sno < 0)
    return NULL;
  if (t == 0)
    return NULL;
  else
    t = Deref(t);
  GLOBAL_Stream[sno].encoding = Yap_DefaultEncoding(); 
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  GLOBAL_Stream[sno].status &= ~FreeOnClose_Stream_f;
  int depths[3];
    depths[0] = LOCAL_max_depth;
      depths[1] =LOCAL_max_list;
      depths[2] = LOCAL_max_args;
      Yap_plwrite(t, GLOBAL_Stream + sno, depths,HR,0, flags, NULL);
  char *new = Yap_MemExportStreamPtr(sno);
  Yap_CloseStream(sno);
  return new;
}

void Yap_InitWriteTPreds(void) {
  Yap_InitCPred("write_term", 2, write_term2, SyncPredFlag);
  Yap_InitCPred("write_term", 3, write_term3, SyncPredFlag);
  Yap_InitCPred("write", 1, write1, SyncPredFlag);
  Yap_InitCPred("write", 2, write2, SyncPredFlag);
  Yap_InitCPred("writeq", 1, writeq1, SyncPredFlag);
  Yap_InitCPred("writeq", 2, writeq, SyncPredFlag);
  Yap_InitCPred("writeln", 1, writeln1, SyncPredFlag);
  Yap_InitCPred("writeln", 2, writeln, SyncPredFlag);
  Yap_InitCPred("write_canonical", 1, write_canonical1, SyncPredFlag);
  Yap_InitCPred("write_canonical", 2, write_canonical, SyncPredFlag);
  Yap_InitCPred("display", 1, display1, SyncPredFlag);
  Yap_InitCPred("display", 2, display, SyncPredFlag);
  Yap_InitCPred("print", 1, print1, SyncPredFlag);
  Yap_InitCPred("print", 2, print, SyncPredFlag);
  Yap_InitCPred("write_depth", 3, p_write_depth, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("term_to_string", 2, term_to_string, 0);
  Yap_InitCPred("term_to_atom", 2, term_to_atom, 0);
  Yap_InitCPred("term_to_codes", 2, term_to_codes, 0);
  Yap_InitCPred("write_depth", 3, p_write_depth, SafePredFlag | SyncPredFlag);
  ;
  Yap_InitCPred("current_dollar_var", 2, dollar_var, SafePredFlag);
  ;
}

/**
 * @}
 */
