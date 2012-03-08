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
* File:		write.c							 *
* Last rev:								 *
* mods:									 *
* comments:	Writing a Prolog Term					 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include <stdlib.h>
#include <math.h>
#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "clause.h"
#if COROUTINING
#include "attvar.h"
#endif

#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif

/* describe the type of the previous term to have been written */
typedef enum {
  separator,	/* the previous term was a separator like ',', ')', ... */
  alphanum,	/* the previous term was an atom or number */
  symbol        /* the previous term was a symbol like +, -, *, .... */
} wtype;

typedef  void      *wrf;

typedef struct union_slots {
  Int old;
  Int ptr;
} uslots;

typedef struct union_direct {
  Term old;
  CELL *ptr;
} udirect;


typedef  struct  rewind_term {
  struct rewind_term *parent;
  union {
    struct union_slots s;
    struct union_direct d;
  } u;
} rwts;

typedef struct write_globs {
  void     *stream;
  int      Quote_illegal, Ignore_ops, Handle_vars, Use_portray;
  int      Keep_terms;
  int      Write_Loops;
  int      Write_strings;
  UInt     MaxDepth, MaxArgs;
  wtype    lw;
} wglbs;

#define lastw wglb->lw

STATIC_PROTO(void wrputn, (Int, struct write_globs *));
STATIC_PROTO(void wrputf, (Float, struct write_globs *));
STATIC_PROTO(void wrputref, (CODEADDR, int, struct write_globs *));
STATIC_PROTO(int legalAtom, (unsigned char *));
/*STATIC_PROTO(int LeftOpToProtect, (Atom, int));
  STATIC_PROTO(int RightOpToProtect, (Atom, int));*/
STATIC_PROTO(wtype AtomIsSymbols, (unsigned char *));
STATIC_PROTO(void putAtom, (Atom, int, struct write_globs *));
STATIC_PROTO(void writeTerm, (Term, int, int, int, struct write_globs *, struct rewind_term *));

#define wrputc(X,WF)	Sputcode(X,WF)	/* writes a character */

static void 
wrputn(Int n, struct write_globs *wglb)	/* writes an integer	 */
                
{
  wrf stream = wglb->stream;
  char s[256], *s1=s; /* that should be enough for most integers */
  if (n < 0) {
    if (lastw == symbol)
      wrputc(' ', stream);  
  } else {
    if (lastw == alphanum)
      wrputc(' ', stream);
  }
#if HAVE_SNPRINTF
  snprintf(s, 256, Int_FORMAT, n);
#else
  sprintf(s, Int_FORMAT, n);
#endif
  while (*s1)
    wrputc(*s1++, stream);
  lastw = alphanum;
}

#define wrputs(s, stream) Sfputs(s, stream)

static void 
wrputws(wchar_t *s, wrf stream)		/* writes a string	 */
{
  while (*s)
    wrputc(*s++, stream);
}

#ifdef USE_GMP

static char *
ensure_space(size_t sz) {
  CACHE_REGS
  char *s;

  s = (char *) Yap_PreAllocCodeSpace();
  while (s+sz >= (char *)AuxSp) {
#if USE_SYSTEM_MALLOC
    /* may require stack expansion */
    if (!Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE)) {
      s = NULL;
      break;
    }
    s = (char *) Yap_PreAllocCodeSpace();
#else
    s = NULL;
#endif
  }
  if (!s) {
    s = (char *)TR;
    while (s+sz >= LOCAL_TrailTop) {
      if (!Yap_growtrail(sz/sizeof(CELL), FALSE)) {
	s = NULL;
	break;
      }
      s = (char *)TR;
    }
  }
  if (!s) {
    s = (char *)H;
    if (s+sz >= (char *)ASP) {
      Yap_Error(OUT_OF_STACK_ERROR,TermNil,"not enough space to write bignum: it requires %d bytes", sz);
      s = NULL;
    }
  }
  return s;
}

static void
write_mpint(MP_INT *big, struct write_globs *wglb) {
  char *s;

  s = ensure_space(3+mpz_sizeinbase(big, 10));
  if (mpz_sgn(big) < 0) {
    if (lastw == symbol)
      wrputc(' ', wglb->stream);  
  } else {
    if (lastw == alphanum)
      wrputc(' ', wglb->stream);
  }
  if (!s) {
    s = mpz_get_str(NULL, 10, big);
    if (!s)
      return;
    wrputs(s,wglb->stream);
    free(s);
  } else {
    mpz_get_str(s, 10, big);
    wrputs(s,wglb->stream);
  }
}
#endif

	/* writes a bignum	 */
static void 
writebig(Term t, int p, int depth, int rinfixarg, struct write_globs *wglb, struct rewind_term *rwt)
{
  CELL *pt = RepAppl(t)+1;
  CELL big_tag = pt[0];

#ifdef USE_GMP
  if (big_tag == BIG_INT) 
  {
    MP_INT *big = Yap_BigIntOfTerm(t);
    write_mpint(big, wglb);
    return;
  } else if (big_tag == BIG_RATIONAL) {
    Term trat = Yap_RatTermToApplTerm(t);
    writeTerm(trat, p, depth, rinfixarg, wglb, rwt);
    return;
  }
#endif
  if (big_tag == BLOB_STRING) {
    if (wglb->Write_strings)
      wrputc('`',wglb->stream);
    else
      wrputc('"',wglb->stream);
    wrputs(Yap_BlobStringOfTerm(t),wglb->stream);
    if (wglb->Write_strings)
      wrputc('`',wglb->stream);
    else
      wrputc('"',wglb->stream);
    return;
  } else if (big_tag == BLOB_WIDE_STRING) {
    wchar_t *s = Yap_BlobWideStringOfTerm(t);
    if (wglb->Write_strings)
      wrputc('`',wglb->stream);
    else
      wrputc('"', wglb->stream);
    while (*s) {
      wrputc(*s++, wglb->stream);
    }
    if (wglb->Write_strings)
      wrputc('`',wglb->stream);
    else
      wrputc('"',wglb->stream);
    return;
  } else if (big_tag >= USER_BLOB_START && big_tag < USER_BLOB_END) {
    Opaque_CallOnWrite f;
    CELL blob_info;

    blob_info = big_tag - USER_BLOB_START;
    if (GLOBAL_OpaqueHandlers &&
	(f= GLOBAL_OpaqueHandlers[blob_info].write_handler)) {
      (f)(wglb->stream, big_tag, (void *)((MP_INT *)(pt+1)), 0);
    }
  }
  wrputs("0",wglb->stream);
}	                  

static void 
wrputf(Float f, struct write_globs *wglb)	/* writes a float	 */
	                  
{
  char            s[256];
  wrf stream = wglb->stream;

#if HAVE_ISNAN || defined(__WIN32)
  if (isnan(f)) {
    wrputs("(nan)", stream);
    lastw = separator;
    return;
  }
#endif
#if HAVE_ISINF || defined(_WIN32)
    if (isinf(f)) {
      if (f < 0) {
	wrputs("(-inf)", stream);
      } else {
	wrputs("(+inf)", stream);
      }
      lastw = separator;
      return;
    }
#endif
#if THREADS
  /* old style writing */
  int found_dot = FALSE, found_exp = FALSE;
  char            *pt = s;
  int ch;

  if (lastw == symbol) {
    wrputc(' ', stream);  
  } else if (lastw == alphanum) {
    wrputc(' ', stream);
  }
  lastw = alphanum;
  //  sprintf(s, "%.15g", f);
  sprintf(s, RepAtom(AtomFloatFormat)->StrOfAE, f);
  while (*pt == ' ')
    pt++;
  if (*pt == '-') {
    wrputc('-', stream);
    pt++;
  }
  while ((ch = *pt) != '\0') {
    switch (ch) {
    case '.':
      found_dot = TRUE;
      wrputc('.', stream);    
      break;
    case 'e':
    case 'E':
      if (!found_dot) {
        found_dot = TRUE;
        wrputs(".0", stream);
      }
      found_exp = TRUE;
    default:
      wrputc(ch, stream);
    }
    pt++;
  }
  if (!found_dot) {
    wrputs(".0", stream);    
  }
#else
  char *format_float(double f, char *buf);
  char *buf;

  /* use SWI's format_float */
  buf = format_float(f, s);
  if (!buf) return;
  wrputs(buf, stream);
#endif
}

/* writes a data base reference */
static void 
wrputref(CODEADDR ref, int Quote_illegal, struct write_globs *wglb)
{
  char            s[256];
  wrf  stream = wglb->stream;

  putAtom(AtomDBref, Quote_illegal, wglb);
#if defined(__linux__) || defined(__APPLE__)
  sprintf(s, "(%p," UInt_FORMAT ")", ref, ((LogUpdClause*)ref)->ClRefCount);
#else
  sprintf(s, "(0x%p," UInt_FORMAT ")", ref, ((LogUpdClause*)ref)->ClRefCount);
#endif
  wrputs(s, stream);
  lastw = alphanum;
}

/* writes a blob (default) */
static void 
wrputblob(CODEADDR ref, int Quote_illegal, struct write_globs *wglb)
{
  char            s[256];
  wrf  stream = wglb->stream;

  putAtom(AtomSWIStream, Quote_illegal, wglb);
#if defined(__linux__) || defined(__APPLE__)
  sprintf(s, "(%p)", ref);
#else
  sprintf(s, "(0x%p)", ref);
#endif
  wrputs(s, stream);
  lastw = alphanum;
}

static int 
legalAtom(unsigned char *s)			/* Is this a legal atom ? */
{
  wchar_t ch = *s;

  if (ch == '\0')
    return FALSE;
  if (Yap_chtype[ch] != LC) {
    if (ch == '[') {
      return (s[1] == ']' && !s[2]);
    } else if (ch == '{') {
	return (s[1] == '}' && !s[2]);
    } else if (Yap_chtype[ch] == SL) {
      return (!s[1]);
    } else if ((ch == ',' || ch == '.') && !s[1]) {
      return FALSE;
    } else {
      if (ch == '/') {
	if (s[1] == '*') return FALSE;
      }
      while (ch) {
	if (Yap_chtype[ch] != SY) {
	  return FALSE;
	}
	ch = *++s;
      }
    }
    return TRUE;
  } else
    while ((ch = *++s) != 0)
      if (Yap_chtype[ch] > NU)
	return FALSE;
  return (TRUE);
}

static wtype 
AtomIsSymbols(unsigned char *s)		/* Is this atom just formed by symbols ? */
{
  int ch;
  if (Yap_chtype[(int)s[0]] == SL && s[1] == '\0')
    return(separator);
  while ((ch = *s++) != '\0') {
    if (Yap_chtype[ch] != SY)
      return(alphanum);
  }
  return(symbol);
}

static void
write_quoted(int ch, int quote, wrf stream)
{
  if (yap_flags[CHARACTER_ESCAPE_FLAG] == CPROLOG_CHARACTER_ESCAPES) {
    wrputc(ch, stream);
    if (ch == '\'')
      wrputc('\'', stream);	/* be careful about quotes */
    return;
  }
  if (!(ch < 0xff  && chtype(ch) == BS) && ch != '\'' && ch != '\\') {
    wrputc(ch, stream);
  } else {
    switch (ch) {
    case '\\':
    case '\'':
      wrputc('\\', stream);	
      wrputc(ch, stream);	
      break;
    case 7:
      wrputc('\\', stream);	
      wrputc('a', stream);	
      break;
    case '\b':
      wrputc('\\', stream);
      wrputc('b', stream);	
      break;
    case '\t':
      wrputc('\\', stream);
      wrputc('t', stream);	
      break;
    case ' ':
    case 160:
      wrputc(' ', stream);
      break;
    case '\n':
      wrputc('\\', stream);
      wrputc('n', stream);	
      break;
    case 11:
      wrputc('\\', stream);
      wrputc('v', stream);	
      break;
    case '\r':
      wrputc('\\', stream);
      wrputc('r', stream);	
      break;
    case '\f':
      wrputc('\\', stream);
      wrputc('f', stream);	
      break;
    default:
      if ( ch <= 0xff ) {
	char esc[8];
	
	if (yap_flags[CHARACTER_ESCAPE_FLAG] == SICSTUS_CHARACTER_ESCAPES) {
	  sprintf(esc, "\\%03o", ch);
	} else {
	  /* last backslash in ISO mode */
	  sprintf(esc, "\\%03o\\", ch);
	}
	wrputs(esc, stream);
      }
    }
  }
}


/* writes an atom	 */
static void 
putAtom(Atom atom, int Quote_illegal,  struct write_globs *wglb)
{
  unsigned char           *s;
  wtype          atom_or_symbol;
  wrf stream = wglb->stream;

  if (IsBlob(atom)) {
    wrputblob((CODEADDR)RepAtom(atom),wglb->Quote_illegal,wglb);
    return;
  }
  if (IsWideAtom(atom)) {
    wchar_t *ws = RepAtom(atom)->WStrOfAE;

    if (Quote_illegal) {
      wrputc('\'', stream);
      while (*ws) {
	wchar_t ch = *ws++;
	write_quoted(ch, '\'', stream);
      }
      wrputc('\'', stream);
    } else {
      wrputws(ws, stream);
    }
    return;
  }
  s  = (unsigned char *)RepAtom(atom)->StrOfAE;
  /* #define CRYPT_FOR_STEVE 1*/
#ifdef CRYPT_FOR_STEVE
  if (Yap_GetValue(AtomCryptAtoms) != TermNil && Yap_GetAProp(atom, OpProperty) == NIL) {
    char s[16];
    sprintf(s,"x%x", (CELL)s);
    wrputs(s, stream);
    return;
  }
#endif
  atom_or_symbol = AtomIsSymbols(s);
  if (lastw == atom_or_symbol && atom_or_symbol != separator /* solo */)
    wrputc(' ', stream);
  lastw = atom_or_symbol;
  if (Quote_illegal && !legalAtom(s)) {
    wrputc('\'', stream);
    while (*s) {
      wchar_t ch = *s++;
      write_quoted(ch, '\'', stream);
    }
    wrputc('\'', stream);
  } else {
    wrputs((char *)s, stream);
  }
}

static int 
IsStringTerm(Term string)	/* checks whether this is a string */
{
  if (IsVarTerm(string))
    return FALSE;
  do {
    Term hd;
    int ch;

    if (!IsPairTerm(string)) return(FALSE);
    hd = HeadOfTerm(string);
    if (IsVarTerm(hd)) return(FALSE);
    if (!IsIntTerm(hd)) return(FALSE);
    ch = IntOfTerm(HeadOfTerm(string));
    if ((ch < ' ' || ch > MAX_ISO_LATIN1) && ch != '\n' && ch != '\t')
      return(FALSE);
    string = TailOfTerm(string);
    if (IsVarTerm(string)) return(FALSE);
  } while (string != TermNil);
  return(TRUE);
}

/* writes a string	 */
static void 
putString(Term string, struct write_globs *wglb)
	                     
{
  wrf stream = wglb->stream;
  wrputc('"', stream);
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    write_quoted(ch, '"', stream);
    string = TailOfTerm(string);
  }
  wrputc('"', stream);
  lastw = alphanum;
}

/* writes a string	 */
static void 
putUnquotedString(Term string, struct write_globs *wglb)
	                     
{
  wrf stream = wglb->stream;
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch, stream);
    string = TailOfTerm(string);
  }
  lastw = alphanum;
}


static Term
from_pointer(CELL *ptr0, struct rewind_term *rwt, struct write_globs *wglb)
{
  CACHE_REGS
  Term t;
  CELL *ptr = ptr0;

  while (IsVarTerm(*ptr) && !IsUnboundVar(ptr))
    ptr = (CELL *)*ptr;
  t = *ptr;
  if (wglb->Keep_terms) {
    struct rewind_term *x = rwt->parent;

    rwt->u.s.old = Yap_InitSlot(t PASS_REGS);
    rwt->u.s.ptr = Yap_InitSlot((CELL)ptr0 PASS_REGS);
    if (!IsAtomicTerm(t) && !IsVarTerm(t)) {
      while (x) {
	if (Yap_GetDerefedFromSlot(x->u.s.old PASS_REGS) == t)
	  return TermFoundVar;
	x = x->parent;
      }
    }
  } else {
    rwt->u.d.old = t;
    rwt->u.d.ptr = ptr0;
    if (!IsAtomicTerm(t) && !IsVarTerm(t)) {
      struct rewind_term *x = rwt->parent;
      
      while (x) {
	if (x->u.d.old == t)
	  return TermFoundVar;
	x = x->parent;
      }
    }
  }
  return t;
}

static CELL *
restore_from_write(struct rewind_term *rwt, struct write_globs *wglb)
{
  CACHE_REGS
  CELL *ptr;

  if (wglb->Keep_terms) {
    ptr = (CELL*)Yap_GetPtrFromSlot(rwt->u.s.ptr PASS_REGS);
    Yap_RecoverSlots(2 PASS_REGS);
  } else {
    ptr = rwt->u.d.ptr;
  }
  rwt->u.s.ptr = 0;
  return ptr;
}

/* writes an unbound variable	 */
static void
write_var(CELL *t,  struct write_globs *wglb, struct rewind_term *rwt) 
{
  CACHE_REGS
  if (lastw == alphanum) {
    wrputc(' ', wglb->stream);
  }
  wrputc('_', wglb->stream);
  /* make sure we don't get no creepy spaces where they shouldn't be */
  lastw = separator;
  if (IsAttVar(t)) {
    Int vcount = (t-H0);
#if defined(COROUTINING) && defined(DEBUG)
    if (Yap_Portray_delays) {
      exts ext = ExtFromCell(t);
      struct rewind_term nrwt;
      nrwt.parent = rwt;
      nrwt.u.s.ptr = 0;

      Yap_Portray_delays = FALSE;
      if (ext == attvars_ext) {
	attvar_record *attv = RepAttVar(t);
	CELL *l = &attv->Value; /* dirty low-level hack, check atts.h */

	wrputs("$AT(",wglb->stream);
	write_var(t, wglb, rwt);
	wrputc(',', wglb->stream);      
	writeTerm(from_pointer(l, &nrwt, wglb), 999, 1, FALSE, wglb, &nrwt);
	l = restore_from_write(&nrwt, wglb);
	wrputc(',', wglb->stream);
	l += 2;
	writeTerm(from_pointer(l, &nrwt, wglb), 999, 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	wrputc(')', wglb->stream);
      }
      Yap_Portray_delays = TRUE;
      return;
    }
    wrputc('D', wglb->stream);
    wrputn(vcount,wglb);
#endif
  } else {
    wrputn(((Int) (t- H0)),wglb);
  }
}

static Term
check_infinite_loop(Term t, struct rewind_term *x, struct write_globs *wglb)
{
  CACHE_REGS
  if (wglb->Keep_terms) {
    while (x) {
      if (Yap_GetFromSlot(x->u.s.old PASS_REGS) == t)
	return TermFoundVar;
      x = x->parent;
    }
  } else {
    while (x) {
      if (x->u.d.old == t)
	return TermFoundVar;
	x = x->parent;
    }
  }
  return t;
}

static void
write_list(Term t, int direction, int depth, struct write_globs *wglb, struct rewind_term *rwt)
{
  CACHE_REGS
  Term ti;
  struct rewind_term nrwt;
  nrwt.parent = rwt;
  nrwt.u.s.ptr = 0;

  while (1) {
    int ndirection;
    int do_jump;

    writeTerm(from_pointer(RepPair(t), &nrwt, wglb), 999, depth+1, FALSE, wglb, &nrwt);
    t = AbsPair(restore_from_write(&nrwt, wglb));
    ti = TailOfTerm(t);
    if (IsVarTerm(ti))
      break;
    if (!IsPairTerm(ti) ||
	!IsPairTerm((ti = check_infinite_loop(ti, rwt, wglb))))
      break;
    ndirection = RepPair(ti)-RepPair(t);
    /* make sure we're not trapped in loops */
    if (ndirection > 0) {
      do_jump = (direction <= 0);
    } else if (ndirection == 0) {
      wrputc(',', wglb->stream);
      putAtom(AtomFoundVar, wglb->Quote_illegal, wglb);
      lastw = separator;
      return;
    } else {
      do_jump = (direction >= 0);
    }
    if (wglb->MaxDepth != 0 && depth > wglb->MaxDepth) {
      wrputc('|', wglb->stream);
      putAtom(Atom3Dots, wglb->Quote_illegal, wglb);
      return;
    }
    lastw = separator;
    direction = ndirection;
    depth++;
    if (do_jump)
      break;
    wrputc(',', wglb->stream);
    t = ti;
  }
  if (IsPairTerm(ti)) {
    Term nt = from_pointer(RepPair(t)+1, &nrwt, wglb);
    /* we found an infinite loop */
    if (IsAtomTerm(nt)) {
      wrputc('|', wglb->stream);      
      writeTerm(nt, 999, depth, FALSE, wglb, rwt);
    } else {
      /* keep going on the list */
      wrputc(',', wglb->stream);      
      write_list(nt, direction, depth, wglb, &nrwt);
    }
    restore_from_write(&nrwt, wglb);
  } else if (ti != MkAtomTerm(AtomNil)) {
    wrputc('|', wglb->stream);
    lastw = separator;
    writeTerm(from_pointer(RepPair(t)+1, &nrwt, wglb), 999, depth, FALSE, wglb, &nrwt);
    restore_from_write(&nrwt, wglb);
  }
 }

static void 
writeTerm(Term t, int p, int depth, int rinfixarg, struct write_globs *wglb, struct rewind_term *rwt)
/* term to write			 */
/* context priority			 */
                     
{
  CACHE_REGS
  struct rewind_term nrwt;
  nrwt.parent = rwt;
  nrwt.u.s.ptr = 0;

  if (wglb->MaxDepth != 0 && depth > wglb->MaxDepth) {
    putAtom(Atom3Dots, wglb->Quote_illegal, wglb);
    return;
  }
  if (EX)
    return;
  t = Deref(t);
  if (IsVarTerm(t)) {
    write_var((CELL *)t, wglb, &nrwt);
  } else if (IsIntTerm(t)) {
    wrputn((Int) IntOfTerm(t),wglb);
  } else if (IsAtomTerm(t)) {
    putAtom(AtomOfTerm(t), wglb->Quote_illegal, wglb);
  } else if (IsPairTerm(t)) {
    if (wglb->Ignore_ops) {
      wrputs("'.'(",wglb->stream);
      lastw = separator;
      writeTerm(from_pointer(RepPair(t),  &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
      t = AbsPair(restore_from_write(&nrwt, wglb));
      wrputs(",",wglb->stream);	
      writeTerm(from_pointer(RepPair(t)+1, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      wrputc(')', wglb->stream);
      lastw = separator;
      return;
    } 
    if (wglb->Use_portray) {
      Term targs[1];
      struct DB_TERM *old_EX = NULL;
      Int sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX) old_EX = EX;
      sl = Yap_InitSlot(t PASS_REGS);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs), 0, 1);
      t = Yap_GetFromSlot(sl PASS_REGS);
      Yap_RecoverSlots(1 PASS_REGS);
      if (old_EX != NULL) EX = old_EX;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue))
	return;
    }
    if (yap_flags[WRITE_QUOTED_STRING_FLAG] && IsStringTerm(t)) {
      putString(t, wglb);
    } else {
      wrputc('[', wglb->stream);
      lastw = separator;
      /* we assume t was already saved in the stack */ 
      write_list(t, 0, depth, wglb, rwt);
      wrputc(']', wglb->stream);
      lastw = separator;
    }
  } else {		/* compound term */
    Functor         functor = FunctorOfTerm(t);
    int             Arity;
    Atom            atom;
    int             op, lp, rp;

    if (IsExtensionFunctor(functor)) {
      switch((CELL)functor) {
      case (CELL)FunctorDouble:
	wrputf(FloatOfTerm(t),wglb);
	return;
      case (CELL)FunctorAttVar:	
	write_var(RepAppl(t)+1, wglb, &nrwt);
	return;
      case (CELL)FunctorDBRef:
	wrputref(RefOfTerm(t), wglb->Quote_illegal, wglb);
	return;
      case (CELL)FunctorLongInt:
	wrputn(LongIntOfTerm(t),wglb);
	return;
	/* case (CELL)FunctorBigInt: */
      default:
	writebig(t, p, depth, rinfixarg, wglb, rwt);
	return;
      }
    }
    Arity = ArityOfFunctor(functor);
    atom = NameOfFunctor(functor);
#ifdef SFUNC
    if (Arity == SFArity) {
      int             argno = 1;
      CELL           *p = ArgsOfSFTerm(t);
      putAtom(atom, wglb->Quote_illegal, wglb);
      wrputc('(', wglb->stream);
      lastw = separator;
      while (*p) {
	Int sl = 0;

	while (argno < *p) {
	  wrputc('_', wglb->stream), wrputc(',', wglb->stream);
	  ++argno;
	}
	*p++;
	lastw = separator;
	/* cannot use the term directly with the SBA */
	writeTerm(from_pointer(p, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	p = restore_from_write(&nrwt, wglb)+1;
	if (*p)
	  wrputc(',', wglb->stream);
	argno++;
      }
      wrputc(')', wglb->stream);
      lastw = separator;
      return;
    }
#endif
    if (wglb->Use_portray) {
      Term targs[1];
      struct DB_TERM *old_EX = NULL;
      Int sl = 0;

      targs[0] = t;
      Yap_PutValue(AtomPortray, MkAtomTerm(AtomNil));
      if (EX) old_EX = EX;
      sl = Yap_InitSlot(t PASS_REGS);      
      Yap_execute_goal(Yap_MkApplTerm(FunctorPortray, 1, targs),0, 1);
      t = Yap_GetFromSlot(sl PASS_REGS);
      Yap_RecoverSlots(1 PASS_REGS);
      if (old_EX) EX = old_EX;
      if (Yap_GetValue(AtomPortray) == MkAtomTerm(AtomTrue) || EX)
	return;
    }
    if (!wglb->Ignore_ops &&
	Arity == 1 &&  Yap_IsPrefixOp(atom, &op, &rp)
#ifdef DO_NOT_WRITE_PLUS_AND_MINUS_AS_PREFIX
	&&
	/* never write '+' and '-' as infix
	   operators */
	( (RepAtom(atom)->StrOfAE[0] != '+' &&
	   RepAtom(atom)->StrOfAE[0] != '-') ||
	  RepAtom(atom)->StrOfAE[1] )
#endif /* DO_NOT_WRITE_PLUS_AND_MINUS_AS_PREFIX */
	) {
      Term  tright = ArgOfTerm(1, t);
      int            bracket_right =
	!IsVarTerm(tright) && IsAtomTerm(tright) &&
	Yap_IsOp(AtomOfTerm(tright));
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->stream);
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      putAtom(atom, wglb->Quote_illegal, wglb);
      if (bracket_right) {
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), rp, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_right) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
      if (op > p) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops &&
	       Arity == 1 &&
	       Yap_IsPosfixOp(atom, &op, &lp)) {
      Term  tleft = ArgOfTerm(1, t);

      int            bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	Yap_IsOp(AtomOfTerm(tleft)); 
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->stream);
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), lp, depth + 1, rinfixarg, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_left) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
      putAtom(atom, wglb->Quote_illegal, wglb);
      if (op > p) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops &&
	       Arity == 2  && Yap_IsInfixOp(atom, &op, &lp,
						 &rp) ) {
      Term  tleft = ArgOfTerm(1, t);
      Term  tright = ArgOfTerm(2, t);
      int   bracket_left =
	!IsVarTerm(tleft) && IsAtomTerm(tleft) &&
	Yap_IsOp(AtomOfTerm(tleft));
      int   bracket_right =
	!IsVarTerm(tright) && IsAtomTerm(tright) &&
	Yap_IsOp(AtomOfTerm(tright));

      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	if (lastw != separator && !rinfixarg)
	  wrputc(' ', wglb->stream);
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      if (bracket_left) {
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), lp, depth + 1, rinfixarg, wglb, &nrwt);
      t = AbsAppl(restore_from_write(&nrwt, wglb)-1);
      if (bracket_left) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
      /* avoid quoting commas */
      if (strcmp(RepAtom(atom)->StrOfAE,","))
	putAtom(atom, wglb->Quote_illegal, wglb);
      else {
	wrputc(',', wglb->stream);
	lastw = separator;
      }
      if (bracket_right) {
	wrputc('(', wglb->stream);
	lastw = separator;
      }
      writeTerm(from_pointer(RepAppl(t)+2, &nrwt, wglb), rp, depth + 1, TRUE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_right) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
      if (op > p) {
	wrputc(')', wglb->stream);
	lastw = separator;
      }
    } else if (wglb->Handle_vars && functor == FunctorVar) {
      Term ti = ArgOfTerm(1, t);
      if (lastw == alphanum) {
	wrputc(' ', wglb->stream);
      }
      if (!IsVarTerm(ti) && (IsIntTerm(ti) || IsStringTerm(ti) || IsAtomTerm(ti))) {
	if (IsIntTerm(ti)) {
	  Int k = IntOfTerm(ti);
	  if (k == -1)  {
	    wrputc('_', wglb->stream);
	    lastw = alphanum;
	    return;
	  } else {
	    wrputc((k % 26) + 'A', wglb->stream);
	    if (k >= 26) {
	      /* make sure we don't get confused about our context */
	      lastw = separator;
	      wrputn( k / 26 ,wglb);
	    } else
	      lastw = alphanum;
	  }
	} else if (IsAtomTerm(ti)) {
	  putAtom(AtomOfTerm(ti), FALSE, wglb);
	} else {
	  putUnquotedString(ti, wglb);
	}
      } else {
	wrputs("'$VAR'(",wglb->stream);
	lastw = separator;
	writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	wrputc(')', wglb->stream);
	lastw = separator;
      }
    } else if (!wglb->Ignore_ops && functor == FunctorBraces) {
      wrputc('{', wglb->stream);
      lastw = separator;
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), 1200, depth + 1, FALSE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      wrputc('}', wglb->stream);
      lastw = separator;
    } else  if (atom == AtomArray) {
      wrputc('{', wglb->stream);
      lastw = separator;
      for (op = 1; op <= Arity; ++op) {
	if (op == wglb->MaxArgs) {
	  wrputs("...", wglb->stream);
	  break;
	}
	writeTerm(from_pointer(RepAppl(t)+op, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	t = AbsAppl(restore_from_write(&nrwt, wglb)-op);
	if (op != Arity) {
	  wrputc(',', wglb->stream);
	  lastw = separator;
	}
      }
      wrputc('}', wglb->stream);
      lastw = separator;
    } else {
      putAtom(atom, wglb->Quote_illegal, wglb);
      lastw = separator;
      wrputc('(', wglb->stream);
      for (op = 1; op <= Arity; ++op) {
	if (op == wglb->MaxArgs) {
	  wrputc('.', wglb->stream);
	  wrputc('.', wglb->stream);
	  wrputc('.', wglb->stream);
	  break;
	}
	writeTerm(from_pointer(RepAppl(t)+op, &nrwt, wglb), 999, depth + 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	if (op != Arity) {
	  wrputc(',', wglb->stream);
	  lastw = separator;
	}
      }
      wrputc(')', wglb->stream);
      lastw = separator;
    }
  }
}

void 
Yap_plwrite(Term t, void *mywrite, int max_depth, int flags, int priority)
     /* term to be written			 */
     /* consumer				 */
     /* write options			 */
{
  struct write_globs wglb;
  struct rewind_term rwt;

  if (!mywrite)
    wglb.stream = Serror;
  else
    wglb.stream = mywrite;

  wglb.lw = separator;
  wglb.Quote_illegal = flags & Quote_illegal_f;
  wglb.Handle_vars = flags & Handle_vars_f;
  wglb.Use_portray = flags & Use_portray_f;
  wglb.MaxDepth = max_depth;
  wglb.MaxArgs = max_depth;
  /* notice: we must have ASP well set when using portray, otherwise
     we cannot make recursive Prolog calls */
  wglb.Keep_terms = (flags & (Use_portray_f|To_heap_f)); 
  /* initialise wglb */
  rwt.parent = NULL;
  wglb.Ignore_ops = flags & Ignore_ops_f;
  wglb.Write_strings = flags & BackQuote_String_f;
  /* protect slots for portray */
  writeTerm(from_pointer(&t, &rwt, &wglb), priority, 1, FALSE, &wglb, &rwt);
  restore_from_write(&rwt, &wglb);
}

