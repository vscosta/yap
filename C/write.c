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
#include "pl-shared.h"
#include "pl-utf8.h"

#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_LOCALE_H
#include <locale.h>
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
  } u_sd;
} rwts;

typedef struct write_globs {
  IOSTREAM*stream;
  int      Quote_illegal, Ignore_ops, Handle_vars, Use_portray, Portray_delays;
  int      Keep_terms;
  int      Write_Loops;
  int      Write_strings;
  int      last_atom_minus;
  UInt     MaxDepth, MaxArgs;
  wtype    lw;
} wglbs;

#define lastw wglb->lw
#define last_minus wglb->last_atom_minus

static bool
callPortray(Term t, struct DB_TERM **old_EXp USES_REGS)
{
  PredEntry *pe;
  Int b0 = LCL0-(CELL*)B;

  EX = NULL;
  if ( (pe = RepPredProp(Yap_GetPredPropByFunc(FunctorPortray, USER_MODULE) ) ) &&
       pe->OpcodeOfPred != FAIL_OPCODE &&
       pe->OpcodeOfPred != UNDEF_OPCODE &&
       Yap_execute_pred(pe, &t PASS_REGS) ) {
    choiceptr B0 = (choiceptr)(LCL0-b0);
    if (EX && !*old_EXp) *old_EXp = EX;
    Yap_fail_all(B0 PASS_REGS);
    return true;
  }
  if (EX && !*old_EXp) *old_EXp = EX;
  return false;
}
 
static void wrputn(Int, struct write_globs *);
static void wrputf(Float, struct write_globs *);
static void wrputref(CODEADDR, int, struct write_globs *);
static int legalAtom(unsigned char *);
/*static int LeftOpToProtect(Atom, int);
  static int RightOpToProtect(Atom, int);*/
static wtype AtomIsSymbols(unsigned char *);
static void putAtom(Atom, int, struct write_globs *);
static void writeTerm(Term, int, int, int, struct write_globs *, struct rewind_term *);

#define wrputc(X,WF)	Sputcode(X,WF)	/* writes a character */

/*
  protect bracket from merging with previoous character.
  avoid stuff like not (2,3) -> not(2,3) or
*/
static void
wropen_bracket(struct write_globs *wglb, int protect)
{
  wrf stream = wglb->stream;

  if (lastw != separator && protect)
    wrputc(' ', stream);
  wrputc('(', stream);  
  lastw = separator;
}

static void
wrclose_bracket(struct write_globs *wglb, int protect)
{
  wrf stream = wglb->stream;

  wrputc(')', stream);  
  lastw = separator;
}

static int
protect_open_number(struct write_globs *wglb, int lm, int minus_required)
{
  wrf stream = wglb->stream;

  if (lastw == symbol && lm && !minus_required) {
    wropen_bracket(wglb, TRUE);
    return TRUE;
  } else if (lastw == alphanum ||
	     (lastw == symbol && minus_required)) {
    wrputc(' ', stream);  
  }  
  return FALSE;
}

static void
protect_close_number(struct write_globs *wglb, int used_bracket)
{
  if (used_bracket) {
    wrclose_bracket(wglb, TRUE);
  } else {
    lastw = alphanum;
  }
  last_minus = FALSE;
}

static void 
wrputn(Int n, struct write_globs *wglb)	/* writes an integer	 */
                
{
  wrf stream = wglb->stream;
  char s[256], *s1=s; /* that should be enough for most integers */
  int has_minus = (n < 0);
  int ob;

  ob = protect_open_number(wglb, last_minus, has_minus);
#if HAVE_SNPRINTF
  snprintf(s, 256, Int_FORMAT, n);
#else
  sprintf(s, Int_FORMAT, n);
#endif
  while (*s1)
    wrputc(*s1++, stream);
  protect_close_number(wglb, ob);
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
    s = (char *)HR;
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
  int has_minus = mpz_sgn(big);
  int ob;

  s = ensure_space(3+mpz_sizeinbase(big, 10));
  ob = protect_open_number(wglb, last_minus, has_minus);
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
  protect_close_number(wglb, ob);
}
#endif

	/* writes a bignum	 */
static void 
writebig(Term t, int p, int depth, int rinfixarg, struct write_globs *wglb, struct rewind_term *rwt)
{
  CELL *pt = RepAppl(t)+1;
  CELL big_tag = pt[0];

  if (big_tag == ARRAY_INT || big_tag == ARRAY_FLOAT) {
    wrputc('{', wglb->stream);
    wrputs("...", wglb->stream);
    wrputc('}', wglb->stream);
    lastw = separator;
    return;
#ifdef USE_GMP
  } else if (big_tag == BIG_INT) 
  {
    MP_INT *big = Yap_BigIntOfTerm(t);
    write_mpint(big, wglb);
    return;
  } else if (big_tag == BIG_RATIONAL) {
    Term trat = Yap_RatTermToApplTerm(t);
    writeTerm(trat, p, depth, rinfixarg, wglb, rwt);
    return;
#endif
  } else if (big_tag >= USER_BLOB_START && big_tag < USER_BLOB_END) {
    Opaque_CallOnWrite f;
    CELL blob_info;

    blob_info = big_tag - USER_BLOB_START;
    if (GLOBAL_OpaqueHandlers &&
	(f= GLOBAL_OpaqueHandlers[blob_info].write_handler)) {
      (f)(wglb->stream, big_tag, ExternalBlobFromTerm(t), 0);
      return;
    }
  }
  wrputs("0",wglb->stream);
}	                  

static void 
wrputf(Float f, struct write_globs *wglb)	/* writes a float	 */
	                  
{
  char            s[256];
  wrf stream = wglb->stream;
  int sgn;
  int ob;


#if HAVE_ISNAN || defined(__WIN32)
  if (isnan(f)) {
    wrputs("(nan)", stream);
    lastw = separator;
    return;
  }
#endif
  sgn  = (f < 0.0);
#if HAVE_ISINF || defined(_WIN32)
  if (isinf(f)) {
    if (sgn) {
      wrputs("(-inf)", stream);
    } else {
      wrputs("(+inf)", stream);
    }
    lastw = separator;
    return;
  }
#endif
  ob = protect_open_number(wglb, last_minus, sgn);
#if THREADS
  /* old style writing */
  int found_dot = FALSE;
  char            *pt = s;
  int ch;
  /* always use C locale for writing numbers */
#if O_LOCALE
  const unsigned char *decimalpoint = (unsigned char*)
    localeconv()->decimal_point;
  size_t l1 = strlen((const char *)decimalpoint+1);
#else
  const unsigned char decimalpoint[2] = ".";
  size_t l1 = 0;
#endif

  if (lastw == symbol || lastw == alphanum) {
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
    // skip locale
    if (ch == decimalpoint[0] && !strncmp(pt+1, (char *)decimalpoint+1, l1)) {
      found_dot = TRUE;
      pt += l1;
      ch = '.';
    }
    if (ch == 'e' || ch == 'E') {
      if (!found_dot) {
        found_dot = TRUE;
	wrputs(".0" , stream);
      }
      found_dot = TRUE;
    }
    wrputc(ch, stream);
    pt++;
  }
  if (!found_dot) {
    wrputs(".0", stream);
  }
#else
  char *format_float(double f, char *buf);
  char *buf;

  if (lastw == symbol || lastw == alphanum) {
    wrputc(' ', stream);
  }
  /* use SWI's format_float */
  buf = format_float(f, s);
  if (!buf) return;
  wrputs(buf, stream);
#endif
  protect_close_number(wglb, ob);
}

int
Yap_FormatFloat( Float f, const char *s, size_t sz )
{
  struct write_globs wglb;
  char *ws = (char *)s;
  IOSTREAM *smem = Sopenmem(&ws, &sz, "w");
  wglb.stream = smem;
  wglb.lw = separator;
  wglb.last_atom_minus = FALSE;
  wrputf(f, &wglb);
  Sclose(smem); 
  return TRUE;
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
static int
wrputblob(AtomEntry * ref, int Quote_illegal, struct write_globs *wglb)
{
  char            s[256];
  wrf  stream = wglb->stream;
  PL_blob_t *type = RepBlobProp(ref->PropsOfAE)->blob_t;

  if (type->write) {
    atom_t at = YAP_SWIAtomFromAtom(AbsAtom(ref));
    return type->write(stream, at, 0);
  } else {
    putAtom(AtomSWIStream, Quote_illegal, wglb);
#if defined(__linux__) || defined(__APPLE__)
    sprintf(s, "(%p)", ref);
#else
    sprintf(s, "(0x%p)", ref);
#endif
    wrputs(s, stream);
  }
  lastw = alphanum;
  return 1;
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
      return alphanum;
  }
  return symbol;
}

static void
write_quoted(wchar_t ch, wchar_t quote, wrf stream)
{
  CACHE_REGS
  if (!(Yap_GetModuleEntry(CurrentModule)->flags & M_CHARESCAPE)) {
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
      wrputc('\\', stream);	
      wrputc('\\', stream);	
      break;
    case '\'':
      if (ch == quote)
	wrputc('\\', stream);	
      wrputc(ch, stream);	
      break;
    case '"':
      if (ch == quote)
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
	
	/* last backslash in ISO mode */
	sprintf(esc, "\\%03o\\", ch);
	wrputs(esc, stream);
      }
    }
  }
}

static void 
write_string(const char *s, struct write_globs *wglb)	/* writes an integer	 */
{
  IOSTREAM *stream = wglb->stream;
  int chr;
  char *ptr = (char *)s;
  
  if (wglb->Write_strings)
    wrputc('`', stream);
  else
    wrputc('"', stream);
  do {
    ptr = utf8_get_char(ptr, &chr);
    if (chr == '\0') break;
    write_quoted(chr, '"', stream);
  } while (TRUE);
  if (wglb->Write_strings)
    wrputc('`', stream);
  else
    wrputc('"', stream);
}


/* writes an atom	 */
static void 
putAtom(Atom atom, int Quote_illegal,  struct write_globs *wglb)
{
  unsigned char           *s;
  wtype          atom_or_symbol;
  wrf stream = wglb->stream;

  if (IsBlob(atom)) {
    wrputblob(RepAtom(atom),Quote_illegal,wglb);
    return;
  }
  if (IsWideAtom(atom) ) {
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
  /* if symbol then last_minus is important */
  last_minus = FALSE;
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

void
Yap_WriteAtom(IOSTREAM *s, Atom atom)
{
	struct write_globs wglb;
	wglb.stream = s;
	wglb.Quote_illegal = FALSE;
	putAtom(atom, 0, &wglb);
}

static int 
IsCodesTerm(Term string)	/* checks whether this is a string */
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
    wchar_t ch = IntOfTerm(HeadOfTerm(string));
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

    rwt->u_sd.s.old = Yap_InitSlot(t PASS_REGS);
    rwt->u_sd.s.ptr = Yap_InitSlot((CELL)ptr0 PASS_REGS);

    if (!IsAtomicTerm(t) && !IsVarTerm(t)) {
      while (x) {
	if (Yap_GetDerefedFromSlot(x->u_sd.s.old PASS_REGS) == t)
	  return TermFoundVar;
	x = x->parent;
      }
    }
  } else {
    rwt->u_sd.d.old = t;
    rwt->u_sd.d.ptr = ptr0;
    if ( !IsVarTerm(t) && !IsAtomicTerm(t)) {
      struct rewind_term *x = rwt->parent;
      
      while (x) {
	if (x->u_sd.d.old == t)
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
    ptr = (CELL*)Yap_GetPtrFromSlot(rwt->u_sd.s.ptr PASS_REGS);
    if (!Yap_RecoverSlots(2, rwt->u_sd.s.ptr PASS_REGS))
      return NULL;
  } else {
    ptr = rwt->u_sd.d.ptr;
  }
  rwt->u_sd.s.ptr = 0;
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
    if (wglb->Portray_delays) {
      exts ext = ExtFromCell(t);
      struct rewind_term nrwt;
      nrwt.parent = rwt;
      nrwt.u_sd.s.ptr = 0;

      wglb->Portray_delays = FALSE;
      if (ext == attvars_ext) {
	attvar_record *attv = RepAttVar(t);
	CELL *l = &attv->Value; /* dirty low-level hack, check atts.h */

	wrputs("$AT(",wglb->stream);
	write_var(t, wglb, rwt);
	wrputc(',', wglb->stream);      
	writeTerm(from_pointer(l, &nrwt, wglb), 999, 1, FALSE, wglb, &nrwt);
	l = restore_from_write(&nrwt, wglb);
	wrputc(',', wglb->stream);
	l ++;
	writeTerm(from_pointer(l, &nrwt, wglb), 999, 1, FALSE, wglb, &nrwt);
	restore_from_write(&nrwt, wglb);
	wrclose_bracket(wglb, TRUE);
      }
      wglb->Portray_delays = TRUE;
      return;
    }
    wrputc('D', wglb->stream);
    wrputn(vcount,wglb);
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
      if (Yap_GetFromSlot(x->u_sd.s.old PASS_REGS) == t)
	return TermFoundVar;
      x = x->parent;
    }
  } else {
    while (x) {
      if (x->u_sd.d.old == t)
	return TermFoundVar;
	x = x->parent;
    }
  }
  return t;
}

static void
write_list(Term t, int direction, int depth, struct write_globs *wglb, struct rewind_term *rwt)
{
  Term ti;
  struct rewind_term nrwt;
  nrwt.parent = rwt;
  nrwt.u_sd.s.ptr = 0;

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
  nrwt.u_sd.s.ptr = 0;

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
      wrclose_bracket(wglb, TRUE);
      return;
    } 
    if (wglb->Use_portray)
      if (callPortray(t, &EX PASS_REGS) ) return;
    if (yap_flags[WRITE_QUOTED_STRING_FLAG] && IsCodesTerm(t)) {
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
      case (CELL)FunctorString:
	write_string(StringOfTerm(t),wglb);
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
      wropen_bracket(wglb, FALSE);
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
      wrclose_bracket(wglb, TRUE);
      return;
    }
#endif
    if (wglb->Use_portray) {
      if (callPortray(t, &EX PASS_REGS) ) return;
    }
    if (!wglb->Ignore_ops &&
	Arity == 1 &&
	Yap_IsPrefixOp(atom, &op, &rp)
	) {
      Term  tright = ArgOfTerm(1, t);
      int            bracket_right =
	!IsVarTerm(tright) && IsAtomTerm(tright) &&
	Yap_IsOp(AtomOfTerm(tright));
      if (op > p) {
	wropen_bracket(wglb, TRUE);
      }
      putAtom(atom, wglb->Quote_illegal, wglb);
      if (bracket_right) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	wropen_bracket(wglb, TRUE);
      } else if (atom == AtomMinus) {
	last_minus = TRUE;
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), rp, depth + 1, TRUE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_right) {
	wrclose_bracket(wglb, TRUE);
      }
      if (op > p) {
	wrclose_bracket(wglb, TRUE);
      }
    } else if (!wglb->Ignore_ops &&
	       ( Arity == 1 || ((atom == AtomEmptyBrackets || atom == AtomEmptyCurlyBrackets || atom == AtomEmptySquareBrackets) && Yap_IsListTerm(ArgOfTerm(1, t)))) &&
	       Yap_IsPosfixOp(atom, &op, &lp)) {
      Term  tleft = ArgOfTerm(1, t);

      int            bracket_left, offset;

      if (Arity != 1) {
	tleft = ArgOfTerm(1, t);
	offset = 2;
      } else {
	tleft = ArgOfTerm(1, t);
	offset = 1;
      }
      bracket_left =
	!IsVarTerm(tleft) &&
	IsAtomTerm(tleft) &&
	Yap_IsOp(AtomOfTerm(tleft)); 
      if (op > p) {
	/* avoid stuff such as \+ (a,b) being written as \+(a,b) */
	wropen_bracket(wglb, TRUE);
      }
      if (bracket_left) {
	wropen_bracket(wglb, TRUE);
      }
      writeTerm(from_pointer(RepAppl(t)+offset, &nrwt, wglb), lp, depth + 1, rinfixarg, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_left) {
	wrclose_bracket(wglb, TRUE);
      }
      if (Arity > 1 ) {
	if (atom == AtomEmptyBrackets) {
	  wrputc('(', wglb->stream);  
	} else if (atom == AtomEmptySquareBrackets) {
	  wrputc('[', wglb->stream);  
	} else if (atom == AtomEmptyCurlyBrackets) {
	  wrputc('{', wglb->stream);  
	}
	lastw = separator;
	write_list(tleft, 0, depth, wglb, rwt);
	if (atom == AtomEmptyBrackets) {
	  wrputc(')', wglb->stream);  
	} else if (atom == AtomEmptySquareBrackets) {
	  wrputc(']', wglb->stream);  
	} else if (atom == AtomEmptyCurlyBrackets) {
	  wrputc('}', wglb->stream);  
	}
 	lastw = separator;
      } else {
	putAtom(atom, wglb->Quote_illegal, wglb);
      }
      if (op > p) {
	wrclose_bracket(wglb, TRUE);
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
	wropen_bracket(wglb, TRUE);
	lastw = separator;
      }
      if (bracket_left) {
	wropen_bracket(wglb, TRUE);
      }
      writeTerm(from_pointer(RepAppl(t)+1, &nrwt, wglb), lp, depth + 1, rinfixarg, wglb, &nrwt);
      t = AbsAppl(restore_from_write(&nrwt, wglb)-1);
      if (bracket_left) {
	wrclose_bracket(wglb, TRUE);
      }
      /* avoid quoting commas and bars */
      if (!strcmp(RepAtom(atom)->StrOfAE,",")) {
	wrputc(',', wglb->stream);
	lastw = separator;
      } else if (!strcmp(RepAtom(atom)->StrOfAE,"|")) {
	wrputc('|', wglb->stream);
	lastw = separator;
      } else
	putAtom(atom, wglb->Quote_illegal, wglb);
      if (bracket_right) {
	wropen_bracket(wglb, TRUE);
      }
      writeTerm(from_pointer(RepAppl(t)+2, &nrwt, wglb), rp, depth + 1, TRUE, wglb, &nrwt);
      restore_from_write(&nrwt, wglb);
      if (bracket_right) {
	wrclose_bracket(wglb, TRUE);
      }
      if (op > p) {
	wrclose_bracket(wglb, TRUE);
      }
    } else if (wglb->Handle_vars && functor == LOCAL_FunctorVar) {
      Term ti = ArgOfTerm(1, t);
      if (lastw == alphanum) {
	wrputc(' ', wglb->stream);
      }
      if (!IsVarTerm(ti) && (IsIntTerm(ti) || IsCodesTerm(ti) || IsAtomTerm(ti))) {
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
	wrclose_bracket(wglb, TRUE);
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
      wropen_bracket(wglb, FALSE);
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
      wrclose_bracket(wglb, TRUE);
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
  wglb.last_atom_minus = FALSE;
  wglb.Quote_illegal = flags & Quote_illegal_f;
  wglb.Handle_vars = flags & Handle_vars_f;
  wglb.Use_portray = flags & Use_portray_f;
  wglb.Portray_delays = flags & AttVar_Portray_f;
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

