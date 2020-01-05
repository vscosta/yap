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
 * File:		write.c * Last
 *rev:								 * mods:
 ** comments:	Writing a Prolog Term					 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "clause.h"
#include "yapio.h"
#include <math.h>
#include <stdlib.h>
#if COROUTINING
#include "attvar.h"
#endif
#include "iopreds.h"

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
  start,     /* initialization */
  separator, /* the previous term was a separator like ',', ')', ... */
  alphanum,  /* the previous term was an atom or number */
  symbol     /* the previous term was a symbol like +, -, *, .... */
} wtype;

typedef StreamDesc *wrf;

typedef struct union_slots {
  Int old;
  Int ptr;
} uslots;

typedef struct union_direct {
  Term old;
  CELL *ptr;
} udirect;

typedef struct write_globs {
  StreamDesc *stream;
  bool Quote_illegal, Ignore_ops, Handle_vars, Use_portray, Portray_delays;
  bool Keep_terms;
  bool Write_Loops;
  bool Write_strings;
  UInt  last_atom_minus;
  UInt MaxDepth, MaxArgs;
int trailings;
  wtype lw;
} wglbs;

#define lastw wglb->lw
#define last_minus wglb->last_atom_minus

static bool callPortray(Term t, int sno USES_REGS) {
  PredEntry *pe;
  Int b0 = LCL0 - (CELL *)B;

  UNLOCK(GLOBAL_Stream[sno].streamlock);
  if ((pe = RepPredProp(Yap_GetPredPropByFunc(FunctorPortray, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, &t, true PASS_REGS)) {
    choiceptr B0 = (choiceptr)(LCL0 - b0);
    Yap_fail_all(B0 PASS_REGS);
    LOCK(GLOBAL_Stream[sno].streamlock);
    return true;
  }
  LOCK(GLOBAL_Stream[sno].streamlock);

  return false;
}

#define PROTECT(t, F)                                                          \
  {                                                                            \
    yhandle_t yt = Yap_InitHandle(t);                                          \
    F;                                                                         \
    t = Yap_PopHandle(yt);                                                     \
  }
static void wrputn(Int, struct write_globs *);
static void wrputf(Float, struct write_globs *);
static void wrputref(void *, int, struct write_globs *);
static int legalAtom(unsigned char *);
/*static int LeftOpToProtect(Atom, int);
  static int RightOpToProtect(Atom, int);*/
static wtype AtomIsSymbols(unsigned char *);
static void putAtom(Atom, int, struct write_globs *);
static void writeTerm(Term, int, int, int, struct write_globs *);

#define wrputc(WF, X)                                                          \
  (X)->stream_wputc(X - GLOBAL_Stream, WF) /* writes a character */

/*
  protect bracket from merging with previoous character.
  avoid stuff like not (2,3) -> not(2,3) or
*/
static void wropen_bracket(struct write_globs *wglb, int protect) {
  StreamDesc *stream = wglb->stream;

  if (lastw != separator && protect)
    wrputc(' ', stream);
  wrputc('(', stream);
  lastw = separator;
}

static void wrclose_bracket(struct write_globs *wglb, int protect) {
  wrf stream = wglb->stream;

  wrputc(')', stream);
  lastw = separator;
}

static int protect_open_number(struct write_globs *wglb, int lm,
                               int minus_required) {
  wrf stream = wglb->stream;

  if (lastw == symbol && lm && !minus_required) {
    wropen_bracket(wglb, TRUE);
    return TRUE;
  } else if (lastw == alphanum || (lastw == symbol && minus_required)) {
    wrputc(' ', stream);
  }
  return FALSE;
}

static void protect_close_number(struct write_globs *wglb, int used_bracket) {
  if (used_bracket) {
    wrclose_bracket(wglb, TRUE);
  } else {
    lastw = alphanum;
  }
  last_minus = FALSE;
}

static void wrputn(Int n,
                   struct write_globs *wglb) /* writes an integer	 */
{
  wrf stream = wglb->stream;
  char s[256], *s1 = s; /* that should be enough for most integers */
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

inline static void wrputs(char *s, StreamDesc *stream) {
  int c;
  while ((c = *s++))
    wrputc(c, stream);
}

#ifdef USE_GMP

static char *ensure_space(size_t sz) {
  CACHE_REGS
  char *s;

  s = (char *)Yap_PreAllocCodeSpace();
  while (s + sz >= (char *)AuxSp) {
#if USE_SYSTEM_MALLOC
    /* may require stack expansion */
    if (!Yap_ExpandPreAllocCodeSpace(sz, NULL, TRUE)) {
      s = NULL;
      break;
    }
    s = (char *)Yap_PreAllocCodeSpace();
#else
    s = NULL;
#endif
  }
  if (!s) {
    s = (char *)TR;
    while (s + sz >= LOCAL_TrailTop) {
      if (!Yap_growtrail(sz / sizeof(CELL), FALSE)) {
        s = NULL;
        break;
      }
      s = (char *)TR;
    }
  }
  if (!s) {
    s = (char *)HR;
    if (s + sz >= (char *)ASP) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil,
                "not enough space to write bignum: it requires %d bytes", sz);
      s = NULL;
    }
  }
  return s;
}

static void write_mpint(MP_INT *big, struct write_globs *wglb) {
  char *s;
  int has_minus = mpz_sgn(big);
  int ob;

  s = ensure_space(3 + mpz_sizeinbase(big, 10));
  ob = protect_open_number(wglb, last_minus, has_minus);
  if (!s) {
    s = mpz_get_str(NULL, 10, big);
    if (!s)
      return;
    wrputs(s, wglb->stream);
    free(s);
  } else {
    mpz_get_str(s, 10, big);
    wrputs(s, wglb->stream);
  }
  protect_close_number(wglb, ob);
}
#endif

/* writes a bignum	 */
static void writebig(Term t, int p, int depth, int rinfixarg,
                     struct write_globs *wglb) {
  CELL *pt = RepAppl(t) + 1;
  CELL big_tag = pt[0];

  if (big_tag == ARRAY_INT || big_tag == ARRAY_FLOAT) {
    wrputc('{', wglb->stream);
    wrputs("...", wglb->stream);
    wrputc('}', wglb->stream);
    lastw = separator;
    return;
#ifdef USE_GMP
  } else if (big_tag == BIG_INT) {
    MP_INT *big = Yap_BigIntOfTerm(t);
    write_mpint(big, wglb);
    return;
  } else if (big_tag == BIG_RATIONAL) {
    Term trat = Yap_RatTermToApplTerm(t);
    writeTerm(trat, p, depth, rinfixarg, wglb);
    return;
#endif
  } else if (big_tag >= USER_BLOB_START && big_tag < USER_BLOB_END) {
    YAP_Opaque_CallOnWrite f;
    CELL blob_info;

    blob_info = big_tag;
    if (GLOBAL_OpaqueHandlers &&
        (f = GLOBAL_OpaqueHandlers[blob_info].write_handler)) {
      (f)(wglb->stream->file, big_tag, ExternalBlobFromTerm(t), 0);
      return;
    }
  }
  wrputs("0", wglb->stream);
}

static void wrputf(Float f, struct write_globs *wglb) /* writes a float	 */

{
#if THREADS
  char s[256];
#endif
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
  sgn = (f < 0.0);
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
  char *pt = s;
  int ch;
  /* always use C locale for writing numbers */
#if O_LOCALE
  const unsigned char *decimalpoint =
      (unsigned char *)localeconv()->decimal_point;
  size_t l1 = strlen((const char *)decimalpoint + 1);
#else
  const unsigned char decimalpoint[2] = ".";
  size_t l1 = 0;
#endif

  if (lastw == symbol || lastw == alphanum) {
    wrputc(' ', stream);
  }
  lastw = alphanum;
  //  sprintf(s, "%.15g", f);
  sprintf(s, floatFormat(), f);
  while (*pt == ' ')
    pt++;
  if (*pt == '-') {
    wrputc('-', stream);
    pt++;
  }
  while ((ch = *pt) != '\0') {
    // skip locale
    if (ch == decimalpoint[0] &&
        !strncmp(pt + 1, (char *)decimalpoint + 1, l1)) {
      found_dot = TRUE;
      pt += l1;
      ch = '.';
    }
    if (ch == 'e' || ch == 'E') {
      if (!found_dot) {
        found_dot = TRUE;
        wrputs(".0", stream);
      }
      found_dot = true;
    }
    wrputc(ch, stream);
    pt++;
  }
  if (!found_dot) {
    wrputs(".0", stream);
  }
#else
  char buf[256];

  if (lastw == symbol || lastw == alphanum) {
    wrputc(' ', stream);
  }
  /* use SWI's format_float */
  sprintf(buf, (char *)floatFormat(), f);

  wrputs(buf, stream);
#endif
  protect_close_number(wglb, ob);
}

int Yap_FormatFloat(Float f, char **s, size_t sz) {
  CACHE_REGS
  struct write_globs wglb;
  int sno;

  sno = Yap_open_buf_write_stream(GLOBAL_Stream[LOCAL_c_output_stream].encoding,
                                  0);
  if (sno < 0)
    return false;
  wglb.lw = separator;
  wglb.stream = GLOBAL_Stream + sno;
  wrputf(f, &wglb);
  *s = Yap_MemExportStreamPtr(sno);
  Yap_CloseStream(sno);
  return true;
}

/* writes a data base reference */
static void wrputref(void *ref, int Quote_illegal,
                     struct write_globs *wglb) {
  char s[256];
  wrf stream = wglb->stream;

  putAtom(AtomDBref, Quote_illegal, wglb);
#if  defined(__linux__) || defined(__APPLE__)
#if 1
  snprintf(s, 255, "(%p)", ref);
#else
  sprintf(s, "(%p," UInt_FORMAT ")", ref, ((LogUpdClause *)ref)->ClRefCount);
  #endif
#else
#if 1
  snprintf(s, 255, "(0x%p)", ref);
  #else
  sprintf(s, "(0x%p," UInt_FORMAT ")", ref, ((LogUpdClause *)ref)->ClRefCount);
  #endif
#endif
  wrputs(s, stream);
  lastw = alphanum;
}

/* writes a blob (default) */
static int wrputblob(AtomEntry *ref, int Quote_illegal,
                     struct write_globs *wglb) {
  wrf stream = wglb->stream;
  int rc;
  int Yap_write_blob(AtomEntry * ref, StreamDesc * stream);

  if ((rc = Yap_write_blob(ref, stream))) {
    return rc;
  }
  lastw = alphanum;
  return 1;
}

static int legalAtom(unsigned char *s) /* Is this a legal atom ? */
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
    } else if (ch == '`') {
      return false;
    } else if ((ch == ',' || ch == '.') && !s[1]) {
      return false;
    } else {
      if (ch == '/') {
        if (s[1] == '*')
          return false;
      }
      while (ch) {
        if (Yap_chtype[ch] != SY) {
          return false;
        }
        ch = *++s;
      }
    }
    return true;
  } else
    while ((ch = *++s) != 0)
      if (Yap_chtype[ch] > NU)
        return false;
  return true;
}

static wtype
AtomIsSymbols(unsigned char *s) /* Is this atom just formed by symbols ? */
{
  int ch;
  if (Yap_chtype[(int)s[0]] == SL && s[1] == '\0')
    return (separator);
  while ((ch = *s++) != '\0') {
    if (Yap_chtype[ch] != SY)
      return alphanum;
  }
  return symbol;
}

static void write_quoted(wchar_t ch, wchar_t quote, wrf stream) {
  CACHE_REGS
  if (!(Yap_GetModuleEntry(CurrentModule)->flags & M_CHARESCAPE)) {
      wrputc(ch, stream);
      if (ch == '\'')
          wrputc('\'', stream); /* be careful about quotes */
      return;
  }
  if (!(ch < 0xff && chtype(ch) == BS) && ch != '\'' && ch != '\\' &&
      ch != '`') {
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
    case '`':
      if (ch == quote)
        wrputc('`', stream);
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
      if (ch <= 0xff) {
        char esc[8];

        /* last backslash in ISO mode */
        sprintf(esc, "\\%03o\\", ch);
        wrputs(esc, stream);
      }
    }
  }
}

static void write_string(const unsigned char *s,
                         struct write_globs *wglb) /* writes an integer	 */
{
  StreamDesc *stream = wglb->stream;
  utf8proc_int32_t chr, qt;
  unsigned char *ptr = (unsigned char *)s;

  if (wglb->Write_strings)
    qt = '`';
  else
    qt = '"';
  wrputc(qt, stream);
  do {
    int delta;
    ptr += (delta = get_utf8(ptr, -1, &chr));

    if (chr == '\0') {
      break;
    }
    if (delta == 0) {
      chr = *ptr++;
    }
    write_quoted(chr, qt, stream);
  } while (true);
  wrputc(qt, stream);
}

/* writes an atom	 */
static void putAtom(Atom atom, int Quote_illegal, struct write_globs *wglb) {
  unsigned char *s;
  wtype atom_or_symbol;
  wrf stream = wglb->stream;
  if (atom == NULL) return;
  s = RepAtom(atom)->UStrOfAE;
  if (s[0] ==  '\0') {
    if (Quote_illegal) {
    wrputc('\'', stream);
    wrputc('\'', stream);
    }
    return;
  }
  if (IsBlob(atom)) {
    wrputblob(RepAtom(atom), Quote_illegal, wglb);
    return;
  }
  /* #define CRYPT_FOR_STEVE 1*/
#ifdef CRYPT_FOR_STEVE
  if (Yap_GetValue(AtomCryptAtoms) != TermNil &&
      Yap_GetAProp(atom, OpProperty) == NIL) {
    char s[16];
    sprintf(s, "x%x", (CELL)s);
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
      int32_t ch;
      s += get_utf8(s, -1, &ch);
      write_quoted(ch, '\'', stream);
    }
    wrputc('\'', stream);
  } else {
    wrputs((char *)s, stream);
  }
}

void Yap_WriteAtom(StreamDesc *s, Atom atom) {
  struct write_globs wglb;
  wglb.stream = s;
  wglb.Quote_illegal = FALSE;
  putAtom(atom, 0, &wglb);
}

static int IsCodesTerm(Term string) /* checks whether this is a string */
{
  if (IsVarTerm(string))
    return FALSE;
  do {
    Term hd;
    int ch;

    if (!IsPairTerm(string))
      return (FALSE);
    hd = HeadOfTerm(string);
    if (IsVarTerm(hd))
      return (FALSE);
    if (!IsIntTerm(hd))
      return (FALSE);
    ch = IntOfTerm(HeadOfTerm(string));
    if ((ch < ' ' || ch > MAX_ISO_LATIN1) && ch != '\n' && ch != '\t')
      return (FALSE);
    string = TailOfTerm(string);
    if (IsVarTerm(string))
      return (FALSE);
  } while (string != TermNil);
  return (TRUE);
}

/* writes a string	 */
static void putString(Term string, struct write_globs *wglb)

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
static void putUnquotedString(Term string, struct write_globs *wglb)

{
  wrf stream = wglb->stream;
  while (string != TermNil) {
    int ch = IntOfTerm(HeadOfTerm(string));
    wrputc(ch, stream);
    string = TailOfTerm(string);
  }
  lastw = alphanum;
}

static void write_var(CELL *t, int depth, struct write_globs *wglb) {
  CACHE_REGS
  if (lastw == alphanum) {
    wrputc(' ', wglb->stream);
  }
  wrputc('_', wglb->stream);
  /* make sure we don't get no creepy spaces where they shouldn't be */
  lastw = separator;
  if (IsAttVar(t)) {
    Int vcount = (t - H0);
    if (wglb->Portray_delays) {
      exts ext = ExtFromCell(t);

      wglb->Portray_delays = FALSE;
      if (ext == attvars_ext) {
        attvar_record *attv = RepAttVar(t);
        CELL *l = &attv->Value; /* dirty low-level hack, check atts.h */

        wrputs("$AT(", wglb->stream);
        write_var(t, depth, wglb);
        wrputc(',', wglb->stream);
        PROTECT(*t, writeTerm(*l, 999, depth, FALSE, wglb));
        attv = RepAttVar(t);
        wrputc(',', wglb->stream);
        l++;
        writeTerm(*l, 999, depth, FALSE, wglb);
        wrclose_bracket(wglb, TRUE);
      }
      wglb->Portray_delays = TRUE;
      return;
    }
    wrputc('D', wglb->stream);
    wrputn(vcount, wglb);
  } else {
    wrputn(((Int)(t - H0)), wglb);
  }
}

static void write_list(Term t, int direction, int depth,
                       struct write_globs *wglb) {
  Term ti;

  while (1) {
    if (t == TermNil)
      break;
    if ( depth && --depth<=0) {
      if (lastw == symbol || lastw == separator) {
        wrputc(' ', wglb->stream);
      }
      wrputc('|', wglb->stream);
      putAtom(Atom3Dots, wglb->Quote_illegal, wglb);
      return;
    }
    PROTECT(t, writeTerm(HeadOfTerm(t), 999, depth , FALSE
			 , wglb));
    ti = TailOfTerm(t);
    if (IsVarTerm(ti))
      break;
    if (!IsPairTerm(ti))
      break;
    lastw = separator;
    wrputc(',', wglb->stream);
    t = ti;
  }
  if (IsPairTerm(ti)) {
    /* we found an infinite loop */
    /* keep going on the list */
    wrputc(',', wglb->stream);
    write_list(ti, direction, depth, wglb);
  } else if (ti != MkAtomTerm(AtomNil)) {
    if (lastw == symbol || lastw == separator) {
      wrputc(' ', wglb->stream);
    }
    wrputc('|', wglb->stream);
    lastw = separator;
    writeTerm(ti, 999, depth, FALSE, wglb);
  }
}

static void writeTerm(Term t, int p, int depth, int rinfixarg,
                      struct write_globs *wglb)
/* term to write			 */
/* context priority			 */
{
  CACHE_REGS

  if (depth && --depth <= 0) {
    putAtom(Atom3Dots, wglb->Quote_illegal, wglb);
    return;
  }
  t = Deref(t);
  if (IsVarTerm(t)) {
    write_var((CELL *)t, depth, wglb);
  } else if (IsIntTerm(t)) {

    wrputn((Int)IntOfTerm(t), wglb);
  } else if (IsAtomTerm(t)) {
    putAtom(AtomOfTerm(t), wglb->Quote_illegal, wglb);
  } else if (IsPairTerm(t)) {
    if (wglb->Ignore_ops && false) {
      wrputs("'.'(", wglb->stream);
      lastw = separator;

      PROTECT(t, writeTerm(HeadOfTerm(t), 999, depth, FALSE, wglb));
      wrputs(",", wglb->stream);
      writeTerm(TailOfTerm(t), 999, depth, FALSE, wglb);
      wrclose_bracket(wglb, TRUE);
      return;
    }
    if (wglb->Use_portray)
      if (callPortray(t, wglb->stream - GLOBAL_Stream PASS_REGS)) {
        return;
      }
    if (trueGlobalPrologFlag(WRITE_STRINGS_FLAG) && IsCodesTerm(t)) {
      putString(t, wglb);
    } else {
      wrputc('[', wglb->stream);
      lastw = separator;
      /* we assume t was already saved in the stack */
      write_list(t, 0, depth, wglb);
      wrputc(']', wglb->stream);
      lastw = separator;
    }
  } else { /* compound term */
    Functor functor = FunctorOfTerm(t);
    int Arity;
    Atom atom;
    int op, lp, rp;

    if (IsExtensionFunctor(functor)) {
      switch ((CELL)functor) {
      case (CELL)FunctorDouble:
        wrputf(FloatOfTerm(t), wglb);
        return;
      case (CELL)FunctorString:
        write_string(UStringOfTerm(t), wglb);
        return;
      case (CELL)FunctorAttVar:
        write_var(RepAppl(t) + 1, depth, wglb);
        return;
      case (CELL)FunctorDBRef:
        wrputref(RefOfTerm(t), wglb->Quote_illegal, wglb);
        return;
      case (CELL)FunctorLongInt:
        wrputn(LongIntOfTerm(t), wglb);
        return;
        /* case (CELL)FunctorBigInt: */
      default:
        writebig(t, p, depth, rinfixarg, wglb);
        return;
      }
    }
    Arity = ArityOfFunctor(functor);
    atom = NameOfFunctor(functor);
#ifdef SFUNC
    if (Arity == SFArity) {
      int argno = 1;
      CELL *p = ArgsOfSFTerm(t);
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
        PROTECT(t, writeTerm(*p, 999, depth, FALSE, wglb));
        if (*p)
          wrputc(',', wglb->stream);
        argno++;
      }
      wrclose_bracket(wglb, TRUE);
      return;
    }
#endif
    if (wglb->Use_portray) {
      if (callPortray(t, wglb->stream - GLOBAL_Stream PASS_REGS)) {
        return;
      }
    }
    if (!wglb->Ignore_ops && Arity == 1 && Yap_IsPrefixOp(atom, &op, &rp)) {
      Term tright = ArgOfTerm(1, t);
      int bracket_right = !IsVarTerm(tright) && IsAtomTerm(tright) &&
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
      writeTerm(tright, rp, depth, TRUE, wglb);
      if (bracket_right) {
        wrclose_bracket(wglb, TRUE);
      }
      if (op > p) {
        wrclose_bracket(wglb, TRUE);
      }
    } else if (!wglb->Ignore_ops &&
               (Arity == 1 ||
                ((atom == AtomEmptyBrackets || atom == AtomCurly ||
                  atom == AtomEmptySquareBrackets) &&
                 Yap_IsListTerm(ArgOfTerm(1, t)))) &&
               Yap_IsPosfixOp(atom, &op, &lp)) {
      Term tleft = ArgOfTerm(1, t);

      int bracket_left, offset;

      if (Arity != 1) {
        tleft = ArgOfTerm(1, t);
        offset = 2;
      } else {
        tleft = ArgOfTerm(1, t);
        offset = 1;
      }
      bracket_left =
          !IsVarTerm(tleft) && IsAtomTerm(tleft) && Yap_IsOp(AtomOfTerm(tleft));
      if (op > p) {
        /* avoid stuff such as \+ (a,b) being written as \+(a,b) */
        wropen_bracket(wglb, TRUE);
      }
      if (bracket_left) {
        wropen_bracket(wglb, TRUE);
      }
      writeTerm(ArgOfTerm(offset, t), lp, depth, rinfixarg, wglb);
      if (bracket_left) {
        wrclose_bracket(wglb, TRUE);
      }
      if (Arity > 1) {
        if (atom == AtomEmptyBrackets) {
          wrputc('(', wglb->stream);
        } else if (atom == AtomEmptySquareBrackets) {
          wrputc('[', wglb->stream);
        } else if (atom == AtomCurly) {
          wrputc('{', wglb->stream);
        }
        lastw = separator;
        write_list(tleft, 0, depth, wglb);
        if (atom == AtomEmptyBrackets) {
          wrputc(')', wglb->stream);
        } else if (atom == AtomEmptySquareBrackets) {
          wrputc(']', wglb->stream);
        } else if (atom == AtomCurly) {
          wrputc('}', wglb->stream);
        }
        lastw = separator;
      } else {
        putAtom(atom, wglb->Quote_illegal, wglb);
      }
      if (op > p) {
        wrclose_bracket(wglb, TRUE);
      }
    } else if (!wglb->Ignore_ops && Arity == 2 &&
               Yap_IsInfixOp(atom, &op, &lp, &rp)) {
      Term tleft = ArgOfTerm(1, t);
      Term tright = ArgOfTerm(2, t);
      int bracket_left =
          !IsVarTerm(tleft) && IsAtomTerm(tleft) && Yap_IsOp(AtomOfTerm(tleft));
      int bracket_right = !IsVarTerm(tright) && IsAtomTerm(tright) &&
                          Yap_IsOp(AtomOfTerm(tright));

      if (op > p) {
        /* avoid stuff such as \+ (a,b) being written as \+(a,b) */
        wropen_bracket(wglb, TRUE);
        lastw = separator;
      }
      if (bracket_left) {
        wropen_bracket(wglb, TRUE);
      }
      PROTECT(
          t, writeTerm(ArgOfTerm(1, t), lp, depth , rinfixarg, wglb));
      if (bracket_left) {
        wrclose_bracket(wglb, TRUE);
      }
      /* avoid quoting commas and bars */
      if (!strcmp((char *)RepAtom(atom)->StrOfAE, ",")) {
        wrputc(',', wglb->stream);
        lastw = separator;
      } else if (!strcmp((char *)RepAtom(atom)->StrOfAE, "|")) {
        if (lastw == symbol || lastw == separator) {
          wrputc(' ', wglb->stream);
        }
        wrputc('|', wglb->stream);
        lastw = separator;
      } else
        putAtom(atom, wglb->Quote_illegal, wglb);
      if (bracket_right) {
        wropen_bracket(wglb, TRUE);
      }
      writeTerm(ArgOfTerm(2, t), rp, depth, TRUE, wglb);
      if (bracket_right) {
        wrclose_bracket(wglb, TRUE);
      }
      if (op > p) {
        wrclose_bracket(wglb, TRUE);
      }
    } else if (functor == FunctorDollarVar) {
      Term ti = ArgOfTerm(1, t);
      if (lastw == alphanum) {
        wrputc(' ', wglb->stream);
      }
      if (wglb->Handle_vars && !IsVarTerm(ti) &&
          (IsIntTerm(ti) || IsCodesTerm(ti) || IsAtomTerm(ti) ||
           IsStringTerm(ti))) {
        if (IsIntTerm(ti)) {
          Int k = IntOfTerm(ti);
	  if (k < -1) {
	                wrputc('_', wglb->stream);

	    k = -k-1;
	  }
          if (k == -1) {
            wrputc('_', wglb->stream);
            lastw = alphanum;
            return;
          } else {
            wrputc((k % 26) + 'A', wglb->stream);
            if (k >= 26) {
              /* make sure we don't get confused about our context */
              lastw = separator;
              wrputn(k / 26, wglb);
            } else
              lastw = alphanum;
          }
        } else if (IsAtomTerm(ti)) {
          putAtom(AtomOfTerm(ti), FALSE, wglb);
        } else if (IsStringTerm(ti)) {
          putString(ti, wglb);
        } else {
          putUnquotedString(ti, wglb);
        }
      } else {
        wrputs("'$VAR'(", wglb->stream);
         lastw = separator;
        writeTerm(ArgOfTerm(1, t), 999, depth , FALSE, wglb);
        wrclose_bracket(wglb, TRUE);
      }
    } else if (!wglb->Ignore_ops && functor == FunctorBraces) {
      wrputc('{', wglb->stream);
      lastw = separator;
      writeTerm(ArgOfTerm(1, t), GLOBAL_MaxPriority, depth , FALSE, wglb );
      wrputc('}', wglb->stream);
      lastw = separator;
    } else if (atom == AtomArray) {
      wrputc('{', wglb->stream);
      lastw = separator;
      for (op = 1; op <= Arity; ++op) {
        if (op == wglb->MaxArgs) {
          wrputs("...", wglb->stream);
          break;
        }
        writeTerm(ArgOfTerm(op, t), 999, depth, FALSE, wglb);
        if (op != Arity) {
          PROTECT(t, writeTerm(ArgOfTerm(op, t), 999, depth , FALSE, wglb));
          wrputc(',', wglb->stream);
          lastw = separator;
        }
      }
      writeTerm(ArgOfTerm(op, t), 999, depth , FALSE, wglb);
      wrputc('}', wglb->stream);
      lastw = separator;
    }  else {
      if (!wglb->Ignore_ops && atom == AtomHeapData) {
	Arity = 3+2*IntegerOfTerm(ArgOfTerm(1,t));
    }
      putAtom(atom, wglb->Quote_illegal, wglb);
      lastw = separator;
      wropen_bracket(wglb, FALSE);
      for (op = 1; op < Arity; ++op) {
        if (op == wglb->MaxArgs) {
          wrputc('.', wglb->stream);
          wrputc('.', wglb->stream);
          wrputc('.', wglb->stream);
          break;
        }
        PROTECT(
            t, writeTerm(ArgOfTerm(op, t), 999, depth , FALSE, wglb));
        wrputc(',', wglb->stream);
        lastw = separator;
      }
      writeTerm(ArgOfTerm(op, t), 999, depth , FALSE, wglb);
      wrclose_bracket(wglb, TRUE);
    }
  }
}

static bool bind_variable_names(Term t, size_t *np USES_REGS) {
  tr_fr_ptr TR0 = TR;
  while (!IsVarTerm(t) && IsPairTerm(t)) {
    Term tl = HeadOfTerm(t);
    Functor f;
    Term tv, t2, t1;

    if (!IsApplTerm(tl))
      return FALSE;
    if ((f = FunctorOfTerm(tl)) != FunctorEq) {
      return FALSE;
    }
    t1 = ArgOfTerm(1, tl);
    if (IsVarTerm(t1)) {
      Yap_Error(INSTANTIATION_ERROR, t1, "variable_names");
      return false;
    }
    t2 = ArgOfTerm(2, tl);
    tv = Yap_MkApplTerm(FunctorDollarVar, 1, &t1);
    if (IsVarTerm(t2)) {
      YapBind(VarOfTerm(t2), tv);
    }
    *np = TR-TR0;
    t = TailOfTerm(t);
  }
  return true;
}


void Yap_plwrite(Term t, StreamDesc *mywrite, int max_depth, int flags,
                 xarg *args)
/* term to be written			 */
/* consumer				 */
/* write options			 */
{
  CACHE_REGS

  yhandle_t lvl = push_text_stack();
  int  priority = GLOBAL_MaxPriority;
  struct write_globs wglb;
  Term cm = CurrentModule;
  
  t = Deref(t);
  CELL *hi = HR;
  tr_fr_ptr tr0 = TR;
  Term tn = t;
  size_t n;
  Int g;
 yhandle_t h0_y = Yap_InitHandle(MkVarTerm());
  if (args && args[WRITE_PRIORITY].used) {
    priority = IntegerOfTerm(args[WRITE_PRIORITY].tvalue);
  }
  if (args && args[WRITE_MODULE].used) {
    CurrentModule = args[WRITE_MODULE].tvalue;
  }
  if (args && args[WRITE_VARIABLE_NAMES].used) {
    bind_variable_names(args[WRITE_VARIABLE_NAMES].tvalue, &n PASS_REGS);
    tn = Yap_HackCycles(t PASS_REGS);
    flags |= Handle_vars_f;
  } else if ( ((flags & (Singleton_vars_f|Handle_cyclics_f)) == (Singleton_vars_f|Handle_cyclics_f)) ||
	      (args && args[WRITE_SINGLETONS].used &&
	       args[WRITE_SINGLETONS].tvalue == TermTrue) ) {
    tn = Yap_HackCycles(t PASS_REGS);
    flags |= Handle_vars_f;
 } else if ((args && args[WRITE_NUMBERVARS].used &&
	     args[WRITE_NUMBERVARS].tvalue == TermTrue) ) {
    tn = Yap_HackCycles(t PASS_REGS);
    flags |= Handle_vars_f;
}
wglb.trailings  = TR-tr0;
   wglb.stream = mywrite;
  wglb.Ignore_ops = flags & Ignore_ops_f;
  wglb.Write_strings = flags & BackQuote_String_f;
  wglb.Use_portray = flags & Use_portray_f;
  wglb.Handle_vars = flags & Handle_vars_f;
  wglb.Portray_delays = flags & AttVar_Portray_f;
  wglb.Keep_terms = flags & To_heap_f;
  wglb.Write_Loops = flags & Handle_cyclics_f;
  wglb.Quote_illegal = flags & Quote_illegal_f;
  wglb.lw = separator;
  LOCAL_max_depth=200;
   /* protect slots for portray */
   writeTerm(tn, priority, LOCAL_max_depth, false, &wglb);
     if (flags & New_Line_f) {
    if (flags & Fullstop_f) {
      wrputc('.', wglb.stream);
      wrputc('\n', wglb.stream);
    } else {
      wrputc('\n', wglb.stream);
    }
  } else {
    if (flags & Fullstop_f) {
      wrputc('.', wglb.stream);
      wrputc(' ', wglb.stream);
    }
  }
     if (!(flags & Use_portray_f)) {
       HR = (CELL *)Yap_PopHandle(h0_y);
     } else {
     Yap_PopHandle(h0_y);
}
     CurrentModule = cm;
       pop_text_stack(lvl);
 }

