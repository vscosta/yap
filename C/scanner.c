/*************************************************************************
 *                   *
 *   YAP Prolog                *
 *                   *
 *  Yap Prolog was developed at NCCUP - Universidade do Porto   *
 *                   *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2003   *
 *                   *
 **************************************************************************
 *                   *
 * File:    %W% %G%                     *
 * Last rev:  22-1-03               *
 * mods:                   *
 *  comments:  Prolog's scanner           *
 *                   *
 *************************************************************************/

/**
 * 
 * @file scanner.c
 * 
 * @defgroup Scanner Scanner Implementation.
 * @ingroup ParserImplementation
 *
 * @{
 *
 * @brief convert Prolog text into tokens. The tokens are supposed to be used by
 * the parser, but can also be exported as a list.
 *
 * Description:
 *
 * This module produces a list of tokens for use by the parser. The calling
 * program should supply a routine `int nextch(charpos) int *charpos`; which,
 * when called should produce the next char or -1 if none availlable. The
 * scanner will stop producing tokens when it either finds an end of file
 * (-1) or a token consisting of just '.' followed by a blank or control
 * char. Scanner errors will be signalled by the scanner exiting with a non-
 * zero  ErrorMsg and ErrorPos. Note that, even in this case, the scanner
 * will try to find the end of the term. A function char
 * *AllocScannerMemory(nbytes) should be supplied for allocating (temporary)
 * space for strings and for the table of prolog variables occurring in the
 * term.
 *
 */

#include "Yap.h"

#include "YapDefs.h"
#include "YapEval.h"

#include "YapHeap.h"
#include "Yatom.h"
#include "alloc.h"
#include "yapio.h"

#include "ScannerTypes.h"

/* stuff we want to use in standard YAP code */
#include "YapText.h"
#if _MSC_VER || defined(__MINGW32__)
#if HAVE_FINITE == 1
#undef HAVE_FINITE
#endif
#include <windows.h>
#endif
#include "iopreds.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if O_LOCALE
#include "locale.h"
#endif

/* You just can't trust some machines */
static inline bool my_isxdigit(int C, int SL, int SU)
{									\
  if (chtype(C) == NU)
    return true;
  return (C >= 'A' && C <= (SU)) || (C >= 'a' && C <= (SL));
}
#define my_isupper(C) (chtype(C) == UC)
#define my_islower(C) (chtype(C) == LC)

static Term float_send(char *, int);

/**
 * @brief try to tell the parser and friends where we are located.
 * 
 */
static void Yap_setCurrentSourceLocation(struct stream_desc *s) {
  CACHE_REGS
#if HAVE_SOCKET
  if (s->status & Socket_Stream_f)
    LOCAL_SourceFileName = AtomSocket;
  else
#endif
      if (s->status & Pipe_Stream_f)
    LOCAL_SourceFileName = AtomPipe;
  else if (s->status & InMemory_Stream_f)
    LOCAL_SourceFileName = s->name;
  else
    LOCAL_SourceFileName = s->name;
  LOCAL_SourceFileLineno = s->linecount;
}

/* token table with some help from Richard O'Keefe's PD scanner */
char_kind_t Yap_chtype0[NUMBER_OF_CHARS + 1] = {
    EF,
 /* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

 /* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

 /* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / */
    BS, SL, DC, SY, SY, CC, SY, QT, BK, BK, SY, SY, BK, SY, SY, SY,

 /* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
    NU, NU, NU, NU, NU, NU, NU, NU, NU, NU, SY, SL, SY, SY, SY, SY,

 /* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
    SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

 /* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
    UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, BK, SY, BK, SY, UL,

 /* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
    QT, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

 /* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~ del */
    LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, BK, BK, BK, SY, BS,
#if defined(ISO_LATIN) && FALSE
 /* 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143  */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

 /* 144 145    147 148 149 150 151 152 153 154 155 156 157 158 159  */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

  /*     ¡   ¢   £   ¤   ¥   ¦   §   ¨   ©   ª   «   ¬   ­   ®   ¯ */
    BS, SY, SY, SY, SY, SY, SY, SY, SY, SY, LC, SY, SY, SY, SY, SY,

 /* °   ±   ²   ³   ´   µ   ¶   ·   ¸   ¹   º   »   ¼   ½   ¾   ¿ */
    SY, SY, LC, LC, SY, SY, SY, SY, SY, LC, LC, SY, SY, SY, SY, SY,

 /* À   Á   Â   Ã   Ä   Å   Æ   Ç   È   É   Ê   Ë   Ì   Í   Î   Ï */
    UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

 /* Ð   Ñ   Ò   Ó   Ô   Õ   Ö   ×   Ø   Ù   Ú   Û   Ü   Ý   Þ   ß    */
#ifdef vms
    UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, LC,
#else
    UC, UC, UC, UC, UC, UC, UC, SY, UC, UC, UC, UC, UC, UC, UC, LC,
#endif
 /* à   á   â   ã   ä   å   æ   ç   è   é   ê   ë   ì   í   î   ï */
    LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

 /* ð   ñ   ò   ó   ô   õ   ö   ÷   ø   ù   ú   û   ü   cannot write the last
 * three because of lcc    */
#ifdef vms
    LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC
#else
    LC, LC, LC, LC, LC, LC, LC, SY, LC, LC, LC, LC, LC, LC, LC, LC
#endif
#endif
};

// standard get char, uses conversion table
// and converts to wide
static inline int getchr(struct stream_desc *inp) {
  /* if (inp != inp0) { fprintf(stderr,"\n %s
   * **********************************\n", AtomName(inp->name)); */
  /*   inp0 = inp; */
  /* } */
  int sno = inp - GLOBAL_Stream;
int ch = inp->stream_wgetc_for_read(sno);
  // fprintf(stderr,"%c", ch);
  return ch;
}
// get char for quoted data, eg, quoted atoms and so on
// converts to wide
static inline int getchrq(struct stream_desc *inp) {
  int ch = inp->stream_wgetc(inp - GLOBAL_Stream);
  return ch;
}

/* in case there is an overflow */
typedef struct scanner_extra_alloc {
  struct scanner_extra_alloc *next;
  void *filler;
} ScannerExtraBlock;

#define CodeSpaceError(t, p, l) CodeSpaceError__(t, p, l PASS_REGS)
static TokEntry *CodeSpaceError__(TokEntry *t, TokEntry *p,
                                  TokEntry *l USES_REGS) {
  LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
  LOCAL_ErrorMessage = "Code Space Overflow";
  if (t) {
    t->Tok = eot_tok;
    t->TokInfo = TermOutOfHeapError;
  }
  /* serious error now */
  return l;
}

#define TrailSpaceError(t, l) TrailSpaceError__(t, l PASS_REGS)
static TokEntry *TrailSpaceError__(TokEntry *t, TokEntry *l USES_REGS) {
  LOCAL_ErrorMessage = "Trail Overflow";
  LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
  if (t) {
    t->Tok = eot_tok;
    t->TokInfo = TermOutOfTrailError;
  }
  return l;
}


int bad_nl_error(int quote, const char *TokImage, struct stream_desc *st) {
  CACHE_REGS
    if (trueGlobalPrologFlag(MULTILINE_QUOTED_TEXT_FLAG) && ! trueGlobalPrologFlag(ISO_FLAG)) {
      return 10;
    }
    if (st->status & RepClose_Prolog_f) {
      Yap_CloseStream(st-GLOBAL_Stream);
      return EOF;
    }
    if (st->status & RepError_Prolog_f || trueGlobalPrologFlag(ISO_FLAG)) {
      LOCAL_Error_TYPE = SYNTAX_ERROR;
    } else {
      LOCAL_Error_TYPE = SYNTAX_WARNING;
    }
    LOCAL_ErrorMessage=malloc(2048);
    snprintf(LOCAL_ErrorMessage, 2047, "unexpected newline while  reading quoted text %s", TokImage);
    return 10;
}


extern double atof(const char *);

static Term float_send(char *s, int sign) {
  Float f = (Float)(sign * atof(s));
#if HAVE_ISFINITE || defined(isfinite)
  if (trueGlobalPrologFlag(ISO_FLAG)) { /* iso */
    if (!isfinite(f)) {
      CACHE_REGS
      LOCAL_ErrorMessage = "Float overflow while scanning";
      return (MkEvalFl(f));
    }
  }
#elif HAVE_FINITE
  if (trueGlobalPrologFlag(ISO_FLAG)) { /* iso */
    if (!finite(f)) {
      LOCAL_ErrorMessage = "Float overflow while scanning";
      return (MkEvalFl(f));
    }
  }
#endif
  {
    CACHE_REGS
    return MkFloatTerm(f);
  }
}

/* we have an overflow at s */
/**
 * This is a bug while encoding a symbol, and should always result in a syntax
 * error.
 * @param image
 * @param ch
 * @param code
 * @param st
 * @param s
 * @returnppp
 */
static int number_encoding_error(int ch, seq_type_t code, struct stream_desc *st)
{
  CACHE_REGS
  if ((st->status & RepError_Prolog_f) || trueGlobalPrologFlag(ISO_FLAG)) {
      LOCAL_Error_TYPE = SYNTAX_ERROR;
    LOCAL_ErrorMessage=malloc(2048);
    Yap_ThrowError(SYNTAX_ERROR,TermNil,"character encoding error: code so far %d scanned %d",code,ch);
} else if(st->status & RepClose_Prolog_f) {
    Yap_CloseStream(st-GLOBAL_Stream);
    return EOF;
  }
return ch;

}

Term read_int_overflow(const char *s, Int base, Int val, int sign) {
#ifdef USE_GMP
  /* try to scan it as a bignum */
  mpz_t new;
  Term t;

  mpz_init_set_str(new, s, base);
  if (sign < 0)
    mpz_neg(new, new);
  t = Yap_MkBigIntTerm(new);
  mpz_clear(new);
  return t;
#else
  CACHE_REGS
  /* try to scan it as a float */
  return MkIntegerTerm(val);
#endif
}

static wchar_t read_escaped_char( bool *got_charp, struct stream_desc *st) {
  int ch;
  *got_charp=true;

/* escape sequence */
do_switch:
  ch = getchrq(st);
  switch (ch) {
  case '\\':
    return '\\';
  case 'a':
    return '\a';
  case 'b':
    return '\b';
  case 'c':
    while (chtype((ch = getchrq(st))) == BS)
      ;
    {
      if (ch == '\\') {
        goto do_switch;
      }
      return ch;
    }
  case 'd':
    return 127;
  case 'e':
    return '\x1B'; /* <ESC>, a.k.a. \e */
  case 'f':
    return '\f';
  case 'n':
       return 10;
  case '\n':
    *got_charp=false;
    return 0;
  case 'r':
    return '\r';
  case 's': /* space */
    return ' ';
  case 't':
    return '\t';
  case 'u': {
    int i;
    wchar_t wc = '\0';

    for (i = 0; i < 4; i++) {
      ch = getchrq(st);
      if (ch >= '0' && ch <= '9') {
        wc += (ch - '0') << ((3 - i) * 4);
      } else if (ch >= 'a' && ch <= 'f') {
        wc += ((ch - 'a') + 10) << ((3 - i) * 4);
      } else if (ch >= 'A' && ch <= 'F') {
        wc += ((ch - 'A') + 10) << ((3 - i) * 4);
      } else {
	return number_encoding_error(wc, 1, st);
      }
    }
    return wc;
  }
  case 'U': {
    int i;
    wchar_t wc = '\0';

    for (i = 0; i < 8; i++) {
      ch = getchrq(st);
      if (ch >= '0' && ch <= '9') {
        wc += (ch - '0') << ((7 - i) * 4);
      } else if (ch >= 'a' && ch <= 'f') {
        wc += ((ch - 'a') + 10) << ((7 - i) * 4);
      } else if (ch >= 'A' && ch <= 'F') {
        wc += ((ch - 'A') + 10) << ((7 - i) * 4);
      } else {
        return  number_encoding_error(wc, 1, st);
      }
    }
    return wc;
  }
  case 'v':
    return '\v';
  case 'z': /* Prolog end-of-file */
    return number_encoding_error(ch, 1, st);
  case '\'':
    return '\'';
  case '"':
    return '"';
  case '`':
    return '`';
  case '^':
    if (trueGlobalPrologFlag(ISO_FLAG)) {
      return  number_encoding_error(ch, 1, st);
    } else {
      ch = getchrq(st);
      if (ch == '?') { /* delete character */
        return 127;
      } else if (ch >= 'a' && ch < 'z') { /* hexa */
        return ch - 'a';
      } else if (ch >= 'A' && ch < 'Z') { /* hexa */
        return ch - 'A';
      } else {
        return '^';
      }
    }
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    /* character in octal: maximum of 3 digits, terminates with \ */
    /* follow ISO */
    {
      unsigned char so_far = ch - '0';
      ch = getchrq(st);
      if (ch >= '0' && ch < '8') { /* octal */
        so_far = so_far * 8 + (ch - '0');
        ch = getchrq(st);
        if (ch >= '0' && ch < '8') { /* octal */
          so_far = so_far * 8 + (ch - '0');
          ch = getchrq(st);
          if (ch != '\\') {
           return number_encoding_error(ch, 1, st);
          }
          return so_far;
        } else if (ch == '\\') {
          return so_far;
        } else {
          return  number_encoding_error(ch, 1, st);
        }
      } else if (ch == '\\') {
        return so_far;
      } else {
        return  number_encoding_error(ch, 1, st);
      }
    }
  case '9':
  case '8':
    return  number_encoding_error(ch, 1, st);
  case 'x':
    /* hexadecimal character (YAP allows empty hexadecimal  */
    {
      unsigned char so_far = 0;
      ch = getchrq(st);
      int i=0;
      while (my_isxdigit(ch, 'f', 'F') && i<4) { /* hexa */
        so_far =
            so_far * 16 + (chtype(ch) == NU
                               ? ch - '0'
                               : (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
        ch = getchrq(st);
	i++;
      }
  if (ch == '\\') {
        return so_far;
      } else {
         return  number_encoding_error(ch, 1, st);
      }
    }
  default:
    /* reject sequence. The ISO standard does not
     consider this sequence legal, whereas SICStus would
     eat up the escape sequence. */
        return  number_encoding_error(ch, 1, st);
  }
}

#define number_overflow()                                                      \
  {                                                                            \
   CACHE_REGS                                          			       \
    imgsz = Yap_Min(imgsz * 2, imgsz);                                         \
    char *nbuf;                                                                \
    nbuf = Realloc(buf, imgsz);                                                \
    left = imgsz - max_size;                                                   \
    max_size = imgsz;                                                          \
    buf = nbuf;                                                                \
  }

/* reads a number, either integer or float */

static Term get_num(int *chp, StreamDesc *st, int sign,
                    char **bufp, size_t *szp, bool throw_error USES_REGS) {
  int ch = *chp;
  Int val = 0L, base = ch - '0';
  int might_be_float = true, has_overflow = false;
//  const unsigned char *decimalpoint;
  char *buf0 = *bufp, *sp = buf0, *buf = buf0;
  size_t imgsz = *szp, max_size = imgsz, left = max_size - 2;

  *sp++ = ch;
  ch = getchr(st);
  /*
   * because of things like 00'2, 03'2 and even better 12'2, I need to
   * do this (have mercy)
   */
  if (chtype(ch) == NU) {
    might_be_float = true;
    *sp++ = ch;
    if (--left == 0)
      number_overflow();
    base = 10 * base + ch - '0';
    ch = getchr(st);
  }
  if (ch == '\'') {
    if (base > 36) {
      ch=Yap_encoding_error(ch, 1, st); //, "Admissible bases are 11..36");
    } else {
      might_be_float = FALSE;
      if (--left == 0)
	number_overflow();
      ch = getchr(st);
      if (base == 0) {
	CACHE_REGS
	wchar_t ascii = ch;
	
      if (ch == '\\' &&
          Yap_GetModuleEntry(CurrentModule)->flags & M_CHARESCAPE) {
	bool got_char;
        ascii = read_escaped_char(&got_char, st);
	if (!got_char) {
	  bad_nl_error('\\',sp,st);
	}
	if (ascii == EOF) return TermNil;
      }
      *chp = getchr(st);
      if (sign == -1) {
        return MkIntegerTerm(-ascii);
      }
      return MkIntegerTerm(ascii);
    } else if (base >= 10 && base <= 36) {
	int upper_case = 'A' - 11 + base;
      int lower_case = 'a' - 11 + base;

      while (my_isxdigit(ch, lower_case, upper_case)) {
        Int oval = val;
        int chval =
            (chtype(ch) == NU ? ch - '0'
                              : (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
        if (--left == 0)
          number_overflow();
        *sp++ = ch;
        val = oval * base + chval;
        if (oval != (val - chval) / base) /* overflow */
          has_overflow = (has_overflow || TRUE);
        ch = getchr(st);
      }
    }
    }
  }else if (ch == 'x' && base == 0) {
    might_be_float = FALSE;
    if (--left == 0)
      number_overflow();
    *sp++ = ch;
    ch = getchr(st);
    if (!my_isxdigit(ch, 'f', 'F')) {
      if (throw_error)  {
	*sp = '\0';
	return Yap_encoding_error(ch, 1, st );//nvalid hexadecimal digit");
      } else {
	return TermNil;
      }

    }
    while (my_isxdigit(ch, 'f', 'F')) {
      Int oval = val;
      int chval =
          (chtype(ch) == NU ? ch - '0'
                            : (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
      if (--left == 0)
        number_overflow();
      *sp++ = ch;
      val = val * 16 + chval;
      if (oval != (val - chval) / 16) /* overflow */
        has_overflow = TRUE;
      ch = getchr(st);
    }
    *chp = ch;
  } else if (ch == 'o' && base == 0) {
    might_be_float = false;
    base = 8;
    ch = getchr(st);
    if (ch < '0' || ch > '7') {
      if (throw_error)  {
	*sp = '\0';
	return Yap_encoding_error( ch, 1, st); //, "invalid octal digit");
      }
      return MkIntTerm( 0);
    }
  } else if (ch == 'b' && base == 0) {
    might_be_float = false;
    base = 2;
    ch = getchr(st);
    if (ch < '0' || ch > '1') { 
	*sp = '\0';
      if (throw_error)  {
	return Yap_encoding_error(ch, 1, st); //, "invalid octal digit");
      }
      return MkIntTerm( 0);
    }
  } else {
    val = base;
    base = 10;
  }
  while (chtype(ch) == NU) {
    Int oval = val;
    if (!(val == 0 && ch == '0') || has_overflow) {
      if (--left == 0)
        number_overflow();
      *sp++ = ch;
    }
    if (ch - '0' >= base) {
      CACHE_REGS
	  *sp = '\0';
      if (sign == -1)
        return MkIntegerTerm(-val);
      return MkIntegerTerm(val);
    }
    if (oval > Int_MAX / 10 - (ch - '0')) /* overflow */
      has_overflow = true;
    else
      val = val * base + ch - '0';
    ch = getchr(st);
  }
#if O_LOCALE
  //      if ((decimalpoint = (unsigned char *)(localeconv()->decimal_point)) ==
          NULL)
#endif
//        decimalpoint = (const unsigned char *)".";
int decp = '.'; //decimalpoint[0];
      bool has_dot = ch==decp;
      if (might_be_float && has_dot) {
      int nch = Yap_peekWide(st-GLOBAL_Stream);
      	if (chtype(nch
		   ) != NU) {
	  if (trueGlobalPrologFlag(ISO_FLAG)) {
	  *sp = '\0';
	  Yap_encoding_error( ch, 1, st); //, "e/E float format not allowed in ISO mode");
	  return TermNil;
	}
	    *chp = ch;
	    *sp++='\0';
	    if (has_overflow)
	      return read_int_overflow(buf, base, val, sign);
	    if (sign == -1)
	      return MkIntegerTerm(-val);
	    return MkIntegerTerm(val);
	}
	ch = getchr(st);
	
	*sp++ = decp;
      /* numbers after . */
        do {
          if (--left == 0)
            number_overflow();
          *sp++ = ch;
        } while (chtype(ch = getchr(st)) == NU);
	if (ch != 'e' && ch != 'E') {
	  *chp = ch;
          *sp++ = '\0';
	    return float_send(buf, sign);

	}
      }
    
      
      if (might_be_float && (ch == 'e' || ch == 'E')) {
      if (--left == 0)
        number_overflow();
      *sp++ = ch;
      int nch = Yap_peekWide(st-GLOBAL_Stream);
      if (nch == '-') {
	ch = getchr(st);
        if (--left == 0)
          number_overflow();
        *sp++ = '-';
      } else if (nch == '+') {
	ch = getchr(st);
	if (--left == 0)
          number_overflow();
      }
      ch = getchr(st);
      while (chtype(ch) == NU) {
	if (--left == 0)
          number_overflow();
	*sp++ = ch;
	ch = getchr(st);	
      }
      *sp++ = '\0';
      *chp = ch;	
      CACHE_REGS
      if (might_be_float) {
        return float_send(buf, sign);
      }
      return MkIntegerTerm(sign * val);
  }
if (has_overflow) {
    *sp = '\0';
    /* skip base */
    *chp = ch;
    if (buf[0] == '0' && buf[1] == 'x')
      return read_int_overflow(buf + 2, 16, val, sign);
    else if (buf[0] == '0' && buf[1] == 'o')
      return read_int_overflow(buf + 2, 8, val, sign);
    else if (buf[0] == '0' && buf[1] == 'b')
      return read_int_overflow(buf + 2, 2, val, sign);
    if (buf[1] == '\'')
      return read_int_overflow(buf + 2, base, val, sign);
    if (buf[2] == '\'')
      return read_int_overflow(buf + 3, base, val, sign);
    return read_int_overflow(buf, base, val, sign);
  } else {
    CACHE_REGS
    *chp = ch;
    *sp = '\0';
    return MkIntegerTerm(val * sign);
  }
}

/** This routine is used when we need to parse a string into a number. */
Term Yap_scan_num(StreamDesc *inp, bool throw_error) {
  CACHE_REGS
  Term  out;
  int sign = 1;
  int ch;
  char *ptr;
  int lvl = push_text_stack();
  LOCAL_VarTable = LOCAL_AnonVarTable = NULL;
  LOCAL_VarList = LOCAL_VarTail = NULL;

  if (!(ptr = Malloc(4096) )){
        pop_text_stack(lvl);

	if (throw_error) {
	  Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil, "scanner: failed to allocate token image");
	}
	return TermNil;
    }
  else if ( inp->file == NULL)
    {
     ch = EOFCHAR;
    TokEntry *tokptr = Malloc(sizeof(TokEntry));
  tokptr->TokNext = NULL;
  tokptr->TokLine = 1;
  tokptr->TokLinePos =0;
  tokptr->TokOffset = 0;
  tokptr->Tok = Ord(eot_tok);		\
  tokptr->TokInfo = TermEof;
    return TermNil;
  } 
#if HAVE_ISWSPACE
  while (iswspace(ch = getchr(inp)))
    ;
#else
  while (isspace(ch = getchr(inp)))
    ;
#endif
  TokEntry *tokptr = Malloc(sizeof(TokEntry));
  tokptr->TokNext = NULL;
  tokptr->TokLine = GetCurInpLine(inp);
  tokptr->TokLinePos = GetCurInpLineStart(inp);
  tokptr->TokOffset = GetCurInpOffset(inp);
  if (ch == '-') {
    sign = -1;
    ch = getchr(inp);
  } else if (ch == '+') {
    ch = getchr(inp);
  }
  if (chtype(ch) == NU) {
      if (ASP - HR < 1024) {
        pop_text_stack(lvl);
	Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil, "scanner: failed to allocate token image");
	return TermNil;
    }
    size_t sz = 1024;
    char *buf = Malloc(sz);
    out = get_num(&ch, inp, sign, &buf, &sz, throw_error PASS_REGS); /*  */   
  } else {
    out = TermNil;
  }
  pop_text_stack(lvl);
 
#if !HAVE_ISWSPACE
#define iswspace(ch) isspace(ch)
#endif
if (ch == EOFCHAR) {
    return out;
}
 while(iswspace(ch)) {
    ch = getchr(inp);
    if (ch == EOFCHAR) {
    return out;
   }
  }
  if (ch=='.') {
 while(iswspace(ch)) {
    ch = getchr(inp);
    if (ch == EOFCHAR) {
    return out;
    }
  }
    }
  if (throw_error)
    Yap_ThrowError(SYNTAX_ERROR, MkIntTerm(ch),"should just have a  number");
  return 0;
}

#define CHECK_SPACE()                                                          \
  if (ASP - HR < 1024) {                                                       \
    LOCAL_ErrorMessage = "Stack Overflow";                                     \
    LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;                                   \
    LOCAL_Error_Size = 0L;                                                     \
    if (p) {                                                                   \
      p->Tok = Ord(kind = eot_tok);                                            \
      p->TokInfo = TermOutOfStackError;                                        \
    }                                                                          \
    /* serious error now */                                                    \
    return l;                                                                  \
  }


  /**
   * @brief convert a token to text.
   */
Term Yap_tokRep(void *tokptre) {
  CACHE_REGS
  TokEntry *tokptr = tokptre;
  Term info = tokptr->TokInfo;

  switch (tokptr->Tok) {
  case Name_tok:
    if (!info) {
      info = TermNil;
    } else {
      info = MkAtomTerm((Atom)info);
    }
    return Yap_MkApplTerm(Yap_MkFunctor(AtomAtom, 1), 1, &info);
  case QuasiQuotes_tok:
    info = MkAtomTerm(Yap_LookupAtom("<QQ>"));
    return Yap_MkApplTerm(Yap_MkFunctor(AtomAtom, 1), 1, &info);
  case Number_tok:
    return Yap_MkApplTerm(Yap_MkFunctor(AtomNumber, 1), 1, &info);
    break;
  case Var_tok: {
    Term t[2];
    VarEntry *varinfo = (VarEntry *)info;
    if ((t[0] = varinfo->VarAdr) == TermNil) {
      t[0] = varinfo->VarAdr = MkVarTerm();
    }
    t[1] = MkAtomTerm((Atom)(varinfo->VarRep));
    return Yap_MkApplTerm(Yap_MkFunctor(AtomGVar, 2), 2, t);
  }
  case String_tok:
    return Yap_MkApplTerm(Yap_MkFunctor(AtomString, 1), 1, &info);
  case BQString_tok:
    return Yap_MkApplTerm(Yap_MkFunctor(AtomString, 1), 1, &info);
  case Error_tok:
    return MkAtomTerm(AtomError);
  case Comment_tok:
    return Yap_MkApplTerm(Yap_MkFunctor(AtomComment, 1), 1, &info);
  case eot_tok:
    return MkAtomTerm(Yap_LookupAtom("EOT"));
  case Ponctuation_tok:
    if (info == Terml)
      return TermBeginBracket;
    else
      return info;
  }
  return TermDot;
}

Term Yap_tokFullRep(void *tokptre) {
  Term ts[4];
  TokEntry *tokptr = tokptre;
  if (tokptr->Tok == Ponctuation_tok)
    ts[0] = tokptr->TokInfo;
  else
    ts[0] = Yap_tokRep(tokptre);
  ts[1] = MkIntTerm(tokptr->TokLine);
  ts[2] = MkIntTerm(tokptr->TokOffset);
  ts[3] = MkIntTerm(tokptr->TokSize);
  return Yap_MkApplTerm(Yap_MkFunctor(AtomT,4),4,ts);
}  

const char *Yap_tokText(void *tokptre) {
  CACHE_REGS
  TokEntry *tokptr = tokptre;
  Term info = tokptr->TokInfo;

  switch (tokptr->Tok) {
  case eot_tok:
    return "EOT";
  case Ponctuation_tok:
    if (info == Terml)
      return "(";
  case Error_tok:
  case BQString_tok:
  case String_tok:
  case Name_tok:
    return AtomOfTerm(info)->StrOfAE;
  case Comment_tok:
        return StringOfTerm(info);
  case QuasiQuotes_tok:
    return "<QQ>";
  case Number_tok:
    if (IsIntegerTerm(info)) {
      char *s = Malloc(36);
      snprintf(s, 35, Int_FORMAT, IntegerOfTerm(info));
      return s;
    } else if (IsFloatTerm(info)) {
      char *s = Malloc(64);
      snprintf(s, 63, "%6g", FloatOfTerm(info));
      return s;
    } else {
      size_t len = Yap_gmp_to_size(info, 10);
      char *s = Malloc(len + 2);
      return Yap_gmp_to_string(info, s, len + 1, 10);
    }
    break;
  case Var_tok:
    if (info == 0)
      return "[]";
    VarEntry *  varinfo = (VarEntry *)info;
    return RepAtom(varinfo->VarRep)->StrOfAE;
  }
  return ".";
}

// mark that we reached EOF,
// next  token will be end_of_file)
static void mark_eof(struct stream_desc *st) {
  st->buf.on = true;
  st->buf.ch = EOF;
  st->stream_wgetc_for_read =  st->stream_wgetc = Yap_popChar;
  st->stream_getc =  Yap_popChar;
  st->status |= Push_Eof_Stream_f;
}


#define safe_add_ch_to_buff(ch) charp += put_utf8(charp, ch);
  
#define add_ch_to_buff(ch)                                                     \
  {\
  charp += put_utf8(charp, ch); }

TokEntry *Yap_tokenizer(void *st_, void *params_) {
  struct stream_desc *st = st_;
  scanner_params *params = params_;
  CACHE_REGS
  TokEntry *t, *l, *p;
  enum TokenKinds kind;
  int solo_flag = TRUE;
  int32_t ch, och = ' ';
  struct qq_struct_t *cur_qq = NULL;
  int sign = 1;
  size_t imgsz = 1024;
  char *TokImage = Malloc(imgsz);

  LOCAL_VarTable = NULL;
  LOCAL_AnonVarTable = NULL;
  l = NULL;
  p = NULL; /* Just to make lint happy */
  ch = getchr(st);
  while (chtype(ch) == BS) {
    och = ch;
    ch = getchr(st);
  }
    if (ch == EOF) {
    TokEntry *tokptr = Malloc(sizeof(TokEntry));
  tokptr->TokNext = NULL;
  tokptr->TokLine = 1;
  tokptr->TokLinePos =0;
  tokptr->TokOffset = 0;
  tokptr->Tok = Ord(eot_tok);		\
  tokptr->TokInfo = TermEof;
    return l = p = tokptr;
    }
  params->tposOUTPUT = Yap_StreamPosition(st - GLOBAL_Stream);
  Yap_setCurrentSourceLocation(st);
  do {
    int quote, isvar;
    unsigned char *charp, *mp;
    size_t len;

    t = Malloc(sizeof(TokEntry));
    memset(t,0,sizeof(TokEntry));
    t->TokNext = NULL;
    if (t == NULL) {
      return CodeSpaceError(t, p, l);
    }
    if (!l)
      l = t;
    else
      p->TokNext = t;
    p = t;
  restart:
    t->TokLinePos = GetCurInpLineStart(st);
    t->TokLine = GetCurInpLine(st);
    t->TokOffset = GetCurInpOffset(st);
    switch (chtype(ch)) {
    case CC:
      charp = (unsigned char *)TokImage;

      while (chtype(ch) == CC) {
	add_ch_to_buff(ch);
	while ((ch = getchr(st)) != 10 && chtype(ch) != EF) {
          if (charp >= (unsigned char *)TokImage + (imgsz - 10)) {
            size_t sz = charp - (unsigned char *)TokImage;
            imgsz = Yap_Min(imgsz * 2, imgsz + MBYTE);
            TokImage = Realloc(TokImage, imgsz);
            if (!TokImage) {
              return CodeSpaceError(t, p, l);
            }
            charp = (unsigned char *)TokImage + sz;
          }
	  add_ch_to_buff(ch);
       }

	if (ch != EOF)
	  ch = getchr(st);
      }
      add_ch_to_buff('\0');
      
      t->TokSize = strlen(TokImage);
	t->TokInfo = MkStringTerm(TokImage);
      t->Tok = Ord(kind = Comment_tok);
      if (ch==EOF)
	goto do_eof;
      break;

    case UC:
    case UL:
    case LC:
      och = ch;
      ch = getchr(st);
     {
      charp = (unsigned char *)TokImage;
      isvar = (chtype(och) != LC);
      add_ch_to_buff(och);
      for (; chtype(ch) <= NU; ch = getchr(st)) {
        if (charp == (unsigned char *)TokImage + (imgsz - 1)) {
          unsigned char *p0 = (unsigned char *)TokImage;
          imgsz = Yap_Min(imgsz * 2, imgsz + 1024 * 1024 * 1024);
          TokImage = Realloc(p0, imgsz);

          if (TokImage == NULL) {
            return CodeSpaceError(t, p, l);
          }
          charp = (unsigned char *)TokImage + (charp - p0);
        }
        add_ch_to_buff(ch);
      }
      while (ch == '\'' && isvar && params->ce) {
        if (charp == (unsigned char *)AuxSp - 1024) {
          return CodeSpaceError(t, p, l);
        }
        safe_add_ch_to_buff(ch);
        ch = getchr(st);
      }
      add_ch_to_buff('\0');
      if (!isvar || (ch == '(' && params->vn_asfl) ||
          (TokImage[0] != '_' && params->vprefix)) {
        Atom ae;
        /* don't do this in iso */
        ae = Yap_LookupAtom(TokImage);
        if (ae == NIL) {
          return CodeSpaceError(t, p, l);
        }
        t->TokSize = strlen(TokImage);
        t->TokInfo = MkAtomTerm(ae);
        if (ch == '(')
          solo_flag = FALSE;
        t->Tok = Ord(kind = Name_tok);
      } else {
        VarEntry *ve = Yap_LookupVar((const char *)TokImage, t->TokLine, t->TokOffset+1);
        t->TokSize = strlen(TokImage);
        t->TokInfo = Unsigned(ve);
        if (cur_qq) {
          ve->refs++;
        }
        t->Tok = Ord(kind = Var_tok);
	
      }

    } break;

    case NU: {
      int cha;
      sign = 1;

      cha = ch;
      CHECK_SPACE();
	t->TokInfo = get_num( &cha, st, sign, &TokImage, &imgsz,true PASS_REGS);
	t->Tok = kind = Number_tok;
        /* serious error now */
      t->TokSize= strlen(TokImage);
      ch = cha;
    }
      break;
	  

    case QT:
    case DC:
    quoted_string:
      {
	charp = (unsigned char *)TokImage;
      quote = ch;
      len = 0;
      ch = getchrq(st);

      while (true) {
        if (ch == 10 && !(Yap_GetModuleEntry(CurrentModule)->flags & M_MULTILINE)) {
	  ch = bad_nl_error( quote, TokImage,  st);           /* in ISO a new linea terminates a string */
	}
	if (ch == EOFCHAR) {
	  *charp = '\0';
	  if (LOCAL_ActiveError->errorNo)
	    goto ldefault;
         }
        if (charp > (unsigned char *)TokImage + (imgsz - 1)) {
          size_t sz = charp - (unsigned char *)TokImage;
          TokImage =
              Realloc(TokImage, (imgsz = Yap_Min(imgsz * 2, imgsz + MBYTE)));
          if (TokImage == NULL) {
            return CodeSpaceError(t, p, l);
          }
          charp = (unsigned char *)TokImage + sz;
          break;
        }
        else if (ch == quote) {
          ch = getchrq(st);

          if (ch != quote) {
	    *charp = '\0';
            break;
	  }
        } else if (ch == '\\'  &&
          Yap_GetModuleEntry(CurrentModule)->flags & M_CHARESCAPE) {
	  bool got_char;
          ch = read_escaped_char(&got_char,st);
	  if (!got_char) {
	ch = getchrq(st);
	continue;
	  }
	  if (LOCAL_ActiveError->errorNo)
	    goto ldefault;
  	}
	
        add_ch_to_buff(ch);
	ch = getchrq(st);
      }
      
      t->TokInfo = Yap_QuotedToTerm(quote, (char *)TokImage, CurrentModule,
                                    LOCAL_encoding PASS_REGS);
      t->TokSize = strlen(TokImage)+2;
      if (IsAtomTerm(t->TokInfo)) {
	  t->Tok = Ord(kind = Name_tok);
	  if (ch == '(')
	    solo_flag = false;
	} else if (IsStringTerm(t->TokInfo)) {
	  t->Tok = Ord(kind = String_tok);
	} else	if (IsPairTerm(t->TokInfo)) {
	  t->Tok = Ord(kind = String_tok);
	}
      }
      if (ch == EOF)
	goto do_eof;
      break;

    case BS:
      while (chtype(ch) == BS ) {
	och = ch;
	ch = getchr(st);
      }

      goto restart;
    case SY: {
      if (ch == '.') {
	int pch = Yap_peekWide(st-GLOBAL_Stream);
	if(chtype(pch) == BS || chtype(pch) == EF || pch == '%') {
        t->Tok = Ord(kind = eot_tok);
	t->TokInfo = TermDot;
	t->TokSize = 1;
	return l;
	}
      }
      if (ch == '`')
        goto quoted_string;
      och = ch;
      ch = getchr(st);
      if (och == '/' && ch == '*') {
	charp = (unsigned char *)TokImage;
	add_ch_to_buff(och);
	while ((och != '*' || ch != '/') && chtype(ch) != EF) {
          if (charp >= (unsigned char *)TokImage + (imgsz - 10)) {
            size_t sz = charp - (unsigned char *)TokImage;
            imgsz = Yap_Min(imgsz * 2, imgsz + MBYTE);
            TokImage = Realloc(TokImage, imgsz);
            if (!TokImage) {
              return CodeSpaceError(t, p, l);
            }
            charp = (unsigned char *)TokImage + sz;
          }
	 add_ch_to_buff(ch);
	  
         och = ch;
            ch = getchr(st);
	}
	if (chtype(ch) != EF) {
	  add_ch_to_buff(ch);
	  ch = getchr(st);
	}
	add_ch_to_buff('\0');
        t->TokSize = strlen(TokImage);
	   t->TokInfo = MkStringTerm(TokImage);
        t->Tok = Ord(kind = Comment_tok);
	break;
      }
    }
    
      {
        Atom ae;
        charp = (unsigned char *)TokImage;
        add_ch_to_buff(och);
        for (; chtype(ch) == SY; ch = getchr(st)) {
          if (charp >= (unsigned char *)TokImage + (imgsz - 10)) {
            size_t sz = charp - (unsigned char *)TokImage;
            imgsz = Yap_Min(imgsz * 2, imgsz + MBYTE);
            TokImage = Realloc(TokImage, imgsz);
            if (!TokImage) {
              return CodeSpaceError(t, p, l);
            }
            charp = (unsigned char *)TokImage + sz;
          }
          add_ch_to_buff(ch);
        }
	
        add_ch_to_buff('\0');
        ae = Yap_LookupAtom(TokImage);
        if (ae == NIL) {
          return CodeSpaceError(t, p, l);
        }
	t->TokSize = strlen(TokImage);
        t->TokInfo = MkAtomTerm(ae);
        if (t->TokInfo == (CELL)NIL) {
          return CodeSpaceError(t, p, l);
        }
        t->Tok = Ord(kind = Name_tok);
        if (ch == '(')
          solo_flag = false;
        else
          solo_flag = true;
      }
      break;

    case SL: {
      unsigned char chs[2];
      chs[0] = ch;
      chs[1] = '\0';
      ch = getchr(st);
      t->TokSize = 2;
      t->TokInfo = MkAtomTerm(Yap_ULookupAtom(chs));
      t->Tok = Ord(kind = Name_tok);
      if (ch == '(')
        solo_flag = FALSE;
    } break;

    case BK:
      och = ch;
      ch = getchr(st);
      {
        unsigned char *chs;
        charp = chs = (unsigned char *)TokImage;
        add_ch_to_buff(och);
        charp[0] = '\0';
	t->TokSize = strlen(TokImage);
        t->TokInfo = MkAtomTerm(Yap_ULookupAtom(chs));
      }
      if (och == '(') {
        while (chtype(ch) == BS) {
          ch = getchr(st);
        }
        if (ch == ')') {
          t->TokInfo = TermEmptyBrackets;
          t->Tok = Ord(kind = Name_tok);
	  t->TokSize = 2;
          ch = getchr(st);
          solo_flag = FALSE;
          break;
        } else if (!solo_flag) {
	  t->TokSize = 1;
          t->TokInfo = Terml;
          solo_flag = TRUE;
        }
      } else if (och == '[') {
        while (chtype(ch) == BS) {
          ch = getchr(st);
        };
        if (ch == ']') {
          t->TokInfo = TermNil;
	  t->TokSize = 2;
          t->Tok = Ord(kind = Name_tok);
          ch = getchr(st);
          solo_flag = FALSE;
          break;
        }
      } else if (och == '{') {
        if (ch == '|') {
          qq_t *qq = (qq_t *)calloc(sizeof(qq_t), 1);
          if (!qq) {
            LOCAL_ErrorMessage = "not enough heap space to read in quasi quote";
            t->Tok = Ord(kind = eot_tok);
            t->TokInfo = TermOutOfHeapError;
            return l;
          }
          if (cur_qq) {
            LOCAL_ErrorMessage = "quasi quote in quasi quote";
            //                      Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
            t->Tok = Ord(kind = eot_tok);
            t->TokInfo = TermOutOfHeapError;
            return l;
          } else {
            cur_qq = qq;
          }
          t->TokInfo = (CELL)qq;
          if (st->status & Seekable_Stream_f) {
            qq->start.byteno = fseek(st->file, 0, 0);
          } else {
            qq->start.byteno = st->charcount - 1;
          }
          qq->start.lineno = st->linecount;
          qq->start.linepos = st->charcount + 1 - st->linestart;
          qq->start.charno = st->charcount + 1;
          t->Tok = Ord(kind = QuasiQuotes_tok);
          ch = getchr(st);
          solo_flag = FALSE;
          break;
        }
        while (chtype(ch) == BS) {
          ch = getchr(st);
        };
        if (ch == '}') {
	  t->TokSize = 2;
          t->TokInfo = TermBraces;
t->Tok = Ord(kind = Name_tok);
          ch = getchr(st);
          solo_flag = FALSE;
          break;
        }
      } else if (och == '|' && ch == '|') {
        qq_t *qq = cur_qq;
        if (!qq) {
          LOCAL_ErrorMessage = "quasi quoted's || without {|";
          //          Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
          cur_qq = NULL;
          t->Tok = Ord(kind = eot_tok);
          t->TokInfo = TermError;
          return l;
        }
        cur_qq = NULL;
        t->TokInfo = (CELL)qq;
        if (st->status & Seekable_Stream_f) {
          qq->mid.byteno = fseek(st->file, 0, 0);
        } else {
          qq->mid.byteno = st->charcount - 1;
        }
        qq->mid.lineno = st->linecount;
        qq->mid.linepos = st->charcount+1-st->linestart;
        qq->mid.charno = st->charcount - 1;
        t->Tok = Ord(kind = QuasiQuotes_tok);
        ch = getchr(st);
        charp = (unsigned char *)TokImage;
        quote = ch;
        len = 0;
        ch = getchrq(st);

        while (TRUE) {
          if (ch == '|') {
            ch = getchrq(st);
            if (ch != '}') {
            } else {
              charp += put_utf8((unsigned char *)charp, och);
              charp += put_utf8((unsigned char *)charp, ch);
              /* we're done */
              break;
            }
          } else if (chtype(ch) == EF) {
	    goto do_eof;
          } else {
            charp += put_utf8(charp, ch);
            ch = getchrq(st);
          }
        }
        len = charp - (unsigned char *)TokImage;
        mp = malloc(len + 1);
        if (mp == NULL) {
          LOCAL_ErrorMessage = "not enough heap space to read in quasi quote";
          t->Tok = Ord(kind = eot_tok);
          t->TokInfo = TermOutOfHeapError;
          return l;
        }
        strncpy((char *)mp, (const char *)TokImage, len + 1);
        qq->text = (unsigned char *)mp;
        if (st->status & Seekable_Stream_f) {
          qq->end.byteno = fseek(st->file, 0, 0);
        } else {
          qq->end.byteno = st->charcount - 1;
        }
        qq->end.lineno  = st->linecount;
        qq->end.linepos = st->charcount  - st->linestart;
        qq->end.charno = st->charcount - 1;
        if (!(t->TokInfo)) {
          return CodeSpaceError(t, p, l);
        }
        //        Yap_ReleasePreAllocCodeSpace((CODEADDR)TokImage);
        solo_flag = FALSE;
        ch = getchr(st);
        break;
      }
      t->Tok = Ord(kind = Ponctuation_tok);
      break;
    case EF:
    do_eof:
      if (LOCAL_ActiveError->errorNo)
	goto ldefault;
      mark_eof(st);
      if (!l) {
      t->Tok = Ord(kind = Name_tok);
      t->TokInfo = TermEof;
      t->TokNext = NULL;
      return t;
      }
      p->TokNext=t;
      t->Tok = Ord(kind = eot_tok);
      t->TokInfo = TermEof;
      t->TokNext = NULL;
      p = t;
      break;
    default: {
      ldefault:
      kind = Error_tok;
      char err[1024];
      snprintf(err, 1023, "\n++++ token: unrecognised char %c (%d), type %c\n",
	       ch, ch, chtype(ch));
      t->Tok = Ord(kind = eot_tok);
      t->TokInfo = MkIntTerm(0);
    }
    }
    if (LOCAL_ErrorMessage) {
      /* insert an error token to inform the system of what happened */
      TokEntry *e = Malloc(sizeof(TokEntry));
      if (e == NULL) {
        return TrailSpaceError(p, l);
      }
      memset(e,0,sizeof(TokEntry));
      p->TokNext = e;
      e->Tok = Error_tok;
      e->TokInfo = MkAtomTerm(Yap_LookupAtom(LOCAL_ErrorMessage));
      e->TokLinePos = GetCurInpLineStart(st);
      e->TokLine = GetCurInpLine(st);
      e->TokOffset= GetCurInpOffset(st);
      e->TokNext = NULL;
      LOCAL_ErrorMessage = NULL;
      p = e;
    }
  } while (kind != eot_tok);

  return (l);
}

/**
 * terminate scanning: just closes the comment store.
 */
void Yap_clean_tokenizer(void) {
}


/// @}
