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
* File:		%W% %G%						 *
* Last rev:								 *
* mods:									 *
* comments:	Prolog's scanner					 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "@(#)scanner.c	1.2";

#endif

/*
 * Description: 
 *
 * This module produces a list of tokens for use by the parser. The calling
 * program should supply a routine int nextch(charpos) int *charpos; which,
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
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "alloc.h"
#include "eval.h"
#if HAVE_STRING_H
#include <string.h>
#endif

/* You just can't trust some machines */
#define my_isxdigit(C,SU,SL)	(chtype[C] == NU || (C >= 'A' && \
				 C <= (SU)) || (C >= 'a' && C <= (SL)))
#define my_isupper(C)	( C >= 'A' && C <= 'Z' )

STATIC_PROTO(void my_ungetch, (void));
STATIC_PROTO(int my_getch, (void));
STATIC_PROTO(Term float_send, (char *));
STATIC_PROTO(Term get_num, (void));
STATIC_PROTO(enum TokenKinds token, (void));

/* token table with some help from Richard O'Keefe's PD scanner */
static char chtype0[NUMBER_OF_CHARS+1] =
{
EF,
/* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
  BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
  BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / */
  BS, SL, DC, SY, LC, CC, SY, QT, BK, BK, SY, SY, BK, SY, SY, SY,

/* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
  NU, NU, NU, NU, NU, NU, NU, NU, NU, NU, SY, SL, SY, SY, SY, SY,

/* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
  SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
  UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, BK, SY, BK, SY, UL,

/* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
  SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~ del */
  LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, BK, BK, BK, SY, BS,

/* 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 */
  BS, BS,  BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* 144 145 ’   147 148 149 150 151 152 153 154 155 156 157 158 159 */
   BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/*     ¡   ¢   £   ¤   ¥   ¦   §   ¨   ©   ª   «   ¬   ­   ®   ¯   */
   BS, SY, SY, SY, SY, SY, SY, SY, SY, SY, LC, SY, SY, SY, SY, SY,

/* °   ±   ²   ³   ´   µ   ¶   ·   ¸   ¹   º   »   ¼   ½   ¾   ¿   */
   SY, SY, LC, LC, SY, SY, SY, SY, SY, LC, LC, SY, SY, SY, SY, SY,

/* À   Á   Â   Ã   Ä   Å   Æ   Ç   È   É   Ê   Ë   Ì   Í   Î   Ï    */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/* Ð   Ñ   Ò   Ó   Ô   Õ   Ö   ×   Ø   Ù   Ú   Û   Ü   Ý   Þ   ß    */
#ifdef  vms
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, LC,
#else
   UC, UC, UC, UC, UC, UC, UC, SY, UC, UC, UC, UC, UC, UC, UC, LC,
#endif
/* à   á   â   ã   ä   å   æ   ç   è   é   ê   ë   ì   í   î   ï    */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/* ð   ñ   ò   ó   ô   õ   ö   ÷   ø   ù   ú   û   ü   ý   þ   ÿ    */
#ifdef  vms
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC
#else
   LC, LC, LC, LC, LC, LC, LC, SY, LC, LC, LC, LC, LC, LC, LC, LC
#endif
};

char *chtype = chtype0+1;

int eot_before_eof = FALSE;

static int ch, chbuff, o_ch;

static char *TokImage;

static int BUF = FALSE;

static Int TokenPos;

static CELL TokenInfo;

static int (*Nextch) (int);

static int (*QuotedNextch) (int);

char *
AllocScannerMemory(unsigned int size)
{
  char *AuxSpScan;

  AuxSpScan = (char *)TR;
  size = AdjustSize(size);
  TR = (tr_fr_ptr)(AuxSpScan+size);
#if !OS_HANDLES_TR_OVERFLOW
  if (Unsigned(TrailTop) == Unsigned(TR)) {
    if(!growtrail (sizeof(CELL) * 16 * 1024L)) {
      return(NULL);
    }
  }
#endif
  return (AuxSpScan);
}

inline static void
my_ungetch(void)
{
  chbuff = ch;
  ch = o_ch;
  BUF = TRUE;
}

inline static int
my_getch(void)
{
  o_ch = ch;
  if (BUF) {
    BUF = FALSE;
    ch = chbuff;
  }
  else {
    ch = (*Nextch) (c_input_stream); 
  }
#ifdef DEBUG
  if (Option[1])
    YP_fprintf(YP_stderr, "[getch %c]", ch);
#endif
  return(ch);
}

inline static int
my_get_quoted_ch(void)
{
  o_ch = ch;
  if (BUF) {
    BUF = FALSE;
    ch = chbuff;
  }
  else {
    ch = (*QuotedNextch) (c_input_stream);
  }
#ifdef DEBUG
  if (Option[1])
    YP_fprintf(YP_stderr, "[getch %c]",ch);
#endif
  return (ch);
}

extern double atof(const char *);

static Term
float_send(char *s)
{
  Float f = (Float)atof(s);
#if HAVE_FINITE
  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) { /* iso */
    if (!finite(f)) {
      ErrorMessage = "Float overflow while scanning";
      return(MkEvalFl(0.0));
    }
  }
#endif
  return (MkEvalFl(f));
}

/* we have an overflow at s */
static Term
read_int_overflow(const char *s, Int base, Int val)
{
#ifdef USE_GMP
  /* try to scan it as a bignum */
  MP_INT *new = PreAllocBigNum();

  mpz_init_set_str (new, s, base);
  return(MkBigIntTerm(new));
#else
  /* try to scan it as a float */
  return(MkIntegerTerm(val));
#endif    
}

/* reads a number, either integer or float */

static Term
get_num(void)
{
  char *s = (char *)TR, *sp = s;
  Int val = 0, base = ch - '0';
  int might_be_float = TRUE, has_overflow = FALSE;

  *sp++ = ch;
  my_getch();
  /*
   * because of things like 00'2, 03'2 and even better 12'2, I need to
   * do this (have mercy) 
   */
  if (chtype[ch] == NU) {
    *sp++ = ch;
    base = 10 * base + ch - '0';
    my_getch();
  }
  if (ch == '\'') {
    if (base > 36) {
      ErrorMessage = "Admissible bases are 0..36";
      return (TermNil);
    }
    might_be_float = FALSE;
    *sp++ = ch;
    my_getch();
    if (base == 0) {
      Int ascii = ch;

      if (ch == '\\' &&
	  yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES) {
	/* escape sequence */
	ch = my_get_quoted_ch();
	switch (ch) {
	case 'a':
	  ascii = '\a';
	  break;
	case 'b':
	  ascii = '\b';
	  break;
	case 'r':
	  ascii = '\r';
	  break;
	case 'f':
	  ascii = '\f';
	  break;
	case 't':
	  ascii = '\t';
	  break;
	case 'n':
	  ascii = '\n';
	  break;
	case 'v':
	  ascii = '\v';
	  break;
	case '\\':
	  ascii = '\\';
	  break;
	case '\'':
	  ascii = '\'';
	  break;
	case '"':
	  ascii = '"';
	  break;
	case '`':
	  ascii = '`';
	  break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	  /* character in octal: maximum of 3 digits, terminates with \ */
	  {
	    unsigned char so_far = ch-'0';
	    my_get_quoted_ch();
	    if (ch >= '0' && ch < '8') {/* octal */
	      so_far = so_far*8+(ch-'0');
	      my_get_quoted_ch();
	      if (ch >= '0' && ch < '8') { /* octal */
		ascii = so_far*8+(ch-'0');
		my_get_quoted_ch();
		if (ch != '\\') {
		  ErrorMessage = "invalid octal escape sequence";
		}
	      } else if (ch == '\\') {
		ascii = so_far;
	      } else {
		ErrorMessage = "invalid octal escape sequence";
	      }
	    } else if (ch == '\\') {
	      ascii = so_far;
	    } else {
	      ErrorMessage = "invalid octal escape sequence";
	    }
	  }
	  break;
	case 'x':
	  /* hexadecimal character (YAP allows empty hexadecimal  */
	  {
	    unsigned char so_far = 0; 
	    my_get_quoted_ch();
	    if (my_isxdigit(ch,'f','F')) {/* hexa */
	      so_far = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
				      (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	      my_get_quoted_ch();
	      if (my_isxdigit(ch,'f','F')) { /* hexa */
		ascii = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
					  (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
		my_get_quoted_ch();
		if (ch != '\\') {
		  ErrorMessage = "invalid hexadecimal escape sequence";
		}
	      } else if (ch == '\\') {
		ascii = so_far;
	      } else {
		ErrorMessage = "invalid hexadecimal escape sequence";
	      } 
	    } else if (ch == '\\') {
	      ascii = so_far;
	      my_get_quoted_ch();
	    }
	  }
	  break;
	default:
	  /* accept sequence. Note that the ISO standard does not
	     consider this sequence legal, whereas SICStus would
	     eat up the escape sequence. */
	  ErrorMessage = "invalid escape sequence";
	}
      }
      /* a quick way to represent ASCII */
      my_getch();
      return (MkIntTerm(ascii));
    }
    else if (base >= 10 && base <= 36) {
      int upper_case = 'A' - 11 + base;
      int lower_case = 'a' - 11 + base;

      while (my_isxdigit(ch, upper_case, lower_case)) {
	Int oval = val;
	*sp++ = ch;
	val = val * base + (chtype[ch] == NU ? ch - '0' :
			    (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	if (oval >= val && oval != 0) /* overflow */
	  has_overflow = (has_overflow || TRUE);
	my_getch();
      }
    }
  }
  else if ((ch == 'x' || ch == 'X') && base == 0) {
    might_be_float = FALSE;
    *sp++ = ch;
    my_getch();
    while (my_isxdigit(ch, 'F', 'f')) {
      Int oval = val;
      *sp++ = ch;
      val = val * 16 + (chtype[ch] == NU ? ch - '0' :
			(my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
      if (oval >= val && oval != 0) /* overflow */
	has_overflow = (has_overflow || TRUE);
      my_getch();
    }
  }
  else if ((ch == 'o') && base == 0) {
    might_be_float = FALSE;
    base = 8;
    *sp++ = ch;
    my_getch();
  }
  else {
    val = base;
    base = 10;
  }
  while (chtype[ch] == NU) {
    Int oval = val;
    *sp++ = ch;
    if (ch - '0' >= base)
      return (MkIntegerTerm(val));
    val = val * base + ch - '0';
    if (oval >= val && oval != 0) /* overflow */
      has_overflow = (has_overflow || TRUE);
    my_getch();
  }
  if (might_be_float && (ch == '.' || ch == 'e' || ch == 'E')) {
    if (ch == '.') {
      *sp++ = '.';
      if (chtype[my_getch()] != NU) {
	my_ungetch();
	*--sp = '\0';
	if (has_overflow)
	  return(read_int_overflow(s,base,val));
	return (MkIntegerTerm(val));
      }
      do
	*sp++ = ch;
      while (chtype[my_getch()] == NU);
    }
    if (ch == 'e' || ch == 'E') {
      *sp++ = 'e';
      my_getch();
      if (ch == '-') {
	*sp++ = ch;
	my_getch();
      }
      else if (ch == '+')
	my_getch();
      if (chtype[ch] != NU) {
	my_ungetch();
	*--sp = '\0';
	return (float_send(s));
      }
      do
	*sp++ = ch;
      while (chtype[my_getch()] == NU);
    }
    *sp = '\0';
    return (float_send(s));
  } else if (has_overflow) {
    *sp = '\0';
    /* skip base */
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
      return(read_int_overflow(s+2,16,val));
    if (s[1] == '\'')
      return(read_int_overflow(s+2,base,val));
    if (s[2] == '\'')
      return(read_int_overflow(s+3,base,val));
    return(read_int_overflow(s,base,val));
  } else
    return (MkIntegerTerm(val));
}

/* given a function Nxtch scan until we  either find the number
   or end of file */
Term
scan_num(int (*Nxtch) (int))
{
  Term out;
  int sign = 1;

  Nextch = Nxtch;
  ErrorMessage = NULL;
  ch = Nextch(c_input_stream);
  if (ch == '-') {
    sign = -1;
    ch = Nextch(c_input_stream);
  } else if (ch == '+') {
    ch = Nextch(c_input_stream);
  }
  if (chtype[ch] != NU) {
    return(TermNil);
  }
  out = get_num();
  if (sign == -1) {
    if (IsIntegerTerm(out))
      out = MkIntegerTerm(-IntegerOfTerm(out));
    else if (IsFloatTerm(out))
      out = MkFloatTerm(-FloatOfTerm(out));
  }
  if (ErrorMessage != NULL || ch != -1)
    return(TermNil);
  return(out);
}

/* gets a token */

static enum TokenKinds
token(void)
{
  int och, quote, isvar;
  char *charp, *mp;
  unsigned int len;

  TokImage = ((AtomEntry *) ( PreAllocCodeSpace()))->StrOfAE;
  charp = TokImage;
  while (chtype[ch] == BS)
    my_getch();
#ifdef EMACS
  TokenPos = GetCurInpPos();
#endif
  switch (chtype[ch]) {
  case CC:
    while (my_getch() != 10 && chtype[ch] != EF);
    if (chtype[ch] != EF) {
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (token());
    } else {
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (eot_tok);
    }
  case UC:
  case UL:
  case LC:
    isvar = (chtype[ch] != LC);
    *charp++ = ch;
    for (my_getch(); chtype[ch] <= NU; my_getch())
      *charp++ = ch;
    *charp++ = '\0';
    if (!isvar) {
      /* don't do this in iso */
      TokenInfo = Unsigned(LookupAtom(TokImage));
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (Name_tok);
    }
    else {
      TokenInfo = Unsigned(LookupVar(TokImage));
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (Var_tok);
    }

  case NU:
    TokenInfo = get_num();
    ReleasePreAllocCodeSpace((CODEADDR)TokImage);
    return (Number_tok);

  case QT:
  case DC:
    quote = ch;
    len = 0;
    my_get_quoted_ch();
    while (1) {
      if (charp + 1024 > (char *)AuxSp) {
	ErrorMessage = "Heap Overflow While Scanning: please increase heap";
	break;
      }
      if (ch == quote) {
	my_get_quoted_ch();
	if (ch != quote)
	  break;
	*charp++ = ch;
	my_get_quoted_ch();
      } else if (ch == '\\' && yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES) {
	/* escape sequence */
	ch = my_get_quoted_ch();
	switch (ch) {
	case 'a':
	  *charp++ = '\a';
	  my_get_quoted_ch();
	  break;
	case 'b':
	  *charp++ = '\b';
	  my_get_quoted_ch();
	  break;
	case 'r':
	  *charp++ = '\r';
	  my_get_quoted_ch();
	  break;
	case 'f':
	  *charp++ = '\f';
	  my_get_quoted_ch();
	  break;
	case 't':
	  *charp++ = '\t';
	  my_get_quoted_ch();
	  break;
	case 'n':
	  *charp++ = '\n';
	  my_get_quoted_ch();
	  break;
	case 'v':
	  *charp++ = '\v';
	  my_get_quoted_ch();
	  break;
	case '\\':
	  *charp++ = '\\';
	  my_get_quoted_ch();
	  break;
	case '\'':
	  *charp++ = '\'';
	  my_get_quoted_ch();
	  break;
	case '"':
	  *charp++ = '"';
	  my_get_quoted_ch();
	  break;
	case '`':
	  *charp++ = '`';
	  my_get_quoted_ch();
	  break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	  /* character in octal: maximum of 3 digits, terminates with \ */
	  {
	    unsigned char so_far = ch-'0';
	    my_get_quoted_ch();
	    if (ch >= '0' && ch < '8') {/* octal */
	      so_far = so_far*8+(ch-'0');
	      my_get_quoted_ch();
	      if (ch >= '0' && ch < '8') { /* octal */
		*charp++ = so_far*8+(ch-'0');
		my_get_quoted_ch();
		if (ch != '\\') {
		  ErrorMessage = "invalid octal escape sequence";
		} else {
		  my_get_quoted_ch();
		}
	      } else if (ch == '\\') {
		*charp++ = so_far;
		my_get_quoted_ch();
	      } else {
		ErrorMessage = "invalid octal escape sequence";
	      }
	    } else if (ch == '\\') {
	      *charp++ = so_far;
	      my_get_quoted_ch();
	    } else {
	      ErrorMessage = "invalid octal escape sequence";
	    }
	  }
	  break;
	case 'x':
	  /* hexadecimal character (YAP allows empty hexadecimal  */
	  {
	    unsigned char so_far = 0; 
	    my_get_quoted_ch();
	    if (my_isxdigit(ch,'f','F')) {/* hexa */
	      so_far = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
				      (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	      my_get_quoted_ch();
	      if (my_isxdigit(ch,'f','F')) { /* hexa */
		*charp++ = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
					  (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
		my_get_quoted_ch();
		if (ch != '\\') {
		  ErrorMessage = "invalid hexadecimal escape sequence";
		} else {
		  my_get_quoted_ch();
		}
	      } else if (ch == '\\') {
		*charp++ = so_far;
		my_get_quoted_ch();
	      } else {
		ErrorMessage = "invalid hexadecimal escape sequence";
	      } 
	    } else if (ch == '\\') {
	      *charp++ = so_far;
	      my_get_quoted_ch();
	    } else {
	      ErrorMessage = "invalid hexadecimal escape sequence";
	    }
	  }
	  break;
	default:
	  /* accept sequence. Note that the ISO standard does not
	     consider this sequence legal, whereas SICStus would
	     eat up the escape sequence. */
	  ErrorMessage = "invalid escape sequence";
	}
      } else {
	*charp++ = ch;
	my_get_quoted_ch();
      }
      ++len;
      if (charp > (char *)AuxSp - 1024) {
	/* Not enough space to read in the string. */
	ErrorMessage = "not enough heap space to read in string or quoted atom";
	/* serious error now */
	ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	return(eot_tok);
      }
    }
    *charp = '\0';
    if (quote == '"') {
      mp = AllocScannerMemory(len + 1);
      if (mp == NULL) {
	ErrorMessage = "not enough stack space to read in string or quoted atom";
	ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	return(eot_tok);
      }
      strcpy(mp, TokImage);
      TokenInfo = Unsigned(mp);
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (String_tok);
    }
    else {
      TokenInfo = Unsigned(LookupAtom(TokImage));
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (Name_tok);
    }

  case SY:
    och = ch;
    my_getch();
    if (och == '/' && ch == '*') {
      while ((och != '*' || ch != '/') && chtype[ch] != EF) {
	och = ch;
	my_getch();
      }
      if (chtype[ch] == EF) {
	ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	return (eot_tok);
      }
      my_getch();
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (token());
    }
    if (och == '.' && (chtype[ch] == BS || chtype[ch] == EF
		       || chtype[ch] == CC)) {
      eot_before_eof = TRUE;
      if (chtype[ch] == CC)
	while (my_getch() != 10 && chtype[ch] != EF);
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (eot_tok);
    }
    else {
      *charp++ = och;
      for (; chtype[ch] == SY; my_getch())
	*charp++ = ch;
      *charp = '\0';
      TokenInfo = Unsigned(LookupAtom(TokImage));
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (Name_tok);
    }

  case SL:
    *charp++ = ch;
    *charp++ = '\0';
    my_getch();
    TokenInfo = Unsigned(LookupAtom(TokImage));
    ReleasePreAllocCodeSpace((CODEADDR)TokImage);
    return (Name_tok);

  case BK:
    och = ch;
    do {
      my_getch();
    } while (chtype[ch] == BS);
    if (och == '[' && ch == ']') {
      TokenInfo = Unsigned(AtomNil);
      my_getch();
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (Name_tok);
    }
    else {
      TokenInfo = och;
      ReleasePreAllocCodeSpace((CODEADDR)TokImage);
      return (Ponctuation_tok);
    }

  case EF:
    ReleasePreAllocCodeSpace((CODEADDR)TokImage);
    return (eot_tok);
#ifdef DEBUG
  default:
    YP_fprintf(YP_stderr, "\n++++ token: wrong char type %c %d\n", ch, chtype[ch]);
    ReleasePreAllocCodeSpace((CODEADDR)TokImage);
    return (eot_tok);
#else
  default:
    ReleasePreAllocCodeSpace((CODEADDR)TokImage);
    return (eot_tok);		/* Just to make lint happy */
#endif
  }
}

TokEntry *
tokenizer(int (*Nxtch) (int), int (*QuotedNxtch) (int))
{
  TokEntry *t, *l, *p;
  enum TokenKinds kind;
  int solo_flag = TRUE;

  ErrorMessage = NULL;
  VarTable = NULL;
  AnonVarTable = NULL;
  Nextch = Nxtch;
  QuotedNextch = QuotedNxtch;
  eot_before_eof = FALSE;
  l = NIL;
  p = NIL;			/* Just to make lint happy */
  ch = ' ';
  my_getch();
  while (chtype[ch] == BS)
    my_getch();
  FirstLineInParse();
  do {
    t = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));

    if (t == NULL) {
      ErrorMessage = "not enough stack space to read in term";
      if (p != NIL)
	p->TokInfo = eot_tok;
      /* serious error now */
      return(l);
    }

    if (l == NIL)
      l = t;
    else
      p->TokNext = t;
    p = t;
    if ((kind = token()) == Name_tok && ch == '(')
      solo_flag = FALSE;
    else if (kind == Ponctuation_tok && TokenInfo == '(' && !solo_flag) {
      TokenInfo = 'l';
      solo_flag = TRUE;
    }
    t->Tok = Ord(kind);
#ifdef DEBUG
    if(Option[2]) YP_fprintf(YP_stderr,"[Token %d %ld]",Ord(kind),(unsigned long int)TokenInfo);
#endif
    t->TokInfo = (Term) TokenInfo;
    t->TokPos = TokenPos;
    t->TokNext = NIL;
  } while (kind != eot_tok);
  return (l);
}

extern int PlFGetchar(void);

#if DEBUG
static inline int
debug_fgetch(void)
{
  int ch = PlFGetchar();
  if (Option[1])
    YP_fprintf(YP_stderr, "[getch %c,%d]", ch,ch);
  return (ch);
}
#define my_fgetch() (ch = debug_fgetch())
#else
#define my_fgetch() (ch = PlFGetchar())
#endif

TokEntry *
fast_tokenizer(void)
{
  /* I hope, a compressed version of the last
   * three files */

  TokEntry *t, *l, *p;
  enum TokenKinds kind;
  register int ch, och;
  int solo_flag = TRUE;

  ErrorMessage = NULL;
  VarTable = NULL;
  AnonVarTable = NULL;
  eot_before_eof = FALSE;
  l = NIL;
  p = NIL;			/* Just to make lint happy */
  my_fgetch();
  while (chtype[ch] == BS)
    my_fgetch();
  if (chtype[ch] == EF)
    return(NIL);
  FirstLineInParse();
  do {
    t = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
    if (t == NULL) {
      ErrorMessage = "not enough stack space to read in term";
      if (p != NIL)
	p->TokInfo = eot_tok;
      /* serious error now */
      return(l);
    }

    if (l == NIL)
      l = t;
    else
      p->TokNext = t;
    p = t;
    /* old code for token() */
    {
      int quote, isvar;
      char *charp, *mp;
      unsigned int len;

    get_tok:

      charp = TokImage = ((AtomEntry *) ( PreAllocCodeSpace()))->StrOfAE;
      while (chtype[ch] == BS)
	my_fgetch();
#ifdef EMACS
      TokenPos = GetCurInpPos();
#endif
      switch (chtype[ch]) {
      case CC:
	while (my_fgetch() != 10 && chtype[ch] != EF);
	if (chtype[ch] != EF) {
	  my_fgetch();
	  if (t == l)
	    FirstLineInParse();
	  ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  goto get_tok;
	}
	else
	  kind = eot_tok;
	break;
      case UC:
      case UL:
      case LC:
	isvar = (chtype[ch] != LC);
	*charp++ = ch;

      inside_letters:

	for (my_fgetch(); chtype[ch] <= NU; my_fgetch())
	  *charp++ = ch;
	*charp++ = '\0';
	if (!isvar) {
	  TokenInfo = Unsigned(LookupAtom(TokImage));
	  if (ch == '(')
	    solo_flag = FALSE;
	  kind = Name_tok;
	}
	else {
	  TokenInfo = Unsigned(LookupVar(TokImage));
	  kind = Var_tok;
	}
	break;

      case NU:

	{
	  char *sp = TokImage;
	  Int val = 0, base = ch - '0';
	  int might_be_float = TRUE, has_overflow = FALSE;

	  *sp++ = ch;
	  my_fgetch();
	  /*
	   * because of things like 00'2, 03'2
	   * and even better 12'2, I need to do
	   * this (have mercy) 
	   */
	  if (chtype[ch] == NU) {
	    *sp++ = ch;
	    base = 10 * base + ch - '0';
	    my_fgetch();
	  }
	  if (ch == '\'') {
	    might_be_float = FALSE;
	    *sp++ = ch;
	    my_fgetch();
	    if (base == 0) {
	      Int ascii = ch;

	      /*
	       * a quick way to
	       * represent ASCII 
	       */
	      if (ch == '\\' &&
		  yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES) {
		/* escape sequence */
		ch = my_fgetch();
		switch (ch) {
		case 'a':
		  ascii = '\a';
		  break;
		case 'b':
		  ascii = '\b';
		  break;
		case 'r':
		  ascii = '\r';
		  break;
		case 'f':
		  ascii = '\f';
		  break;
		case 't':
		  ascii = '\t';
		  break;
		case 'n':
		  ascii = '\n';
		  break;
		case 'v':
		  ascii = '\v';
		  break;
		case '\\':
		  ascii = '\\';
		  break;
		case '\'':
		  ascii = '\'';
		  break;
		case '"':
		  ascii = '"';
		  break;
		case '`':
		  ascii = '`';
		  break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		  /* character in octal: maximum of 3 digits, terminates with \ */
		  {
		    unsigned char so_far = ch-'0';
		    my_fgetch();
		    if (ch >= '0' && ch < '8') {/* octal */
		      so_far = so_far*8+(ch-'0');
		      my_fgetch();
		      if (ch >= '0' && ch < '8') { /* octal */
			ascii = so_far*8+(ch-'0');
			my_fgetch();
			if (ch != '\\') {
			  ErrorMessage = "invalid octal escape sequence";
			}
		      } else if (ch == '\\') {
			ascii = so_far;
		      } else {
			ErrorMessage = "invalid octal escape sequence";
		      }
		    } else if (ch == '\\') {
		      ascii = so_far;
		    } else {
		      ErrorMessage = "invalid octal escape sequence";
		    }
		  }
		  break;
		case 'x':
		  /* hexadecimal character (YAP allows empty hexadecimal  */
		  {
		    unsigned char so_far = 0; 
		    my_fgetch();
		    if (my_isxdigit(ch,'f','F')) {/* hexa */
		      so_far = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
					      (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
		      my_fgetch();
		      if (my_isxdigit(ch,'f','F')) { /* hexa */
			ascii = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
					       (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
			my_fgetch();
			if (ch != '\\') {
			  ErrorMessage = "invalid hexadecimal escape sequence";
			}
		      } else if (ch == '\\') {
			ascii = so_far;
		      } else {
			ErrorMessage = "invalid hexadecimal escape sequence";
		      } 
		    } else if (ch == '\\') {
		      ascii = so_far;
		      my_fgetch();
		    } else {
		      ErrorMessage = "invalid hexadecimal escape sequence";
		    }
		  }
		  break;
		default:
		  /* accept sequence. Note that the ISO standard does not
		     consider this sequence legal, whereas SICStus would
		     eat up the escape sequence. */
		  ErrorMessage = "invalid escape sequence";
		}
	      }
	      my_fgetch();
	      TokenInfo = (CELL) MkIntTerm(ascii);
	      goto end_of_read_number;
	    }
	    else if (base >= 10 && base <= 36) {
	      int upper_case = 'A' - 11 + base;
	      int lower_case = 'a' - 11 + base;

	      while (my_isxdigit(ch, upper_case, lower_case)) {
		Int oval = val;

		*sp++ = ch;
		val = val * base + (chtype[ch] == NU ? ch - '0' :
			       (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
		if (oval >= val && oval != 0) /* overflow */
		  has_overflow = (has_overflow || TRUE);
		my_fgetch();
	      }
	    }
	  }
	  else if ((ch == 'x' || ch == 'X') && base == 0) {
	    might_be_float = FALSE;
	    *sp++ = ch;
	    my_fgetch();
	    while (my_isxdigit(ch, 'F', 'f')) {
	      Int oval = val;

	      *sp++ = ch;
	      val = val * 16 + (chtype[ch] == NU ? ch - '0' :
				(my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
	      if (oval >= val && oval != 0) /* overflow */
		has_overflow = (has_overflow || TRUE);
	      my_fgetch();
	    }
	  }
	  else {
	    val = base;
	    base = 10;
	  }
	  while (chtype[ch] == NU) {
	    Int oval = val;
	    *sp++ = ch;
	    if (ch - '0' >= base) {
	      TokenInfo = (CELL) MkIntegerTerm(val);
	      goto end_of_read_number;
	    }
	    val = val * base + ch - '0';
	    if (oval >= val && oval != 0) /* overflow */
	      has_overflow = (has_overflow || TRUE);
	    my_fgetch();
	  }
	  if (might_be_float && (ch == '.' || ch == 'e' || ch == 'E')) {
	    if (ch == '.') {
	      *sp++ = '.';
	      if (chtype[my_fgetch()] != NU) {
		/*
		 * first
		 * process
		 * the new
		 * token 
		 */
		t->Tok = Ord(Number_tok);
#ifdef DEBUG
		/*
		 * if(Option[2
		 * ])
		 * YP_fprintf(YP_stderr,"[To
		 * ken %d
		 * %d]",Ord(ki
		 * nd),TokenIn
		 * fo); 
		 */
#endif
		if (has_overflow)
		  t->TokInfo = read_int_overflow(TokImage,base,val);
		else
		  t->TokInfo = MkIntegerTerm(val);
		t->TokPos = TokenPos;
		t = (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
		if (t == NULL) {
		  ErrorMessage = "not enough stack space to read in term";
		  if (p != NIL)
		    p->TokInfo = eot_tok;
		  /* serious error now */
		  ReleasePreAllocCodeSpace((CODEADDR)TokImage);
		  return(l);
		}

		if (l == NIL)
		  l = t;
		else
		  p->TokNext = t;
		p = t;

		/*
		 * continue 
		 * analysis 
		 */
		och = '.';
		goto inside_symbol;
	      }
	      do
		*sp++ = ch;
	      while (chtype[my_fgetch()] == NU);
	    }
	    if (ch == 'e' || ch == 'E') {
	      *sp++ = 'e';
	      my_fgetch();
	      if (ch == '-') {
		*sp++ = ch;
		my_fgetch();
	      }
	      else if (ch == '+')
		my_fgetch();
	      if (chtype[ch] != NU) {
		/*
		 * first
		 * finish
		 * processing 
		 */
		--sp;
		och = *sp;
		*sp = '\0';
		/*
		 * first
		 * process
		 * the new
		 * token 
		 */
		t->Tok = Ord(Number_tok);
		t->TokPos = TokenPos;
		t->TokInfo = float_send(TokImage);
		t =
		  (TokEntry *) AllocScannerMemory(sizeof(TokEntry));
		if (t == NULL) {
		  ErrorMessage = "not enough stack space to read in term";
		  if (p != NIL)
		    p->TokInfo = eot_tok;
		  /* serious error now */
		  ReleasePreAllocCodeSpace((CODEADDR)TokImage);
		  return(l);
		}

		if (l == NIL)
		  l = t;
		else
		  p->TokNext = t;
		p = t;

		/*
		 * now try to
		 * backtrack
		 * statically 
		 */

		if (chtype[och] == SY)
		  goto inside_symbol;
		else {
		  charp = TokImage;
		  isvar = (chtype[och] != LC);
		  *charp++ = och;
		  goto inside_letters;
		}
	      }
	      do
		*sp++ = ch;
	      while (chtype[my_fgetch()] == NU);
	    }
	    *sp = '\0';
	    TokenInfo = (CELL) float_send(TokImage);
	    goto end_of_read_number;
	  }
	  if (has_overflow) {
	    *sp = '\0';
	    /* skip base */
	    if (TokImage[0] == '0' && (TokImage[1] == 'x' || TokImage[1] == 'X'))
	      TokenInfo = read_int_overflow(TokImage+2,16,val);
	    else if (TokImage[1] == '\'')
	      TokenInfo = read_int_overflow(TokImage+2,base,val);
	    else if (TokImage[2] == '\'')
	      TokenInfo = read_int_overflow(TokImage+3,base,val);
	    else
	      TokenInfo = read_int_overflow(TokImage,base,val);
	  } else
	    TokenInfo = (CELL) MkIntegerTerm(val);
	}

      end_of_read_number:

	kind = Number_tok;
	break;

      case QT:
      case DC:
	quote = ch;
	len = 0;
	my_fgetch();
	while (1) {
	  if (charp + 1024 > (char *)AuxSp) {
	    ErrorMessage = "Heap Overflow While Scanning: please increase heap";
	    break;
	  }
	  if (ch == quote) {
	    my_fgetch();
	    if (ch != quote)
	      break;
	    my_fgetch();
	    *charp++ = ch;
	  } else if (ch == '\\' && yap_flags[CHARACTER_ESCAPE_FLAG] != CPROLOG_CHARACTER_ESCAPES) {
	    /* escape sequence */
	    ch = my_fgetch();
	    switch (ch) {
	    case 'a':
	      *charp++ = '\a';
	      my_fgetch();
	      break;
	    case 'b':
	      *charp++ = '\b';
	      my_fgetch();
	      break;
	    case 'r':
	      *charp++ = '\r';
	      my_fgetch();
	      break;
	    case 'f':
	      *charp++ = '\f';
	      my_fgetch();
	      break;
	    case 't':
	      *charp++ = '\t';
	      my_fgetch();
	      break;
	    case 'n':
	      *charp++ = '\n';
	      my_fgetch();
	      break;
	    case 'v':
	      *charp++ = '\v';
	      my_fgetch();
	      break;
	    case '\\':
	      *charp++ = '\\';
	      my_fgetch();
	      break;
	    case '\'':
	      *charp++ = '\'';
	      my_fgetch();
	      break;
	    case '"':
	      *charp++ = '"';
	      my_fgetch();
	      break;
	    case '`':
	      *charp++ = '`';
	      my_fgetch();
	      break;
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      /* character in octal: maximum of 3 digits, terminates with \ */
	      {
		unsigned char so_far = ch-'0';
		my_fgetch();
		if (ch >= '0' && ch < '8') {/* octal */
		  so_far = so_far*8+(ch-'0');
		  my_fgetch();
		  if (ch >= '0' && ch < '8') { /* octal */
		    *charp++ = so_far*8+(ch-'0');
		    my_fgetch();
		    if (ch != '\\') {
		      ErrorMessage = "invalid octal escape sequence";
		    } else {
		      my_fgetch();
		    }
		  } else if (ch == '\\') {
		    *charp++ = so_far;
		    my_fgetch();
		  } else {
		    ErrorMessage = "invalid octal escape sequence";
		  }
		} else if (ch == '\\') {
		  *charp++ = so_far;
		  my_fgetch();
		} else {
		  ErrorMessage = "invalid octal escape sequence";
		}
	      }
	      break;
	    case 'x':
              /* hexadecimal character (YAP allows empty hexadecimal  */
	      {
		unsigned char so_far = 0; 
		my_fgetch();
		if (my_isxdigit(ch,'f','F')) {/* hexa */
		  so_far = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
			    (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
		  my_fgetch();
		  if (my_isxdigit(ch,'f','F')) { /* hexa */
		    *charp++ = so_far * 16 + (chtype[ch] == NU ? ch - '0' :
			    (my_isupper(ch) ? ch - 'A' : ch - 'a') + 10);
		    my_fgetch();
		    if (ch != '\\') {
		      ErrorMessage = "invalid hexadecimal escape sequence";
		    } else {
		      my_fgetch();
		    }
		  } else if (ch == '\\') {
		    *charp++ = so_far;
		    my_fgetch();
		  } else {
		    ErrorMessage = "invalid hexadecimal escape sequence";
		  } 
		} else if (ch == '\\') {
		  *charp++ = so_far;
		  my_fgetch();
		} else {
		  ErrorMessage = "invalid hexadecimal escape sequence";
		}
	      }
	      break;
	    default:
	      /* accept sequence. Note that the ISO standard does not
		 consider this sequence legal, whereas SICStus would
	         eat up the escape sequence. */
	      ErrorMessage = "invalid escape sequence";
	    }
	  } else {
	    *charp++ = ch;
	    my_fgetch();
	  }
	  ++len;
	  if (charp > (char *)AuxSp - 1024) {
	    /* Not enough space to read in the string. */
	    ErrorMessage = "not enough heap space to read in string or quoted atom";
	    /* serious error now */
	    kind = eot_tok;
	  }
	}
	*charp = '\0';
	if (quote == '"') {
	  mp = AllocScannerMemory(len + 1);
	  if (mp == NULL) {
	    ErrorMessage = "not enough stack space to read in string or quoted atom";
	    /* serious error now */
	    kind = eot_tok;
	  }
	  strcpy(mp, TokImage);
	  TokenInfo = Unsigned(mp);
	  kind = String_tok;
	}
	else {
	  TokenInfo = Unsigned(LookupAtom(TokImage));
	  if (ch == '(')
	    solo_flag = FALSE;
	  kind = Name_tok;
	}
	break;

      case SY:
	och = ch;
	my_fgetch();
      inside_symbol:
	if (och == '/' && ch == '*') {
	  while ((ch != '/' || och != '*') && chtype[ch] != EF) {
	    och = ch;
	    my_fgetch();
	  }
	  if (chtype[ch] == EF) {
	    kind = eot_tok;
	    break;
	  }
	  my_fgetch();
	  if (t == l)
	    FirstLineInParse();
	  ReleasePreAllocCodeSpace((CODEADDR)TokImage);
	  goto get_tok;
	}
	if (och == '.' && (chtype[ch] == BS || chtype[ch] == EF
			   || chtype[ch] == CC)) {
	  eot_before_eof = TRUE;
	  if (chtype[ch] == CC)
	    while (my_fgetch() != 10 && chtype[ch] != EF);
	  kind = eot_tok;
	}
	else {
	  *charp++ = och;
	  for (; chtype[ch] == SY; my_fgetch())
	    *charp++ = ch;
	  *charp = '\0';
	  TokenInfo = Unsigned(LookupAtom(TokImage));
	  if (ch == '(')
	    solo_flag = FALSE;
	  kind = Name_tok;
	}
	break;

      case SL:
	*charp++ = ch;
	*charp++ = '\0';
	my_fgetch();
	TokenInfo = Unsigned(LookupAtom(TokImage));
	if (ch == '(')
	  solo_flag = FALSE;
	kind = Name_tok;
	break;

      case BK:
	och = ch;
	do {
	  /* skip spaces to look for stuff such as [   ] */
	  my_fgetch();
	} while (chtype[ch] == BS);
	if (och == '[' && ch == ']') {
	  TokenInfo = Unsigned(AtomNil);
	  my_fgetch();
	  if (ch == '(')
	    solo_flag = FALSE;
	  kind = Name_tok;
	}
	else {
	  if (!solo_flag && och == '(') {
	    TokenInfo = 'l';
	    solo_flag = TRUE;
	  }
	  else
	    TokenInfo = och;
	  kind = Ponctuation_tok;
	}
	break;

      case EF:
	kind = eot_tok;
	break;
#ifdef DEBUG
      default:
	YP_fprintf(YP_stderr, "\n++++ token: wrong char type %c %d\n", ch, chtype[ch]);
	kind = eot_tok;
#else
      default:
	kind = eot_tok;		/* Just to make lint happy */
#endif
      }
    }

    t->Tok = Ord(kind);
#ifdef DEBUG
    if(Option[2]) YP_fprintf(YP_stderr,"[Token %d %ld]\n",Ord(kind),(unsigned long int)TokenInfo);
#endif
    t->TokInfo = (Term) TokenInfo;
    t->TokPos = TokenPos;
    t->TokNext = NIL;
    ReleasePreAllocCodeSpace((CODEADDR)TokImage);
  } while (kind != eot_tok);
  return (l);
}
