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
 * File:		charcodes.c *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Character codes and character conversion		 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif
/**
 * @file   chartypes.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 12:05:14 2015
 *
 * @brief  Character Types
 *
 *
 */

///{@

/**
 * @addtogroup CharacterCodes
 * @{
 */

/*
 * This file includes the definition of a character properties.
 *
 */

#include "Yap.h"
#include "YapHeap.h"
#include "YapText.h"
#include "Yatom.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
/* Windows */
#include <io.h>
#endif
#if HAVE_SOCKET
#include <winsock2.h>
#endif
#include <windows.h>
#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif
#include "YapEval.h"
#include "iopreds.h"

static Int p_change_type_of_char(USES_REGS1);


const char *encvs[] = {"LANG", "LC_ALL", "LC_CTYPE", NULL};

// where we can fins an encoding
typedef struct enc_map {
  const char *s;
  encoding_t e;
} enc_map_t;

static enc_map_t ematches[] = {
    {"UTF-8", ENC_ISO_UTF8},
    {"utf8", ENC_ISO_UTF8},
    {"UTF-16", ENC_UTF16_LE}, // ok, this is a very bad name
    {"UCS-2", ENC_UTF16_LE},  // ok, this is probably gone by now
    {"ISO-LATIN1", ENC_ISO_LATIN1},
    {"ISO-8859-1", ENC_ISO_LATIN1},
    {"Windows-1252", ENC_ISO_LATIN1}, // almost, but not quite
    {"CP-1252", ENC_ISO_LATIN1},
    {"C", ENC_ISO_ASCII},
#ifdef _WIN32
    {NULL, ENC_ISO_ASCII}
#else
    {NULL, ENC_ISO_UTF8}
#endif
};

static encoding_t enc_os_default(encoding_t rc) {
  // by default, return UTF-8
  // note that we match the C locale to UTF8/16, as all Unix machines will work
  // on UNICODE.
  // WIN32 we will rely on BOM

  if (rc == ENC_ISO_ASCII) {
    return ENC_ISO_UTF8;
  }
  return rc;
}


int Yap_encoding_error(int ch, seq_type_t code, struct stream_desc *st ) {
  //  IF (LOCAL_encoding_errors == TermIgnore)
  //  return ch;

   //  return ch;
    if (st &&st->status & RepClose_Prolog_f) {
      if (st)      Yap_CloseStream(st-GLOBAL_Stream);
      return EOF;
    }
    if (!st ||st->status & RepError_Prolog_f || trueGlobalPrologFlag(ISO_FLAG)) {
      CACHE_REGS
      LOCAL_Error_TYPE = SYNTAX_ERROR;
      LOCAL_ErrorMessage = "bad codes ";
      return EOF;
   } else {
      Yap_Warning("unexpected newline while  reading quoted ");
    }

     return code;
  
}

encoding_t Yap_SystemEncoding(void) {
  int i = -1;
  while (i == -1 || encvs[i]) {
    char *v;
    if (i == -1) {
      if ((v = setlocale(LC_CTYPE, NULL)) == NULL || !strcmp(v, "C")) {
        if ((v = getenv("LC_CTYPE")))
          setlocale(LC_CTYPE, v);
        else if ((v = getenv("LANG")))
          setlocale(LC_CTYPE, v);
      }
    } else {
      v = getenv(encvs[i]);
    }
    if (v) {
      int j = 0;
      const char *coding;
      while ((coding = ematches[j].s) != NULL) {
        char *v1;
        if ((v1 = strstr(v, coding)) && strlen(v1) == strlen(coding)) {
          return ematches[j].e;
        }
        j++;
      }
    }
    i++;
  }
  return ENC_ISO_ASCII;
}

static encoding_t DefaultEncoding(void) {
  return enc_os_default(Yap_SystemEncoding());
}

encoding_t Yap_DefaultEncoding(void) {
  CACHE_REGS
  return LOCAL_encoding;
}

void Yap_SetDefaultEncoding(encoding_t new_encoding) {
  CACHE_REGS
  LOCAL_encoding = new_encoding;
}

static Int get_default_encoding(USES_REGS1) {
  Term out = MkIntegerTerm(Yap_DefaultEncoding());
  return Yap_unify(ARG1, out);
}

/**
@pred encoding(_Stream, Code )

@brief Show or set the encoding from the current stream.

*/
static Int p_encoding(USES_REGS1) { /* '$encoding'(Stream,N) */
  int sno =
      Yap_CheckStream(ARG1, Input_Stream_f | Output_Stream_f, "encoding/2");
  Term t = Deref(ARG2);
  if (sno < 0)
    return FALSE;
  if (IsVarTerm(t)) {
    return Yap_unify(ARG2, MkIntegerTerm(GLOBAL_Stream[sno].encoding));
  }
  GLOBAL_Stream[sno].encoding = IntegerOfTerm(Deref(ARG2));
  return TRUE;
}

/**
   get_char( + Code, -Char)
   if the number _Code_ represents a valid Unicode point, the atom _Char_ will represent the same
   unicode point.
*/
static int get_char(Term t) {
  CACHE_REGS
  if (IsVarTerm(t = Deref(t))) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, NULL);
    return 0;
  }
  if (!IsAtomTerm(t)) {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t, NULL);
    return 0;
  }
  Atom at = AtomOfTerm(t);
  unsigned char *s = RepAtom(at)->UStrOfAE;
  utf8proc_int32_t c;
  s += get_utf8(s, 1, &c);
  return c;
  if (s[0] != '\0') {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t, NULL);
    return 0;
  }
  return c;
}

/**
   @pred get_char( +Char -Code)

   if the  atom _Char_ represents a valid Unicode point, the number _Coder_ will represent the same
   unicode point.
*/
static int get_code(Term t) {
  if (IsVarTerm(t = Deref(t))) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, NULL);
    return 0;
  }
  if (!IsIntegerTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_CHARACTER_CODE, t, NULL);
    return 0;
  }
  Int ch = IntegerOfTerm(t);
  if (ch < -1) {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER_CODE, t, NULL);
    return 0;
  }
  return ch;
}

/**
   @pred get_char_or_code( +CharOrCode, -CodeOrChar)

 convert from char to code or from code to char.
*/
static int get_char_or_code(Term t, bool *is_char) {
  CACHE_REGS
  if (!IsAtomTerm(t)) {
    if (!IsIntegerTerm(t)) {
      Yap_ThrowError(TYPE_ERROR_CHARACTER, t, NULL);
      return 0;
    }
    Int ch = IntegerOfTerm(t);
    if (ch < -1) {
      Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER_CODE, t, NULL);
      return 0;
    }
    *is_char = false;
    return ch;
  }
  unsigned char *s0 = RepAtom(AtomOfTerm(t))->UStrOfAE;
  int val;
  s0 += get_utf8(s0, 1, &val);
  if (s0[0] != '\0') {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t, NULL);
    return 0;
  }
  *is_char = true;
  return val;
}

static bool to_upper( Term t, Term t2)
{
   bool is_char = false;
   Int out = get_char_or_code(t, &is_char), uout;
    uout = towupper(out);
    if (is_char)
      return Yap_unify(t2, MkCharTerm(uout));
    else
      return Yap_unify(t2,MkIntegerTerm(uout));
  
}

static bool to_lower( Term t, Term t2)
{
    bool is_char = false;

    Int out = get_char_or_code(t, &is_char), uout;
    uout = towlower(out);
    if (is_char)
      return Yap_unify(t2, MkCharTerm(uout));
    else
      return Yap_unify(t2,MkIntegerTerm(uout));
  
}

/** @pred  to_upper(?LowC, ?LUpC)

    UpC is the upper case bersion of LowC, or LowC is the lower case version of UpC.
 */
static Int toupper2(USES_REGS1) {
  Term t;
  if (!IsVarTerm(t = Deref(ARG1))) {
    return to_upper(t,ARG2);
  } else if (!IsVarTerm(t = Deref(ARG2))) {
     return to_lower(t,ARG1);
  } else {
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
  }
  return false;
}

/** @pred  to_lower( ?LUpC, ?LowC)

      LowC is the lower case version of UpC., or UpC is the upper case bersion of LowC,
 */
static Int tolower2(USES_REGS1) {
  Term t;
  if (!IsVarTerm(t = Deref(ARG1))) {
     return to_lower(t,ARG2);
  } else if (IsVarTerm(t = Deref(ARG2))) {
     return to_upper(t,ARG1);  } else {
    Yap_ThrowError(INSTANTIATION_ERROR, ARG1, NULL);
  }
  return false;
}

/** @pred  change_type_of_char( ?Code, Value)

    set the type of ASCII code Code to Value (a number defined in YapScanner.h)
 */
static Int
    p_change_type_of_char(USES_REGS1) { /* change_type_of_char(+char,+type) */
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  if (!IsVarTerm(t1) && !IsIntegerTerm(t1))
    return FALSE;
  if (!IsVarTerm(t2) && !IsIntegerTerm(t2))
    return FALSE;
  Yap_chtype[IntegerOfTerm(t1)] = IntegerOfTerm(t2);
  return TRUE;
}

static bool type_alpha(int ch)
{
  return iswalpha(ch);
}


/** @pred  code_type_alpha( Code )

Holds true if Code is a letter of the alphabet.
 */
static Int code_type_alpha(USES_REGS1) {
  return type_alpha(  get_code(ARG1) );
}

/** @pred  char_type_alpha( Char )

Holds true if Char is a letter of the alphabet.
 */
static Int char_type_alpha(USES_REGS1) {
  return type_alpha(  get_char(ARG1) );
}


static bool type_alnum(int ch)
{
  return iswalnum(ch);
}
/** @pred  code_type_alnum( Code )

Holds true if Code is a letter of the alphabet or a digit.
 */
static Int code_type_alnum(USES_REGS1) {
  return type_alnum(  get_code(ARG1) );
}

/** @pred  char_type_alnum( Char )

Holds true if Char is a letter of the alphabet or a digit.
 */
static Int char_type_alnum(USES_REGS1) {
  return type_alnum(  get_char(ARG1) );
}


static bool type_ascii(int ch)
{
  return isascii(ch);
}

/** @pred  char_type_ascii( Char )

Holds true if Char belongs to the ASCII code.
 */
static Int char_type_ascii(USES_REGS1) {
  return type_ascii( get_char(ARG1) );
}

/** @pred  code_type_ascii( Code )

Holds true if Code belongs to the ASCII code.
 */
static Int code_type_ascii(USES_REGS1) {
  return type_ascii( get_code(ARG1) );
}

  static bool type_cntrl(int ch)
{
  return iswcntrl(ch);
}

/** @pred  char_type_cntrl( Char )

    Holds true if Char is an ASCII control character.
*/
static Int char_type_cntrl(USES_REGS1) {
  return type_cntrl( get_char(ARG1) );
}

/** @pred  code_type_cntrl( Code )

    Holds true if Code is an ASCII control character.
*/
static Int code_type_cntrl(USES_REGS1) {
  return type_cntrl( get_code(ARG1) );
}

  static bool type_csym(int ch)
{
  if (ch=='_' || ch ==0x0332)
    return true;
  return iswalnum(ch);
}

/** @pred  code_type_csym( Code )

Holds true if Code is a letter, digit or underscore.
 */
static Int code_type_csym(USES_REGS1) {
  return type_csym( get_code(ARG1) );
}

/** @pred  char_type_csym( Char )

Holds true if Char is a letter, digit or underscore.
 */
static Int char_type_csym(USES_REGS1) {
  return type_csym( get_char(ARG1) );
}


static bool type_csymf(int ch)
{
  if (ch=='_' || ch ==0x0332)
    return true;
  return iswupper(ch);
}
/** @pred  char_type_csymf( Char )

Holds true if Char is an uppercase letter, digit or underscore.
 */
static Int char_type_csymf(USES_REGS1) {
  return type_csymf( get_char(ARG1) );
}
/** @pred  code_type_csymf( Code )

Holds true if Code is an uppercase letter, digit or underscore.
 */
static Int code_type_csymf(USES_REGS1) {
  return type_csymf( get_code(ARG1) );
}

static bool type_digit(int ch)
{
  return iswdigit(ch);
}
/** @pred  char_type_digit( Char )

    Holds true if Char is a character between `'0'` and `'9'`.
*/
static Int char_type_digit(USES_REGS1) {
  return type_digit( get_char(ARG1) );
}
/** @pred  code_type_digit( Code )

    Holds true if Code is a character between `'0'` and `'9'`.
*/
static Int code_type_digit(USES_REGS1) {
  return type_digit( get_code(ARG1) );
}


static bool type_xdigit(int ch)
{
  return iswxdigit(ch);
}
/** @pred  char_type_xdigit( Char )

    Holds true if Char is an hexadecimal digit.
*/
static Int char_type_xdigit(USES_REGS1) {
  return type_xdigit( get_char(ARG1) );
}
/** @pred  code_type_xdigit( Code )

    Holds true if Code is an hexadecimal digit.
*/
static Int code_type_xdigit(USES_REGS1) {
  return type_xdigit( get_code(ARG1) );
}





static bool type_graph(int ch)
{
  return iswgraph(ch);
}
/** @pred  char_type_graph( Char )

    Holds true if Char outputs a mark.
*/
static Int char_type_graph(USES_REGS1) {
  return type_graph( get_char(ARG1) );

}

/** @pred  code_type_graph( Code )

    Holds true if Code outputs a mark.
*/
static Int code_type_graph(USES_REGS1) {
  return type_graph( get_code(ARG1) );

}


static bool type_lower(int ch)
{
  return iswlower(ch);
}
/** @pred  code_type_lower( Code )

    Holds true if Code is lower case.
*/
static Int code_type_lower(USES_REGS1) {
  return  type_lower( get_code(ARG1) );
}

/** @pred  char_type_lower( Char )

    Holds true if Char is lower case.
*/
static Int char_type_lower(USES_REGS1) {
    return  type_lower( get_char(ARG1) );
}


static bool type_punct(int ch)
{
  return iswpunct(ch);
}
/** @pred  char_type_punct( Char )

    Holds true if Char is a punctuation character.
*/
static Int char_type_punct(USES_REGS1) {
  return type_punct(get_char(ARG1) );
}

/** @pred  code_type_punct( Code )

    Holds true if Code is a punctuation codeacter.
*/
static Int code_type_punct(USES_REGS1) {
  return type_punct(get_code(ARG1) );
}


static bool type_space(int ch)
{
  return iswspace(ch);
}
/** @pred  char_type_space( Char )

    Holds true if Char only moves the output.

    
*/
static Int char_type_space(USES_REGS1) {
  return type_space(get_char(ARG1) );
}

/** @pred  code_type_space( Code )

    Holds true if Code only moves the output.
*/
static Int code_type_space(USES_REGS1) {
  return type_space(get_code(ARG1) );
}
/** @pred  code_type_space( Code )

    Holds true if Code only moves the output.
*/

static bool type_upper(int ch)
{
  return iswupper(ch);
}
/** @pred  char_type_upper( Char )

    Holds true if Char is upper case.
*/
static Int char_type_upper(USES_REGS1) {
  return type_upper (get_char(ARG1) );

}
/** @pred  code_type_upper( Code )

    Holds true if Code is upper case.
*/
static Int code_type_upper(USES_REGS1) {
  return type_upper( get_code(ARG1) );
}


static bool type_white(int ch)
{
  return ch == ' '||ch == '\t';
}
/** @pred  char_type_white( Char )

    Holds true if Char is a space or tab character.
*/
static Int char_type_white(USES_REGS1) {
  return type_white( get_char(ARG1) );
}
/** @pred  code_type_white( Code )

    Holds true if Code is a space or tab character.
*/
static Int code_type_white(USES_REGS1) {
  return type_white(get_code(ARG1));
}


/** @pred  char_type_end_of_file( Char )

    Holds true if Char represents a file that has been completely read..
*/
static Int char_type_end_of_file(USES_REGS1) {
  Int ch = get_char(ARG1);
  return ch == WEOF || ch == -1;
}

/** @pred  char_type_end_of_line( Char )

    Holds true if Char ends a line, say `newline` or `carriage return .
    
*/
static Int char_type_end_of_line(USES_REGS1) {
  Int ch = get_char(ARG1);
  if (ch < 256) {
    return ch >= 10 && ch <= 13;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return (ct >= UTF8PROC_CATEGORY_ZL && ct <= UTF8PROC_CATEGORY_ZP);
}

/** @pred  char_type_newline( Char )

    Holds true if Char starts a line, say `newline` .
    
*/
static Int char_type_newline(USES_REGS1) {
  Int ch = get_char(ARG1);
  if (ch < 256) {
    return ch == 10;
  }
  return false;
}

/** @pred  char_type_period( Char )
    Holds true if Char closes a sentence, such as `.`, `!`, or `?`.
    
*/
static Int char_type_period(USES_REGS1) {
  Int ch = get_char(ARG1);
  return ch == '.' || ch == '!' || ch == '?';
}

/** @pred  char_type_quote( Char )
    Holds true if Char  delimits quoted text.
    
*/
static Int char_type_quote(USES_REGS1) {
  Int ch = get_char(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PI || ct == UTF8PROC_CATEGORY_PF;
}

/** @pred  char_type_paren( Char )
    Holds true if Char  is a bracket, curly bracket, square bracket, or similar.
    
*/
static Int char_type_paren(USES_REGS1) {
  Int ch = get_char(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PS || ct == UTF8PROC_CATEGORY_PE;
}

/** @pred  char_type_var_start( Char )
    Holds true if Char  can be used to start a variable.
    
*/
static Int char_type_prolog_var_start(USES_REGS1) {
  Int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == LC || ch == '_';
}

/** @pred  char_type_prolog_atom_start( Char )
    Holds true if Char  can be used to start an atom.
    
*/
static Int char_type_prolog_atom_start(USES_REGS1) {
  Int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC;
}

/** @pred  char_type_prolog_identifier_continue( Char )
    Holds true if Char  can be used to extend an atom or variable.
    
*/
static Int char_type_prolog_identifier_continue(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= NU;
}

/** @pred  char_type_prolog_prolog_symbol( Char )
    Holds true if Char  can be used as a symbol.
    
*/
static Int char_type_prolog_prolog_symbol(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == SL || k == SY;
}







/** @pred  code_type_end_of_file( Code )

    Holds true if Code represents a file that has been completely read..
*/
static Int code_type_end_of_file(USES_REGS1) {
  Int ch = get_code(ARG1);
  return ch == WEOF || ch == -1;
}

/** @pred  code_type_end_of_line( Code )

    Holds true if Code ends a line, say `newline` or `carriage return .
    
*/
static Int code_type_end_of_line(USES_REGS1) {
  Int ch = get_code(ARG1);
  if (ch < 256) {
    return ch >= 10 && ch <= 13;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return (ct >= UTF8PROC_CATEGORY_ZL && ct <= UTF8PROC_CATEGORY_ZP);
}

/** @pred  code_type_newline( Code )

    Holds true if Code starts a line, say `newline` .
    
*/
static Int code_type_newline(USES_REGS1) {
  Int ch = get_code(ARG1);
  if (ch < 256) {
    return ch == 10;
  }
  return false;
}

/** @pred  code_type_period( Code )
    Holds true if Code closes a sentence, such as `.`, `!`, or `?`.
    
*/
static Int code_type_period(USES_REGS1) {
  Int ch = get_code(ARG1);
  return ch == '.' || ch == '!' || ch == '?';
}

/** @pred  code_type_quote( Code )
    Holds true if Code  delimits quoted text.
    
*/
static Int code_type_quote(USES_REGS1) {
  Int ch = get_code(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PI || ct == UTF8PROC_CATEGORY_PF;
}

/** @pred  code_type_paren( Code )
    Holds true if Code  is a bracket, curly bracket, square bracket, or similar.
    
*/static Int code_type_paren(USES_REGS1) {
  Int ch = get_code(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PS || ct == UTF8PROC_CATEGORY_PE;
}

/** @pred  code_type_var_start( Code )
    Holds true if Code  can be used to start a variable.
    
*/
static Int code_type_prolog_var_start(USES_REGS1) {
  Int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == LC || ch == '_';
}

/** @pred  code_type_prolog_atom_start( Code )
    Holds true if Code  can be used to start an atom.
    
*/
static Int code_type_prolog_atom_start(USES_REGS1) {
  Int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC;
}

/** @pred  code_type_prolog_identifier_continue( Code )
    Holds true if Code  can be used to extend an atom or variable.
*/
static Int code_type_prolog_identifier_continue(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= NU;



}

/** @pred  code_type_prolog_symbol( Code )
    Holds true if Code  can be used as a symbol.
    
*/
static Int code_type_prolog_prolog_symbol(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == SL || k == SY;
}


/** @pred  code_type_prolog_symbol( Code )
    Holds true if Code  can be used as a symbol.
    
*/
static Int code_char(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  if (IsVarTerm(t1)) {
    if (t2 == TermEof)
      return Yap_unify(ARG1,MkIntTerm(-1));      
    int ch = get_char(t1);
    return Yap_unify(ARG2,MkCharTerm(ch));
  }
  int ch = get_code(t1);
  if (ch < 0)
    return Yap_unify(ARG2,TermEof);
  return Yap_unify(ARG2,MkCharTerm(ch));
}

static Int char_code(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  if (IsVarTerm(t1)) {
    if (t2 == TermEof)
      return Yap_unify(ARG1,MkIntTerm(-1));      
    int ch = get_code(t2);
    return Yap_unify(ARG1,MkCharTerm(ch));
  }
  int ch = get_char(t1);
  if (ch < 0)
    return Yap_unify(ARG2,MkIntTerm(-1));
  return Yap_unify(ARG2,MkIntTerm(ch));
}

int ISOWGetc(int sno) {
  int ch = GLOBAL_Stream[sno].stream_wgetc(sno);
  if (ch != EOF && GLOBAL_CharConversionTable != NULL) {

    if (ch < NUMBER_OF_CHARS) {
      /* only do this in ASCII */
      return GLOBAL_CharConversionTable[ch];
    }
  }
  return ch;
}

/** @pred  force_char_conversion

    Enable the ISO char conversion mechanism.
    
*/
static Int p_force_char_conversion(USES_REGS1) {
  int i;

  /* don't actually enable it until someone tries to add a conversion */
  if (GLOBAL_CharConversionTable2 == NULL)
    return (TRUE);
  for (i = 0; i < MaxStreams; i++) {
    if (!(GLOBAL_Stream[i].status & Free_Stream_f))
      GLOBAL_Stream[i].stream_wgetc_for_read = ISOWGetc;
  }
  GLOBAL_CharConversionTable = GLOBAL_CharConversionTable2;
  return (TRUE);
}

/** @pred  force_char_conversion

    Enable the ISO char conversion mechanism.
    
*/
static Int p_disable_char_conversion(USES_REGS1) {
  int i;

  for (i = 0; i < MaxStreams; i++) {
    if (!(GLOBAL_Stream[i].status & Free_Stream_f))
      GLOBAL_Stream[i].stream_wgetc_for_read = GLOBAL_Stream[i].stream_wgetc;
  }
  GLOBAL_CharConversionTable = NULL;
  return (TRUE);
}

/** @pred  char_conversion(Inp,Out)

    Apply the ISO char conversion mechanism.
    
*/
static Int char_conversion(USES_REGS1) {
  Term t = Deref(ARG1), t1 = Deref(ARG2);
  unsigned char *s0, *s1;

  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "char_conversion/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t)) {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t, "char_conversion/2");
    return (FALSE);
  }
  s0 = RepAtom(AtomOfTerm(t))->UStrOfAE;
  if (s0[1] != '\0') {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t, "char_conversion/2");
    return (FALSE);
  }
  if (IsVarTerm(t1)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t1, "char_conversion/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
    return (FALSE);
  }
  s1 = RepAtom(AtomOfTerm(t1))->UStrOfAE;
  if (s1[1] != '\0') {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
    return (FALSE);
  }
  /* check if we do have a table for converting characters */
  if (GLOBAL_CharConversionTable2 == NULL) {
    int i;

    /* don't create a table if we don't need to */
    if (s0[0] == s1[0])
      return (TRUE);
    GLOBAL_CharConversionTable2 =
        Yap_AllocCodeSpace(NUMBER_OF_CHARS * sizeof(char));
    while (GLOBAL_CharConversionTable2 == NULL) {
      if (!Yap_growheap(FALSE, NUMBER_OF_CHARS * sizeof(char), NULL)) {
        Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
        return (FALSE);
      }
    }
    if (trueGlobalPrologFlag(CHAR_CONVERSION_FLAG)) {
      CACHE_REGS
      if (p_force_char_conversion(PASS_REGS1) == FALSE)
        return (FALSE);
    }
    for (i = 0; i < NUMBER_OF_CHARS; i++)
      GLOBAL_CharConversionTable2[i] = i;
  }
  /* just add the new entry */
  GLOBAL_CharConversionTable2[(int)s0[0]] = s1[0];
  /* done */
  return (TRUE);
}

/** @pred  current_char_conversion(Inp,Out)

    Display the current ISO char conversion mechanism.
    
*/
static Int p_current_char_conversion(USES_REGS1) {
  Term t, t1;
  unsigned char *s0, *s1;

  if (GLOBAL_CharConversionTable == NULL) {
    return (FALSE);
  }
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_ThrowError(INSTANTIATION_ERROR, t, "current_char_conversion/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t)) {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t,
                   "current_char_conversion/2");
    return (FALSE);
  }
  s0 = RepAtom(AtomOfTerm(t))->UStrOfAE;
  if (s0[1] != '\0') {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t,
                   "current_char_conversion/2");
    return (FALSE);
  }
  t1 = Deref(ARG2);
  if (IsVarTerm(t1)) {
    char out[2];
    if (GLOBAL_CharConversionTable[(int)s0[0]] == '\0')
      return (FALSE);
    out[0] = GLOBAL_CharConversionTable[(int)s0[0]];
    out[1] = '\0';
    return (Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(out))));
  }
  if (!IsAtomTerm(t1)) {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t1,
                   "current_char_conversion/2");
    return (FALSE);
  }
  s1 = RepAtom(AtomOfTerm(t1))->UStrOfAE;
  if (s1[1] != '\0') {
    Yap_ThrowError(REPRESENTATION_ERROR_CHARACTER, t1,
                   "current_char_conversion/2");
    return (FALSE);
  } else {
    return (GLOBAL_CharConversionTable[(int)s0[0]] == '\0' &&
            GLOBAL_CharConversionTable[(int)s0[0]] == s1[0]);
  }
}

/** @pred  all_char_conersions(List)

    Returns all the current ISO char conversion mechanism.
    
*/static Int p_all_char_conversions(USES_REGS1) {
  Term out = TermNil;
  int i;

  if (GLOBAL_CharConversionTable == NULL) {
    return (FALSE);
  }
  for (i = NUMBER_OF_CHARS; i > 0;) {
    i--;
    if (GLOBAL_CharConversionTable[i] != '\0') {
      Term t1, t2;
      char s[2];
      s[1] = '\0';
      s[0] = GLOBAL_CharConversionTable[i];
      t1 = MkAtomTerm(Yap_LookupAtom(s));
      out = MkPairTerm(t1, out);
      s[0] = i;
      t2 = MkAtomTerm(Yap_LookupAtom(s));
      out = MkPairTerm(t2, out);
    }
  }
  return (Yap_unify(ARG1, out));
}

void Yap_InitChtypes(void) {
  CACHE_REGS
  LOCAL_encoding = DefaultEncoding();
  Yap_InitCPred("$change_type_of_char", 2, p_change_type_of_char,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("toupper", 2, toupper2, SafePredFlag);
  Yap_InitCPred("tolower", 2, tolower2, SafePredFlag);
  Yap_InitCPred("char_conversion", 2, char_conversion, SyncPredFlag);

  Yap_InitCPred("$get_default_encoding", 1, get_default_encoding,
                SafePredFlag | HiddenPredFlag);

  Yap_InitCPred("$encoding", 2, p_encoding, SafePredFlag | SyncPredFlag),

      Yap_InitCPred("$current_char_conversion", 2, p_current_char_conversion,
                    SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$all_char_conversions", 1, p_all_char_conversions,
                SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$force_char_conversion", 0, p_force_char_conversion,
                SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$disable_char_conversion", 0, p_disable_char_conversion,
                SyncPredFlag | HiddenPredFlag);
  //  CurrentModule = CHTYPE_MODULE;
  Yap_InitCPred("char_type_alnum", 1, char_type_alnum, SafePredFlag);
  Yap_InitCPred("char_type_alpha", 1, char_type_alpha, SafePredFlag);
  Yap_InitCPred("char_type_csym", 1, char_type_csym, SafePredFlag);
  Yap_InitCPred("char_type_csymf", 1, char_type_csymf, SafePredFlag);
  Yap_InitCPred("char_type_ascii", 1, char_type_ascii, SafePredFlag);
  Yap_InitCPred("char_type_white", 1, char_type_white, SafePredFlag);
  Yap_InitCPred("char_type_cntrl", 1, char_type_cntrl, SafePredFlag);
  Yap_InitCPred("char_type_digit", 1, char_type_digit, SafePredFlag);
  Yap_InitCPred("char_type_xdigit", 1, char_type_xdigit, SafePredFlag);
  Yap_InitCPred("char_type_graph", 1, char_type_graph, SafePredFlag);
  Yap_InitCPred("char_type_lower", 1, char_type_lower, SafePredFlag);
  Yap_InitCPred("char_type_upper", 1, char_type_upper, SafePredFlag);
  Yap_InitCPred("char_type_punct", 1, char_type_punct, SafePredFlag);
  Yap_InitCPred("char_type_space", 1, char_type_space, SafePredFlag);
  Yap_InitCPred("char_type_end_of_file", 1, char_type_end_of_file,
                SafePredFlag);
  Yap_InitCPred("char_type_end_of_line", 1, char_type_end_of_line,
                SafePredFlag);
  Yap_InitCPred("char_type_newline", 1, char_type_newline, SafePredFlag);
  Yap_InitCPred("char_type_period", 1, char_type_period, SafePredFlag);
  Yap_InitCPred("char_type_quote", 1, char_type_quote, SafePredFlag);
  Yap_InitCPred("char_type_paren", 1, char_type_paren, SafePredFlag);
  Yap_InitCPred("char_type_prolog_var_start", 1, char_type_prolog_var_start,
                SafePredFlag);
  Yap_InitCPred("char_type_prolog_atom_start", 1, char_type_prolog_atom_start,
                SafePredFlag);
  Yap_InitCPred("char_type_prolog_identifier_continue", 1,
                char_type_prolog_identifier_continue, SafePredFlag);
  Yap_InitCPred("char_type_prolog_prolog_symbol", 1,
                char_type_prolog_prolog_symbol, SafePredFlag);
  Yap_InitCPred("code_type_alnum", 1, code_type_alnum, SafePredFlag);
  Yap_InitCPred("code_type_alpha", 1, code_type_alpha, SafePredFlag);
  Yap_InitCPred("code_type_csym", 1, code_type_csym, SafePredFlag);
  Yap_InitCPred("code_type_csymf", 1, code_type_csymf, SafePredFlag);
  Yap_InitCPred("code_type_ascii", 1, code_type_ascii, SafePredFlag);
  Yap_InitCPred("code_type_white", 1, code_type_white, SafePredFlag);
  Yap_InitCPred("code_type_cntrl", 1, code_type_cntrl, SafePredFlag);
  Yap_InitCPred("code_type_digit", 1, code_type_digit, SafePredFlag);
  Yap_InitCPred("code_type_xdigit", 1, code_type_xdigit, SafePredFlag);
  Yap_InitCPred("code_type_graph", 1, code_type_graph, SafePredFlag);
  Yap_InitCPred("code_type_lower", 1, code_type_lower, SafePredFlag);
  Yap_InitCPred("code_type_upper", 1, code_type_upper, SafePredFlag);
  Yap_InitCPred("code_type_punct", 1, code_type_punct, SafePredFlag);
  Yap_InitCPred("code_type_space", 1, code_type_space, SafePredFlag);
  Yap_InitCPred("code_type_end_of_file", 1, code_type_end_of_file,
                SafePredFlag);
  Yap_InitCPred("code_type_end_of_line", 1, code_type_end_of_line,
                SafePredFlag);
  Yap_InitCPred("code_type_newline", 1, code_type_newline, SafePredFlag);
  Yap_InitCPred("code_type_period", 1, code_type_period, SafePredFlag);
  Yap_InitCPred("code_type_quote", 1, code_type_quote, SafePredFlag);
  Yap_InitCPred("code_type_paren", 1, code_type_paren, SafePredFlag);
  Yap_InitCPred("code_type_prolog_var_start", 1, code_type_prolog_var_start,
                SafePredFlag);
  Yap_InitCPred("code_type_prolog_atom_start", 1, code_type_prolog_atom_start,
                SafePredFlag);
  Yap_InitCPred("code_type_prolog_identifier_continue", 1,
                code_type_prolog_identifier_continue, SafePredFlag);
  Yap_InitCPred("code_type_prolog_prolog_symbol", 1,
                code_type_prolog_prolog_symbol, SafePredFlag);
  Yap_InitCPred("char_code", 2,char_code, SafePredFlag);
  Yap_InitCPred("code_char", 2,code_char, SafePredFlag);
  CurrentModule = PROLOG_MODULE;
}

///  @}
