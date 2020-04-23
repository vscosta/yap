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
 * @brief  Character Properties
 *
 *
 */

///{@

/**
 * @defgroup CharIO Character-Based Input/Output
 * @ingroup  InputOutput
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

Term Yap_StringToNumberTerm(const char *s, encoding_t *encp, bool error_on) {
  CACHE_REGS
  int sno;
  int i = push_text_stack();
  Atom nat = Yap_LookupAtom(Yap_StrPrefix(s, 16));
  sno = Yap_open_buf_read_stream(s, strlen(s), encp, MEM_BUF_USER, nat, MkAtomTerm(Yap_LookupAtom("eval")));
  if (sno < 0)
    return FALSE;
  if (encp)
    GLOBAL_Stream[sno].encoding = *encp;
  else
    GLOBAL_Stream[sno].encoding = LOCAL_encoding;
#ifdef __ANDROID__
  while (*s && isblank(*s) && Yap_wide_chtype(*s) == BS)
    s++;
#endif
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  Term t = Yap_scan_num(GLOBAL_Stream + sno, error_on);
    Yap_CloseStream(sno);
  UNLOCK(GLOBAL_Stream[sno].streamlock);
    pop_text_stack(i);
  return t;
}

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

static Int p_encoding(USES_REGS1) { /* '$encoding'(Stream,N) */
  int sno =
      Yap_CheckStream(ARG1, Input_Stream_f | Output_Stream_f, "encoding/2");
  Term t = Deref(ARG2);
  if (sno < 0)
    return FALSE;
  if (IsVarTerm(t)) {
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify(ARG2, MkIntegerTerm(GLOBAL_Stream[sno].encoding));
  }
  GLOBAL_Stream[sno].encoding = IntegerOfTerm(Deref(ARG2));
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return TRUE;
}

static int get_char(Term t) {
  if (IsVarTerm(t = Deref(t))) {
    Yap_Error(INSTANTIATION_ERROR, t, NULL);
    return 0;
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, NULL);
    return 0;
  }
  Atom at = AtomOfTerm(t);
  unsigned char *s = RepAtom(at)->UStrOfAE;
  utf8proc_int32_t c;
  s += get_utf8(s, 1, &c);
  return c;
  if (s[0] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, NULL);
    return 0;
  }
  return c;
}

static int get_code(Term t) {
  if (IsVarTerm(t = Deref(t))) {
    Yap_Error(INSTANTIATION_ERROR, t, NULL);
    return 0;
  }
  if (!IsIntegerTerm(t)) {
    Yap_Error(TYPE_ERROR_CHARACTER_CODE, t, NULL);
    return 0;
  }
  Int ch = IntegerOfTerm(t);
  if (ch < -1) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, t, NULL);
    return 0;
  }
  return ch;
}

static int get_char_or_code(Term t, bool *is_char) {
  if (!IsAtomTerm(t)) {
    if (!IsIntegerTerm(t)) {
      Yap_Error(TYPE_ERROR_CHARACTER, t, NULL);
      return 0;
    }
    Int ch = IntegerOfTerm(t);
    if (ch < -1) {
      Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, t, NULL);
      return 0;
    }
    *is_char = false;
    return ch;
  }
  unsigned char *s0 = RepAtom(AtomOfTerm(t))->UStrOfAE;
  int val;
  s0 += get_utf8(s0, 1, &val);
  if (s0[0] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, NULL);
    return 0;
  }
  *is_char = true;
  return val;
}

static Int toupper2(USES_REGS1) {
  bool is_char = false;
  Term t;
  if (!IsVarTerm(t = Deref(ARG1))) {
    Int out = get_char_or_code(t, &is_char), uout;
    if (out < 128)
      uout = toupper(out);
    else
      uout = utf8proc_toupper(out);
    if (is_char)
      return Yap_unify(ARG2, MkCharTerm(uout));
    else
      return Yap_unify(ARG2, MkIntegerTerm(uout));
  } else if (!IsVarTerm(t = Deref(ARG2))) {
    Int uout = get_char_or_code(t, &is_char), out;
    char_kind_t charp = Yap_wide_chtype(uout);
    if (charp == UC) {
      if (uout < 128)
        out = tolower(uout);
      else
        out = utf8proc_tolower(uout);
    } else {
      out = uout;
    }
    if (is_char)
      return Yap_unify(ARG1, MkCharTerm(out));
    else
      return Yap_unify(ARG1, MkIntegerTerm(out));
  } else {
    Yap_Error(INSTANTIATION_ERROR, ARG1, NULL);
  }
  return false;
}

static Int tolower2(USES_REGS1) {
  bool is_char = false;
  Term t;
  if (!IsVarTerm(t = Deref(ARG1))) {
    bool is_char = false;
    Int out = get_char_or_code(ARG1, &is_char), uout;
    if (out < 128)
      uout = tolower(out);
    else
      uout = utf8proc_tolower(out);
    if (is_char)
      return Yap_unify(ARG2, MkCharTerm(uout));
    else
      return Yap_unify(ARG2, MkIntegerTerm(uout));
  } else if (IsVarTerm(t = Deref(ARG2))) {
    Int uout = get_char_or_code(t, &is_char), out;
    char_kind_t charp = Yap_wide_chtype(uout);
    if (charp == LC) {
      if (uout < 128)
        out = toupper(uout);
      else
        out = utf8proc_toupper(uout);
    } else {
      out = uout;
    }
    if (is_char)
      return Yap_unify(ARG1, MkCharTerm(out));
    else
      return Yap_unify(ARG1, MkIntegerTerm(out));
  } else {
    Yap_Error(INSTANTIATION_ERROR, ARG1, NULL);
  }
  return false;
}

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

static Int char_type_alnum(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC || k == LC || k == NU;
}

static Int char_type_alpha(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC || k == LC;
}

static Int char_type_csym(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= NU;
}

static Int char_type_csymf(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= LC;
}

static Int char_type_ascii(USES_REGS1) {
  int ch = get_char(ARG1);
  return isascii(ch);
}

static Int char_type_white(USES_REGS1) {
  int ch = get_char(ARG1);
  if (ch < 256) {
    char_kind_t k = Yap_chtype[ch];
    return k == BS;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_ZS;
}

static Int char_type_cntrl(USES_REGS1) {
  Int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == BG;
}

static Int char_type_digit(USES_REGS1) {
  Int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == NU;
}

static Int char_type_xdigit(USES_REGS1) {
  Int ch = get_char(ARG1);
#if HAVE_ISWXDIGIT
  return iswxdigit(ch);
#elif HAVE_ISWHEXNUMBER
  return iswhexnumber(ch);
#else
  return iswdigit(ch) || ((ch >= 'a' && ch <= 'f') && (ch >= 'A' && ch <= 'F'));
#endif
}

static Int char_type_graph(USES_REGS1) {
  Int ch = get_char(ARG1);
  return iswgraph(ch);
}

static Int char_type_lower(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == LC;
}

static Int char_type_upper(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC;
}

static Int char_type_punct(USES_REGS1) {
  int ch = get_char(ARG1);
  if (ch < 256) {
    char_kind_t k = Yap_chtype[ch];
    return k >= QT && k <= BK;
  }
  return false;
}

static Int char_type_space(USES_REGS1) {
  int ch = get_char(ARG1);
  if (ch < 256) {
    char_kind_t k = Yap_chtype[ch];
    return k == BS;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return (ct >= UTF8PROC_CATEGORY_ZS && ct <= UTF8PROC_CATEGORY_PO);
}

static Int char_type_end_of_file(USES_REGS1) {
  Int ch = get_char(ARG1);
  return ch == WEOF || ch == -1;
}

static Int char_type_end_of_line(USES_REGS1) {
  Int ch = get_char(ARG1);
  if (ch < 256) {
    return ch >= 10 && ch <= 13;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return (ct >= UTF8PROC_CATEGORY_ZL && ct <= UTF8PROC_CATEGORY_ZP);
}

static Int char_type_newline(USES_REGS1) {
  Int ch = get_char(ARG1);
  if (ch < 256) {
    return ch == 10;
  }
  return false;
}

static Int char_type_period(USES_REGS1) {
  Int ch = get_char(ARG1);
  return ch == '.' || ch == '!' || ch == '?';
}

static Int char_type_quote(USES_REGS1) {
  Int ch = get_char(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PI || ct == UTF8PROC_CATEGORY_PF;
}

static Int char_type_paren(USES_REGS1) {
  Int ch = get_char(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PS || ct == UTF8PROC_CATEGORY_PE;
}

static Int char_type_prolog_var_start(USES_REGS1) {
  Int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == LC || ch == '_';
}

static Int char_type_prolog_atom_start(USES_REGS1) {
  Int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC;
}

static Int char_type_prolog_identifier_continue(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= NU;
}

static Int char_type_prolog_prolog_symbol(USES_REGS1) {
  int ch = get_char(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == SL && k == SY;
}

static Int code_type_alnum(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC || k == LC || k == NU;
}

static Int code_type_alpha(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC || k == LC;
}

static Int code_type_csym(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= NU;
}

static Int code_type_csymf(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= LC;
}

static Int code_type_ascii(USES_REGS1) {
  int ch = get_code(ARG1);
  return isascii(ch);
}

static Int code_type_white(USES_REGS1) {
  int ch = get_code(ARG1);
  if (ch < 256) {
    char_kind_t k = Yap_chtype[ch];
    return k == BS;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_ZS;
}

static Int code_type_cntrl(USES_REGS1) {
  Int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == BG;
}

static Int code_type_digit(USES_REGS1) {
  Int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == NU;
}

static Int code_type_xdigit(USES_REGS1) {
  Int ch = get_code(ARG1);
#if HAVE_ISWXDIGIT
  return iswxdigit(ch);
#elif HAVE_ISWHEXNUMBER
  return iswhexnumber(ch);
#else
  return iswdigit(ch) || ((ch >= 'a' && ch <= 'f') && (ch >= 'A' && ch <= 'F'));
#endif
}

static Int code_type_graph(USES_REGS1) {
  Int ch = get_code(ARG1);
  return iswgraph(ch);
}

static Int code_type_lower(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == LC;
}

static Int code_type_upper(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC;
}

static Int code_type_punct(USES_REGS1) {
  int ch = get_char(ARG1);
  if (ch < 256) {
    char_kind_t k = Yap_chtype[ch];
    return k >= QT && k <= BK;
  }
  return false;
}

static Int code_type_space(USES_REGS1) {
  int ch = get_code(ARG1);
  if (ch < 256) {
    char_kind_t k = Yap_chtype[ch];
    return k == BS;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return (ct >= UTF8PROC_CATEGORY_ZS && ct <= UTF8PROC_CATEGORY_PO);
}

static Int code_type_end_of_file(USES_REGS1) {
  Int ch = get_code(ARG1);
  return ch == WEOF || ch == -1;
}

static Int code_type_end_of_line(USES_REGS1) {
  Int ch = get_code(ARG1);
  if (ch < 256) {
    return ch >= 10 && ch <= 13;
  }
  utf8proc_category_t ct = utf8proc_category(ch);
  return (ct >= UTF8PROC_CATEGORY_ZL && ct <= UTF8PROC_CATEGORY_ZP);
}

static Int code_type_newline(USES_REGS1) {
  Int ch = get_code(ARG1);
  if (ch < 256) {
    return ch == 10;
  }
  return false;
}

static Int code_type_period(USES_REGS1) {
  Int ch = get_code(ARG1);
  return ch == '.' || ch == '!' || ch == '?';
}

static Int code_type_quote(USES_REGS1) {
  Int ch = get_code(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PI || ct == UTF8PROC_CATEGORY_PF;
}

static Int code_type_paren(USES_REGS1) {
  Int ch = get_code(ARG1);
  utf8proc_category_t ct = utf8proc_category(ch);
  return ct == UTF8PROC_CATEGORY_PS || ct == UTF8PROC_CATEGORY_PE;
}

static Int code_type_prolog_var_start(USES_REGS1) {
  Int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == LC || ch == '_';
}

static Int code_type_prolog_atom_start(USES_REGS1) {
  Int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == UC;
}

static Int code_type_prolog_identifier_continue(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k >= UC && k <= NU;
}

static Int code_type_prolog_prolog_symbol(USES_REGS1) {
  int ch = get_code(ARG1);
  char_kind_t k = Yap_wide_chtype(ch);
  return k == SL && k == SY;
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

static Int p_disable_char_conversion(USES_REGS1) {
  int i;

  for (i = 0; i < MaxStreams; i++) {
    if (!(GLOBAL_Stream[i].status & Free_Stream_f))
      GLOBAL_Stream[i].stream_wgetc_for_read = GLOBAL_Stream[i].stream_wgetc;
  }
  GLOBAL_CharConversionTable = NULL;
  return (TRUE);
}

static Int char_conversion(USES_REGS1) {
  Term t = Deref(ARG1), t1 = Deref(ARG2);
  unsigned char *s0, *s1;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "char_conversion/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, "char_conversion/2");
    return (FALSE);
  }
  s0 = RepAtom(AtomOfTerm(t))->UStrOfAE;
  if (s0[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, "char_conversion/2");
    return (FALSE);
  }
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "char_conversion/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
    return (FALSE);
  }
  s1 = RepAtom(AtomOfTerm(t1))->UStrOfAE;
  if (s1[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
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
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
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

static Int p_current_char_conversion(USES_REGS1) {
  Term t, t1;
  unsigned char *s0, *s1;

  if (GLOBAL_CharConversionTable == NULL) {
    return (FALSE);
  }
  t = Deref(ARG1);
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "current_char_conversion/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, "current_char_conversion/2");
    return (FALSE);
  }
  s0 = RepAtom(AtomOfTerm(t))->UStrOfAE;
  if (s0[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t, "current_char_conversion/2");
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
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "current_char_conversion/2");
    return (FALSE);
  }
  s1 = RepAtom(AtomOfTerm(t1))->UStrOfAE;
  if (s1[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "current_char_conversion/2");
    return (FALSE);
  } else {
    return (GLOBAL_CharConversionTable[(int)s0[0]] == '\0' &&
            GLOBAL_CharConversionTable[(int)s0[0]] == s1[0]);
  }
}

static Int p_all_char_conversions(USES_REGS1) {
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
  CurrentModule = CHTYPE_MODULE;
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
  CurrentModule = PROLOG_MODULE;
}
