/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2003	 *
*									 *
**************************************************************************
*									 *
* File:		yapio.h							 *
* Last rev:	22/1/03							 *
* mods:									 *
* comments:	UNICODE encoding support (based on SWI-Prolog)		 *
*									 *
*************************************************************************/

#ifndef ENCODING_H

#define ENCODING_H 1

#include "YapError.h"
#if HAVE_STRING_H

#include <string.h>

#endif

typedef enum YAP_encoding {
  ENC_OCTET = 0,          /// binary files
  ENC_ISO_LATIN1 = 1,     /// US+West Europe
  ENC_ISO_ASCII = 2,      /// US only
  ENC_ISO_ANSI = 4,       /// Who cares
  ENC_ISO_UTF8 = 8,       /// Most everyone nowadays
  ENC_UTF16_BE = 16,      /// People who made a mistake
  ENC_UTF16_LE = 32,      /// People who made the same mistake
  ENC_ISO_UTF32_BE = 64,  /// nobody
  ENC_ISO_UTF32_LE = 128, /// yes, nobody
  ENC_UCS2_BE = 256,      /// nobody
  ENC_UCS2_LE = 512,      /// yes, nobody
} encoding_t;

#if WORDS_BIGENDIAN
#define ENC_WCHAR ENC_ISO_UTF32_BE
#else
#define ENC_WCHAR ENC_ISO_UTF32_LE
#endif

#ifdef YAP_H

/// read the current environment, as set by the user or as Initial
encoding_t Yap_DefaultEncoding(void);
encoding_t Yap_SystemEncoding(void);
void Yap_SetDefaultEncoding(encoding_t new_encoding);

#if HAVE_XLOCALE_H
typedef enum {
  SEQ_ENC_OCTET,        /// binary files
  SEQ_ENC_ISO_LATIN1,   /// US+West Europe
  SEQ_ENC_ISO_ASCII,    /// US only
  SEQ_ENC_ISO_ANSI,     /// Who cares
  SEQ_ENC_ISO_UTF8,     /// Most everyone nowadays
  SEQ_ENC_UTF16_BE,     /// People who made a mistake
  SEQ_ENC_UTF16_LE,     /// People who made the same mistake
  SEQ_ENC_ISO_UTF32_BE, /// nobody
  SEQ_ENC_ISO_UTF32_LE  /// yes, nobody
} seq_encoding_t;

/// convert from unary to binary representation.
static inline seq_encoding_t seq_encoding(encoding_t inp) {
#if HAVE__BUILTIN_FFSLL
  return __builtin_ffsll(inp);
#elif HAVE_FFSLL
  return ffsll(inp);
#else
  unsigned int out;
  // supports max 16 different encodings.
  if (inp == 0)
    return 0L;
  // if (inp &     ((CELL)0xffffL << 16)) {inp >>= 16; out += 16;}
  if (inp & ((CELL)0xffL << 8)) {
    inp >>= 8;
    out += 8;
  }
  if (inp & ((CELL)0xfL << 4)) {
    inp >>= 4;
    out += 4;
  }
  if (inp & ((CELL)0x3L << 2)) {
    inp >>= 2;
    out += 2;
  }
  if (inp & ((CELL)0x1 << 1))
    out++;
#endif
  return out;
}

extern xlocale enc_locales[SEQ_ENC_ISO_UTF32_LE + 1];
#endif

static inline const char *enc_name(encoding_t enc) {
  switch (enc) {
  case ENC_OCTET:
    return "octet";
  case ENC_ISO_LATIN1:
    return "iso_latin_1";
  case ENC_ISO_ASCII:
    return "ascii";
  case ENC_ISO_ANSI:
    return "octet";
  case ENC_ISO_UTF8:
    return "utf8";
  case ENC_UTF16_BE:
    return "utf16_be";
  case ENC_UTF16_LE:
    return "utf16_le";
  case ENC_UCS2_BE:
    return "ucs2_be";
  case ENC_UCS2_LE:
    return "ucs2_le";
  case ENC_ISO_UTF32_BE:
    return "utf32_be";
  case ENC_ISO_UTF32_LE:
    return "utf32_le";
  default:
    return "thanks for watching!!";
  }
}

static inline encoding_t enc_id(const char *s, encoding_t enc_bom) {
  {
    if (!strcmp(s, "iso_utf8"))
      return ENC_ISO_UTF8;
    if (!strcmp(s, "utf8"))
      return ENC_ISO_UTF8;
    if (!strcmp(s, "UTF-8"))
      return ENC_ISO_UTF8;
    if (!strcmp(s, "utf16_le"))
      return ENC_UTF16_LE;
    if (!strcmp(s, "utf16_be"))
      return ENC_UTF16_BE;
    if (!strcmp(s, "UTF-16")) {
      if (enc_bom == ENC_UTF16_LE)
        return ENC_UTF16_LE;
      return ENC_UTF16_BE;
    }
    if (!strcmp(s, "UTF-16LE"))
      return ENC_UTF16_LE;
    if (!strcmp(s, "UTF-16BE"))
      return ENC_UTF16_BE;
    if (!strcmp(s, "octet"))
      return ENC_OCTET;
    if (!strcmp(s, "iso_latin_1"))
      return ENC_ISO_LATIN1;
    if (!strcmp(s, "iso_ascii"))
      return ENC_ISO_ASCII;
    if (!strcmp(s, "iso_ansi"))
      return ENC_ISO_ANSI;
    if (!strcmp(s, "utf32_be"))
      return ENC_ISO_UTF32_BE;
    if (!strcmp(s, "utf32_le"))
      return ENC_ISO_UTF32_LE;
    if (!strcmp(s, "UTF-32")) {
      if (enc_bom == ENC_ISO_UTF32_LE)
        return ENC_ISO_UTF32_LE;
      return ENC_ISO_UTF32_BE;
    }
    if (!strcmp(s, "UTF-32BE"))
      return ENC_ISO_UTF32_BE;
    if (!strcmp(s, "UTF-32LE"))
      return ENC_ISO_UTF32_LE;
    if (!strcmp(s, "ISO-8859-1"))
      return ENC_ISO_LATIN1;
    if (!strcmp(s, "US_ASCII"))
      return ENC_ISO_ASCII;
    // just for SWI compat, this actually refers to
    // UCS-2
    if (!strcmp(s, "unicode_be"))
      return ENC_UCS2_BE;
    if (!strcmp(s, "unicode_le"))
      return ENC_UCS2_LE;
    if (!strcmp(s, "UCS-2")) {
      if (enc_bom == ENC_UTF16_LE)
        return ENC_UCS2_LE;
      return ENC_UCS2_BE;
    }
    if (!strcmp(s, "UCS-2LE"))
      return ENC_UCS2_LE;
    if (!strcmp(s, "UCS-2BE"))
      return ENC_UCS2_BE;
    if (!strcmp(s, "default")) {
      if (enc_bom != ENC_OCTET)
        return enc_bom;
      return Yap_DefaultEncoding();
    } else {
      return Yap_DefaultEncoding();
    }
  }
}

#endif

#endif
