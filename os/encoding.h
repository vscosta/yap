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

#if defined(_PL_STREAM_H)
typedef IOENC encoding_t;
#define ENC_ISO_LATIN1 ENC_ISO_LATIN_1 
#else
typedef enum {
  ENC_OCTET      = 0,  /// binary files
  ENC_ISO_LATIN1 = 1,  /// US+West Europe
  ENC_ISO_ASCII  = 2,  /// US only
  ENC_ISO_ANSI   = 4,  /// Who cares
  ENC_ISO_UTF8   = 8,  /// Most everyone nowadays
  ENC_UTF16_BE = 16, /// People who made a mistake
  ENC_UTF16_LE = 32, /// People who made the same mistake
  ENC_ISO_UTF32_BE = 64,  /// nobody
  ENC_ISO_UTF32_LE = 128, /// yes, nobody
} encoding_t;
/// read the initial encoding from the Operating System's environment;
encoding_t Yap_InitialEncoding( void );
/// read the current environment, as set by the user or as Initial
encoding_t Yap_DefaultEncoding( void );
void Yap_SetDefaultEncoding(encoding_t new_encoding);

static inline const char *enc_name(encoding_t enc)
{
  switch(enc)
    {
    case ENC_OCTET: return "octet";
    case ENC_ISO_LATIN1: return "iso_latin_1";
    case ENC_ISO_ASCII: return "ascii";
    case ENC_ISO_ANSI: return "octet";
    case ENC_ISO_UTF8: return "utf8";
    case ENC_UTF16_BE: return "utf16_be";
    case ENC_UTF16_LE: return "utf16_le";
    case ENC_ISO_UTF32_BE: return "utf32_be";
    case  ENC_ISO_UTF32_LE: return "utf32_le";
    }
}

static inline
encoding_t enc_id(char *s)
{
    {
        if (!strcmp(s,  "octet")) return ENC_OCTET;
        if (!strcmp(s,  "iso_latin_1")) return ENC_ISO_LATIN1;
        if (!strcmp(s,  "iso_ascii")) return ENC_ISO_ASCII;
        if (!strcmp(s,  "iso_ansi")) return ENC_ISO_ANSI;
        if (!strcmp(s,  "iso_utf8")) return ENC_ISO_UTF8;
        if (!strcmp(s,  "utf16_be")) return ENC_UTF16_BE;
        if (!strcmp(s,  "utf16_le")) return ENC_UTF16_LE;
        if (!strcmp(s,  "utf32_be")) return ENC_ISO_UTF32_BE;
        if (!strcmp(s,  "utf32_le")) return ENC_ISO_UTF32_LE;
        if (!strcmp(s,  "default")) return Yap_DefaultEncoding();
        else {
            Yap_Error(DOMAIN_ERROR_OUT_OF_RANGE, 0, "bad encoding %s, should be {octet,iso_latin_1,iso_ascii,iso_ansi,iso_utf8,utf16_be,utf16_le,utf32_be,utf32_le}", s);
            return ENC_OCTET;
        }
    }
}

#endif

#endif

