
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
* File:		charcodes.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Character codes and character conversion		 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a pipe related IO.
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
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
#define S_ISDIR(x) (((x)&_S_IFDIR)==_S_IFDIR)
#endif
#endif
#include "iopreds.h"
#include "eval.h"

static Int p_change_type_of_char( USES_REGS1);
static Int p_type_of_char( USES_REGS1);

Term
  Yap_StringToNumberTerm(char *s, encoding_t enc)
{
  int sno;
  Term t;

  sno = Yap_open_buf_read_stream(s, strlen(s)+1, enc, MEM_BUF_USER);
   if (sno < 0)
    return FALSE;
  GLOBAL_Stream[sno].encoding = enc;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  while(*s && isblank(*s++));
  t = Yap_scan_num(GLOBAL_Stream+sno);
  GLOBAL_Stream[sno].status = Free_Stream_f;
  if (t == TermNil) {
      CACHE_REGS
    int sign = 1;
    if (s[0] == '+') {
      s++;
    }
    if (s[0] == '-') {
      s++;
      sign = -1;
    }
    if(strcmp(s,"inf") == 0) {
      if (sign > 0) {
	return MkFloatTerm(INFINITY);
      } else {
	return MkFloatTerm(-INFINITY);
      }
    }
    if(strcmp(s,"nan") == 0) {
      if (sign > 0) {
	return MkFloatTerm(NAN);
      } else {
	return MkFloatTerm(-NAN);
      }
    }
  }
  return t;
}

char *
  Yap_TermToString(Term t, char *s,  size_t sz, size_t *length, encoding_t encp, int flags)
{
  CACHE_REGS
    int sno = Yap_open_buf_write_stream(&s, &sz);
  int old_output_stream = LOCAL_c_output_stream;

  if (sno < 0)
    return NULL;
  LOCAL_c_output_stream = sno;
  if (encp)
    GLOBAL_Stream[sno].encoding = encp;
  Yap_plwrite (t, GLOBAL_Stream+sno, 0, flags, 1200);
  s[GLOBAL_Stream[sno].u.mem_string.pos] = '\0';
  LOCK(GLOBAL_Stream[sno].streamlock);
  GLOBAL_Stream[sno].status = Free_Stream_f;
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  LOCAL_c_output_stream = old_output_stream;
  if ( EX == 0 ) return s;
  return NULL;
}

static encoding_t
DefaultEncoding(void)
{
  char *s = getenv("LANG");
  size_t sz;

  /* if we don't have a LANG then just use ISO_LATIN1 */
  if (s == NULL)
    s = getenv("LC_CTYPE");
  if (s == NULL)
    return ENC_ISO_LATIN1;
  sz = strlen(s);
  if (sz >= 5) {
    if (s[sz-5] == 'U' &&
	s[sz-4] == 'T' &&
	s[sz-3] == 'F' &&
	s[sz-2] == '-' &&
	s[sz-1] == '8') {
      return ENC_ISO_UTF8;
    }
  }
  return ENC_ISO_ANSI;
}

encoding_t
Yap_DefaultEncoding(void)
{
    CACHE_REGS
  return LOCAL_encoding;
}

void
Yap_SetDefaultEncoding(encoding_t new_encoding)
{
    CACHE_REGS
  LOCAL_encoding = new_encoding;
}

encoding_t
Yap_InitialEncoding(void)
{
  char *s = getenv("LANG");
  size_t sz;

  /* if we don't have a LANG then just use ISO_LATIN1 */
  if (s == NULL)
    s = getenv("LC_CTYPE");
  if (s == NULL)
    return ENC_ISO_LATIN1;
  sz = strlen(s);
  if (sz >= 5) {
    if (s[sz-5] == 'U' &&
	s[sz-4] == 'T' &&
	s[sz-3] == 'F' &&
	s[sz-2] == '-' &&
	s[sz-1] == '8') {
      return ENC_ISO_UTF8;
    }
  }
  return ENC_ISO_ANSI;
}

static Int
get_default_encoding( USES_REGS1 )
{
  Term out = MkIntegerTerm(Yap_DefaultEncoding());
  return Yap_unify(ARG1, out);
}

static Int
p_encoding ( USES_REGS1 )
{				/* '$encoding'(Stream,N)                      */
  int sno = Yap_CheckStream (ARG1, Input_Stream_f|Output_Stream_f, "encoding/2");
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



static Int
  toupper2( USES_REGS1 )
{
  Int out = IntegerOfTerm(Deref(ARG1)), uout;
  if (out < 0) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, ARG1, "toupper");
    return FALSE;
  }
  if (out < 128)
    uout = toupper(out);
  else
    uout = towupper(out);
  return Yap_unify(ARG2, MkIntegerTerm(uout));
}

static Int
tolower2( USES_REGS1 )
{
  Int out = IntegerOfTerm(Deref(ARG1)), uout;
  if (out < 0) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER_CODE, ARG1, "tolower");
    return FALSE;
  }
  if (out < 128)
    uout = tolower(out);
  else
    uout = towlower(out);
  return Yap_unify(ARG2, MkIntegerTerm(uout));
}

static Int
p_change_type_of_char ( USES_REGS1 )
{				/* change_type_of_char(+char,+type)      */
  Term t1 = Deref (ARG1);
  Term t2 = Deref (ARG2);
  if (!IsVarTerm (t1) && !IsIntegerTerm (t1))
    return FALSE;
  if (!IsVarTerm(t2) && !IsIntegerTerm(t2))
    return FALSE;
  Yap_chtype[IntegerOfTerm(t1)] = IntegerOfTerm(t2);
  return TRUE;
}

static Int
p_type_of_char ( USES_REGS1 )
{				/* type_of_char(+char,-type)      */
  Term t;

  Term t1 = Deref (ARG1);
  if (!IsVarTerm (t1) && !IsIntegerTerm (t1))
    return FALSE;
  t = MkIntTerm(Yap_chtype[IntegerOfTerm (t1)]);
  return Yap_unify(t,ARG2);
}

/* I dispise this code!!!!! */
int
ISOWGetc (int sno)
{
  int ch = GLOBAL_Stream[sno].stream_wgetc(sno);
  if (ch != EOF && GLOBAL_CharConversionTable != NULL) {

    if (ch < NUMBER_OF_CHARS) {
      /* only do this in ASCII */
      return GLOBAL_CharConversionTable[ch];
    }
  }
  return ch; 
}



static Int 
p_force_char_conversion( USES_REGS1 )
{
  int i;

  /* don't actually enable it until someone tries to add a conversion */
  if (GLOBAL_CharConversionTable2 == NULL)
    return(TRUE);
  for (i = 0; i < MaxStreams; i++) {
    if (!(GLOBAL_Stream[i].status & Free_Stream_f))
	GLOBAL_Stream[i].stream_wgetc_for_read = ISOWGetc;
  }
  GLOBAL_CharConversionTable = GLOBAL_CharConversionTable2;
  return(TRUE);
}

static Int 
p_disable_char_conversion( USES_REGS1 )
{
  int i;

  for (i = 0; i < MaxStreams; i++) {
    if (!(GLOBAL_Stream[i].status & Free_Stream_f))
      GLOBAL_Stream[i].stream_wgetc_for_read = GLOBAL_Stream[i].stream_wgetc;
  }
  GLOBAL_CharConversionTable = NULL;
  return(TRUE);
}

static Int 
char_conversion( USES_REGS1 )
{
  Term t0 = Deref(ARG1), t1 = Deref(ARG2);
  char *s0, *s1;

  if (IsVarTerm(t0)) {
    Yap_Error(INSTANTIATION_ERROR, t0, "char_conversion/2");
    return (FALSE);    
  }
  if (!IsAtomTerm(t0)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "char_conversion/2");
    return (FALSE);    
  }
  s0 = RepAtom(AtomOfTerm(t0))->StrOfAE;
  if (s0[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "char_conversion/2");
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
  s1 = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (s1[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "char_conversion/2");
    return (FALSE);    
  }
  /* check if we do have a table for converting characters */
  if (GLOBAL_CharConversionTable2 == NULL) {
    int i;

    /* don't create a table if we don't need to */
    if (s0[0] == s1[0])
      return(TRUE);
    GLOBAL_CharConversionTable2 = Yap_AllocCodeSpace(NUMBER_OF_CHARS*sizeof(char));
    while (GLOBAL_CharConversionTable2 == NULL) {
      if (!Yap_growheap(FALSE, NUMBER_OF_CHARS*sizeof(char), NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR, TermNil, LOCAL_ErrorMessage);
	return(FALSE);
      }
    }
    if (trueGlobalPrologFlag(CHAR_CONVERSION_FLAG)) {
        CACHE_REGS
      if (p_force_char_conversion(PASS_REGS1) == FALSE)
	return(FALSE);
    }
    for (i = 0; i < NUMBER_OF_CHARS; i++) 
      GLOBAL_CharConversionTable2[i] = i;
  }
  /* just add the new entry */
  GLOBAL_CharConversionTable2[(int)s0[0]] = s1[0];
  /* done */
  return(TRUE);
}

static Int 
p_current_char_conversion( USES_REGS1 )
{
  Term t0, t1;
  char *s0, *s1;

  if (GLOBAL_CharConversionTable == NULL) {
    return(FALSE);
  }
  t0 = Deref(ARG1);
  if (IsVarTerm(t0)) {
    Yap_Error(INSTANTIATION_ERROR, t0, "current_char_conversion/2");
    return (FALSE);    
  }
  if (!IsAtomTerm(t0)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "current_char_conversion/2");
    return (FALSE);    
  }
  s0 = RepAtom(AtomOfTerm(t0))->StrOfAE;
  if (s0[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t0, "current_char_conversion/2");
    return (FALSE);    
  }
  t1 = Deref(ARG2);
  if (IsVarTerm(t1)) {
    char out[2];
    if (GLOBAL_CharConversionTable[(int)s0[0]] == '\0') return(FALSE);
    out[0] = GLOBAL_CharConversionTable[(int)s0[0]];
    out[1] = '\0';
    return(Yap_unify(ARG2,MkAtomTerm(Yap_LookupAtom(out))));
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "current_char_conversion/2");
    return (FALSE);    
  }
  s1 = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (s1[1] != '\0') {
    Yap_Error(REPRESENTATION_ERROR_CHARACTER, t1, "current_char_conversion/2");
    return (FALSE);    
  } else {
    return (GLOBAL_CharConversionTable[(int)s0[0]] == '\0' &&
	    GLOBAL_CharConversionTable[(int)s0[0]] == s1[0] );
  }
}

static Int 
p_all_char_conversions( USES_REGS1 )
{
  Term out = TermNil;
  int i;

  if (GLOBAL_CharConversionTable == NULL) {
    return(FALSE);
  }
  for (i = NUMBER_OF_CHARS; i > 0; ) {
    i--;
    if (GLOBAL_CharConversionTable[i] != '\0') {
      Term t1, t2;
      char s[2];
      s[1] = '\0';
      s[0] = GLOBAL_CharConversionTable[i];
      t1 = MkAtomTerm(Yap_LookupAtom(s));
      out = MkPairTerm(t1,out);
      s[0] = i;
      t2 = MkAtomTerm(Yap_LookupAtom(s));
      out = MkPairTerm(t2,out);
    }
  }
  return(Yap_unify(ARG1,out));
}


void
Yap_InitChtypes(void)
{
  DefaultEncoding();
  Yap_InitCPred ("$change_type_of_char", 2, p_change_type_of_char, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$type_of_char", 2, p_type_of_char, SafePredFlag|SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("toupper", 2, toupper2, SafePredFlag);
  Yap_InitCPred ("tolower", 2, tolower2, SafePredFlag);
  Yap_InitCPred ("char_conversion", 2, char_conversion, SyncPredFlag);

  Yap_InitCPred ("$get_default_encoding", 1, get_default_encoding, SafePredFlag|HiddenPredFlag);

  Yap_InitCPred ("$encoding", 2, p_encoding, SafePredFlag|SyncPredFlag),

    Yap_InitCPred ("$current_char_conversion", 2, p_current_char_conversion, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$all_char_conversions", 1, p_all_char_conversions, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$force_char_conversion", 0, p_force_char_conversion, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred ("$disable_char_conversion", 0, p_disable_char_conversion, SyncPredFlag|HiddenPredFlag);

 }
