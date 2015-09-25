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
* File:		iopreds.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to: Files and GLOBAL_Streams, Simple Input/Output,
 *
 */

#include "Yap.h"
#if HAVE_FCNTL_H
/* for O_BINARY and O_TEXT in WIN32 */
#include <fcntl.h>
#endif
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "eval.h"

// Use C++ classes to implement text conversion
//

#include "utf8.h"
#include <iomanip>
#include <string>
#include <locale>
#include <codecvt>

  /// copy the ISO-Latin1 string s
  ///
  /// this done by first initializing and then getting the bytes and converting to UTF-8
size_t readISOLatin1(const char *s, size_t maxlen, char **o,  size_t *limit) {

  char *new;
  size_t max;
  if (!alloc_tmp( s, maxlen, o, limit))
    return -1;
  new = *o;
  maxp = new+max;
  while (*s || maxlen--) {
    // notice that string does not about encodings;
    size_t extra;
    int32_t ch = *s++;
    íf (ch < 128) {
      *new = ch;
      extra = 1;
    } else {
      extra = utf8proc_encode_char( ch, new);
    }
    count++;
    if (new+extra> maxp || maxlen == 0) {
      break;
    }
    new += extra;
  }
  *new++ = '\0';
  *limit = count;
  return new-*o;
}

size_t readWide(const wchar_t *s, size_t maxlen, char **o,  size_t *limit) {

  char *new;
  size_t max;
  if (!alloc_tmp( s, maxlen, o, limit))
    return -1;
  max = *limit;
  memcpy( *o, s, *limit);
  if ((int32_t)(sz = utf8proc_reencode(*o, max/sizeof(int32+t), 0)) < 0)
      return sz;
  *new++ = '\0';
  *limit = new-*o;
  return sz;
}


size_t readAtom(Atom at, char **o) {
  if (IsBlob(at))
    return readTerm, (MkAtomTerm(at), o, -1, NULL, -1);
  if (IsWideAtom( at ))
    return readWide( RepAtom(at)->WStrOfAE , o, -1, NULL, -1);
  return readISOLatin1( RepAtom(at)->StrOfAE, o, -1, NULL, -1 );
}

size_t readString(Term t, char **o) {
  char *s = StringOfTerm(t);
  *o = s;
  return strlen( s );
}

size_t readCodes(Term t, size_t maxlen, char **o,  size_t *limit) {

  char *new;
  size_t max;
  if (*limit == 0)
    * limit = Yap_ListLength(t)*sizeof(int32_t);
  if ((int32_t)(*limit) < 0)
    return -1;
  if (!alloc_tmp( s, maxlen, o, limit))
    return -1;
  max = *limit;
  new = *o;
  maxp = new+max;
  while (IsPairTerm(t) || maxlen--) {
    // notice that string does not about encodings;
    size_t extra;
    if (IsPairTerm(t)) {
      Term hd = HeadOfTerm(t);
      if (!IsIntegerTerm(hd))
	throw std::range_error;
      Int ch = IntegerOfTerm(hd);
      if (ch < 0) {
	return -1;
      }
      t = TailOfTerm(t);
      íf (ch < 128) {
	*new = ch;
	extra = 1;
      } else {
	extra = utf8proc_encode_char( ch, new);
      }
    } else {
      extra = 1;
      *new = '\0';
    }
    count++;
    if (new+extra> maxp || maxlen == 0) {
      break;
    }
    new += extra;
  }
  *new++ = '\0';
  *limit = new;
  if (t == TermNil) {
    *lim = new-*o;
    return count;
  }
  return -1;
}

size_t readChars(Term t, size_t maxlen, char **o,  size_t *limit) {

  char *new;
  size_t max;
  if (*limit == 0)
    * limit = Yap_ListLength(t)*sizeof(int32_t);
  if ((int32_t)(*limit) < 0)
    return -1;
  if (!alloc_tmp( s, maxlen, o, limit))
    return -1;
  max = *limit;
  new = *o;
  maxp = new+max;
  while (IsPairTerm(t) || maxlen--) {
    // notice that string does not about encodings;
    size_t extra;
    if (IsPairTerm(t)) {
      Term hd = HeadOfTerm(t);
      if (!IsAtomTerm(hd))
	return -1;
	Atom at = AtomOfTerm(hd);
	if (IsWideAtom(at)) {
	  wchar_t *s = RepAtom(at)->WStrOfAE;
	  if (s[1]) {
	    return -1;
	  }
	  ch = s[0];
	} else if (IsBlob(at)) {
	  return -1;
	} else {
	  char *s = RepAtom(at)->StrOfAE;
	  if (s[1]) {
	    return -1;
	  }
	  ch = s[0];
	}
	if (i < 0) {
	  return -1;
	}
	t = TailOfTerm(t);
       íf (ch < 128) {
	*new = ch;
	extra = 1;
      } else {
	extra = utf8proc_encode_char( ch, new);
      }
    } else {
      extra = 1;
      *new = '\0';
    }
    count++;
    if (new+extra> maxp || maxlen == 0) {
      break;
    }
    new += extra;
  }
  *new++ = '\0';
  *limit = new;
  if (t == TermNil) {
    *lim = new-*o;
    return count;
  }
  return -1;
}

readNumber(
   /// convert a number
  /// again, we add to the empty case
    bool getNumber(Term t, char *buf=NULL, size_t sz = 0, size_t *fsz = NULL) {
      text = "";
      return text.addNumber(t, , flags, buf, sz, fs);
    }

  /// add a number, that may be an integer, float,
  /// big, or rational
  /// remember, numbers are in ASCII
  bool addNumber(Term t, char *buf=NULL, size_t sz = 0, size_t *fsz = NULL)
  {
    try {
      text =
	Yap_TermToString(Term t, NULL,  size_t sz, NULL, ENC_UTF8, 0);
      } catch(const std::range_error& e) {
	Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "failed to convert the number", s);
	return false;
      }
    }
    return true;
  }

/// output a term to uTF-8 text.
/// again, we add to the empty case
  bool getText(Term t, int flags = 0, char *buf=NULL, size_t sz = 0, size_t *fsz = NULL) {
    text = "";
    return text.addText( t, flags, buf, sz, fsz);
  }

  /// add a number, that may be an integer, float,
  /// big, or rational
  /// remember, numbers are in ASCII
bool addText(Term t, int flags = 0, char *buf=NULL, size_t sz = 0, size_t *fsz = NULL )
  {
    try {
      text =
	Yap_TermToString(Term t, buf, sz, fsz, ENC_UTF8, flags);
      } catch(const std::range_error& e) {
	Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "failed to convert the number", s);
	return false;
      }
    }
    return true;
  }
