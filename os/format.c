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

#include "Yap.h"

#include "YapError.h"
#include "YapInterface.h"

#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file format.c
 *
 */

/**
 * @defgroup FormattedIO Formatted Output
 * @ingroup InputOutput
 *
 * @{
 * @brief This file includes the definition of the formatted output predicates.
 *
 */


/**
 *
 * @pred  format(+ _T_, :_L_)


 Print formatted output to the current output stream. The arguments in
 list _L_ are output according to the string, list of codes or
 characters, or by the atom _T_.

 A control sequence is introduced by a `~`. The following control
 sequences are available in YAP:

 + `~~`
 Print a single tilde.

 + `~a`
 The next argument must be an atom, that will be printed as if by `write`.

 + `~Nc`
 The next argument must be an integer, that will be printed as a
 character code. The number  _N_ is the number of times to print the
 character (default 1).

 + `~Ne`
 + `~NE`
 + `~Nf`
 + `~Ng`
 + `~NG`
 The next argument must be a floating point number. The float  _F_, the number
 _N_ and the control code `c` will be passed to `printf` as:

 ```
 printf("%s.Nc", F)
 ```

 As an example:

 ```
 ?- format("~8e, ~8E, ~8f, ~8g, ~8G~w",
 [3.14,3.14,3.14,3.14,3.14,3.14]).
 3.140000e+00, 3.140000E+00, 3.140000, 3.14, 3.143.14
 ```

 + `~Nd`
 The next argument must be an integer, and  _N_ is the number of digits
 after the decimal point. If  _N_ is `0` no decimal points will be
 printed. The default is  _N = 0_.

 ```
 ?- format("~2d, ~d",[15000, 15000]).
 150.00, 15000
 ```

 + `~ND`
 Identical to `~Nd`, except that commas are used to separate groups
 of three digits.

 ```
 ?- format("~2D, ~D",[150000, 150000]).
 1,500.00, 150,000
 ```

 + `~i`
 Ignore the next argument in the list of arguments:

 ```
 ?- format('The ~i met the boregrove',[mimsy]).
 The  met the boregrove
 ```

 + `~k`
 Print the next argument with `write_canonical`:

 ```
 ?- format("Good night ~k",a+[1,2]).
 Good night +(a,[1,2])
 ```

 + `~Nn`
 Print  _N_ newlines (where  _N_ defaults to 1).

 + `~NN`
 Print  _N_ newlines if at the beginning of the line (where  _N_
 defaults to 1).

 + `~Nr`
 The next argument must be an integer, and  _N_ is interpreted as a
 radix, such that `2 <= N <= 36` (the default is 8).

 ```
 ?- format("~2r, 0x~16r, ~r",
 [150000, 150000, 150000]).
 100100100111110000, 0x249f0, 444760
 ```

 Note that the letters `a-z` denote digits larger than 9.

 + `~NR`
 Similar to `~NR`. The next argument must be an integer, and  _N_ is
 interpreted as a radix, such that `2 <= N <= 36` (the default is 8).

 ```
 ?- format("~2r, 0x~16r, ~r",
 [150000, 150000, 150000]).
 100100100111110000, 0x249F0, 444760
 ```

 The only difference is that letters `A-Z` denote digits larger than 9.

 + `~p`
 Print the next argument with print/1:

 ```
 ?- format("Good night ~p",a+[1,2]).
 Good night a+[1,2]
 ```

 + `~q`
 Print the next argument with writeq/1:

 ```
 ?- format("Good night ~q",'Hello'+[1,2]).
 Good night 'Hello'+[1,2]
 ```

 + `~Ns`
 The next argument must be a list of character codes.The system then
 outputs their representation as a string, where  _N_ is the maximum
 number of characters for the string ( _N_ defaults to the length of the
 string).

 ```
 ?- format("The ~s are ~4s",["woods","lovely"]).
 The woods are love
 ```

 + `~w`
 Print the next argument with write/1:

 ```
 ?- format("Good night ~w",'Hello'+[1,2]).
 Good night Hello+[1,2]
 ```

 + `~W`
 Give the next two arguments to write_term/2. The first is the term to print, and
 the second is a list of write_term/2 options. For example:

 ```
 format(string(S), '~W', [Term, [singletons(true)]]).
 ```

 This option is SWI-Prolog specific.

 The number of arguments, `N`, may be given as an integer, or it
 may be given as an extra argument. The next example shows a small
 procedure to write a variable number of `a` characters:

 ```
 write_many_as(N) :-
 format("~*c",[N,0'a]).
 ```

 The format/2 built-in also allows for formatted output.  One can
 specify column boundaries and fill the intermediate space by a padding
 character:

 + `~N|`
 Set a column boundary at position  _N_, where  _N_ defaults to the
 current position.

 + `~N+`
 Set a column boundary at  _N_ characters past the current position, where
 _N_ defaults to `8`.

 + `~Nt`
 Set padding for a column, where  _N_ is the fill code (default is
 `SPC`).



 The next example shows how to align columns and padding. We first show
 left-alignment:

 ```
 ?- format("~n*Hello~16+*~n",[]).
 *Hello          *
 ```


 Note that we reserve 16 characters for the column.

 The following example shows how to do right-alignment:

 ```
 ?- format("*~tHello~16+*~n",[]).
 *          Hello*

 ```

 The `~t` escape sequence forces filling before `Hello`.

 We next show how to do centering:

 ```
 ?- format("*~tHello~t~16+*~n",[]).
 *     Hello     *
 ```

 The two `~t` escape sequence force filling both before and after
 `Hello`. Space is then evenly divided between the right and the
 left sides.

 + `~@`
 Evaluate the next argument as a goal whose standard
 output is directed to the stream used by format/2.

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
#if HAVE_CTYPE_H

#include <ctype.h>

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
#define S_ISDIR(x) (((x)&_S_IFDIR) == _S_IFDIR)
#endif
#endif

#include "YapEval.h"
#include "iopreds.h"
#include "format.h"


static void
format_clean_up(int sno, int sno0, format_info *finfo) {
  if (sno >= 0 && sno != sno0) {
    sno = format_synch(sno, sno0, finfo);
  }

}

static int format_print_str(int sno, Int size, bool has_size, Term args,
                            int (*f_putc)(int, wchar_t), format_info *finfo) {
  CACHE_REGS
    Term arghd;
  if (IsStringTerm(args)) {
    const unsigned char *pt = UStringOfTerm(args);
    while ((!has_size || size > 0) && pt && *pt ) {
      utf8proc_int32_t ch;

      if ((pt += get_utf8(pt, -1, &ch)) > 0) {
	f_putc(sno, ch);
      }	    
    }
    if (has_size) {
      while (size>0) {
	size--;
	f_putc(sno, ' ');
      }
    }
  } else if (IsAtomTerm(args)) {
    if (args == TermNil) {
      return true;
    }
    int off;
    const unsigned char *pt = RepAtom(AtomOfTerm(args))->UStrOfAE;
    utf8proc_int32_t ch;
    while ( (!has_size || size > 0) &&
	   pt && *pt &&
	    (off=get_utf8(pt, -1, &ch)>0 && ch != '\0') )
      {
      if (off > 0) {
	f_putc(sno, ch);
	pt += off;
	size--;
      } else if (off < 0) {
	Yap_ThrowError(SYNTAX_ERROR_ENCODING,args,NULL);
      }
    }
    while (size>0) {
      size--;
      f_putc(sno, ' ');
    }
  } else {
    bool maybe_chars = true, maybe_codes = true;
    while (!has_size || size > 0) {
      if (IsVarTerm(args)) {
	format_clean_up(sno, finfo->sno0, finfo);
	Yap_ThrowError(INSTANTIATION_ERROR, args, "~s expects a bound argument");
      } else if (args==TermNil) {
	return true;
      } else if (!IsPairTerm(args)) {
	format_clean_up(sno, finfo->sno0, finfo);
	Yap_ThrowError(TYPE_ERROR_TEXT, args, "format expects an atom, string, or list of codes or chars ");
	return FALSE;
      }
      arghd = HeadOfTerm(args);
      args = TailOfTerm(args);
      if (IsVarTerm(arghd)) {
	format_clean_up(sno, finfo->sno0, finfo);
	Yap_ThrowError(INSTANTIATION_ERROR, arghd, "~s expects a bound argument");
	return FALSE;
      } else if (maybe_codes && IsIntTerm(arghd)) {
	f_putc(sno, (int) IntOfTerm(arghd));
	size--;
	maybe_chars = false;
      } else if (maybe_chars && IsAtomTerm(arghd)) {
	unsigned char *fptr = RepAtom(AtomOfTerm(arghd))->UStrOfAE;
	int ch;
	int off = get_utf8(fptr, -1, &ch);
	if (off <0) {
	  format_clean_up(sno, finfo->sno0, finfo);
	  Yap_ThrowError(TYPE_ERROR_TEXT, arghd, "~s expects a list of chars ");
	  return false;
	}
	f_putc(sno, ch);
	size--;
	maybe_codes = false;
      } else {
	format_clean_up(sno, finfo->sno0, finfo);
	Yap_ThrowError(TYPE_ERROR_TEXT, arghd, "~s expects an atom, string, or list of codes or chars ");
	return FALSE;
      }
    }
    if (has_size) {
      while (size>0) {
	f_putc(sno, ' ');
	size--;
      }
    }

  }
  return TRUE;
}

static Int format_copy_args(Term args, Term *targs, Int tsz, int sno, format_info *finfo) {
  Int n = 0;
  while (args != TermNil) {
    if (IsVarTerm(args)) {
      format_clean_up(sno, finfo->sno0, finfo);
      Yap_ThrowError(INSTANTIATION_ERROR, args, "format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (!IsPairTerm(args)) {
      format_clean_up(sno, finfo->sno0, finfo);
      Yap_ThrowError(TYPE_ERROR_LIST, args, "format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (n == tsz) {
      return FORMAT_COPY_ARGS_OVERFLOW;
    }
    targs[n] = HeadOfTerm(args);
    args = TailOfTerm(args);
    n++;
  }
  return n;
}

static Int fetch_index_from_args(Term t) {
  Int i;

  if (IsVarTerm(t))
    return -1;
  if (IsAtomTerm(t)) {
    unsigned char *fptr = RepAtom(AtomOfTerm(t))->UStrOfAE;
    int ch;
    CACHE_REGS
      fptr += get_utf8(fptr, -1, &ch);
    if (fptr[0] == '\0') {
      return ch;
    }
  }
  if (!IsIntegerTerm(t))
    return -1;
  i = IntegerOfTerm(t);
  if (i < 0)
    return -1;
  return i;
}

static wchar_t base_dig(Int dig, Int ch) {
  if (dig < 10)
    return dig + '0';
  else if (ch == 'r')
    return (dig - 10) + 'a';
  else /* ch == 'R' */
    return (dig - 10) + 'A';
}

#define TMP_STRING_SIZE 1024

#if 0
static bool tabulated(const unsigned char *fptr)
{
  const unsigned char *pt =  fptr;
  int  ch, off;
  while ((off = get_utf8(pt, -1, &ch))>0 &&
	 (ch = *(pt + off)) >0) {
    pt += off;
    if (ch == '~') {
      while ((off = get_utf8(pt, -1, &ch))>0 &&
	     (isdigit((ch=pt[off]))|| ch != '*'))
	{


	  pt += off;
	  || ch == 't') {
	xoreturn true;
      }
    }
  }
  return false;
       
}

#endif

#define TOO_FEW_ARGUMENTS(Needs, Has_Repeats)				\
  if (!targs||targ+Needs > tnum || Has_Repeats) {			\
    Yap_CloseStream(sno);						\
    Yap_ThrowError(DOMAIN_ERROR_FORMAT_CONTROL_SEQUENCE, MkIntTerm(fptr-fstr), "command %c in format string %s has no arguments %s", ch, \
		   fstr, fptr);						\
    return false;							\
  }

static Int doformat(volatile Term otail, volatile Term oargs,
                    int l, int sno0 USES_REGS) {
  char *tmp1, *tmpbase;
  int ch;
  Term *targs;
  Int tnum, targ;
  const unsigned char *fstr, *fptr;
  Term args;
  Term tail;
  int (*f_putc)(int, wchar_t);
  int sno = sno0;
  Term fmod = CurrentModule;
  bool alloc_fstr = false;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  tmp1 = Malloc(TMP_STRING_SIZE + 1);
  format_info *finfo = Malloc(sizeof(format_info));
  // it starts here
  finfo->sno0 = sno0;
  finfo->phys_start = 0;
  finfo->lstart = 0;
  finfo->lvl = l;
  finfo->gapi = 0;
  finfo->old_handler = GLOBAL_Stream[sno].u.mem_string.error_handler;
  finfo->old_pos = 0;
  /* set up an error handler */
  args = oargs;
  tail = otail;
  targ = 0;
  if (IsVarTerm(tail)) {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(INSTANTIATION_ERROR, tail, "format/2");
    return (FALSE);
  } else if ((fstr = Yap_TextToUTF8Buffer(tail PASS_REGS))) {
    fptr = fstr;
    alloc_fstr = true;
  } else {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(TYPE_ERROR_TEXT, tail, "format/2");
    return false;
  }
  if (IsVarTerm(args)) {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(INSTANTIATION_ERROR, args, "format/2");
    return FALSE;
  }
  args = Yap_YapStripModule(args, &fmod);

  if (IsVarTerm(fmod)) {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(INSTANTIATION_ERROR, fmod, "format/2");
    return false;
  }
  if (!IsAtomTerm(fmod)) {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(TYPE_ERROR_ATOM, fmod, "format/2");
    return false;
  }
  if (IsVarTerm(args)) {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(INSTANTIATION_ERROR, args, "format/2");
    return FALSE;
  }
  if (IsPairTerm(args)) {
    Int tsz = 64;

    targs = Malloc(tsz * sizeof(Term));
    do {
      tnum = format_copy_args(args, targs, tsz, sno, finfo);
      if (tnum == FORMAT_COPY_ARGS_ERROR ||
	  tnum == FORMAT_COPY_ARGS_OVERFLOW) {
	format_clean_up(sno0, sno, finfo);
	return false;
      } else if (tnum == tsz) {
	tsz += 128;
	targs = Realloc(targs, tsz * sizeof(Term));
      }
      break;
    } while (true);
  } else if (args != TermNil) {
    tnum = 1;
    targs = Malloc(sizeof(Term));
    targs[0] = args;
  } else {
    tnum = 0;
    targs = NULL;
  }
  sno =  Yap_open_buf_write_stream(-1, LOCAL_encoding);
  if (sno < 0) {
    if (!alloc_fstr)
      fstr = NULL;
    format_clean_up(sno, sno0, finfo);
    return false;
  }
  f_putc = GLOBAL_Stream[sno].stream_wputc;
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  while ((fptr += get_utf8(fptr, -1, &ch)) && ch) {
    Term t = TermNil;
    int has_repeats = false;
    int repeats = 0;

    if (ch == '~') {
      /* start command */
      fptr += get_utf8(fptr, -1, &ch);
      if (ch == '*') {
	if (!targs)
	  return false;
	fptr += get_utf8(fptr, -1, &ch);
	has_repeats = TRUE;
	repeats = fetch_index_from_args(targs[targ++]);
	if (repeats == -1) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(DOMAIN_ERROR_FORMAT_CONTROL_SEQUENCE, targs[targ-1], "command %c in format %s", ch,
			 fstr);
	}
      } else if (ch == '`') {
	/* next character is kept as code */
	has_repeats = TRUE;
	fptr += get_utf8(fptr, -1, &repeats);
	fptr += get_utf8(fptr, -1, &ch);
      } else if (ch >= '0' && ch <= '9') {
	has_repeats = TRUE;
	repeats = 0;
	while (ch >= '0' && ch <= '9') {
	  repeats = repeats * 10 + (ch - '0');
	  fptr += get_utf8(fptr, -1, &ch);
	}
      }
      switch (ch) {
      case 'a': {
	/* print an atom */
	TOO_FEW_ARGUMENTS(1, has_repeats);
	t = targs[targ++];
	if (IsVarTerm(t)) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(INSTANTIATION_ERROR, t, "command %c in format %s", ch, fstr);
	}
	if (!IsAtomTerm(t)) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(TYPE_ERROR_ATOM, t, "command %c in format %s", ch, fstr);
	}
	// stream is already locked.
	Yap_plwrite(t, GLOBAL_Stream + sno,  HR,0, 0, NULL);
      }
	break;
      case 'c': {
	Int nch, i;
	TOO_FEW_ARGUMENTS(1,false);
	t = targs[targ++];
	if (IsVarTerm(t)) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(INSTANTIATION_ERROR, t, "command %c in format %s", ch, fstr);
	}
	if (!IsIntegerTerm(t)) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(TYPE_ERROR_INTEGER, t, "command %c in format %s", ch, fstr);
	}
	nch = IntegerOfTerm(t);
	if (nch < 0) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t, "command %c in format %s", ch,
			 fstr);
	}
	if (!has_repeats)
	  repeats = 1;
	for (i = 0; i < repeats; i++)
	  f_putc(sno, nch);
	break;
      }
      case 'e':
      case 'E':
      case 'f':
      case 'g':
      case 'G': {
	Float fl;
	char *ptr;
	char fmt[32];
	if (targ+1 > tnum) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(DOMAIN_ERROR_FORMAT_CONTROL_SEQUENCE, targs[targ-1], "command ~c in format %s",
			 ch, fstr);
	}
	t = targs[targ++];
	if (IsVarTerm(t)) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(INSTANTIATION_ERROR, t, "command %c in format %s", ch, fstr);
	}
	if (!IsNumTerm(t)) {
	  format_clean_up(sno, sno0, finfo);
	  Yap_ThrowError(TYPE_ERROR_NUMBER, t, "command %c in format %s", ch, fstr);
	}
	if (IsIntegerTerm(t)) {
	  fl = (Float) IntegerOfTerm(t);
#ifdef HAVE_GMP
	} else if (IsBigIntTerm(t)) {
	  fl = Yap_gmp_to_float(t);
#endif
	} else {
	  fl = FloatOfTerm(t);
	}
	if (!has_repeats)
	  repeats = 6;
	fmt[0] = '%';
	fmt[1] = '.';
	ptr = fmt + 2;
#if HAVE_SNPRINTF
	snprintf(ptr, 31 - 5, "%d", repeats);
#else
	sprintf(ptr, "%d", repeats);
#endif
	while (*ptr)
	  ptr++;
	ptr[0] = ch;
	ptr[1] = '\0';
	{
	  unsigned char *uptr = (unsigned char *) tmp1;
#if HAVE_SNPRINTF
	  snprintf(tmp1, repeats + 10, fmt, fl);
#else
	  sprintf(tmp1, fmt, fl);
#endif
	  while ((uptr += get_utf8(uptr, -1, &ch)) && ch != 0)
	    f_putc(sno, ch);
	}
	break;
	case 'd':
	  case 'D': {
	    /* print a decimal, using weird . stuff */
	    TOO_FEW_ARGUMENTS(1,false);
	    t = targs[targ++];
	    if (IsVarTerm(t)) {
	      format_clean_up(sno, sno0, finfo);
	      Yap_ThrowError(INSTANTIATION_ERROR, t, "command %c in format %s", ch, fstr);
	    }
	    if (!IsIntegerTerm(t)
#ifdef HAVE_GMP
		&& !IsBigIntTerm(t)
#endif

		) {
	      format_clean_up(sno, sno0, finfo);
	      Yap_ThrowError(TYPE_ERROR_INTEGER, t, "command %c in format %s", ch, fstr);
	    }
	    {
	      Int siz = 0;
	      char *ptr = tmp1;
	      tmpbase = tmp1;

	      if (IsIntegerTerm(t)) {
		Int il = IntegerOfTerm(t);
#if HAVE_SNPRINTF
		snprintf(tmp1, 256, "%ld", (long int) il);
#else
		sprintf(tmp1, "%ld", (long int)il);
#endif
		siz = strlen(tmp1);
		if (il < 0)
		  siz--;
#ifdef HAVE_GMP
	      } else if (IsBigIntTerm(t) && RepAppl(t)[1] == BIG_INT) {
		char *res;

		tmpbase = tmp1;

		while (
		       !(res = Yap_gmp_to_string(t, tmpbase, TMP_STRING_SIZE, 10))) {
		  if (tmpbase == tmp1) {
		    tmpbase = NULL;
		  } else {
		    tmpbase = res;
		    format_clean_up(sno, sno0, finfo);
		    Yap_ThrowError(DOMAIN_ERROR_INTEGER, targs[targ-1], "command %c in format %s", ch, fstr);
		    return false;
		  }
		  tmpbase = res;
		  ptr = tmpbase;
		  |
#endif
		    siz = strlen(tmpbase);
                                                                 
		} else {
		  format_clean_up(sno, sno0, finfo);
		  Yap_ThrowError(TYPE_ERROR_INTEGER, targs[targ-1], "command %c in format %s", ch, fstr);
		  return false;
		}

		if (tmpbase[0] == '-') {
		  f_putc(sno, (int) '-');
		  ptr++;
		}
		if (ch == 'D') {
		  int first = TRUE;

		  while (siz > repeats) {
		    if ((siz - repeats) % 3 == 0 && !first) {
		      f_putc(sno, (int) ',');
		    }
		    f_putc(sno, (int) (*ptr++));
		    first = FALSE;
		    siz--;
		  }
		} else {
		  while (siz > repeats) {
		    f_putc(sno, (int) (*ptr++));
		    siz--;
		  }
		}
		if (repeats) {
		  if (ptr == tmpbase || ptr[-1] == '-') {
		    f_putc(sno, (int) '0');
		  }
		  f_putc(sno, (int) '.');
		  while (repeats > siz) {
		    f_putc(sno, (int) '0');
		    repeats--;
		  }
		  while (repeats) {
		    f_putc(sno, (int) (*ptr++));
		    repeats--;
		  }
		}
		if (tmpbase != tmp1)
		  free(tmpbase);
		break;
	      case 'r':
	      case 'R': {
		Int numb, radix;
		UInt divfactor = 1, size = 1, i;
		wchar_t och;

		/* print a decimal, using weird . stuff */
		TOO_FEW_ARGUMENTS(1,false);
		t = targs[targ++];
		if (IsVarTerm(t)) {
		  format_clean_up(sno, sno0, finfo);
		  Yap_ThrowError(INSTANTIATION_ERROR, t, "command %c in format %s", ch,
				 fstr);
		}
		if (!has_repeats)
		  radix = 8;
		else
		  radix = repeats;
		if (radix > 36 || radix < 2) {
		  format_clean_up(sno, sno0, finfo);
		  Yap_ThrowError(DOMAIN_ERROR_RADIX, targs[targ-1], "command %c in format %s", ch,
				 fstr);
		}
#ifdef HAVE_GMP
		if (IsBigIntTerm(t) && RepAppl(t)[1] == BIG_INT) {
		  char *pt, *res;

		  tmpbase = tmp1;
		  while (!(
			   res = Yap_gmp_to_string(t, tmpbase, TMP_STRING_SIZE, radix))) {
		    if (tmpbase == tmp1) {
		      tmpbase = NULL;
		    } else {
		      tmpbase = res;

		      format_clean_up(sno, sno0, finfo);
		      Yap_ThrowError(TYPE_ERROR_INTEGER, targs[targ-1], "command %c in format %s", ch, fstr);
		    }
		  }
		  tmpbase = res;
		  pt = tmpbase;
		  while ((ch = *pt++))
		    f_putc(sno, ch);
		  if (tmpbase != tmp1)
		    free(tmpbase);
		  break;
		}
#endif
		if (!IsIntegerTerm(t)) {
		  format_clean_up(sno, sno0, finfo);
		  Yap_ThrowError(TYPE_ERROR_INTEGER, targs[targ-1], "command %c in format %s", ch,
				 fstr);
		}
		numb = IntegerOfTerm(t);
		if (numb < 0) {
		  numb = -numb;
		  f_putc(sno, (int) '-');
		}
		while (numb / divfactor >= radix) {
		  divfactor *= radix;
		  size++;
		}
		for (i = 1; i < size; i++) {
		  Int dig = numb / divfactor;
		  och = base_dig(dig, ch);
		  f_putc(sno, och);
		  numb %= divfactor;
		  divfactor /= radix;
		}
		och = base_dig(numb, ch);
		f_putc(sno, och);

		break;
	      }
	      case 's':
		TOO_FEW_ARGUMENTS(1,false);
		t = targs[targ++];
		if (IsVarTerm(t)) {
		  format_clean_up(sno, sno0, finfo);
		  Yap_ThrowError(INSTANTIATION_ERROR, targs[targ-1], "command %c in format %s", ch,
				 fstr);
		}
		if (!format_print_str(sno, repeats, has_repeats, t, f_putc, finfo)) {
		  return false;
		}
	      }
	      break;
	      case 'i':
		if (targ +1> tnum || has_repeats) {
		  format_clean_up(sno, finfo->sno0, finfo);
		  Yap_ThrowError(DOMAIN_ERROR_FORMAT_CONTROL_SEQUENCE, targs[targ-1],
				 "command ~c in format %s", ch,
				 fstr);
		}
		targ++;
		break;
		case 'k':
		  TOO_FEW_ARGUMENTS(1, has_repeats);
		  t = targs[targ++];
		  yhandle_t sl = Yap_StartSlots();
		  Yap_plwrite(t, GLOBAL_Stream + sno, HR,0,
			      YAP_WRITE_QUOTED | YAP_WRITE_IGNORE_OPS | YAP_WRITE_HEAP_TERMS ,
			      NULL);
		  Yap_CloseSlots(sl);
		  break;
		  case '@': {
		    t = targs[targ++];
		    yhandle_t sl0 = Yap_StartSlots(), s1 = Yap_PushHandle(ARG1),
		      sl = Yap_InitSlots(tnum - targ, targs + targ);

		    Int res;
		    int os = LOCAL_c_output_stream;
		    LOCAL_c_output_stream = sno;
		    res = Yap_execute_goal(t, 0, fmod, true);
		    LOCAL_c_output_stream = os;
		    if (Yap_HasException(PASS_REGS1))
		      goto ex_handler;
		    if (!res) {
		      if (!alloc_fstr)
			fstr = NULL;
		      format_clean_up(sno, sno0, finfo);
		      return false;
		    }
		    ARG1 = Yap_GetFromHandle(s1);
		    Yap_RecoverHandles(tnum - targ, sl);
		    Yap_CloseSlots(sl0);
		  }
		    break;
		    case 'p':
		      TOO_FEW_ARGUMENTS(1,has_repeats);
		      t = targs[targ++];
		      {
			Int sl = Yap_InitSlot(args);
	
			Yap_plwrite(t, GLOBAL_Stream + sno, HR,
				    0,                                        Number_vars_f | YAP_WRITE_USE_PORTRAY | YAP_WRITE_HEAP_TERMS ,
				    NULL
				    );
			args = Yap_GetFromSlot(sl);
			Yap_CloseSlots(sl);
		      }
		      if (Yap_HasException(PASS_REGS1)) {

		      ex_handler:
			if (tnum <= 8)
			  targs = NULL;
			if (IsAtomTerm(tail)) {
			  fstr = NULL;
			}
			if (!alloc_fstr)
			  fstr = NULL;
			if (tnum == 0) {
			  targs = NULL;
			}
			format_clean_up(sno, sno0, finfo);
			Yap_RaiseException();
			return false;
		      }
		      break;
		      case 'q':
			TOO_FEW_ARGUMENTS(1,has_repeats);
                        t = targs[targ++];
			{
			  yhandle_t sl0 = Yap_StartSlots();
			  Yap_plwrite(t, GLOBAL_Stream + sno, HR,0,
				      YAP_WRITE_HANDLE_CYCLES|Number_vars_f | YAP_WRITE_QUOTED | YAP_WRITE_HEAP_TERMS,
				      NULL);
			  Yap_CloseSlots(sl0);
                        }
                        break;
                        case 'w':
			  TOO_FEW_ARGUMENTS(1,has_repeats);
			  t = targs[targ++];
			  {
			    yhandle_t slf = Yap_StartSlots();
                            Yap_plwrite(t, GLOBAL_Stream + sno, HR,0,
                                        YAP_WRITE_HANDLE_CYCLES | Number_vars_f | YAP_WRITE_HEAP_TERMS ,
                                        NULL);
                            Yap_CloseSlots(slf);
			  }
			  break;
			  case 'W':
			    TOO_FEW_ARGUMENTS(2,has_repeats);
			    {
			      yhandle_t slf = Yap_StartSlots();
			      Yap_WriteTerm(sno, targs[targ], targs[targ + 1] PASS_REGS);
			      targ += 2;
			      Yap_CloseSlots(slf);
			    }
			    break;
			    case '~':
			      TOO_FEW_ARGUMENTS(0,has_repeats);
			      f_putc(sno, (int) '~');
			      break;
			      case 'n':
				if (!has_repeats)
				  repeats = 1;
				while (repeats--) {
				  f_putc(sno, (int) '\n');
	                        }
				sno = format_synch(sno, sno0, finfo);
				break;
				case 'N':

				  if (!has_repeats)
				    repeats = 1;
				  if (GLOBAL_Stream[sno].linestart !=
				      GLOBAL_Stream[sno].charcount ) {
				    f_putc(sno, '\n');
				    sno = format_synch(sno, sno0, finfo);
				  }
				  if (repeats > 1) {
				    Int i;
				    for (i = 1; i < repeats; i++)
				      {
					f_putc(sno, '\n');
                       
					sno = format_synch(sno, sno0, finfo);
				      }
				  }
				  break;


				  /* padding */
				  case '|':
				    sno = fill_pads(sno, sno0, repeats, finfo PASS_REGS);
				    finfo->phys_start = GLOBAL_Stream[sno].charcount
				      -GLOBAL_Stream[sno].linestart;
				    finfo->gapi=0;
				    break;
				    case '+':
				      if (!has_repeats)
					repeats = 8;
				      sno = fill_pads(sno, sno0, repeats, finfo PASS_REGS);
				      finfo->phys_start = GLOBAL_Stream[sno].charcount
					-GLOBAL_Stream[sno].linestart;
				      finfo->gapi=0;
				      break;
				      case 't': {
					Yap_flush(sno);
					finfo->gap[finfo->gapi].log = GLOBAL_Stream[sno].charcount;
					if (has_repeats)
					  finfo->gap[finfo->gapi].filler = repeats;
					else
					  finfo->gap[finfo->gapi].filler = ' ';
					finfo->gapi++;
				      }
					break;

					if (tnum <= 8)
					  targs = NULL;
					if (IsAtomTerm(tail)) {
					  fstr = NULL;
					}
					{
					  Term ta[2];
					  ta[0] = otail;
					  ta[1] = oargs;
					  format_clean_up(sno, sno0, finfo);
					  Yap_ThrowError(LOCAL_Error_TYPE,
							 Yap_MkApplTerm(Yap_MkFunctor(AtomFormat, 2), 2, ta),
							 "arguments to format");
					}
	    }
	    if (!alloc_fstr)
	      fstr = NULL;
	    if (tnum == 0) {
	      targs = NULL;
	    }
	    format_clean_up(sno, sno0, finfo);
	    return false;
	  }
	    /* ok, now we should have a command */
      }
      } else {
	if (ch == '\n') {
	  sno = format_synch(sno, sno0, finfo);
	}
	f_putc(sno, ch)
	  ;
      }
    }

    //    fill_pads( sno, 0, finfo);
    if (IsAtomTerm(tail) || IsStringTerm(tail)) {
      fstr = NULL;
    } 
    if (tnum <= 8)
      targs = NULL;
    fstr = NULL;
    targs = NULL;
    if (targ < tnum) {
      Yap_ThrowError(DOMAIN_ERROR_FORMAT_TOO_MANY_ARGUMENTS,MkIntTerm(tnum-targ),NULL);
    }
    format_clean_up(sno, sno0, finfo);

    Yap_CloseStream(sno);
    return true;
  }



  /**
   * @pred  with_output_to(+ _Ouput_,: _Goal_)

   Run  _Goal_ as once/1, while characters written to the current
   output are sent to  _Output_. The predicate was introduced by SWI-Prolog.

   The example below
   defines the DCG rule `term/3` to insert a term in the output:

   ```
   term(Term, In, Tail) :-
   with_output_to(codes(In, Tail), write(Term)).

   ?- phrase(term(hello), X).

   X = [104, 101, 108, 108, 111]
   ```

   + A Stream handle or alias
   Temporary switch current output to the given stream. Redirection using
   with_output_to/2 guarantees the original output is restored, also if Goal fails
   or raises an exception. See also call_cleanup/2.
   + atom(- _Atom_)
   Create an atom from the emitted characters.
   Applications should generally avoid creating atoms by breaking and
   concatenating other atoms as the creation of large numbers of
   intermediate atoms puts pressure on the atom table and the data-base. This may
   lead to collisions in the hash tables used to implement atoms, and may result in
   frequent calls to the garbage collector. In multi-threaded applications, access
   to the atom table is controlled by locks. This predicate supports creating the
   therms by expanding
   difference-list.

   + string(- _String_)
   Create a string-object, notice that strings are atomic objects.
   + codes(- _Codes_)
   Create a list of character codes from the emitted characters, similar to
   atom_codes/2.
   + codes(- _Codes_, - _Tail_)
   Create a list of character codes as a difference-list.
   + chars(- _Chars_)
   Create a list of one-character-atoms codes from the emitted characters, similar
   to atom_chars/2.
   + chars(- _Chars_, - _Tail_)
   Create a list of one-character-atoms as a difference-list.

  */
  static Int with_output_to(USES_REGS1) {
    int old_out = LOCAL_c_output_stream;
    int output_stream;
    Term tin = Deref(ARG1);
    Functor f;
    bool out;
    bool mem_stream = false;
    yhandle_t hdl = Yap_PushHandle(tin);
    if (IsVarTerm(tin)) {
      Yap_ThrowError(INSTANTIATION_ERROR, tin, "with_output_to/3");
      return false;
    }
    if (IsApplTerm(tin) && (f = FunctorOfTerm(tin))) {
      if (f == FunctorAtom || f == FunctorString || f == FunctorCodes1 ||
	  f == FunctorCodes || f == FunctorChars1 || f == FunctorChars) {
	output_stream =Yap_open_buf_write_stream(-1, LOCAL_encoding);
	mem_stream = true;
      }
    }
    if (!mem_stream) {
      output_stream = Yap_CheckStream(ARG1, Output_Stream_f, "format/3");
      f = NIL;
    }
    if (output_stream == -1) {
      return false;
    }
    LOCAL_c_output_stream = output_stream;
    out = Yap_Execute(Deref(ARG2) PASS_REGS);
    LOCAL_c_output_stream = old_out;
    if (mem_stream) {
      Term tat;
      Term inp = Yap_GetFromHandle(hdl);
      if (out) {
	tat = Yap_memStreamToTerm(output_stream, f, inp);
	out = Yap_unify(tat, ArgOfTerm(1, inp));
      }
    }
    Yap_CloseStream(output_stream);
    return out;
  }

  static Int format(Term tf, Term tas, Term tout USES_REGS) {
    Functor f;
    int output_stream;

    if (IsVarTerm(tout)) {
      Yap_ThrowError(INSTANTIATION_ERROR, tout, "format/3");
      return false;
    }
    yhandle_t hl = Yap_StartHandles();
    int l = push_text_stack();
    if (IsApplTerm(tout) && (f = FunctorOfTerm(tout)) &&
        (f == FunctorAtom || f == FunctorString1 || f == FunctorCodes1 ||
         f == FunctorCodes || f == FunctorChars1 || f == FunctorChars)) {
      output_stream = Yap_open_buf_write_stream(-1,LOCAL_encoding);
    } else {
      output_stream = Yap_CheckStream(tout, Output_Stream_f, "format/3");
    }
    if (output_stream == -1) {
      pop_text_stack(l);
      return false;
    } else {
      Term out = doformat(tf, tas, l, output_stream PASS_REGS);
      pop_text_stack(l);

      if (IsApplTerm(tout)) {
	      
	if (f == FunctorAtom) {
	  const char *s =Yap_MemExportStreamPtr( output_stream);
	  Term rc = MkAtomTerm(Yap_LookupAtom(s));
	  out = Yap_unify(rc,ArgOfTerm(1,ARG1));
	  Yap_CloseStream(output_stream);
	  return out;
	}else if ( f == FunctorString1) {
	  const char *s =Yap_MemExportStreamPtr( output_stream);
	  Term rc = MkStringTerm((s));
	  out = Yap_unify(rc,ArgOfTerm(1,ARG1));
	  Yap_CloseStream(output_stream);
	  return out;
	}else if ( f == FunctorChars) {
	  const unsigned char *s = (const unsigned char*)Yap_MemExportStreamPtr( output_stream);
	  Term rc = Yap_UTF8ToDiffListOfChars(s,ArgOfTerm(2,ARG1));
	  out = Yap_unify(rc,ArgOfTerm(1,ARG1));
	  Yap_CloseStream(output_stream);
	  return out;
	}else if ( f == FunctorChars1) {
	  const char *s =Yap_MemExportStreamPtr( output_stream);
	  Term rc = Yap_CharsToListOfAtoms(s,ENC_ISO_UTF8);
	  out = Yap_unify(rc,ArgOfTerm(1,ARG1));
	  Yap_CloseStream(output_stream);
	  return out;
	}else if ( f == FunctorCodes1) {
	  const char *s =Yap_MemExportStreamPtr( output_stream);
	  Term rc = Yap_CharsToListOfCodes(s,ENC_ISO_UTF8);
	  out = Yap_unify(rc,ArgOfTerm(1,ARG1));
	  Yap_CloseStream(output_stream);
	  return out;
	}else if ( f == FunctorCodes) {
	  const unsigned char *s =(const unsigned char*)Yap_MemExportStreamPtr( output_stream);
	  Term rc = Yap_UTF8ToDiffListOfCodes(s,ArgOfTerm(2,ARG1));
	  out = Yap_unify(rc,ArgOfTerm(1,ARG1));
	  Yap_CloseStream(output_stream);
	  return out;
	}
      }
	Yap_CloseHandles(hl);
      return out;
    }
  }

  /** @pred  format(+ _T_, :ListWithArguments)
   *
   * Print formatted output to the current output stream.
   */
  static Int format2(USES_REGS1) { /* 'format'(Stream,Control,Args)          */
    Int res;
    int l = push_text_stack();
    
    res = doformat(Deref(ARG1), Deref(ARG2),l, LOCAL_c_output_stream PASS_REGS);
    pop_text_stack(l);
    return res;
  }

  /** @pred  format(+_Stream_+ _T_, :ListWithArguments)
   *
   * Print formatted output to the stream _Stream_.
   */
  static Int format3(USES_REGS1) {
    Int res;
    res = format(Deref(ARG2), Deref(ARG3), Deref(ARG1) PASS_REGS);
    return res;
  }

  void Yap_InitFormat(void) {
    Yap_InitCPred("format", 2, format2, SyncPredFlag);
    Yap_InitCPred("format", 3, format3, SyncPredFlag);
    Yap_InitCPred("with_output_to", 2, with_output_to, SyncPredFlag);
  }

  /// @}
