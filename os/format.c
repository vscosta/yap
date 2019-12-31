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
 * @file format.c
 *
 */

/**
 * @defgroup FormattedIO Formatted Output
 * @ingroup InputOutput 
 *             
 * @{
 *
 * This file includes the definition of the formatted output predicates.
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

~~~~~
    printf("%s.Nc", F)
~~~~~

As an example:

~~~~~
?- format("~8e, ~8E, ~8f, ~8g, ~8G~w",
          [3.14,3.14,3.14,3.14,3.14,3.14]).
3.140000e+00, 3.140000E+00, 3.140000, 3.14, 3.143.14
~~~~~

+ `~Nd`
The next argument must be an integer, and  _N_ is the number of digits
after the decimal point. If  _N_ is `0` no decimal points will be
printed. The default is  _N = 0_.

~~~~~
?- format("~2d, ~d",[15000, 15000]).
150.00, 15000
~~~~~

+ `~ND`
Identical to `~Nd`, except that commas are used to separate groups
of three digits.

~~~~~
?- format("~2D, ~D",[150000, 150000]).
1,500.00, 150,000
~~~~~

+ `~i`
Ignore the next argument in the list of arguments:

~~~~~
?- format('The ~i met the boregrove',[mimsy]).
The  met the boregrove
~~~~~

+ `~k`
Print the next argument with `write_canonical`:

~~~~~
?- format("Good night ~k",a+[1,2]).
Good night +(a,[1,2])
~~~~~

+ `~Nn`
Print  _N_ newlines (where  _N_ defaults to 1).

+ `~NN`
Print  _N_ newlines if at the beginning of the line (where  _N_
defaults to 1).

+ `~Nr`
The next argument must be an integer, and  _N_ is interpreted as a
radix, such that `2 <= N <= 36` (the default is 8).

~~~~~
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249f0, 444760
~~~~~

Note that the letters `a-z` denote digits larger than 9.

+ `~NR`
Similar to `~NR`. The next argument must be an integer, and  _N_ is
interpreted as a radix, such that `2 <= N <= 36` (the default is 8).

~~~~~
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249F0, 444760
~~~~~

The only difference is that letters `A-Z` denote digits larger than 9.

+ `~p`
Print the next argument with print/1:

~~~~~
?- format("Good night ~p",a+[1,2]).
Good night a+[1,2]
~~~~~

+ `~q`
Print the next argument with writeq/1:

~~~~~
?- format("Good night ~q",'Hello'+[1,2]).
Good night 'Hello'+[1,2]
~~~~~

+ `~Ns`
The next argument must be a list of character codes.The system then
outputs their representation as a string, where  _N_ is the maximum
number of characters for the string ( _N_ defaults to the length of the
string).

~~~~~
?- format("The ~s are ~4s",["woods","lovely"]).
The woods are love
~~~~~

+ `~w`
Print the next argument with write/1:

~~~~~
?- format("Good night ~w",'Hello'+[1,2]).
Good night Hello+[1,2]
~~~~~


The number of arguments, `N`, may be given as an integer, or it
may be given as an extra argument. The next example shows a small
procedure to write a variable number of `a` characters:

~~~~~
write_many_as(N) :-
        format("~*c",[N,0'a]).
~~~~~

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

~~~~~
   ?- format("~n*Hello~16+*~n",[]).
*Hello          *
~~~~~

Note that we reserve 16 characters for the column.

The following example shows how to do right-alignment:

~~~~~
   ?- format("*~tHello~16+*~n",[]).
*          Hello*

~~~~~

The `~t` escape sequence forces filling before `Hello`.

We next show how to do centering:

~~~~~
   ?- format("*~tHello~t~16+*~n",[]).
*     Hello     *
~~~~~

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


static int format_print_str(Int sno, Int size, Int has_size, Term args,
                            int (*f_putc)(int, wchar_t)) {
  Term arghd;
  if (IsStringTerm(args)) {
    const unsigned char *pt = UStringOfTerm(args);
    while (*pt && (!has_size || size > 0)) {
      utf8proc_int32_t ch;
      
      if ((pt += get_utf8(pt, -1, &ch)) > 0) {
	f_putc(sno, ch);
      }	
    }
  } else if (IsAtomTerm(args)) {
    if (args == TermNil) {
        return TRUE;
      } 
    const unsigned char *pt =  RepAtom(AtomOfTerm(args))->UStrOfAE;
    while (*pt && (!has_size || size > 0)) {
      utf8proc_int32_t ch;
      
      if ((pt += get_utf8(pt, -1, &ch)) > 0) {
	f_putc(sno, ch);
      }	
    }
  } else {
    while (!has_size || size > 0) {
      bool maybe_chars = true, maybe_codes = true;
      if (IsVarTerm(args)) {
        Yap_ThrowError(INSTANTIATION_ERROR, args, "~s expects a bound argument");
        return FALSE;
      } else    if (args == TermNil) {
        return TRUE;
          } else if (!IsPairTerm(args)) {
        Yap_ThrowError(TYPE_ERROR_TEXT, args, "format expects an atom, string, or list of codes or chars ");
        return FALSE;
      }
      arghd = HeadOfTerm(args);
      args = TailOfTerm(args);
      if (IsVarTerm(arghd)) {
        Yap_ThrowError(INSTANTIATION_ERROR, arghd, "~s expects a bound argument");
        return FALSE;
      } else if (maybe_codes && IsIntTerm(arghd)) {
	f_putc(sno, (int)IntOfTerm(arghd));
	size--;
	maybe_chars = false;
      } else if (maybe_chars && IsAtomTerm(arghd)) {
	unsigned char *fptr = RepAtom(AtomOfTerm(arghd))->UStrOfAE;
	int ch;
	fptr += get_utf8(fptr, -1, &ch);
	if (fptr[0] != '\0') {
	  Yap_ThrowError(TYPE_ERROR_TEXT, arghd, "~s expects a list of chars ");
	}
	f_putc(sno, ch);
	size--;
	maybe_codes = false;
      }  else  {
	Yap_ThrowError(TYPE_ERROR_TEXT, arghd, "~s expects an atom, string, or list of codes or chars ");
        return FALSE;
      }
     }
  }
  return TRUE;
}

static Int format_copy_args(Term args, Term *targs, Int tsz) {
  Int n = 0;
  while (args != TermNil) {
    if (IsVarTerm(args)) {
      Yap_ThrowError(INSTANTIATION_ERROR, args, "format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (!IsPairTerm(args)) {
      Yap_ThrowError(TYPE_ERROR_LIST, args, "format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (n == tsz)
      return FORMAT_COPY_ARGS_OVERFLOW;
    targs[n] = HeadOfTerm(args);
    args = TailOfTerm(args);
    n++;
  }
  return n;
}

static void

format_clean_up(int sno, int sno0, format_info *finfo) {
  if (sno >= 0 && sno != sno0) {
    sno = format_synch(sno, sno0, finfo);
    Yap_CloseStream(sno);
  }

  pop_text_stack(finfo->lvl);

}

static Int fetch_index_from_args(Term t) {
  Int i;

  if (IsVarTerm(t))
    return -1;
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

static Int doformat(volatile Term otail, volatile Term oargs,
                    int sno0 USES_REGS) {
  char *tmp1, *tmpbase;
  int ch;
  Term *targs;
  Int tnum, targ;
  const unsigned char *fstr, *fptr;
  Term args;
  Term tail;
  int (*f_putc)(int, wchar_t);
  int sno = sno0;
  jmp_buf format_botch;
  volatile void *old_handler;
  volatile int old_pos;
  Term fmod = CurrentModule;
  bool alloc_fstr = false;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  int l = push_text_stack();
  tmp1 = Malloc(TMP_STRING_SIZE+1);
  format_info *finfo = Malloc(sizeof(format_info));
  // it starts here
  finfo->gapi = 0;
  finfo->phys_start = 0;
  finfo->lstart = 0;
  finfo->lvl = l;

  if (GLOBAL_Stream[sno0].status & InMemory_Stream_f) {
    old_handler = GLOBAL_Stream[sno].u.mem_string.error_handler;
    GLOBAL_Stream[sno].u.mem_string.error_handler = (void *)&format_botch;
    old_pos = GLOBAL_Stream[sno].u.mem_string.pos;
    /* set up an error handler */
    if (setjmp(format_botch)) {
      restore_machine_regs();
      *HR++ = oargs;
      *HR++ = otail;
      if (!Yap_growheap(FALSE, LOCAL_Error_Size, NULL)) {
	pop_text_stack(l);
        Yap_ThrowError(RESOURCE_ERROR_HEAP, otail, "format/2");
        return false;
      }
      oargs = HR[-2];
      otail = HR[-1];
      GLOBAL_Stream[sno].u.mem_string.pos = old_pos;
      HR -= 2;
    }
  } else {
    old_handler = NULL;
  }
  args = oargs;
  tail = otail;
  targ = 0;
  if (IsVarTerm(tail)) {
    format_clean_up(sno0, sno, finfo );
    Yap_ThrowError(INSTANTIATION_ERROR, tail, "format/2");
    return (FALSE);
  } else if ((fstr = Yap_TextToUTF8Buffer(tail))) {
    fptr = fstr;
    alloc_fstr = true;
  } else {
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(TYPE_ERROR_TEXT, tail, "format/2");
    return false;
  }
  if (IsVarTerm(args)) {
    pop_text_stack(l);
    format_clean_up(sno0, sno, finfo);
    Yap_ThrowError(INSTANTIATION_ERROR, args, "format/2");
    return FALSE;
  }
  while (IsApplTerm(args) && FunctorOfTerm(args) == FunctorModule) {
    fmod = ArgOfTerm(1, args);
    args = ArgOfTerm(2, args);
    if (IsVarTerm(fmod)) {
      format_clean_up(sno0, sno, finfo);
      pop_text_stack(l);
      Yap_ThrowError(INSTANTIATION_ERROR, fmod, "format/2");
      return false;
    }
    if (!IsAtomTerm(fmod)) {
      format_clean_up(sno0, sno, finfo);
      pop_text_stack(l);
      Yap_ThrowError(TYPE_ERROR_ATOM, fmod, "format/2");
      return false;
    }
    if (IsVarTerm(args)) {
      format_clean_up(sno0, sno, finfo);
      pop_text_stack(l);
      Yap_ThrowError(INSTANTIATION_ERROR, args, "format/2");
      return FALSE;
    }
  }
  if (IsPairTerm(args)) {
    Int tsz = 16;

    targs = Malloc(32*sizeof(Term));
    do {
      tnum = format_copy_args(args, targs, tsz);
      if (tnum == FORMAT_COPY_ARGS_ERROR ||
	  tnum == FORMAT_COPY_ARGS_OVERFLOW) {
	format_clean_up(sno0, sno, finfo);
	pop_text_stack(l);
	return false;
      }
      else if (tnum == tsz ) {
	tnum += 32;
	targs = Realloc(targs, tnum*sizeof(Term));
      }
      break;
    } while (true);      
  } else if (args != TermNil) {
    tnum = 1;
     targs = Malloc(sizeof(Term));
     targs[0] = args;
  } else {
    tnum = 0;
  }

  if ( !(GLOBAL_Stream[sno].status & InMemory_Stream_f)) {
    sno = Yap_OpenBufWriteStream(PASS_REGS1);	\
  }
  if (sno < 0) {
    if (!alloc_fstr)
      fstr = NULL;

    format_clean_up(sno, sno0, finfo);
    pop_text_stack(l);
    return false;
  }
  GLOBAL_Stream[sno].status |= CloseOnException_Stream_f;
  f_putc = GLOBAL_Stream[sno].stream_wputc;
  while ((fptr += get_utf8(fptr, -1, &ch)) && ch) {
    Term t = TermNil;
    int has_repeats = false;
    int repeats = 0;

    if (ch == '~') {
      /* start command */
      fptr += get_utf8(fptr, -1, &ch);
      if (ch == '*') {
	fptr += get_utf8(fptr, -1, &ch);
	has_repeats = TRUE;
	if (targ > tnum - 1) {
	  goto do_format_control_sequence_error;
	}
	repeats = fetch_index_from_args(targs[targ++]);
	if (repeats == -1)
	  goto do_format_control_sequence_error;
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
      case 'a':
	/* print an atom */
	if (has_repeats || targ > tnum - 1)
	  goto do_format_control_sequence_error;
	t = targs[targ++];
	if (IsVarTerm(t))
	  goto do_instantiation_error;
	if (!IsAtomTerm(t))
	  goto do_type_atom_error;
	yhandle_t sl = Yap_StartSlots();
	// stream is already locked.
	Yap_plwrite(t, GLOBAL_Stream + sno, 0, Handle_vars_f | To_heap_f | Handle_cyclics_f,		 NULL);
	Yap_CloseSlots(sl);
	break;
      case 'c': {
	Int nch, i;

	if (targ > tnum - 1)
	  goto do_format_control_sequence_error;
	t = targs[targ++];
	if (IsVarTerm(t))
	  goto do_instantiation_error;
	if (!IsIntegerTerm(t))
	  goto do_type_int_error;
	nch = IntegerOfTerm(t);
	if (nch < 0)
	  goto do_domain_not_less_zero_error;
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

	if (targ > tnum - 1)
	  goto do_format_control_sequence_error;
	t = targs[targ++];
	if (IsVarTerm(t))
	  goto do_instantiation_error;
	if (!IsNumTerm(t))
	  goto do_type_number_error;
	if (IsIntegerTerm(t)) {
	  fl = (Float)IntegerOfTerm(t);
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
	  unsigned char *uptr = (unsigned char *)tmp1;
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
	  case 'D':
	    /* print a decimal, using weird . stuff */
	    if (targ > tnum - 1)
	      goto do_format_control_sequence_error;
	    t = targs[targ++];
	    if (IsVarTerm(t))
	      goto do_instantiation_error;
	    if (!IsIntegerTerm(t)
#ifdef HAVE_GMP
		&& !IsBigIntTerm(t)
#endif

		)
	      goto do_type_int_error;

	    {
	      Int siz = 0;
	      char *ptr = tmp1;
	      tmpbase = tmp1;

	      if (IsIntegerTerm(t)) {
		Int il = IntegerOfTerm(t);
#if HAVE_SNPRINTF
		snprintf(tmp1, 256, "%ld", (long int)il);
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

		    goto do_type_int_error;
		  }
		}
		tmpbase = res;
		ptr = tmpbase;
#endif
		siz = strlen(tmpbase);
	      } else {
		goto do_type_int_error;
	      }

	      if (tmpbase[0] == '-') {
		f_putc(sno, (int)'-');
		ptr++;
	      }
	      if (ch == 'D') {
		int first = TRUE;

		while (siz > repeats) {
		  if ((siz - repeats) % 3 == 0 && !first) {
		    f_putc(sno, (int)',');
		  }
		  f_putc(sno, (int)(*ptr++));
		  first = FALSE;
		  siz--;
		}
	      } else {
		while (siz > repeats) {
		  f_putc(sno, (int)(*ptr++));
		  siz--;
		}
	      }
	      if (repeats) {
		if (ptr == tmpbase || ptr[-1] == '-') {
		  f_putc(sno, (int)'0');
		}
		f_putc(sno, (int)'.');
		while (repeats > siz) {
		  f_putc(sno, (int)'0');
		  repeats--;
		}
		while (repeats) {
		  f_putc(sno, (int)(*ptr++));
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
		  if (targ > tnum - 1)
		    goto do_format_control_sequence_error;
		  t = targs[targ++];
		  if (IsVarTerm(t))
		    goto do_instantiation_error;
		  if (!has_repeats)
		    radix = 8;
		  else
		    radix = repeats;
		  if (radix > 36 || radix < 2)
		    goto do_domain_error_radix;
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
			goto do_type_int_error;
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
		  if (!IsIntegerTerm(t))
		    goto do_type_int_error;
		  numb = IntegerOfTerm(t);
		  if (numb < 0) {
		    numb = -numb;
		    f_putc(sno, (int)'-');
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
		    if (targ > tnum - 1)
		      goto do_format_control_sequence_error;
		    t = targs[targ++];
		    if (IsVarTerm(t))
		      goto do_instantiation_error;
		    if (!format_print_str(sno, repeats, has_repeats, t, f_putc)) {
		      goto do_default_error;
		    }
		    break;
		    case 'i':
		      if (targ > tnum - 1 || has_repeats)
			goto do_format_control_sequence_error;
		      targ++;
		      break;
		      case 'k':
			if (targ > tnum - 1 || has_repeats)
			  goto do_format_control_sequence_error;
			t = targs[targ++];
			yhandle_t sl = Yap_StartSlots();
			Yap_plwrite(t, GLOBAL_Stream + sno, 0,
				    Quote_illegal_f | Ignore_ops_f | To_heap_f | Handle_cyclics_f,
				    NULL);
			Yap_CloseSlots(sl);
			break;
			case '@':
			  t = targs[targ++];
			  {
			    yhandle_t sl0 = Yap_StartSlots(), s1 = Yap_PushHandle(ARG1),
			      sl = Yap_InitSlots(tnum - targ, targs + targ);

			    Int res;
			    int os = LOCAL_c_output_stream;
			    LOCAL_c_output_stream = sno;
			    res = Yap_execute_goal(t, 0, fmod, true);
			    LOCAL_c_output_stream = os;
			    if (Yap_HasException())
			      goto ex_handler;
			    if (!res) {
			      if (!alloc_fstr)
				fstr = NULL;
			      format_clean_up(sno, sno0, finfo);
			      pop_text_stack(l);
			      return false;
			    }
			    ARG1 = Yap_GetFromHandle(s1);
			    Yap_RecoverHandles(tnum - targ, sl);
			    Yap_CloseSlots(sl0);
			  }
			  break;
			  case 'p':
			    if (targ > tnum - 1 || has_repeats)
			      goto do_format_control_sequence_error;
			    t = targs[targ++];
			    {
			      Int sl = Yap_InitSlot(args);
			      Yap_plwrite(t, GLOBAL_Stream + sno, 0,
					  Handle_vars_f | Use_portray_f | To_heap_f | Handle_cyclics_f,
					  NULL);
			      args = Yap_GetFromSlot(sl);
			      Yap_CloseSlots(sl);
			    }
			    if (Yap_HasException()) {

			    ex_handler:
			      if (tnum <= 8)
				targs = NULL;
			      if (IsAtomTerm(tail)) {
				fstr = NULL;
			      }
			      if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
				GLOBAL_Stream[sno].u.mem_string.error_handler = old_handler;
			      }
			      if (!alloc_fstr)
				fstr = NULL;
			      if (tnum == 0) {
				targs = NULL;
			      }
			      format_clean_up(sno, sno0, finfo);
			      pop_text_stack(l);
			      Yap_RaiseException();
			      return false;
			    }
			    break;
			    case 'q':
			      if (targ > tnum - 1 || has_repeats)
				goto do_format_control_sequence_error;
			      t = targs[targ++];
			      {
				yhandle_t sl0 = Yap_StartSlots();
				Yap_plwrite(t, GLOBAL_Stream + sno, 0,
					    Handle_vars_f | Quote_illegal_f | To_heap_f | Handle_cyclics_f,
					    NULL);
				Yap_CloseSlots(sl0);
			      }
			      break;
			      case 'w':
				if (targ > tnum - 1 || has_repeats)
				  goto do_format_control_sequence_error;
				t = targs[targ++];
				{
				  yhandle_t slf = Yap_StartSlots();
				  Yap_plwrite(t, GLOBAL_Stream + sno, 0, Handle_vars_f | To_heap_f | Handle_cyclics_f,
					      NULL);
				  Yap_CloseSlots(slf);
				}
				break;
				case 'W':
				  if (targ > tnum - 2 || has_repeats)
				    goto do_format_control_sequence_error;
				  targ -= 2;
				  {
				    yhandle_t slf = Yap_StartSlots();
				    if (!Yap_WriteTerm(sno, targs[1], targs[0] PASS_REGS)) {
				      Yap_CloseSlots(slf);
				      goto do_default_error;
				    };
				    Yap_CloseSlots(slf);
				  }
				  break;
				  case '~':
				    if (has_repeats)
				      goto do_format_control_sequence_error;
				    f_putc(sno, (int)'~');
				    break;
				    case 'n':
				      if (!has_repeats)
					repeats = 1;
				      while (repeats--) {
					f_putc(sno, (int)'\n');
				      }
				      sno = format_synch(sno, sno0, finfo);
				      break;
				      case 'N':
					if (!has_repeats)
					  has_repeats = 1;
					if (GLOBAL_Stream[sno].linepos != 0) {
					  f_putc(sno, '\n');
					  sno = format_synch(sno, sno0, finfo);
					}
					if (repeats > 1) {
					  Int i;
					  for (i = 1; i < repeats; i++)
					    f_putc(sno, '\n');
					}
					sno = format_synch(sno, sno0, finfo);
					break;
					/* padding */
					case '|':
					  fill_pads(sno, sno0, repeats, finfo PASS_REGS);
					  break;
					  case '+':
					    fill_pads(sno, sno0, finfo->lstart + repeats, finfo PASS_REGS);
					    break;
					    case 't': {
#if MAY_WRITE
					      if (fflush(GLOBAL_Stream[sno].file) == 0) {
						finfo->gap[finfo->gapi].phys = ftell(GLOBAL_Stream[sno].file);
					      }
#else
					      finfo->gap[finfo->gapi].phys = GLOBAL_Stream[sno].u.mem_string.pos;
#endif
					      finfo->gap[finfo->gapi].log = GLOBAL_Stream[sno].linepos;
					      if (has_repeats)
						finfo->gap[finfo->gapi].filler = fptr[-2];
					      else
						finfo->gap[finfo->gapi].filler = ' ';
					      finfo-> gapi++;
					    } break;

	    do_instantiation_error:
					    LOCAL_Error_TYPE = INSTANTIATION_ERROR;
					    goto do_default_error;
	    do_type_int_error:
					    LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
					    goto do_default_error;
	    do_type_number_error:
					    LOCAL_Error_TYPE = TYPE_ERROR_NUMBER;
					    goto do_default_error;
	    do_type_atom_error:
					    LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
					    goto do_default_error;
	    do_domain_not_less_zero_error:
					    LOCAL_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
					    goto do_default_error;
	    do_domain_error_radix:
					    LOCAL_Error_TYPE = DOMAIN_ERROR_RADIX;
					    goto do_default_error;
	    do_format_control_sequence_error:
					    LOCAL_Error_TYPE = DOMAIN_ERROR_FORMAT_CONTROL_SEQUENCE;
					    default:
					      LOCAL_Error_TYPE = YAP_NO_ERROR;
	    do_default_error:
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
					      if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
						GLOBAL_Stream[sno].u.mem_string.error_handler = old_handler;
					      }
					      if (!alloc_fstr)
						fstr = NULL;
					      if (tnum == 0) {
						targs = NULL;
					      }
					      format_clean_up(sno, sno0, finfo);
					      LOCAL_Error_TYPE = YAP_NO_ERROR;
					      pop_text_stack(l);
					      return false;
	    }
      }
	/* ok, now we should have a command */
      }
    } else {
      if (ch == '\n') {
	sno = format_synch(sno, sno0, finfo);
      }
      f_putc(sno, ch);
    }
  }
  //    fill_pads( sno, 0, finfo);
  if (IsAtomTerm(tail) || IsStringTerm(tail)) {
    fstr = NULL;
  }
  if (tnum <= 8)
    targs = NULL;
  if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
    GLOBAL_Stream[sno].u.mem_string.error_handler = old_handler;
  }
  fstr = NULL;
  targs = NULL;
  format_clean_up(sno, sno0, finfo);
  pop_text_stack(l);
  return true;
}

static Term memStreamToTerm(int output_stream, Functor f, Term inp) {
  const char *s = Yap_MemExportStreamPtr(output_stream);

  encoding_t enc = GLOBAL_Stream[output_stream].encoding;
  if (f == FunctorAtom) {
    return MkAtomTerm(Yap_LookupAtom(s));
  } else if (f == FunctorCodes) {
    return Yap_CharsToDiffListOfCodes(s, ArgOfTerm(2, inp), enc PASS_REGS);
  } else if (f == FunctorCodes1) {
    return Yap_CharsToListOfCodes(s, enc PASS_REGS);
  } else if (f == FunctorChars) {
    return Yap_CharsToDiffListOfAtoms(s, ArgOfTerm(2, inp), enc PASS_REGS);
  } else if (f == FunctorChars1) {
    return Yap_CharsToListOfAtoms(s, enc PASS_REGS);
  } else if (f == FunctorString1) {
    return Yap_CharsToString(s, enc PASS_REGS);
  }
  Yap_ThrowError(DOMAIN_ERROR_FORMAT_OUTPUT, inp, NULL);
  return 0L;
}

/**
 * @pred  with_output_to(+ _Ouput_,: _Goal_)

Run  _Goal_ as once/1, while characters written to the current
output are sent to  _Output_. The predicate was introduced by SWI-Prolog.

 The example below
defines the DCG rule `term/3` to insert a term in the output:

~~~~~
 term(Term, In, Tail) :-
        with_output_to(codes(In, Tail), write(Term)).

?- phrase(term(hello), X).

X = [104, 101, 108, 108, 111]
~~~~~

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
		  output_stream = Yap_OpenBufWriteStream(PASS_REGS1);
		  mem_stream = true;
	  }
  }
  if (!mem_stream){
    output_stream = Yap_CheckStream(ARG1, Output_Stream_f, "format/3");
    f = NIL;
  }
  if (output_stream == -1) {
    return false;
  }
  LOCAL_c_output_stream = output_stream;
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  out = Yap_Execute(Deref(ARG2) PASS_REGS);
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  LOCAL_c_output_stream = old_out;
  if (mem_stream) {
    Term tat;
    Term inp = Yap_GetFromHandle(hdl);
    if (out) {
      tat = memStreamToTerm(output_stream, f, inp);
      out = Yap_unify(tat, ArgOfTerm(1, inp));
    }
    Yap_CloseStream(output_stream);
  }
  return out;
}

static Int format(Term tf, Term tas, Term tout USES_REGS) {
  Int out;
  Functor f;
  int output_stream;
  bool mem_stream = false;

  if (IsVarTerm(tout)) {
    Yap_ThrowError(INSTANTIATION_ERROR, tout, "format/3");
    return false;
  }
  yhandle_t hl = Yap_StartHandles(), yo = Yap_PushHandle(tout);
  if (IsApplTerm(tout) && (f = FunctorOfTerm(tout)) &&
      (f == FunctorAtom || f == FunctorString1 || f == FunctorCodes1 ||
       f == FunctorCodes || f == FunctorChars1 || f == FunctorChars) ){
    output_stream = Yap_OpenBufWriteStream(PASS_REGS1);
    mem_stream = true;
  } else {
      output_stream = Yap_CheckStream(tout, Output_Stream_f, "format/3");
  }
  if (output_stream == -1) {
    UNLOCK(GLOBAL_Stream[output_stream].streamlock);
    return false;
  } else {
    out = doformat(tf, tas, output_stream PASS_REGS);
    UNLOCK(GLOBAL_Stream[output_stream].streamlock);
    if (mem_stream) {

      if (out) {
        Term to = Yap_GetFromHandle(yo);
        Term tat = memStreamToTerm(output_stream, f, to);
        if (tat == 0)
          return false;
        out = Yap_unify(tat, ArgOfTerm(1, to));
      }

      Yap_CloseStream(output_stream);
    }
  }
  Yap_CloseHandles(hl);
  return out;
}

/** @pred  format(+ _T_, :ListWithArguments)
 *
 * Print formatted output to the current output stream.
 */
static Int format2(USES_REGS1) { /* 'format'(Stream,Control,Args)          */
  Int res;

  res = doformat(Deref(ARG1), Deref(ARG2), LOCAL_c_output_stream PASS_REGS);
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
