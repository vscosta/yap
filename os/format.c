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
 * @defgroup FormattedIO Formatted Output
 * @ingroup YAPIO
 * This file includes the definition of the formatted output predicates.
 *
 * @{
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

~~~~~{.prolog}
    printf("%s.Nc", F)
~~~~~

As an example:

~~~~~{.prolog}
?- format("~8e, ~8E, ~8f, ~8g, ~8G~w",
          [3.14,3.14,3.14,3.14,3.14,3.14]).
3.140000e+00, 3.140000E+00, 3.140000, 3.14, 3.143.14
~~~~~

+ `~Nd`
The next argument must be an integer, and  _N_ is the number of digits
after the decimal point. If  _N_ is `0` no decimal points will be
printed. The default is  _N = 0_.

~~~~~{.prolog}
?- format("~2d, ~d",[15000, 15000]).
150.00, 15000
~~~~~

+ `~ND`
Identical to `~Nd`, except that commas are used to separate groups
of three digits.

~~~~~{.prolog}
?- format("~2D, ~D",[150000, 150000]).
1,500.00, 150,000
~~~~~

+ `~i`
Ignore the next argument in the list of arguments:

~~~~~{.prolog}
?- format('The ~i met the boregrove',[mimsy]).
The  met the boregrove
~~~~~

+ `~k`
Print the next argument with `write_canonical`:

~~~~~{.prolog}
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

~~~~~{.prolog}
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249f0, 444760
~~~~~

Note that the letters `a-z` denote digits larger than 9.

+ `~NR`
Similar to `~NR`. The next argument must be an integer, and  _N_ is
interpreted as a radix, such that `2 <= N <= 36` (the default is 8).

~~~~~{.prolog}
?- format("~2r, 0x~16r, ~r",
          [150000, 150000, 150000]).
100100100111110000, 0x249F0, 444760
~~~~~

The only difference is that letters `A-Z` denote digits larger than 9.

+ `~p`
Print the next argument with print/1:

~~~~~{.prolog}
?- format("Good night ~p",a+[1,2]).
Good night a+[1,2]
~~~~~

+ `~q`
Print the next argument with writeq/1:

~~~~~{.prolog}
?- format("Good night ~q",'Hello'+[1,2]).
Good night 'Hello'+[1,2]
~~~~~

+ `~Ns`
The next argument must be a list of character codes.The system then
outputs their representation as a string, where  _N_ is the maximum
number of characters for the string ( _N_ defaults to the length of the
string).

~~~~~{.prolog}
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
#include "eval.h"
#include "iopreds.h"

#define FORMAT_MAX_SIZE 1024

typedef struct {
  Int len, start; /* tab point */
  Int filler;     /* character to dump */
  char *pad;      /* ok, it's not standard english */
} pads;

typedef struct format_status {
  char *mem; // direct access to stream fields
  int format_error;
  pads pad_entries[16];
  int padders;
} format_info;

static int fill(int sno, int n, wchar_t nch) {
  int (*f_putc)(int, wchar_t);
  f_putc = GLOBAL_Stream[sno].stream_wputc;
  while (n--)
    f_putc(sno, nch);
  return nch;
}

static int f_puts(int sno, char *s, int n) {
  int (*f_putc)(int, wchar_t);
  f_putc = GLOBAL_Stream[sno].stream_wputc;
  while (n--)
    f_putc(sno, *s++);
  return *s;
}

// uses directly the buffer in the memory stream.
static bool fill_pads(int sno, int nchars, format_info *fg USES_REGS) {
  int nfillers, fill_space, lfill_space;

  if (nchars < 0)
    nchars = 0; /* ignore */
  nfillers = fg->padders;
  if (fg->padders == 0) {
    return fill(sno, nchars, ' ');
  }
  fill_space = nchars / nfillers;
  lfill_space = nchars % nfillers;

  pads *padi = fg->pad_entries;

  while (fg->padders--) {
    if (!fg->padders)
      fill_space += lfill_space;
    // give remainder to last block.
    fill(sno, fill_space, padi->filler);
    if (padi->pad) {
      f_puts(sno, (char *)padi->pad, padi->len);
      free(padi->pad);
      padi->pad = NULL;
    }
    padi++;
  }
  return true;
}

static int format_print_str(Int sno, Int size, Int has_size, Term args,
                            int (*f_putc)(int, wchar_t)) {
  Term arghd;
  if (IsStringTerm(args)) {
    const unsigned char *pt = UStringOfTerm(args);
    while (*pt && (!has_size || size > 0)) {
      utf8proc_int32_t ch;
      pt += get_utf8((unsigned char *)pt, -1, &ch);
      f_putc(sno, ch);
    }
  } else {
    while (!has_size || size > 0) {
      if (IsVarTerm(args)) {
        Yap_Error(INSTANTIATION_ERROR, args, "format/2");
        return FALSE;
      } else if (args == TermNil) {
        return TRUE;
      } else if (!IsPairTerm(args)) {
        Yap_Error(TYPE_ERROR_LIST, args, "format/2");
        return FALSE;
      }
      arghd = HeadOfTerm(args);
      args = TailOfTerm(args);
      if (IsVarTerm(arghd)) {
        Yap_Error(INSTANTIATION_ERROR, arghd, "format/2");
        return FALSE;
      } else if (!IsIntTerm(arghd)) {
        Yap_Error(TYPE_ERROR_LIST, arghd, "format/2");
        return FALSE;
      }
      f_putc(sno, (int)IntOfTerm(arghd));
      size--;
    }
  }
  return TRUE;
}

typedef enum { fst_ok, fst_error, fst_too_long } format_cp_res;

static format_cp_res copy_format_string(Term inp, char *out, int max) {
  int i = 0;
  while (inp != TermNil) {
    Term hd;
    int ch;

    if (IsVarTerm(inp)) {
      Yap_Error(INSTANTIATION_ERROR, inp, "format/2");
      return fst_error;
    }
    if (!IsPairTerm(inp)) {
      Yap_Error(TYPE_ERROR_LIST, inp, "format/2");
      return fst_error;
    }
    hd = HeadOfTerm(inp);
    if (IsVarTerm(hd)) {
      Yap_Error(INSTANTIATION_ERROR, hd, "format/2");
      return fst_error;
    }
    if (!IsIntTerm(hd)) {
      Yap_Error(TYPE_ERROR_INTEGER, hd, "format/2");
      return fst_error;
    }
    ch = IntOfTerm(hd);
    if (ch < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, hd, "format/2");
      return fst_error;
    }
    if (i + 1 == max) {
      return fst_too_long;
    }
    /* we've got a character */
    out[i++] = ch;
    /* done */
    inp = TailOfTerm(inp);
  }
  out[i] = '\0';
  return fst_ok;
}

#define FORMAT_COPY_ARGS_ERROR -1
#define FORMAT_COPY_ARGS_OVERFLOW -2

static Int format_copy_args(Term args, Term *targs, Int tsz) {
  Int n = 0;
  while (args != TermNil) {
    if (IsVarTerm(args)) {
      Yap_Error(INSTANTIATION_ERROR, args, "format/2");
      return FORMAT_COPY_ARGS_ERROR;
    }
    if (!IsPairTerm(args)) {
      Yap_Error(TYPE_ERROR_LIST, args, "format/2");
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

format_clean_up(int sno, const char *fstr, const Term *targs) {
  if (fstr) {
    Yap_FreeAtomSpace((void *)fstr);
  }
  if (targs)
    Yap_FreeAtomSpace((void *)targs);
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
                    int sno USES_REGS) {
  char tmp1[TMP_STRING_SIZE], *tmpbase;
  int ch;
  Term mytargs[8], *targs;
  Int tnum, targ;
  const char *fstr = NULL, *fptr;
  Term args;
  Term tail;
  int (*f_putc)(int, wchar_t);
  int osno = 0;
  jmp_buf format_botch;
  volatile void *old_handler;
  volatile int old_pos;
  format_info finfo;
  unsigned char *ubuf = NULL;
  Term fmod = CurrentModule;
  size_t sz;

  finfo.padders = 0;
  finfo.format_error = FALSE;
  if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
    old_handler = GLOBAL_Stream[sno].u.mem_string.error_handler;
    GLOBAL_Stream[sno].u.mem_string.error_handler = (void *)&format_botch;
    old_pos = GLOBAL_Stream[sno].u.mem_string.pos;
    /* set up an error handler */
    if (setjmp(format_botch)) {
      restore_machine_regs();
      *HR++ = oargs;
      *HR++ = otail;
      if (!Yap_growheap(FALSE, LOCAL_Error_Size, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, otail, "format/2");
        return FALSE;
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
    Yap_Error(INSTANTIATION_ERROR, tail, "format/2");
    return (FALSE);
  } else if (IsPairTerm(tail)) {
    int sz = 256;
    do {
      format_cp_res fr;
      char *fstr0;

      fstr = fptr = fstr0 = Yap_AllocAtomSpace(sz * sizeof(char));
      if ((fr = copy_format_string(tail, fstr0, sz)) == fst_ok)
        break;
      if (fr == fst_error)
        return FALSE;
      sz += 256;
      Yap_FreeCodeSpace(fstr0);
    } while (TRUE);
  } else if (IsAtomTerm(tail)) {
    fstr = fptr = RepAtom(AtomOfTerm(tail))->StrOfAE;
  } else if (IsStringTerm(tail)) {
    fstr = fptr = StringOfTerm(tail);
  } else {
    Yap_Error(SYSTEM_ERROR_SAVED_STATE, tail, "format/2");
    return FALSE;
  }
  if (IsVarTerm(args)) {
    Yap_Error(INSTANTIATION_ERROR, args, "format/2");
    return FALSE;
  }
  while (IsApplTerm(args) && FunctorOfTerm(args) == FunctorModule) {
    fmod = ArgOfTerm(1, args);
    args = ArgOfTerm(2, args);
    if (IsVarTerm(fmod)) {
      Yap_Error(INSTANTIATION_ERROR, fmod, "format/2");
      return FALSE;
    }
    if (!IsAtomTerm(fmod)) {
      Yap_Error(TYPE_ERROR_ATOM, fmod, "format/2");
      return FALSE;
    }
    if (IsVarTerm(args)) {
      Yap_Error(INSTANTIATION_ERROR, args, "format/2");
      return FALSE;
    }
  }
  if (IsPairTerm(args)) {
    Int tsz = 8;

    targs = mytargs;
    do {
      tnum = format_copy_args(args, targs, tsz);
      if (tnum == FORMAT_COPY_ARGS_ERROR)
        return FALSE;
      else if (tnum == FORMAT_COPY_ARGS_OVERFLOW) {
        if (mytargs != targs) {
          Yap_FreeCodeSpace((char *)targs);
        }
        tsz += 16;
        targs = (Term *)Yap_AllocAtomSpace(tsz * sizeof(Term));
      } else {
        break;
      }
    } while (TRUE);
  } else if (args != TermNil) {
    tnum = 1;
    mytargs[0] = args;
    targs = mytargs;
  } else {
    tnum = 0;
    targs = mytargs;
  }
  finfo.format_error = false;

  f_putc = GLOBAL_Stream[sno].stream_wputc;
  while ((ch = *fptr++)) {
    Term t = TermNil;
    int has_repeats = FALSE;
    int repeats = 0;

    if (ch == '~') {
      /* start command */
      ch = *fptr++;
      if (ch == '*') {
        ch = *fptr++;
        has_repeats = TRUE;
        if (targ > tnum - 1) {
          goto do_consistency_error;
        }
        repeats = fetch_index_from_args(targs[targ++]);
        if (repeats == -1)
          goto do_consistency_error;
      } else if (ch == '`') {
        /* next character is kept as code */
        has_repeats = TRUE;
        repeats = *fptr++;
        ch = *fptr++;
      } else if (ch >= '0' && ch <= '9') {
        has_repeats = TRUE;
        repeats = 0;
        while (ch >= '0' && ch <= '9') {
          repeats = repeats * 10 + (ch - '0');
          ch = *fptr++;
        }
      }
      switch (ch) {
      case 'a':
        /* print an atom */
        if (has_repeats || targ > tnum - 1)
          goto do_consistency_error;
        t = targs[targ++];
        if (IsVarTerm(t))
          goto do_instantiation_error;
        if (!IsAtomTerm(t))
          goto do_type_atom_error;
        yhandle_t sl = Yap_StartSlots();
        // stream is already locked.
        Yap_plwrite(t, GLOBAL_Stream + sno, 0, Handle_vars_f | To_heap_f,
                    GLOBAL_MaxPriority);
        Yap_CloseSlots(sl);
        break;
      case 'c': {
        Int nch, i;

        if (targ > tnum - 1)
          goto do_consistency_error;
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

        if (targ > tnum - 1)
          goto do_consistency_error;
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
        tmp1[0] = '%';
        tmp1[1] = '.';
        ptr = tmp1 + 2;
#if HAVE_SNPRINTF
        snprintf(ptr, 256 - 5, "%d", repeats);
#else
        sprintf(ptr, "%d", repeats);
#endif
        while (*ptr)
          ptr++;
        ptr[0] = ch;
        ptr[1] = '\0';
        {
          char *tmp2;
          if (!(tmp2 = Yap_AllocCodeSpace(repeats + 10))) {
            goto do_type_int_error;
          }
#if HAVE_SNPRINTF
          snprintf(tmp2, repeats + 10, tmp1, fl);
#else
          sprintf(tmp2, tmp1, fl);
#endif
          ptr = tmp2;
          while ((ch = *ptr++) != 0)
            f_putc(sno, ch);
          Yap_FreeCodeSpace(tmp2);
        }
        break;
      case 'd':
      case 'D':
        /* print a decimal, using weird . stuff */
        if (targ > tnum - 1)
          goto do_consistency_error;
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
            goto do_consistency_error;
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
            goto do_consistency_error;
          t = targs[targ++];
          if (IsVarTerm(t))
            goto do_instantiation_error;
          if (!format_print_str(sno, repeats, has_repeats, t, f_putc)) {
            goto do_default_error;
          }
          break;
        case 'i':
          if (targ > tnum - 1 || has_repeats)
            goto do_consistency_error;
          targ++;
          break;
        case 'k':
          if (targ > tnum - 1 || has_repeats)
            goto do_consistency_error;
          t = targs[targ++];
          yhandle_t sl = Yap_StartSlots();
          Yap_plwrite(t, GLOBAL_Stream + sno, 0,
                      Quote_illegal_f | Ignore_ops_f | To_heap_f,
                      GLOBAL_MaxPriority);
          Yap_CloseSlots(sl);
          break;
        case '@':
          t = targs[targ++];
          {
            yhandle_t sl0 = Yap_StartSlots();
            Int sl = Yap_InitSlot(args);
            Int sl2;
            Int res;
            Term ta[2];
            Term ts;

            ta[0] = fmod;
            ta[1] = t;
            ta[0] = Yap_MkApplTerm(FunctorModule, 2, ta);
            ta[1] = MkVarTerm();
            sl2 = Yap_InitSlot(ta[1]);
            ts = Yap_MkApplTerm(FunctorGFormatAt, 2, ta);
            res = Yap_execute_goal(ts, 0, CurrentModule, true);
            args = Yap_GetFromSlot(sl);
            if (Yap_HasException())
              goto ex_handler;
            if (!res)
              return FALSE;
            ts = Yap_GetFromSlot(sl2);
            Yap_CloseSlots(sl0);
            if (!format_print_str(sno, repeats, has_repeats, ts, f_putc)) {
              goto do_default_error;
            }
          }
          break;
        case 'p':
          if (targ > tnum - 1 || has_repeats)
            goto do_consistency_error;
          t = targs[targ++];
          {
            Int sl = Yap_InitSlot(args);
            Yap_plwrite(t, GLOBAL_Stream + sno, 0,
                        Handle_vars_f | Use_portray_f | To_heap_f,
                        GLOBAL_MaxPriority);
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
            format_clean_up(sno, (char *)fstr, targs);
            Yap_RaiseException();
            return false;
          }
          break;
        case 'q':
          if (targ > tnum - 1 || has_repeats)
            goto do_consistency_error;
          t = targs[targ++];
          yhandle_t sl0 = Yap_StartSlots();
          Yap_plwrite(t, GLOBAL_Stream + sno, 0,
                      Handle_vars_f | Quote_illegal_f | To_heap_f,
                      GLOBAL_MaxPriority);
          Yap_CloseSlots(sl0);
          break;
        case 'w':
          if (targ > tnum - 1 || has_repeats)
            goto do_consistency_error;
          t = targs[targ++];
          yhandle_t slf = Yap_StartSlots();
          Yap_plwrite(t, GLOBAL_Stream + sno, 0, Handle_vars_f | To_heap_f,
                      GLOBAL_MaxPriority);
          Yap_CloseSlots(slf);
          break;
        case '~':
          if (has_repeats)
            goto do_consistency_error;
          f_putc(sno, (int)'~');
          break;
        case 'n':
          if (!has_repeats)
            repeats = 1;
          while (repeats--) {
            f_putc(sno, (int)'\n');
          }
          finfo.padders = 0;
          break;
        case 'N':
          if (!has_repeats)
            has_repeats = 1;
          if (GLOBAL_Stream[sno].linepos != 0) {
            f_putc(sno, '\n');
            finfo.padders = 0;
          }
          if (repeats > 1) {
            Int i;
            for (i = 1; i < repeats; i++)
              f_putc(sno, '\n');
            finfo.padders = 0;
          }
          break;
        /* padding */
        case '|':
          if (osno) {
            Yap_CloseStream(sno);
            sno = osno;
            osno = 0;
          }
          repeats -= GLOBAL_Stream[sno].linepos;
          repeats = (repeats < 0 ? 0 : repeats);
          fill_pads(sno, repeats, &finfo PASS_REGS);
          break;
        case '+':
          if (osno) {
            Yap_CloseStream(sno);
            sno = osno;
            osno = 0;
          }
          repeats = (repeats < 0 ? 0 : repeats);
          fill_pads(sno, repeats, &finfo PASS_REGS);
          break;
        case 't': {
          int nsno;

          finfo.pad_entries[finfo.padders].len = sz;
          finfo.pad_entries[finfo.padders].pad = (char *)ubuf;
          nsno = Yap_open_buf_write_stream(GLOBAL_Stream[sno].encoding, 0);
          if (osno) {
            GLOBAL_Stream[nsno].linepos = GLOBAL_Stream[sno].linepos;
            GLOBAL_Stream[nsno].linecount = GLOBAL_Stream[sno].linecount;
            GLOBAL_Stream[nsno].charcount = GLOBAL_Stream[sno].charcount;
            Yap_CloseStream(sno);
          } else {
            osno = sno;
            GLOBAL_Stream[nsno].linepos = GLOBAL_Stream[sno].linepos;
            GLOBAL_Stream[nsno].linecount = GLOBAL_Stream[sno].linecount;
            GLOBAL_Stream[nsno].charcount = GLOBAL_Stream[sno].charcount;
          }
          sno = nsno;
          finfo.pad_entries[finfo.padders].start = GLOBAL_Stream[sno].linepos;
          if (!has_repeats)
            finfo.pad_entries[finfo.padders].filler = ' ';
          else
            finfo.pad_entries[finfo.padders].filler = fptr[-2];
          finfo.padders++;
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
        do_consistency_error:
        default:
          LOCAL_Error_TYPE = SYSTEM_ERROR_SAVED_STATE;
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
            Yap_Error(LOCAL_Error_TYPE,
                      Yap_MkApplTerm(Yap_MkFunctor(AtomFormat, 2), 2, ta),
                      "format/2");
          }
          if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
            GLOBAL_Stream[sno].u.mem_string.error_handler = old_handler;
          }
          format_clean_up(sno, fstr, targs);
          LOCAL_Error_TYPE = YAP_NO_ERROR;
          return FALSE;
        }
      }
        /* ok, now we should have a command */
      }
    } else {
      f_putc(sno, ch);
    }
  }
  if (finfo.padders) {
    if (osno) {
      Yap_CloseStream(sno);
      sno = osno;
    }
    //    fill_pads( sno, 0, &finfo);
  }
  if (IsAtomTerm(tail) || IsStringTerm(tail)) {
    fstr = NULL;
  }
  if (tnum <= 8)
    targs = NULL;
  if (GLOBAL_Stream[sno].status & InMemory_Stream_f) {
    GLOBAL_Stream[sno].u.mem_string.error_handler = old_handler;
  }
  format_clean_up(sno, fstr, targs);
  return (TRUE);
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
  if (IsVarTerm(tin)) {
    Yap_Error(INSTANTIATION_ERROR, tin, "with_output_to/3");
    return false;
  }
  if (IsApplTerm(tin) && (f = FunctorOfTerm(tin)) &&
      (f == FunctorAtom || f == FunctorString || f == FunctorCodes1 ||
       f == FunctorCodes || f == FunctorChars1 || f == FunctorChars)) {
    output_stream = Yap_OpenBufWriteStream(PASS_REGS1);
  } else {
    /* needs to change LOCAL_c_output_stream for write */
    output_stream = Yap_CheckStream(ARG1, Output_Stream_f, "format/3");
  }
  if (output_stream == -1) {
    return false;
  }
  UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  out = Yap_Execute(Deref(ARG2) PASS_REGS);
  LOCK(GLOBAL_Stream[output_stream].streamlock);
  LOCAL_c_output_stream = old_out;
  if (mem_stream) {
    Term tat;
    Term inp = Deref(ARG1);
    if (out) {
      char *s = GLOBAL_Stream[output_stream].nbuf;
      encoding_t enc = GLOBAL_Stream[output_stream].encoding;
      s[GLOBAL_Stream[output_stream].nsize] = '\0';
      if (f == FunctorAtom) {
        tat = MkAtomTerm(Yap_LookupAtom(s));
      } else if (f == FunctorCodes) {
        tat = Yap_CharsToDiffListOfCodes(s, ArgOfTerm(2, inp), enc PASS_REGS);
      } else if (f == FunctorCodes1) {
        tat = Yap_CharsToListOfCodes(s, enc PASS_REGS);
      } else if (f == FunctorChars) {
        tat = Yap_CharsToDiffListOfAtoms(s, ArgOfTerm(2, inp), enc PASS_REGS);
      } else if (f == FunctorChars1) {
        tat = Yap_CharsToListOfAtoms(s, enc PASS_REGS);
      } else if (f == FunctorString1) {
        tat = Yap_CharsToString(s, enc PASS_REGS);
      } else {
        return false;
      }
      out = Yap_unify(tat, ArgOfTerm(1, inp));
    }
  }
  return out;
}

static Int format(Term tout, Term tf, Term tas USES_REGS) {
  bool mem_stream = false;
  int output_stream;
  Functor f;
  Int out;

  if (IsVarTerm(tout)) {
    Yap_Error(INSTANTIATION_ERROR, tout, "format/3");
    return false;
  }
  if (IsApplTerm(tout) && (f = FunctorOfTerm(tout)) &&
      (f == FunctorAtom || f == FunctorString1 || f == FunctorCodes1 ||
       f == FunctorCodes || f == FunctorChars1 || f == FunctorChars)) {
    output_stream = Yap_OpenBufWriteStream(PASS_REGS1);
    mem_stream = true;
  } else {
    /* needs to change LOCAL_c_output_stream for write */
    output_stream = Yap_CheckStream(tout, Output_Stream_f, "format/3");
  }
  if (output_stream == -1) {
    return false;
    UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  } else {
    yhandle_t sls = Yap_CurrentSlot();

    out = doformat(tf, tas, output_stream PASS_REGS);

    Yap_CloseSlots(sls);
    UNLOCK(GLOBAL_Stream[output_stream].streamlock);
  }
  if (mem_stream) {
    Term tat;
    Term inp = Deref(ARG1);
    if (out) {
      char *s = GLOBAL_Stream[output_stream].nbuf;
      encoding_t enc = GLOBAL_Stream[output_stream].encoding;
      s[GLOBAL_Stream[output_stream].nsize] = '\0';
      if (f == FunctorAtom) {
        tat = MkAtomTerm(Yap_LookupAtom(s));
      } else if (f == FunctorCodes) {
        tat = Yap_CharsToDiffListOfCodes(s, ArgOfTerm(2, inp), enc PASS_REGS);
      } else if (f == FunctorCodes1) {
        tat = Yap_CharsToListOfCodes(s, enc PASS_REGS);
      } else if (f == FunctorChars) {
        tat = Yap_CharsToDiffListOfAtoms(s, ArgOfTerm(2, inp), enc PASS_REGS);
      } else if (f == FunctorChars1) {
        tat = Yap_CharsToListOfAtoms(s, enc PASS_REGS);
      } else if (f == FunctorString1) {
        tat = Yap_CharsToString(s, enc PASS_REGS);
      } else {
        return false;
      }
      if (!Yap_unify(tat, ArgOfTerm(1, inp)))
        return FALSE;
    }
    Yap_CloseStream(output_stream);
  }
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
  res = format(Deref(ARG1), Deref(ARG2), Deref(ARG3) PASS_REGS);
  return res;
}

void Yap_InitFormat(void) {
  Yap_InitCPred("format", 2, format2, SyncPredFlag);
  Yap_InitCPred("format", 3, format3, SyncPredFlag);
  Yap_InitCPred("with_output_to", 2, with_output_to, SyncPredFlag);
}

/// @}
