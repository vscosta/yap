/*
 * cheaders.h : A unified headers file for APT.  It does slow down compile
 *              time somewhat, but this allows us to ensure consistent usage
 *              of header files and replace missing ones easily.
 */

#ifndef APT_CHEADERS_H
#define APT_CHEADERS_H

#include "apt_config.h"

#ifdef HAVE_CTYPE_H
  #include <ctype.h>
#else
  #error Your C compiler is very old (ctype.h missing). Time to upgrade. Sorry
#endif

#ifdef HAVE_STRING_H
  #include <string.h>
#else
  #error Your C compiler is very old (string.h missing). Time to upgrade. Sorry
#endif

#ifdef HAVE_STDIO_H
  #include <stdio.h>
#else
  #error Your C compiler is very old (stdio.h missing). Time to upgrade. Sorry
#endif

#ifdef HAVE_STDLIB_H
  #include <stdlib.h>
#else
  #error Your C compiler is not ANSI C (stdlib.h missing). Time to upgrade. Sorry
#endif

#if HAVE_STDARG_H
#  include <stdarg.h>
#  define VA_START(a, f) va_start(a, f)
#else
#  if HAVE_VARARGS_H
#     include <varargs.h>
#     define VA_START(a, f) va_start(a)
#  endif
#endif

#ifndef VA_START
  #error Your C compiler has no support for variable argument functions. Time to upgrade. Sorry.
#endif

#endif
