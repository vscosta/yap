/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Yap.h   						 *
* mods:									 *
* comments:	abstract type definitions for YAP			 *
* version:      $Id: Yap.h,v 1.38 2008-06-18 10:02:27 vsc Exp $	 *
*************************************************************************/

#ifndef YAP_H
#include "YapTermConfig.h"
#include "config.h"

#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

/* truth-values */
/* stdbool defines the booleam type, bool,
   and the constants false and true */
#if HAVE_STDBOOL_H
#include <stdbool.h>
#else
#ifndef true
typedef int _Bool;
#define bool _Bool;

#define false 0
#define true 1
#endif
#endif /* HAVE_STDBOOL_H */


#define ALIGN_BY_TYPE(X, TYPE)                                                 \
  (((CELL)(X) + (sizeof(TYPE) - 1)) & ~(sizeof(TYPE) - 1))

#ifndef EXTERN
#ifdef MSC_VER
#define EXTERN
#else
#define EXTERN extern
#endif
#endif

/* defines integer  types Int and UInt (unsigned) with the same size as a ptr
** and integer types Short and UShort with half the size of a ptr */

#if defined(PRIdPTR)

typedef intptr_t YAP_Int;
typedef uintptr_t YAP_UInt;

#elif defined(_WIN64)


typedef int64_t YAP_Int;
typedef uint64_t YAP_UInt;

#elif defined(_WIN32)

typedef int32_t YAP_Int;
typedef uint32_t YAP_UInt;

#elif SIZEOF_LONG_INT == SIZEOF_INT_P

typedef long int YAP_Int;
typedef unsigned long int YAP_UInt;

#elif SIZEOF_INT == SIZEOF_INT_P

typedef int YAP_Int;
typedef unsigned int YAP_UInt;

#else
#error Yap require integer types of the same size as a pointer
#endif

/*   */ typedef short int YAP_Short;
/*   */ typedef unsigned short int YAP_UShort;

typedef YAP_UInt YAP_CELL;
typedef YAP_UInt YAP_Term;

/* Type definitions */


#ifndef TRUE
#define TRUE true
#endif
#ifndef FALSE
#endif

typedef bool YAP_Bool;
#define FALSE false

typedef YAP_Int YAP_handle_t;


typedef double YAP_Float;

typedef void *YAP_Atom;

typedef void *YAP_Functor;

#ifdef YAP_H

typedef YAP_Int Int;
typedef YAP_UInt UInt;
typedef YAP_Short Short;
typedef YAP_UShort UShort;

typedef uint16_t BITS16;
typedef int16_t SBITS16;
typedef uint32_t BITS32;

typedef YAP_CELL CELL;

typedef YAP_Term Term;

#define WordSize sizeof(BITS16)
#define CellSize sizeof(CELL)
#define SmallSize sizeof(SMALLUNSGN)

typedef YAP_Int Int;
typedef YAP_Float Float;
typedef YAP_handle_t yhandle_t;

#endif

#include "YapError.h"

#include "../os/encoding.h"

typedef encoding_t YAP_encoding_t;

#include "YapFormat.h"

/*************************************************************************************************
                                        type casting macros
*************************************************************************************************/

#if SIZEOF_INT < SIZEOF_INT_P
#define SHORT_INTS 1
#else
#define SHORT_INTS 0
#endif

#ifdef __GNUC__
typedef long long int YAP_LONG_LONG;
typedef unsigned long long int YAP_ULONG_LONG;
#else
typedef long int YAP_LONG_LONG;
typedef unsigned long int YAP_ULONG_LONG;
#endif

#define Unsigned(V) ((CELL)(V))
#define Signed(V) ((Int)(V))

