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

typedef void *Functor;
typedef void *Atom;

#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#define ALIGN_BY_TYPE(X, TYPE)                                                 \
  (((CELL)(X) + (sizeof(TYPE) - 1)) & ~(sizeof(TYPE) - 1))

#ifndef EXTERN
#ifdef _MSC_VER
#define EXTERN
#else
#define EXTERN extern
#endif
#endif

/* defines integer  types Int and UInt (unsigned) with the same size as a ptr
** and integer types Short and UShort with half the size of a ptr */

#if defined(PRIdPTR)
#define Int_FORMAT "%" PRIdPTR
#define Int_ANYFORMAT "%" PRIuPTR
#define UInt_FORMAT "%" PRIuPTR
#define Int_F PRIdPTR
#define Int_ANYF PRIuPTR
#define UInt_F PRIuPTR

typedef intptr_t Int;
typedef uintptr_t UInt;

#elif defined(_WIN64)

typedef int64_t Int;
typedef uint64_t UInt;
#define Int_FORMAT "%I64d"
#define UInt_FORMAT "%I64u"
#define Int_F "I64d"
#define UInt_F "I64u"

#elif defined(_WIN32)

typedef int32_t Int;
typedef uint32_t UInt;
#define Int_FORMAT "%I32d"
#define UInt_FORMAT "%I32u"
#define Int_F "I32d"
#define UInt_F "I32u"

#elif SIZEOF_LONG_INT == SIZEOF_INT_P

typedef long int Int;
typedef unsigned long int UInt;
#define Int_FORMAT "%ld"
#define UInt_FORMAT "%uld"
#define Int_F "ld"
#define UInt_F "uld"

#elif SIZEOF_INT == SIZEOF_INT_P

typedef int Int;
typedef unsigned int UInt;
#define Int_FORMAT "%l"
#define UInt_FORMAT "%ul"
#define Int_F "l"
#define UInt_F "ul"

#else
#error Yap require integer types of the same size as a pointer
#endif

/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

typedef UInt CELL;

typedef uint16_t BITS16;
typedef int16_t SBITS16;
typedef uint32_t BITS32;

#define WordSize sizeof(BITS16)
#define CellSize sizeof(CELL)
#define SmallSize sizeof(SMALLUNSGN)

/*************************************************************************************************
                                        type casting macros
*************************************************************************************************/

typedef UInt Term;

typedef Int yhandle_t;

typedef double Float;

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
