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

#if HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

typedef void *Functor;
typedef void *Atom;

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

#if HAVE_INTTYPES_H
#include <inttypes.h>
#define Int_FORMAT "%" PRIdPTR
#define UInt_ANYFORMAT "%" PRIuPTR
#define UInt_FORMAT "%" PRIuPTR

typedef intptr_t Int;
typedef uintptr_t UInt;

#elif SIZEOF_INT_P == 4

#if SIZEOF_LONGINT == 4

#define Int_FORMAT "%l"
#define UInt_FORMAT "%ul"

#elif SIZEOF_LONG_INT == 4
/*   typedef long int Int;*/
/*  typedef unsigned long int UInt;  */

#if _WIN32 || __ANDROID__
#define Int_FORMAT "%d"
#define UInt_FORMAT "%u"
#else
#define Int_FORMAT "%ld"
#define UInt_FORMAT "%lu"
#endif
#else
#error Yap require integer types of the same size as a pointer
#endif

#if SIZEOF_SHORT_INT == 2
/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

#else
#error Yap requires integer types half the size of a pointer
#endif

#elif SIZEOF_INT_P == 8

#if SIZEOF_INT == 8

#define Int_FORMAT "%d"
#define UInt_FORMAT "%u"

#elif SIZEOF_LONG_INT == 8

#define Int_FORMAT "%ld"
#define UInt_FORMAT "%lu"

#elif SIZEOF_LONG_LONG_INT == 8

#define Int_FORMAT "%I64d"
#define UInt_FORMAT "%I64u"

#else
#error Yap requires integer types of the same size as a pointer
#endif

#if SIZEOF_SHORT_INT == 4
/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

#elif SIZEOF_INT == 4
/*   */ typedef int Short;
/*   */ typedef unsigned int UShort;

#else
#error Yap requires integer types half the size of a pointer
#endif

#else

#error Yap requires pointers of size 4 or 8

#endif

typedef uintptr_t CELL;

typedef uint16_t BITS16;
typedef int16_t SBITS16;
typedef uint32_t BITS32;

#define WordSize sizeof(BITS16)
#define CellSize sizeof(CELL)
#define SmallSize sizeof(SMALLUNSGN)

/*************************************************************************************************
                                        type casting macros
*************************************************************************************************/

typedef uintptr_t Term;

typedef intptr_t yhandle_t;

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
