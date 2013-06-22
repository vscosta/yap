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

typedef void *Functor;
typedef void *Atom;

#endif

#ifndef EXTERN
#define EXTERN extern
#endif

/* defines integer  types Int and UInt (unsigned) with the same size as a ptr
** and integer types Short and UShort with half the size of a ptr */

#if SIZEOF_INT_P==4

#if SIZEOF_INT==4
/*   */ typedef int Int;
/*   */ typedef unsigned int UInt;

#define Int_FORMAT "%d"
#define UInt_FORMAT "%u"

#elif SIZEOF_LONG_INT==4
/*   */ typedef long int Int;
/*   */ typedef unsigned long int UInt;

#define Int_FORMAT "%ld"
#define UInt_FORMAT "%lu"

#else
#error Yap require integer types of the same size as a pointer
#endif

#if SIZEOF_SHORT_INT==2
/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

#else
#	error Yap requires integer types half the size of a pointer
#endif

#elif SIZEOF_INT_P==8

#if SIZEOF_INT==8
/*   */ typedef int Int;
/*   */ typedef unsigned int UInt;

#define Int_FORMAT "%d"
#define UInt_FORMAT "%u"

#elif SIZEOF_LONG_INT==8
/*   */ typedef long int Int;
/*   */ typedef unsigned long int UInt;

#define Int_FORMAT "%ld"
#define UInt_FORMAT "%lu"

#   elif SIZEOF_LONG_LONG_INT==8
/*   */ typedef long long int Int;
/*   */ typedef unsigned long long int UInt;

#define Int_FORMAT "%I64d"
#define UInt_FORMAT "%I64u"

#   else
#	error Yap requires integer types of the same size as a pointer
#   endif

#   if SIZEOF_SHORT_INT==4
/*   */ typedef short int Short;
/*   */ typedef unsigned short int UShort;

#   elif SIZEOF_INT==4
/*   */ typedef int Short;
/*   */ typedef unsigned int UShort;

#   else
#	error Yap requires integer types half the size of a pointer
#   endif

#else

#  error Yap requires pointers of size 4 or 8

#endif


typedef UInt CELL;
#if HAVE_STDINT_H
#include <stdint.h>

typedef uint16_t BITS16;
typedef int16_t SBITS16;
typedef uint32_t BITS32;
#else
typedef UShort BITS16;
typedef Short  SBITS16;
typedef UInt BITS32;
#endif

#define WordSize     sizeof(BITS16)
#define CellSize     sizeof(CELL)
#define SmallSize    sizeof(SMALLUNSGN)

/*************************************************************************************************
                                        type casting macros
*************************************************************************************************/


typedef CELL Term;

/*   */ typedef double Float;

#if SIZEOF_INT<SIZEOF_INT_P
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

#define	Unsigned(V)	((CELL) (V))
#define	Signed(V)	((Int) (V))

