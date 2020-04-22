#ifndef _YAP_INLINE_ONLY_H_
#define _YAP_INLINE_ONLY_H_

#ifdef __clang__
#define INLINE_ONLY __attribute__(always_inline )
#elif defined(__GNUC__)
//#define __GNUC_GNU_INLINE__ 1
#define INLINE_ONLY __attribute__((always_inline))
#else
#define INLINE_ONLY  EXTERN
#endif

#endif
