#ifndef _YAP_INLINE_ONLY_H_
#define _YAP_INLINE_ONLY_H_

#define __GNUC_GNU_INLINE__ 1
#ifdef __clang__
#define INLINE_ONLY __attribute__(always_inline )
#ifdef __0GNUC__
#define INLINE_ONLY __attribute__((gnu_inline,always_inline))
//#define INLINE_ONLY
#else
#define INLINE_ONLY  EXTERN
#endif
#endif
#endif