#ifndef _YAP_INLINE_ONLY_H_
#define _YAP_INLINE_ONLY_H_

#ifdef __clang__
#define INLINE_ONLY __attribute__((gnu_inline,always_inline))
#else
#ifdef __GNUC__
#define INLINE_ONLY __attribute__((always_inline))
#else
#define INLINE_ONLY
#endif
#endif

#endif
