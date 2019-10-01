
#ifndef PL_TYPES_H

#define PL_TYPES_H

                 /*******************************
                 *             TYPES            *
                 *******************************/


#ifdef __WINDOWS__
#ifndef INT64_T_DEFINED
#define INT64_T_DEFINED 1
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#if (_MSC_VER < 1300) && !defined(__MINGW32__)
typedef long intptr_t;
typedef unsigned long uintptr_t;
#endif
#endif
#else
#include <inttypes.h>			/* more portable than stdint.h */
#endif


#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
#include "Yap.h"
typedef	yhandle_t    term_t;
#endif
typedef	struct mod_entry *module_t;
typedef struct DB_STRUCT *record_t;
typedef uintptr_t	atom_t;
typedef	struct pred_entry    *predicate_t;
typedef uintptr_t qid_t;
typedef uintptr_t    functor_t;
typedef int     (*PL_agc_hook_t)(atom_t);
typedef uintptr_t	foreign_t;	/* return type of foreign functions */
typedef wchar_t pl_wchar_t;             /* wide character support */
#include <inttypes.h>			/* more portable than stdint.h */
typedef uintptr_t	PL_fid_t;	/* opaque foreign context handle */
typedef int  (*PL_dispatch_hook_t)(int fd);
typedef void *pl_function_t;

#define fid_t PL_fid_t			/* avoid AIX name-clash */

#endif /* PL_TYPES */
