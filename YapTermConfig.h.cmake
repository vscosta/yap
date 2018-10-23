
#ifndef YAP_TERM_CONFIG

#define YAP_TERM_CONFIG 1

/* Define sizes of some basic types	*/
#ifndef SIZEOF_INT_P
#cmakedefine SIZEOF_INT_P ${SIZEOF_INT_P}
#endif

/* The size of `int', as computed by sizeof. */
#ifndef SIZEOF_INT
#cmakedefine SIZEOF_INT ${SIZEOF_INT}
#endif

/* The size of `short int', as computed by sizeof. */
#ifndef SIZEOF_SHORT_INT
#define SIZEOF_SHORT_INT ${SIZEOF_SHORT_INT}
#endif

/* The size of `long int', as computed by sizeof. */
#ifndef SIZEOF_LONG_INT
#cmakedefine SIZEOF_LONG_INT ${SIZEOF_LONG_INT}
#endif

/* The size of `long long', as computed by sizeof. */
#ifndef SIZEOF_LONG_LONG
#cmakedefine SIZEOF_LONG_LONG ${SIZEOF_LONG_LONG}
#endif

/* The size of `float', as computed by sizeof. */
#ifndef SIZEOF_FLOAT
#cmakedefine SIZEOF_FLOAT ${SIZEOF_FLOAT}
#endif

/* The size of `float', as computed by sizeof. */
#ifndef SIZEOF_FLOAT
#cmakedefine SIZEOF_FLOAT ${SIZEOF_FLOAT}
#endif

/* Define to 1 if you have the <inttypes.h> header file. */
#ifndef HAVE_INTTYPES_H
#cmakedefine HAVE_INTTYPES_H ${HAVE_INTTYPES_H}
#endif

/* Define to 1 if you have the <stdbool.h> header file. */
#ifndef HAVE_STDBOOL_H
#cmakedefine HAVE_STDBOOL_H ${HAVE_STDBOOL_H}
#endif


/* Define to 1 if you have the <stdint.h> header file. */
#ifndef HAVE_STDINT_H
#cmakedefine HAVE_STDINT_H ${HAVE_STDINT_H}
#endif


#include "YapTerm.h"

#endif
