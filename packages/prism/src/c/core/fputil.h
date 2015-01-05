#ifndef FPUTIL_H
#define FPUTIL_H

/*--------------------------------------------------------------------*/

#include <math.h>

#ifdef __STDC_VERSION__
#if __STDC_VERSION__ >= 199901L
#define C99
#endif
#endif

/*--------------------------------------------------------------------*/

#if   defined C99
/* (empty) */
#elif defined _MSC_VER
#include <float.h>
#define isfinite    _finite
#define isnan       _isnan
#define INFINITY    HUGE_VAL
#elif defined LINUX
#  ifndef isfinite
#    define isfinite    finite
#  endif
#  ifndef isnan
#    define isnan       isnan
#  endif
#  ifndef INFINITY
#    define INFINITY    HUGE_VAL
#  endif
#elif defined DARWIN
/* (empty) */
#else
#ifndef isfinite
#define isfinite(x) (0.0 * (x) != 0.0)
#endif
#ifndef isnan
#define isnan(x)    ((x) != (x))
#endif
#ifndef INFINITY
#define INFINITY    HUGE_VAL
#endif
#endif

#define SNAN fputil_snan()
#define QNAN fputil_qnan()

/*--------------------------------------------------------------------*/

double  fputil_snan(void);
double  fputil_qnan(void);

/*--------------------------------------------------------------------*/

#endif /* FPUTIL_H */
