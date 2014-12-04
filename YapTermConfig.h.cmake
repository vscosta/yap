
#ifndef YAP_TERM_CONFIG

#define YAP_TERM_CONFIG 1

/* Define sizes of some basic types	*/
#undef  SIZEOF_INT_P
#undef  SIZEOF_INT
#undef  SIZEOF_SHORT_INT
#undef  SIZEOF_LONG_INT
#undef  SIZEOF_LONG_LONG_INT
#undef  SIZEOF_FLOAT
#undef  SIZEOF_DOUBLE

#ifndef HAVE_INTTYPES_H
#undef  HAVE_INTTYPES_H
#endif

#ifndef HAVE_STDINT_H
#undef  HAVE_STDINT_H
#endif

#endif
