
#ifndef DLOCALS_H
#define DLOCALS_H

#undef LOCAL
#undef LOCAL_INIT
#undef LOCAL_INITF
#undef LOCAL_INIT_RESTORE
#undef LOCAL_ARRAY
#undef LOCAL_ARRAY_ARRAY

#define LOCAL(TYPE, NAME) TYPE NAME
#define LOCAL_INIT(TYPE, NAME, INIT) TYPE NAME
#define LOCAL_INITF(TYPE, NAME, INIT) TYPE NAME
#define LOCAL_INIT_RESTORE(TYPE, NAME, INIT, RESTORE) TYPE NAME
#define LOCAL_ARRAY(TYPE, NAME, DIM1) TYPE NAME [ DIM1 ]
#define LOCAL_ARRAY_ARRAY(TYPE, NAME, DIM1, DIM2) TYPE NAME [ DIM1 ][ DIM2 ]

    // Stuff that must be considered local to a thread or worker
    typedef struct worker_local {
#include "locals.h"
} w_local;

#endif
