
#ifndef ILOCALS_H
#define ILOCALS_H

#undef LOCAL
#undef LOCAL_INIT
#undef LOCAL_INITF
#undef LOCAL_INIT_RESTORE
#undef LOCAL_ARRAY
#undef LOCAL_ARRAY_ARRAY

#define LOCAL(TYPE, NAME)
#define LOCAL_INIT(TYPE, NAME, INIT) REMOTE_##NAME(wid) = INIT
#define LOCAL_INITF(TYPE, NAME, INIT) INIT
#define LOCAL_INIT_RESTORE(TYPE, NAME, INIT, RESTORE) REMOTE_##NAME(wid) = INIT
#define LOCAL_ARRAY(TYPE, NAME, INIT)
#define LOCAL_ARRAY_ARRAY(TYPE, NAME, DIM1, DIM2)

static void InitWorker(int wid){
#include "locals.h"
} ;

#endif
