
#ifndef HLOCALS_H
#define HLOCALS_H

#undef LOCAL
#undef LOCAL_INIT
#undef LOCAL_INITF
#undef LOCAL_INIT_RESTORE
#undef LOCAL_ARRAY
#undef LOCAL_ARRAY_ARRAY

#define LOCAL(TYPE, NAME)
#define LOCAL_INIT(TYPE, NAME, INIT)
#define LOCAL_INITF(TYPE, NAME, INIT)
#define LOCAL_INIT_RESTORE(TYPE, NAME, INIT, RESTORE)  REMOTE_##NAME(wid) = RESTORE( REMOTE_##NAME(wid) )
#define LOCAL_ARRAY(TYPE, NAME, INIT)
#define LOCAL_ARRAY_ARRAY(TYPE, NAME, DIM1, DIM2)

static void RestoreWorker(int wid USES_REGS) {
#include "locals.h"
}

#endif
