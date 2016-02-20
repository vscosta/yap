
#ifndef _FIELDS_H_
#define _FIELDS_H_ 1

#undef HM
#undef HSPACE
#undef HSPACEN
#undef HI
#undef H_R
#undef HLOCK
#undef HRWLOCK
#undef HMOPCODE
#undef HPROC
#undef HATOMT
#undef HAROP
#undef HFOP
#undef HYOP
#undef HENVYOP
#undef HCPYOP
#define HM(TYPE, NAME, INIT, RECOVER) DEF NAME Yap_heap_regs->NAME##_
#define HSPACE(TYPE, NAME) DEF NAME Yap_heap_regs->NAME##_
#define HSPACEN(TYPE, N, NAME) DEF NAME Yap_heap_regs->NAME##_
#define HI(TYPE, NAME, INIT) DEF NAME Yap_heap_regs->NAME##_
#define H_R(TYPE, NAME, INIT) DEF NAME Yap_heap_regs->NAME##_
#define HLOCK(TYPE, NAME) DEF NAME Yap_heap_regs->NAME##_
#define HRWLOCK(TYPE, NAME) DEF NAME Yap_heap_regs->NAME##_
#define HMOPCODE(NAME, OP) DEF NAME Yap_heap_regs->NAME##_
#define HPROC(TYPE, NAME, INIT, RECOVER) DEF NAME Yap_heap_regs->NAME##_
#define HPROCN(TYPE, N, NAME, INIT, RECOVER) DEF NAME Yap_heap_regs->NAME##_
#define HATOMT(NAME, ATOM) DEF NAME Yap_heap_regs->NAME##_
#define HAROP(NAME, ATOM, ARITY, MODULE) DEF NAME Yap_heap_regs->NAME##_
#define HFOP(NAME, FUNC, MODULE) DEF NAME Yap_heap_regs->NAME##_
#define HYOP(N, NAME, FAILOP) DEF NAME Yap_heap_regs->NAME##_
#define HENVYOP(N, NAME, op, NEXTNAME, PFAIL) DEF NAME Yap_heap_regs->NAME##_
#define HCPYOP(N, NAME, OP, FAILP) DEF NAME Yap_heap_regs->NAME##_

#include "heap.h"

#undef HM
#undef HSPACE
#undef HSPACEN
#undef HI
#undef H_R
#undef HLOCK
#undef HRWLOCK
#undef HMOPCODE
#undef HPROC
#undef HATOMT
#undef HAROP
#undef HFOP
#undef HYOP
#undef HENVYOP
#undef HCPYOP
#define HM(TYPE, NAME, INIT, RECOVER) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HSPACE(TYPE, NAME) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HSPACEN(TYPE, N, NAME) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HI(TYPE, NAME, INIT) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define H_R(TYPE, NAME, INIT) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HLOCK(TYPE, NAME) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HRWLOCK(TYPE, NAME) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HMOPCODE(NAME, OP) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HPROC(TYPE, NAME, INIT, RECOVER)                                       \
  DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HPROCN(TYPE, N, NAME, INIT, RECOVER)                                   \
  DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HATOMT(NAME, ATOM) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HAROP(NAME, ATOM, ARITY, MODULE)                                       \
  DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HFOP(NAME, FUNC, MODULE) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HYOP(N, NAME, FAILOP) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HENVYOP(N, NAME, op, NEXTNAME, PFAIL)                                  \
  DEF GLOBAL_##NAME Yap_heap_regs->NAME##_
#define HCPYOP(N, NAME, OP, FAILP) DEF GLOBAL_##NAME Yap_heap_regs->NAME##_

#include "heap.h"

#undef LOC
#undef LOCL
#undef LOCN
#undef LOCLR
#define LOC(TYPE, NAME) DEF LOCAL_##NAME LOCAL->NAME
#define LOCL(TYPE, NAME, INIT) DEF LOCAL_##NAME LOCAL->NAME
#define LOCN(TYPE, N, NAME) DEF LOCAL_##NAME LOCAL->NAME
#define LOCLR(TYPE, NAME, INIT, RESTORE) DEF LOCAL_##NAME LOCAL->NAME

#include "locals.h"

#undef LOC
#undef LOCL
#undef LOCLR
#undef LOCN
#define LOC(TYPE, NAME) DEF REMOTE_##NAME(wid) REMOTE(wid)->NAME
#define LOCL(TYPE, NAME, INIT) DEF REMOTE_##NAME(wid) REMOTE(wid)->NAME
#define LOCN(TYPE, N, NAME) DEF REMOTE_##NAME(wid) REMOTE(wid)->NAME
#define LOCLR(TYPE, NAME, INIT, RESTORE)                                       \
  DEF REMOTE_##NAME(wid) REMOTE(wid)->NAME

#include "locals.h"

#endif
