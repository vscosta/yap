/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapHandles.h   						 *
* mods:									 *
* comments:	term handles for YAP: basic ops				 *
* version:      $Id: Yap.h,v 1.38 2008-06-18 10:02:27 vsc Exp $	 *
*************************************************************************/

#ifndef YAP_HANDLES_H
#define YAP_HANDLES_H 1
#include "Regs.h"

/**
   @groupdef  term_t_slots

Also known as term handles, slots are offsets to entries in the local stack. YAP
never compresses the local stack, so slots are respected by the garbage
collector,
hence providing a way to access terms without being exposed to stack shifts or
garbage-collection.

 Space is released when the function terminates. Thus, slots will be
automatically released at the end
of a function. Hence, slots should always be used as local variables.

Slots are organized as follows:
---- Offset of next pointer in chain (tagged as an handle_t)
---- Number of entries (tagged as handle_t), in the example TAG(INT,4)
Entry
Entry
Entry
Entry
---- Number of entries (tagged as handle_t), in the example TAG(INT,4)

Slots are not known to the yaam. Instead, A new set of slots is created when the
emulator calls user C-code.
(see YAP_Execute* functions). They are also created:

   - by SWI's PL_foreign_frame() function,

   - by the YAP_RunGoal() routines and friends, when they exit successfully.
Notice that all handles created by c-goals  within
     a `Goal` execution should not be used afterwards.

     This section lists the main internal functions for slot management. These
functions are then exported through corresponding FLI C-functions

*************************************************************************************************/

#include <stdio.h>

/// @brief reboot the slot system.
/// Used when wwe start from scratch (Reset).
#define Yap_RebootSlots(wid) Yap_RebootSlots__(wid PASS_REGS)

static inline void Yap_RebootSlots__(int wid USES_REGS) {
  //  // fprintf(stderr,  " StartSlots = %ld", LOCAL_CurSlot);
  REMOTE_CurSlot(wid) = 1;
}

/// @brief declares a new set of slots.
/// Used to tell how many slots we had when we entered a segment of code.
#define Yap_StartSlots() Yap_StartSlots__(PASS_REGS1)

static inline yhandle_t Yap_StartSlots__(USES_REGS1) {
  //  // fprintf(stderr,  " StartSlots = %ld", LOCAL_CurSlot);
  // fprintf(stderr,"SS %s:%d\n", __FUNCTION__, __LINE__);;
  if (LOCAL_CurSlot < 0) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, 0L, " StartSlots = %ld", LOCAL_CurSlot);
  }
  return LOCAL_CurSlot;
}

/// @brief reset slots to a well-known position in the stack
#define Yap_CloseSlots(slot) Yap_CloseSlots__(slot PASS_REGS)

static inline void Yap_CloseSlots__(yhandle_t slot USES_REGS) {
  // fprintf(stderr,"CS %s:%d\n", __FUNCTION__, __LINE__);;
  LOCAL_CurSlot = slot;
}

/// @brief report the current position of the slots, assuming that they occupy
/// the top of the stack.
static inline yhandle_t Yap_CurrentSlot(USES_REGS1) { return LOCAL_CurSlot; }

#define Yap_GetFromSlot(slot) Yap_GetFromSlot__(slot PASS_REGS)
/// @brief read from a slot.
static inline Term Yap_GetFromSlot__(yhandle_t slot USES_REGS) {
  // fprintf(stderr,"GS %s:%d\n", __FUNCTION__, __LINE__);;
  return (Deref(LOCAL_SlotBase[slot]));
}

/// @brief read from a slot. but does not try to dereference the slot.
static inline Term Yap_GetDerefedFromSlot(yhandle_t slot USES_REGS) {
  // fprintf(stderr,"GDS %s:%d\n", __FUNCTION__, __LINE__);
  return LOCAL_SlotBase[slot];
}

/// @brief read the object in a slot. but do not try to dereference the slot.
static inline Term Yap_GetPtrFromSlot(yhandle_t slot USES_REGS) {
  // fprintf(stderr,"GPS %s:%d\n", __FUNCTION__, __LINE__);
  return LOCAL_SlotBase[slot];
}

#define Yap_AddressFromSlot(slot) Yap_AddressFromSlot__(slot PASS_REGS)

/// @brief get the memory address of a slot
static inline Term *Yap_AddressFromSlot__(yhandle_t slot USES_REGS) {
  return LOCAL_SlotBase + slot;
}

/// @brief store  term in a slot
static inline void Yap_PutInSlot(yhandle_t slot, Term t USES_REGS) {
  // fprintf(stderr,"PS %s:%d\n", __FUNCTION__, __LINE__);
  LOCAL_SlotBase[slot] = t;
}

#ifndef max
#define max(X, Y) (X > Y ? X : Y)
#endif

static inline void ensure_slots(int N USES_REGS) {
  if (LOCAL_CurSlot + N >= LOCAL_NSlots) {
    size_t inc = max(16 * 1024, LOCAL_NSlots / 2); // measured in cells
    inc = max(inc, N + 16);                        // measured in cells
    LOCAL_SlotBase =
        (CELL *)realloc(LOCAL_SlotBase, (inc + LOCAL_NSlots) * sizeof(CELL));
    LOCAL_NSlots += inc;
    if (!LOCAL_SlotBase) {
      unsigned long int kneeds = ((inc + LOCAL_NSlots) * sizeof(CELL)) / 1024;
      Yap_Error(
          SYSTEM_ERROR_INTERNAL, 0 /* TermNil */,
          "Out of memory for the term handles (term_t) aka slots, l needed",
          kneeds);
    }
  }
}

#define Yap_InitSlot(t) Yap_InitSlot__(t PASS_REGS)

/// @brief create a new slot with term t
static inline yhandle_t Yap_InitSlot__(Term t USES_REGS) {
  yhandle_t old_slots = LOCAL_CurSlot;
  // fprintf(stderr,"IS %s:%d\n", __FUNCTION__, __LINE__);

  ensure_slots(1 PASS_REGS);
  LOCAL_SlotBase[old_slots] = t;
  LOCAL_CurSlot++;
  return old_slots;
}

#define Yap_NewSlots(n) Yap_NewSlots__(n PASS_REGS)

/// @brief allocate n empty new slots
static inline yhandle_t Yap_NewSlots__(int n USES_REGS) {
  yhandle_t old_slots = LOCAL_CurSlot;
  int i;
  // fprintf(stderr,"NS %s:%d\n", __FUNCTION__, __LINE__);

  ensure_slots(n PASS_REGS);
  for (i = 0; i < n; i++) {
    LOCAL_SlotBase[old_slots + i] = MkVarTerm();
  }
  LOCAL_CurSlot += n;
  return old_slots;
}

#define Yap_InitSlots(n, ts) Yap_InitSlots__(n, ts PASS_REGS)

/// @brief create n new slots with terms ts[]
static inline yhandle_t Yap_InitSlots__(int n, Term *ts USES_REGS) {
  yhandle_t old_slots = LOCAL_CurSlot;
  int i;
  // fprintf(stderr,"1S %s:%d\n", __FUNCTION__, __LINE__);

  ensure_slots(n PASS_REGS);
  for (i = 0; i < n; i++)
    LOCAL_SlotBase[old_slots + i] = ts[i];
  LOCAL_CurSlot += n;
  return old_slots;
}

/// @brief Succeeds if it is to recover the space allocated for $n$ contiguos
/// slots starting at topSlot.
static inline bool Yap_RecoverSlots(int n, yhandle_t topSlot USES_REGS) {
  if (topSlot + n < LOCAL_CurSlot)
    return false;
// fprintf(stderr,"RS %s:%d\n", __FUNCTION__, __LINE__);
#ifdef DEBUG
  if (topSlot + n > LOCAL_CurSlot) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, 0 ,  "Inconsistent slot state in Yap_RecoverSlots.", 0);
    return false;
  }
#endif
  return true;
}

#endif
