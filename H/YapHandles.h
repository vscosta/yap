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

Also known as term handles, slots are offsets to entries in the local stack. YAP never compresses the local stack, so slots are respected by the garbage collector,
hence providing a way to access terms without being exposed to stack shifts or garbage-collection.

 Space is released when the function terminates. Thus, slots will be automatically released at the end
of a function. Hence, slots should always be used as local variables.

Slots are organized as follows:
---- Offset of next pointer in chain (tagged as an handle_t)
---- Number of entries (tagged as handle_t), in the example TAG(INT,4)
Entry 
Entry 
Entry 
Entry 
---- Number of entries (tagged as handle_t), in the example TAG(INT,4)

Slots are not known to the yaam. Instead, A new set of slots is created when the emulator calls user C-code.
(see YAP_Execute* functions). They are also created:

   - by SWI's PL_foreign_frame() function,

   - by the YAP_RunGoal() routines and friends, when they exit successfully. Notice that all handles created by c-goals  within
     a `Goal` execution should not be used afterwards.

     This section lists the main internal functions for slot management. These functions are then exported through corresponding FLI C-functions

*************************************************************************************************/

/// @brief declares a new set of slots.
/// Used to tell how many slots we had when we entered a segment of code.
#define Yap_StartSlots() Yap_StartSlots__( PASS_REGS1 )

static inline yhandle_t
Yap_StartSlots__( USES_REGS1 ) {
  //  fprintf( stderr, " StartSlots = %ld", LOCAL_CurSlot);    
if (LOCAL_CurSlot < 0) {
    Yap_Error( SYSTEM_ERROR, 0L, " StartSlots = %ld", LOCAL_CurSlot);
  }
  return LOCAL_CurSlot;
}


/// @brief reset slots to a well-known position in the stack
#define Yap_CloseSlots( slot ) Yap_CloseSlots__( slot PASS_REGS )

static inline void
Yap_CloseSlots__( yhandle_t slot USES_REGS ) {
  LOCAL_CurSlot = slot;
}

/// @brief report the current position of the slots, assuming that they occupy the top of the stack.
static inline yhandle_t
Yap_CurrentSlot( USES_REGS1 ) {
  return LOCAL_CurSlot;
}

/// @brief read from a slot.
static inline Term
Yap_GetFromSlot(yhandle_t slot USES_REGS)
{
  return(Deref(LOCAL_SlotBase[slot]));
}

/// @brief read from a slot. but does not try to dereference the slot.
static inline Term
Yap_GetDerefedFromSlot(yhandle_t slot USES_REGS)
{
  return LOCAL_SlotBase[slot];
}

/// @brief read the object in a slot. but do not try to dereference the slot.
static inline Term
Yap_GetPtrFromSlot(yhandle_t slot USES_REGS)
{
  return LOCAL_SlotBase[slot];
}

#define Yap_AddressFromSlot( slot ) Yap_AddressFromSlot__(slot PASS_REGS)


/// @brief get the memory address of a slot
static inline Term *
Yap_AddressFromSlot__(yhandle_t slot USES_REGS)
{
  return LOCAL_SlotBase+slot;
}

/// @brief store  term in a slot
static inline void
Yap_PutInSlot(yhandle_t slot, Term t USES_REGS)
{
  LOCAL_SlotBase[slot] = t;
}

#define max(X,Y) ( X > Y ? X : Y )

static inline void
ensure_slots(int N USES_REGS)
{
  if (LOCAL_CurSlot+N >= LOCAL_NSlots) {
    size_t inc = max(16*1024, LOCAL_NSlots/2); // measured in cells
    LOCAL_SlotBase = (CELL *)realloc( LOCAL_SlotBase, (inc + LOCAL_NSlots )*sizeof(CELL));
    if (!LOCAL_SlotBase) {
      unsigned long int kneeds =  ((inc + LOCAL_NSlots )*sizeof(CELL))/1024;
      Yap_Error(SYSTEM_ERROR, 0 /* TermNil */, "Out of memory for the term handles (term_t) aka slots, l needed", kneeds);
    }
  }
}

/// @brief create a new slot with term t
static inline Int
Yap_InitSlot(Term t USES_REGS)
{
  yhandle_t old_slots = LOCAL_CurSlot;
  
  ensure_slots( 1 PASS_REGS);
  LOCAL_SlotBase[old_slots] = t;
  LOCAL_CurSlot++;
  return old_slots;
}

/// @brief allocate n empty new slots
static inline yhandle_t
Yap_NewSlots(int n USES_REGS)
{
  yhandle_t old_slots = LOCAL_CurSlot;
  int i;

  ensure_slots(n PASS_REGS);
  for (i = 0; i< n; i++) {
    RESET_VARIABLE(Yap_AddressFromSlot(old_slots+i) );
  }
  LOCAL_CurSlot += n;
  return old_slots;
}

#define  Yap_InitSlots(n, ts) Yap_InitSlots__(n, ts PASS_REGS) 

/// @brief create n new slots with terms ts[]
  static inline yhandle_t
Yap_InitSlots__(int n, Term *ts USES_REGS)
{
  yhandle_t old_slots = LOCAL_CurSlot;
  int i;
  
  ensure_slots( n PASS_REGS);
  for (i=0; i< n; i++)
    LOCAL_SlotBase[old_slots+i] = ts[i];
  LOCAL_CurSlot += n;
  return old_slots;
}

/// @brief Succeeds if it is to recover the space allocated for $n$ contiguos slots starting at topSlot.
static inline bool
Yap_RecoverSlots(int n, yhandle_t topSlot USES_REGS)
{
  if (topSlot + n < LOCAL_CurSlot)
    return false;
  #ifdef DEBUG
  if (topSlot + n > LOCAL_CurSlot) {
    Yap_Error(SYSTEM_ERROR, 0 /* TermNil */, "Inconsistent slot state in Yap_RecoverSlots."); 
    return false;
  }
  #endif
  return true;
}

#endif
