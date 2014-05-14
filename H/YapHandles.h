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
   @group  term_t_slots

Also known as term handles, slots are offsets to entries in the local stack. YAP never compresses the local stack, so slots are respected by the garbage collector,
hence providing a way to access terms without being exposed to stack shifts or garbage-collection.

 Space is released when the function terminataes. Thus, slots will be automatically released at the end
of a function. Hence, slots should always be used as local variables.

Slots are organized as follows:
---- Offset of next pointer in chain (tagged as an Int)
---- Number of entries (tagged as Int), in the example TAG(INT,4)
Entry 
Entry 
Entry 
Entry 
---- Number of entries (tagged as Int), in the example TAG(INT,4)

Slots are not known to the yaam. Instead, A new set of slots is created when the emulator calls user C-code.
(see YAP_Execute* functions). They are also created:

   - by SWI's PL_foreign_frame() function,

   - by the YAP_RunGoal() routines and friends, when they exit successfully. Notice that all handles created by c-goals  within
     a `Goal` execution should not be used afterwards.

This section lists the main internal functions for slot management. These functions are then exported through corresponding FLI C-functions

*************************************************************************************************/

/// @brief start a new set of slots, linking them to the last active slots (who may, or not, be active).
static inline Int
Yap_StartSlots( USES_REGS1 ) {
  Int CurSlot = LOCAL_CurSlot;
  // if (CurSlot == LCL0-(ASP+(IntOfTerm(ASP[0])+2)))
  //  return CurSlot;
  /* new slot */
  *--ASP = MkIntegerTerm(CurSlot);
  LOCAL_CurSlot =  LCL0-ASP;
  *--ASP = MkIntTerm(0);
  *--ASP = MkIntTerm(0);
  return CurSlot;
}


/// @brief reset slots to a well-known position in the stack
static inline void
Yap_CloseSlots( Int slot USES_REGS ) {
  LOCAL_CurSlot = slot;
}

/// @brief report the current position of the slots, assuming that they occupy the top of the stack.
static inline Int
Yap_CurrentSlot( USES_REGS1 ) {
  return IntOfTerm(ASP[0]);
}

/// @brief read from a slot.
static inline Term
Yap_GetFromSlot(Int slot USES_REGS)
{
  return(Deref(LCL0[slot]));
}

/// @brief read from a slot. but does not try to dereference the slot.
static inline Term
Yap_GetDerefedFromSlot(Int slot USES_REGS)
{
  return LCL0[slot];
}

/// @brief read the object in a slot. but do not try to dereference the slot.
static inline Term
Yap_GetPtrFromSlot(Int slot USES_REGS)
{
  return(LCL0[slot]);
}

/// @brief get the memory address of a slot
static inline Term *
Yap_AddressFromSlot(Int slot USES_REGS)
{
  return(LCL0+slot);
}

/// @brief store  term in a slot
static inline void
Yap_PutInSlot(Int slot, Term t USES_REGS)
{
  LCL0[slot] = t;
}

/// @brief allocate n empty new slots
static inline Int
Yap_NewSlots(int n USES_REGS)
{
  Int old_slots = IntOfTerm(ASP[0]), oldn = n;
  while (n > 0) {
    RESET_VARIABLE(ASP);
    ASP--;
    n--;
  }
  ASP[old_slots+oldn+1] = ASP[0] = MkIntTerm(old_slots+oldn);
  return((ASP+1)-LCL0);
}

/// @brief create a new slot with term t
static inline Int
Yap_InitSlot(Term t USES_REGS)
{
  Int old_slots = IntOfTerm(ASP[0]);
  *ASP = t;
  ASP--;
  ASP[old_slots+2] = ASP[0] = MkIntTerm(old_slots+1);
  return((ASP+1)-LCL0);
}

/// @brief Succeeds if it is to recover the space allocated for $n$ contiguos slots starting at topSlot.
static inline int
Yap_RecoverSlots(int n, Int topSlot USES_REGS)
{
  Int old_slots = IntOfTerm(ASP[0]);
  if (old_slots  < n) {
    return FALSE;
  }
  if (ASP+1 != LCL0+topSlot)
	return FALSE;
  ASP += n;
  ASP[old_slots+(n-old_slots+1)] = ASP[0] = MkIntTerm(old_slots-n);
  return TRUE;
}

#endif
