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


/*************************************************************************************************
                                           slots

Also known as term handles, they provide a way to access terms without being exposed to stack shifts in gc.

They should always be used as local variables.

They are organized as follows:
---- Tagged Offset of next pointer in chain 
---- Tagged Number of entries
Entry 
Entry 
Entry 
Entry 
---- Tagged Number of entries

They are not known to the yaam. Instead, 
they are created when entering user code (see YAP_Execute* functions). They are also created:

- by SWI PL_foreign_frame function,
- by YAP_*Goal routines, when they exit successfully. Notice that all handles created by c-goals  within
a *Goal execution should not be used afterwards.



*************************************************************************************************/


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

static inline Int
Yap_CurrentSlot( USES_REGS1 ) {
  return IntOfTerm(ASP[0]);
}

static inline Term
Yap_GetFromSlot(Int slot USES_REGS)
{
  return(Deref(LCL0[slot]));
}

static inline Term
Yap_GetDerefedFromSlot(Int slot USES_REGS)
{
  return LCL0[slot];
}

static inline Term
Yap_GetPtrFromSlot(Int slot USES_REGS)
{
  return(LCL0[slot]);
}

static inline Term *
Yap_AddressFromSlot(Int slot USES_REGS)
{
  return(LCL0+slot);
}

static inline void
Yap_PutInSlot(Int slot, Term t USES_REGS)
{
  LCL0[slot] = t;
}

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

static inline Int
Yap_InitSlot(Term t USES_REGS)
{
  Int old_slots = IntOfTerm(ASP[0]);
  *ASP = t;
  ASP--;
  ASP[old_slots+2] = ASP[0] = MkIntTerm(old_slots+1);
  return((ASP+1)-LCL0);
}

static inline int
Yap_RecoverSlots(int n USES_REGS)
{
  Int old_slots = IntOfTerm(ASP[0]);
  if (old_slots - n < 0) {
    return FALSE;
  }
  ASP += n;
  ASP[old_slots+(n-old_slots+1)] = ASP[0] = MkIntTerm(old_slots-n);
  return TRUE;
}

#endif
