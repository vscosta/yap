/*************************************************************************
*									 *
*	 Yap Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		index.c							 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Indexing a Prolog predicate				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

/*
 * This file compiles and removes the indexation code for the prolog compiler 
 *
 * Some remarks: *try_me always point to inside the code;
 * try always points to outside 
 *
 */

#include "Yap.h"
#include "compile.h"
#include "clause.h"
#include "index.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#ifdef DEBUG
#include "yapio.h"
#endif
#ifndef NULL
#define NULL (void *)0
#endif

int             IPredArity;

STATIC_PROTO(int  clause_has_cut, (yamop *));
STATIC_PROTO(int  followed_by_cut, (yamop *));
STATIC_PROTO(void  emit_tr, (compiler_vm_op, yamop *, int, int));
STATIC_PROTO(void  emit_try, (compiler_vm_op, int, yamop *, int, int));
STATIC_PROTO(yamop *  Body, (yamop *));
STATIC_PROTO(yamop *  SecB, (yamop *));
STATIC_PROTO(yamop *  SecLB, (yamop *));
STATIC_PROTO(yamop *  ThiB, (yamop *));
STATIC_PROTO(yamop *  ThiLB, (yamop *));
STATIC_PROTO(void  emit_cp_inst, (compiler_vm_op, yamop *, int, int));
STATIC_PROTO(CELL  emit_space, (compiler_vm_op, int, int));
STATIC_PROTO(CELL  emit_go, (int, Term));
STATIC_PROTO(void  emit_if_not, (Term, CELL, CELL));
STATIC_PROTO(void  fill_go, (CELL *, CELL));
STATIC_PROTO(void  fill_switch_slots, (CELL *, CELL, CELL, CELL, CELL, int));
STATIC_PROTO(void  ClrSpace, (CELL *, int));
STATIC_PROTO(int   NGroupsIn, (PredEntry *));
STATIC_PROTO(void  CountGroups, (GroupDef *, int));
STATIC_PROTO(yamop *FindFirst, (int, int));
STATIC_PROTO(int   SizeTable, (int));
STATIC_PROTO(void  BuildHash, (CELL *, int, int, int));
STATIC_PROTO(void  BuildIfTable, (CELL *, int));
STATIC_PROTO(void  TreatEntry, (EntryDef *, int, int, GroupDef *));
STATIC_PROTO(CELL  DealFixed, (ClauseDef *, int, compiler_vm_op, int, GroupDef *));
STATIC_PROTO(CELL  DealFixedWithBips, (ClauseDef *, int, int, GroupDef *));
STATIC_PROTO(CELL  DealCons, (int));
STATIC_PROTO(CELL  DealAppl, (int));
STATIC_PROTO(CELL  StartList, (int));
STATIC_PROTO(CELL  DealLAt, (ClauseDef *, int, int));
STATIC_PROTO(CELL  DealLAppl, (ClauseDef *, int, int));
STATIC_PROTO(CELL  DealLList, (ClauseDef *, int, int, int));
STATIC_PROTO(int   NoHeadVar, (ClauseDef *, int));
STATIC_PROTO(CELL  DealList, (int));
STATIC_PROTO(CELL  GetFailToGo, (int));
STATIC_PROTO(int   IsExtendedSingle, (int));
STATIC_PROTO(int   gr_has_cuts, (GroupDef *));
STATIC_PROTO(void  EmitGrSwitch, (int));
STATIC_PROTO(int   IndexNonVarGr, (int));
STATIC_PROTO(void  IndexVarGr, (int));
STATIC_PROTO(int   SimpleCase, (void));
STATIC_PROTO(int   ComplexCase, (void));
STATIC_PROTO(int   SpecialCases, (void));

static int      NClauses, NGroups;

static ClauseDef *ArOfCl;

static GroupDef *Groups;

static EntryDef *Entries;

static CELL     labelno;

static int      ExtendedSingle, AtomsOnlyNil;

static yamop   *FailAddress;

static CELL    *StorePoint;

static yamop * FirstCl;

static int      RemovedCl;	/* There were some clauses removed */

static yamop * indexed_code_for_cut = NIL;

static CELL log_update;

static inline int
clause_has_cut(yamop * C)
{
#ifdef YAPOR
  return(YAMOP_CUT(C));
#else
  return(FALSE);
#endif /* YAPOR */
}

/*
   Detect whether the next instruction is a cut.
   Only useful for try and friends.
*/
static int followed_by_cut(yamop * code)
{
 register yamop *p = code;
 while (TRUE)
   {
     if (p->opc == opcode(_get_x_var))
       p = NEXTOP(p,xx);
     if (p->opc == opcode(_get_y_var))
       p = NEXTOP(p,yx);
     else if (p->opc == opcode(_allocate))
       p = NEXTOP(p,e);
     else if (p->opc == opcode(_unify_x_var))
       p = NEXTOP(p,ox);
     else if (p->opc == opcode(_unify_y_var))
       p = NEXTOP(p,oy);
     else if (p->opc == opcode(_unify_l_x_var))
       p = NEXTOP(p,ox);
     else if (p->opc == opcode(_unify_l_y_var))
       p = NEXTOP(p,oy);
     else if (p->opc == opcode(_unify_void))
       p = NEXTOP(p,o);
     else if (p->opc == opcode(_unify_n_voids))
       p = NEXTOP(p,os);
     else if (p->opc == opcode(_unify_l_void))
       p = NEXTOP(p,o);
     else if (p->opc == opcode(_unify_l_n_voids))
       p = NEXTOP(p,os);
     else if (p->opc == opcode(_cut))
       return(TRUE);
     else if (p->opc == opcode(_cut_t))
       return(TRUE);
     else if (p->opc == opcode(_cut_e))
       return(TRUE);
     else return(FALSE);
   }
 /* make lcc happy */
 return(FALSE);
}


/* emits an opcode followed by an adress */
inline static void 
emit_tr(compiler_vm_op op, yamop * Address, int NClauses, int HasCut)
{
	emit(op, Unsigned(Address), (NClauses << 1) + HasCut);
}

/* emits an opcode followed by an adress */
static void 
emit_try(compiler_vm_op op, int op_offset, yamop * Address, int NClauses, int HasCut)
{
  if (op == try_op) {
    /* if a try, then try to use cuts if possible */ 
    indexed_code_for_cut = NIL;
    if (followed_by_cut(Address)) {
      indexed_code_for_cut = Address;
      return;
    }
  }
  emit((compiler_vm_op)((int)op + op_offset), Unsigned(Address), (NClauses << 1) + HasCut);
}

/*
 * compute address of first instruction of clause: try  addr,n				 
 */
inline static yamop * 
Body(yamop * Arg)
{
  return (NEXTOP(Arg,ld));
}

/*
 * compute address of instruction after get_atom or get_struct try  	 
 * addr,n				get_atom  atom,reg					 
 */
inline static yamop * 
SecB(yamop * Arg)
{
  yamop *pc = NEXTOP(Arg,ld);
  if (pc->opc == opcode(_get_struct))
    return (NEXTOP(pc,xf));
  else
    return (NEXTOP(pc,xc));
}

/*
 * compute address of instruction after get_list try  	  addr,n
 * et_list  reg					 
 */
inline static yamop * 
SecLB(yamop * Arg)
{
  return (NEXTOP(NEXTOP(Arg,ld),x));
}

/*
 * compute address of instruction after get_list and unify try  	 
 * addr,n				get_list  reg
 * nify_atom atom					 
 */
inline static yamop * 
ThiB(yamop * Arg)
{
  yamop *pc = NEXTOP(NEXTOP(Arg,ld),x);
  if (pc->opc == opcode(_unify_struct))
    return (NEXTOP(pc,of));
  else
    return (NEXTOP(pc,oc));
}

/*
 * compute address of instruction after get_list unify_list try  	 
 * addr,n				get_list  reg unify_list					 
 */
inline static yamop * 
ThiLB(yamop * Arg)
{
  return ( NEXTOP(NEXTOP(NEXTOP(Arg,ld),x),o));
}

/* emits a try, retry or trust, optimizing for a particular case */
static void 
emit_cp_inst(compiler_vm_op op, yamop * Address, int Flag, int NClausesAfter)
{
  int HasCut = clause_has_cut(Address);

  indexed_code_for_cut = NIL;
  if (op != try_op && profiling)
    emit(retry_profiled_op, Unsigned(CurrentPred), Zero);
  if (NGroups == 1)
    Flag = Flag | LoneGroup;
  else if (Flag & LastGroup) {
    if (op == try_op) {
      Flag &= (FirstIndex | LastFoundList | HeadIndex | IsAtom);
      switch (Flag) {
      case 0:
	emit_tr(trylf_op, Body(Address), NClausesAfter, HasCut);
	return;
      case FirstIndex | LastFoundList:
	emit_tr(trylf_op, SecLB(Address), NClausesAfter, HasCut);
	return;
      case FirstIndex | IsAtom:
	emit_tr(retry_op, SecB(Address), NClausesAfter, HasCut);
	return;
      case FirstIndex:
	emit_tr(trylf_op, SecB(Address), NClausesAfter, HasCut);
	return;
      case HeadIndex | LastFoundList:
	emit_tr(trylh_op, ThiLB(Address), NClausesAfter, HasCut);
	return;
      case HeadIndex | IsAtom:
	emit_tr(trylf_op, ThiB(Address), NClausesAfter, HasCut);
	return;
      case HeadIndex:
	emit_tr(trylh_op, ThiB(Address), NClausesAfter, HasCut);
	return;
      }
    } else
      Flag ^= LastGroup;
    Flag |= LoneGroup;
  }
  if (ExtendedSingle) {
    if (op == trust_op) {
      emit_cp_inst(retry_op, Address, Flag, NClausesAfter);
      emit_tr(trust_op, FailAddress, NClausesAfter-1, HasCut);
      return;
    }
    Flag |= LoneGroup;
  }
  switch (Flag) {
  case 0:
    emit_try(op, tryin_op - try_op, Body(Address), NClausesAfter, HasCut);
    return;
  case LoneGroup:
    emit_try(op, try_op - try_op, Body(Address), NClausesAfter, HasCut);
    return;
  case FirstIndex:
    emit_try(op, tryfin_op - try_op, SecB(Address), NClausesAfter, HasCut);
    return;
  case FirstIndex | IsAtom:
    emit_try(op, tryin_op - try_op, SecB(Address), NClausesAfter, HasCut);
    return;
  case FirstIndex | LastFoundList:
    emit_try(op, tryfin_op - try_op, SecLB(Address), NClausesAfter, HasCut);
    return;
  case FirstIndex | LoneGroup:
    emit_try(op, tryf_op - try_op, SecB(Address), NClausesAfter, HasCut);
    return;
  case FirstIndex | LoneGroup | IsAtom:
    emit_try(op, try_op - try_op, SecB(Address), NClausesAfter, HasCut);
    return;
  case FirstIndex | LoneGroup | LastFoundList:
    emit_try(op, tryf_op - try_op, SecLB(Address), NClausesAfter, HasCut);
    return;
  case HeadIndex:
    emit_try(op, tryhin_op - try_op, ThiB(Address), NClausesAfter, HasCut);
    return;
  case HeadIndex | LastFoundList:
    emit_try(op, tryhin_op - try_op, ThiLB(Address), NClausesAfter, HasCut);
    return;
  case HeadIndex | IsAtom:
    emit_try(op, trytin_op - try_op, ThiB(Address), NClausesAfter, HasCut);
    return;
  case HeadIndex | LoneGroup:
    emit_try(op, tryh_op - try_op, ThiB(Address), NClausesAfter, HasCut);
    return;
  case HeadIndex | LoneGroup | LastFoundList:
    emit_try(op, tryh_op - try_op, ThiLB(Address), NClausesAfter, HasCut);
    return;
  case HeadIndex | LoneGroup | IsAtom:
    emit_try(op, tryt_op - try_op, ThiB(Address), NClausesAfter, HasCut);
    return;
  default:
    return;
  }
}


/* emits a large switch instruction */
static CELL 
emit_space(compiler_vm_op op, int space, int nof)
{
  labelno += 2;
  emit(label_op, labelno, Zero);
  StorePoint = emit_extra_size(op, Unsigned(nof), space);
  return (labelno);
}

/* emits a go instruction */
static CELL 
emit_go(int Gender, Term Name)
{
  labelno += 2;
  emit(label_op, labelno, Zero);
  if (Gender == ApplCl)
    StorePoint = emit_extra_size(go_f_op, Zero, 3 * CellSize);
  else
    StorePoint = emit_extra_size(go_c_op, Zero, 3 * CellSize);
  *StorePoint++ = Unsigned(Name);
  StorePoint[1] = (CELL)FailAddress;
  return (labelno);
}


/* emits an if_not_then instruction */
static void 
emit_if_not(Term T1, CELL Ad1, CELL Ad2)
{
  StorePoint = emit_extra_size(if_not_op, Zero, 3 * CellSize);
  *StorePoint++ = Unsigned(T1);
  *StorePoint++ = Unsigned(Ad1);
  StorePoint[0] = Unsigned(Ad2);
}


/* places the code where to go in a go instruction */
static void 
fill_go(CELL *LCons, CELL Code)
{
  LCons[0] = Code;
}


/* fills a space after a first instruction with four addresses */
static void 
fill_switch_slots(CELL *WhereTo, CELL FAddr, CELL SecAddr, CELL ThirdAddr, CELL ThourthAddr, int NOf)
{
  *WhereTo++ = FAddr;
  *WhereTo++ = SecAddr;
  *WhereTo++ = ThirdAddr;
  if (NOf > 3)
    *WhereTo = ThourthAddr;
}

/* Places in a space pairs 0 -> Fail */
static void 
ClrSpace(CELL *StSpace, int NBytes)
{
  int             i;

  NBytes /= 2 * CellSize;
  for (i = 0; i < NBytes; i++) {
    *StSpace++ = Zero;
    *StSpace++ = (CELL)FailAddress;
  }
}

/* Evaluates the number of groups and builds the clause and group arrays */
static int 
NGroupsIn(PredEntry *ap)
{
  int             x, y, PresentGroup;
  ClauseDef      *ActualCl = ArOfCl, *LastClauses[MaxOptions];
  GroupDef       *Group = Groups;
  yamop          *q = (yamop *)(ap->FirstClause), *LastOne = (yamop *)(ap->LastClause);

  NGroups = 1;
  LastClauses[VarCl] = NIL;
  LastClauses[AtCl] = NIL;
  LastClauses[ApplCl] = NIL;
  LastClauses[ListCl] = NIL;
  Group = Groups;
  NClauses = 0;
  while (IsVarClause(q)) {
    RemovedCl = TRUE;
    if (q == LastOne)
      return (NGroups);
    else
      q = (yamop *)NextClause(q);
  }
  x = KindOfArg(q);
  if (NonVarCl(x))
    PresentGroup = 1;
  else
    PresentGroup = 0;
  LastClauses[x] = Group->Start = ActualCl;
  Group->NCl = 0;
  Group->First = q;
  Group->Type[VarCl] = 0;
  Group->Type[AtCl] = 0;
  Group->Type[ApplCl] = 0;
  Group->Type[ListCl] = 0;
  Group->SInfo = OnlyNils;
  do {
    while (IsVarClause(q)) {
      RemovedCl = TRUE;
      if (q == LastOne)
	return (NGroups);
      else
	q = (yamop *)NextClause(q);
    }
    x = KindOfArg(q);
    NClauses++;
    ActualCl->Kind = x;
    ActualCl->Code = q;
    if (x == ListCl) {
      ActualCl->Name = HeadOfList(q);
      y = 1;
    } else if (NonVarCl(x)) {
      ActualCl->Name = TermOfCl(q);
      y = 1;
    } else {
      y = 0;
    }
    if (y != PresentGroup) {
      Group++->Last = (ActualCl - 1)->Code;
      NGroups++;
      if ((ADDR)Group > TrailTop-1024)
	growtrail(64 * 1024L);
      Group->First = q;
      Group->Start = ActualCl;
      Group->NCl = 0;
      Group->Type[VarCl] = 0;
      Group->Type[AtCl] = 0;
      Group->Type[ApplCl] = 0;
      Group->Type[ListCl] = 0;
      Group->SInfo = OnlyNils;
      LastClauses[VarCl] = NIL;
      LastClauses[AtCl] = NIL;
      LastClauses[ApplCl] = NIL;
      LastClauses[ListCl] = NIL;
      PresentGroup = y;
    }
    if (x == AtCl) {
      if (ActualCl->Name != MkAtomTerm(AtomNil))
	Group->SInfo &= ~OnlyNils;
      if (KindOfBipArg(q) & (FIsAtom|FIsNum|FIsPrimi)) {
	Group->SInfo |= UsesBips;
	ActualCl->Name = 0x0;
      }
    }
    ActualCl->Next = NIL;
    if (LastClauses[x] != NIL)
      LastClauses[x]->Next = ActualCl;
    LastClauses[x] = ActualCl++;
    /* check for overflow in case we have a really big database */
    if (ASP <= CellPtr (ActualCl) + 256) {
      freep = (char *)ActualCl;
      save_machine_regs();
      longjmp(CompilerBotch, 3);
    }
    (Group->Type[x])++;
    (Group->NCl)++;
    if (q == LastOne)
      q = NIL;
    else
      q = (yamop *)NextClause(q);
  } while (q != NIL);
  return (NGroups);
}

/* for each group find out how many clauses follow that group */
static void
CountGroups(GroupDef *Gr, int NGr)
{
  GroupDef *grp = Gr+NGr;
  int cls = 0;
  do {
    grp--;
    grp->NofClausesAfter = cls;
    cls += grp->NCl;
  } while(grp > Gr);
}

/* Finds the first clause whose arg is neither a list or a var */
static yamop * 
FindFirst(int i, int kind)
{
  ClauseDef      *GrClaus = Groups[i].Start;

  while (GrClaus->Kind != kind)
    GrClaus++;
  if (AtomsOnlyNil)
    return (SecB(GrClaus->Code));
  return (Body(GrClaus->Code));
}

/* evaluates the size (power of two) necessary for a table */
static int 
SizeTable(int Cases)
{
  register int    i = 2;

  if (Cases <= MinHashEntries)
    return (Cases);
  while ((Cases /= 2) > 0)
    i *= 2;
  return (i * 2);
}


#define HASH_SHIFT 6

/*
 * creates the hash table works if TableSize is a power of two 
 */
static void 
BuildHash(CELL *WhereTo, int NOfEntries, int TableSize, int Gend)
{
  register int    hash;
  register int    i;
#ifdef DEBUG
#ifdef CLASHES
  int             clashes = 0;
#endif /* CLASHES */
#endif /* DEBUG */
  Term            WorkTerm;
  EntryDef       *EntryP = Entries;
  yamop          *Base = (yamop *)CodePtr(WhereTo);
  yamop 	 *EndSpace = (yamop *)((char *)Base + TableSize * 2 * CellSize);

  ClrSpace(WhereTo, ((int) ((char *)EndSpace - (char *)Base)));
  TableSize--;
  for (i = 0; i < NOfEntries; ++i) {
    register Int    d;
    CELL *hentry;

    WorkTerm = EntryP->Class;
    hash = (Unsigned(WorkTerm) >> HASH_SHIFT) & TableSize;
    hentry = (CELL *)Base + hash * 2;
    d = TableSize & (Unsigned(WorkTerm) | 1);
    while (*hentry) {
#ifdef DEBUG
#ifdef CLASHES
      ++clashes;
#endif /* CLASHES */
#endif /* DEBUG */
      hash = (hash + d) & TableSize;
      hentry = (CELL *)Base + hash * 2;
    }
    hentry[0] = Unsigned(WorkTerm);
    hentry[1] = (CELL)((EntryP++)->Code);
  }
#ifdef DEBUG
#ifdef CLASHES
  YP_fprintf(YP_stderr,"hash table clashes: %d %d\n", clashes, NOfEntries);
#endif /* CLASHES */
#endif /* DEBUG */
}


static void 
BuildIfTable(CELL *WhereTo, int NOfEntries)
{
  register EntryDef *EntryP = Entries;
  register int    j;

  *WhereTo++ = (CELL)FailAddress;
  for (j = 0; j < NOfEntries; ++j) {
    *WhereTo++ = Unsigned(EntryP->Class);
    *WhereTo++ = (CELL)((EntryP++)->Code);
  }
}


/* Creates the code for each entry in a group */
static void 
TreatEntry(EntryDef *Entrance, int Gender, int PositionFlag, GroupDef *Gr)
{
  if (Entrance->Last == Entrance->First) {
    if (PositionFlag & LastGroup) {
      /* last group, meaning we already have a choice point set */
      register yamop *   k = (Entrance->First)->Code;
      labelno += 2;
      emit(label_op, Entrance->Code = labelno, Zero);
      if (PositionFlag & HeadIndex) {
	emit_tr(trust_op, ThiB(k), 1, clause_has_cut(k));
      } else {
	/* we cannot emit to SecLB because switch might have already
	   set SREG :-(, hence making us jump one step ahead. This
	   is not a problem with a direct jump, in this case the
	   work on S will just have been squandered */
	emit_tr(trust_op, Body(k), 1, clause_has_cut(k));
      }
    } else if (ExtendedSingle) {
      /* a single alternative and a catchall clause */
      register yamop *  k = (Entrance->First)->Code;
      labelno += 2;
      emit(label_op, Entrance->Code = labelno, Zero);
      /* if we are in a list */
      if (PositionFlag & HeadIndex) {
	/* we cannot emit to SecLB because switch might have already
	   set SREG :-( */
	emit_try(try_op, 0, Body(k), 1, clause_has_cut(k));
	if (indexed_code_for_cut != NIL) {
	  Entrance->Code =
	    (CELL)indexed_code_for_cut;
	} else {			    
	  emit_tr(trust_op, FailAddress, 1, clause_has_cut(k));
	  Entrance->Code = labelno;
	}
      } else {
	emit_try(try_op, 0, SecB(k), 2, clause_has_cut(k));
	if (indexed_code_for_cut != NIL) {
	  Entrance->Code =
	    (CELL)indexed_code_for_cut;
	} else {			    
	  emit_tr(trust_op, FailAddress, 1, clause_has_cut(k));
	  Entrance->Code = labelno;
	}
      }
    } else if (PositionFlag & HeadIndex) {
      /* single clause in the middle of code, no need to worry about
	 choicepoints  */
      Entrance->Code = Unsigned(ThiB((Entrance->First)->Code));
    } else {
      Entrance->Code = Unsigned(SecB((Entrance->First)->Code));
    }
  } else {
    ClauseDef      *k = Entrance->First;
    int		nofentries = 1, nofalts;
    
    while (k != Entrance->Last) {
      nofentries++;
      k = k->Next;
    }  

    nofalts = Gr->NofClausesAfter+nofentries;
    k = Entrance->First;
    labelno += 2;
    emit(label_op, (Entrance->Code = labelno), Zero);
    emit_cp_inst(try_op, k->Code, PositionFlag, nofalts);
    nofalts--;
    if (indexed_code_for_cut != NIL) {
      Entrance->Code = (CELL)indexed_code_for_cut;
    } else {
      while ((k = k->Next) != Entrance->Last) {
	emit_cp_inst(retry_op, k->Code, PositionFlag, nofalts);
	nofalts--;
      }
      emit_cp_inst(trust_op, k->Code, PositionFlag, nofalts);
      /* emit a jump with the place to jump to after finishing this group */
      if (NGroups > 1 && !(PositionFlag & LastGroup) && !ExtendedSingle)
	emit(jump_op, (CELL)((Gr+1)->First), Zero);
    }
  }
}

/* Creates the code for either appl or atomic cases */
static CELL 
DealFixed(ClauseDef *j, int Gender, compiler_vm_op op, int Flag, GroupDef *Gr)
{
  int             NDiffTerms = 1;
  int             TableSize, k;
  ClauseDef      *NextInChain;
  Term            HeadName;
  CELL            LFixed;
  CELL           *WhereToStore;
  EntryDef       *Entry = Entries;

  Entry->Class = j->Name;
  Entry->Last = Entry->First = j;
  do {
    int             l = 0;

    HeadName = j->Name;
    NextInChain = j->Next;
    j->Next = NIL;
    Entry = Entries;
    while (l < NDiffTerms && Entry->Class != HeadName)
      Entry++, l++;
    if (l == NDiffTerms) {
      if ((ADDR)Entry > TrailTop-1024)
	growtrail(64 * 1024L);
      Entry->Class = HeadName;
      Entry->Last = Entry->First = j;
      NDiffTerms++;
    } else {
      (Entry->Last)->Next = j;
      Entry->Last = j;
    }
  } while ((j = NextInChain) != NIL && j->Name != 0x0);
  Entry = Entries;
  if (NDiffTerms == 1) {
    CELL           *WhereToStore;

    if (AtomsOnlyNil) {
      TreatEntry(Entry, Gender, Flag, Gr);
      return ((CELL)(Entry->Code));
    }
    LFixed = emit_go(Gender, Entry->Class);
    WhereToStore = StorePoint;
    TreatEntry(Entry, Gender, Flag, Gr);
    fill_go(WhereToStore, (CELL)(Entry->Code));
    return (LFixed);
  }
  TableSize = SizeTable(NDiffTerms);
  if (NDiffTerms <= MinHashEntries) {
    if (op == switch_c_op)
      op = if_c_op;
    else
      op = if_f_op;
    LFixed = emit_space(op, CellSize + TableSize * 2 * CellSize, TableSize);
  } else
    LFixed = emit_space(op, TableSize * 2 * CellSize, TableSize);
  WhereToStore = StorePoint;
  for (k = 0; k < NDiffTerms; k++) {
    TreatEntry(Entry, Gender, Flag, Gr);
    Entry++;
  }
  if (NDiffTerms > MinHashEntries)
    BuildHash(WhereToStore, NDiffTerms, TableSize, Gender);
  else
    BuildIfTable(WhereToStore, NDiffTerms);
  return (LFixed);
}

/*
   If atom(X) and friends are around, and for the moment, just forget
   about doing indexing for constants and generate a traditional try,
   retry chain.
 */
static CELL 
DealFixedWithBips(ClauseDef *j, int NClauses, int Flag, GroupDef *Gr)
{
  int i = 2;
  CELL my_labelno;
  int nofalts = Gr->NofClausesAfter+NClauses;

  labelno += 2;
  my_labelno = labelno;
  emit(label_op, labelno, Zero);
#ifdef AGRESSIVE_BIPS
  if (j->Name != 0x0 && j->Next->Name != 0x0) {
    /*
       we've got a sequence of i clauses with known argument. Can
       index as normal for them
    */
    CELL   old_labelno;
    yamop *old_FailAddress;
    int old_ExtendedSingle, old_NAlts;
    ClauseDef *j0 = j;

    labelno += 2;
    old_labelno = labelno;
    old_FailAddress = FailAddress;
    FailAddress = labelno;
    /* the clauses behaves as if a new group */
    NGroups++;
    old_ExtendedSingle = ExtendedSingle;
    ExtendedSingle = FALSE;
    j = j->Next;
    old_NAlts = Gr->NofClausesAfter;
    while (j->Name != 0x0) i++, j = j->Next;
    Gr->NofClausesAfter = old_NAlts + G->NCl - i;
    DealFixed(j0, AtCl, switch_c_op, FirstIndex | IsAtom, Gr);
    emit(label_op, old_labelno, Zero);
    FailAddress = old_FailAddress;    
    ExtendedSingle = old_ExtendedSingle;
    Gr->NofClausesAfter = old_NAlts;
    NGroups--;
  } else {
#endif /* AGRESSIVE_BIPS */
    emit_cp_inst(try_op, j->Code, Flag, nofalts);

    if (indexed_code_for_cut != NIL) {
      /* Unfortunately, this is bound never to happen */
      return((CELL)indexed_code_for_cut);
    }
    else j = j->Next;
    nofalts--;
#ifdef AGRESSIVE_BIPS
  }
#endif /* AGRESSIVE_BIPS */
      /* we handled a group */
  for (; i < NClauses; i++) {
    emit_cp_inst(retry_op, j->Code, Flag, nofalts);
    j = j->Next;
    nofalts--;
  }
  emit_cp_inst(trust_op, j->Code, Flag, nofalts);
  /* emit a jump with the place to jump to after finishing this group */
  if (NGroups > 1 && !(Flag & LastGroup) && !ExtendedSingle)
    emit(jump_op, (CELL)((Gr+1)->First), Zero);
  return(my_labelno);
}

static CELL 
DealCons(int i)
{
  int             NAtCl = Groups[i].Type[AtCl];
  ClauseDef      *Cla = Groups[i].Start;

  if (NAtCl == 1) {
    if (FinalGr(i)) {
      yamop * Cl = FindFirst(i, AtCl);
      labelno += 2;
      emit(label_op, labelno, Zero);
      emit_tr(trust_op, Cl, 1, clause_has_cut(Cl));
      return (labelno);
    } else if (ExtendedSingle) {
      yamop * Cl = FindFirst(i, AtCl);

      labelno += 2;
      emit(label_op, labelno, Zero);
      emit_tr(try_op, Cl, 2, clause_has_cut(Cl));
      emit_tr(trust_op, FailAddress, 1, clause_has_cut(FailAddress));
      return (labelno);
    }
    return ((CELL)FindFirst(i, AtCl));
    
  } else if (NAtCl == 0)
    return ((CELL)FailAddress);
  
  while (Cla->Kind != AtCl)
    Cla++;

  if (FinalGr(i)) {
    if (Groups[i].SInfo & UsesBips)
      return(DealFixedWithBips(Cla, NAtCl, LastGroup, Groups+i));
    else
      return (DealFixed(Cla, AtCl, switch_c_op,
			FirstIndex | IsAtom | LastGroup, Groups+i));
  } else {
    if (Groups[i].SInfo & UsesBips)
      return(DealFixedWithBips(Cla, NAtCl, 0, Groups+i));
    else
      return (DealFixed(Cla, AtCl, switch_c_op,
			FirstIndex | IsAtom, Groups+i));
  }
}

static CELL 
DealAppl(int i)
{
  int             NApCl = Groups[i].Type[ApplCl];
  ClauseDef      *Cla = Groups[i].Start;

  if (NApCl == 1) {
    if (FinalGr(i)) {
      yamop * Cl = FindFirst(i, ApplCl);
      labelno += 2;
      emit(label_op, labelno, Zero);
      emit_tr(trust_op, Cl, 1, clause_has_cut(Cl));
      return (labelno);
    } else if (ExtendedSingle) {
      yamop * Cl = FindFirst(i, ApplCl);
      labelno += 2;
      emit(label_op, labelno, Zero);
      emit_tr(try_op, Cl, 2, clause_has_cut(Cl));
      emit_tr(trust_op, FailAddress, 1, clause_has_cut(FailAddress));
      return (labelno);
    }
    return ((CELL)FindFirst(i, ApplCl));
    
  } else if (NApCl == 0)
    return ((CELL)FailAddress);
  while (Cla->Kind != ApplCl)
    Cla++;
  if (FinalGr(i)) {
    int             Flag = FirstIndex | LastGroup;
    return (DealFixed(Cla, ApplCl, switch_f_op, Flag, Groups+i));
  }
  return (DealFixed(Cla, ApplCl, switch_f_op, FirstIndex, Groups+i));
}

/* Finds the body of the first clause of a group whose first arg is a list */
static CELL 
StartList(int i)
{
  ClauseDef      *j = Groups[i].Start;

  while (j->Kind != ListCl)
    j++;
  if (FinalGr(i)) {
    labelno += 2;
    emit(label_op, labelno, Zero);
    emit_tr(trust_op, SecLB(j->Code), 1, clause_has_cut(j->Code));
    return (labelno);
  } else if (ExtendedSingle) {
    labelno += 2;
    emit(label_op, labelno, Zero);
    emit_tr(try_op, SecLB(j->Code), 2, clause_has_cut(j->Code));
    emit_tr(trust_op, FailAddress, 1, clause_has_cut(FailAddress));
    return (labelno);
  } else {
    return (Unsigned(SecLB(j->Code)));
  }
}

static CELL 
DealLAt(ClauseDef *Cla, int NOfClau, int NG)
{
  if (NOfClau == 0)
    return ((CELL)FailAddress);
  if (FinalGr(NG)) {
    int             Flag = HeadIndex | IsAtom | LastGroup;
    return (DealFixed(Cla, AtCl, switch_c_op, Flag, Groups+NG));
  }
  return (DealFixed(Cla, AtCl, switch_c_op, HeadIndex | IsAtom, Groups+NG));
}

static CELL 
DealLAppl(ClauseDef *Cla, int NOfClauses, int NG)
{
  if (NOfClauses == 0)
    return ((CELL)FailAddress);
  if (FinalGr(NG)) {
    int             Flag = HeadIndex | LastGroup;

    return (DealFixed(Cla, ApplCl, switch_f_op, Flag, Groups+NG));
  }
  return (DealFixed(Cla, ApplCl, switch_f_op, HeadIndex, Groups+NG));
}

static CELL 
DealLList(ClauseDef *j, int NOfClauses, int PositionFlag, int NG)
{
  int             k, nofalts = 1;
  
  if (NOfClauses == 0)
    return ((CELL)FailAddress);
  else if (NOfClauses == 1) {
    if (FinalGr(NG)) {
      labelno += 2;
      emit(label_op, labelno, Zero);
      if (PositionFlag & FirstIndex)
	emit_tr(trust_op, SecLB(j->Code), 1, clause_has_cut(j->Code));
      else
	emit_tr(trust_op, ThiLB(j->Code), 1, clause_has_cut(j->Code));
      return (labelno);
    } else if (ExtendedSingle) {
      labelno += 2;
      emit(label_op, labelno, Zero);
      if (PositionFlag & FirstIndex)
	emit_tr(try_op, SecLB(j->Code), 2, clause_has_cut(j->Code));
      else
	emit_tr(try_op, ThiLB(j->Code), 2, clause_has_cut(j->Code));
      emit_tr(trust_op, FailAddress, 1, clause_has_cut(FailAddress));
      return (labelno);
    }
    if (PositionFlag & FirstIndex)
      return (Unsigned(SecLB(j->Code)));
    return (Unsigned(ThiLB(j->Code)));
  }
  if (FinalGr(NG))
    PositionFlag |= LastGroup;
  labelno += 2;
  emit(label_op, labelno, Zero);
  nofalts = Groups[NG].NofClausesAfter+NOfClauses;
  emit_cp_inst(try_op, j->Code, PositionFlag, nofalts);
  nofalts--;
  if (indexed_code_for_cut != NIL) {
    return((CELL)indexed_code_for_cut);
  } else {
    j = j->Next;
    for (k = 2; k < NOfClauses; ++k) {
      emit_cp_inst(retry_op, j->Code, PositionFlag, nofalts);
      nofalts--;
      j = j->Next;
    }
    emit_cp_inst(trust_op, j->Code, PositionFlag, nofalts);
    /* emit a jump with the place to jump to after finishing this group */
    if (NGroups > 1 && !(PositionFlag & LastGroup) && !ExtendedSingle)
      emit(jump_op, (CELL)(Groups[NG+1].First), Zero);
    return (labelno);
  }
}


static int 
NoHeadVar(ClauseDef *CInfo, int NClauses)
{
	while (NClauses--) {
		if (KindOfListArg(CInfo->Code) == VarCl)
			return (FALSE);
		CInfo = CInfo->Next;
	}
	return (TRUE);
}

/* Creates code when several clauses first arg is a list */
static CELL 
DealList(int i)
{
  int           k;
  ClauseDef    *j;
  CELL          LLFirst;
  CELL         *WhereToStore;
  int           NListCl = Groups[i].Type[ListCl];
  int           VFlags;
  int		nofalts = Groups[i].NofClausesAfter+NListCl;

  if (NListCl == 0)
    return ((CELL)FailAddress);
  if (NListCl == 1)
    return (StartList(i));
  j = Groups[i].Start;
  while (j->Kind != ListCl)
    j++;
  if (NoHeadVar(j, NListCl)) {
    CELL            LLVar, LLAt, LLList, LLAppl;
    ClauseDef      *Firsts[MaxOptions], *Lasts[MaxOptions];
    int             NOfSameCl[MaxOptions], l;

    NOfSameCl[AtCl] = 0;
    NOfSameCl[VarCl] = 0;
    NOfSameCl[ListCl] = 0;
    NOfSameCl[ApplCl] = 0;
    LLFirst = emit_space(switch_h_op, 4 * CellSize, 0);
    WhereToStore = StorePoint;
    LLVar = DealLList(j, NListCl, LastFoundList | FirstIndex, i);
    for (l = 0; l < NListCl; ++l) {
      int             z = KindOfListArg(j->Code);
      if (NOfSameCl[z] != 0)
	Lasts[z]->Next = j;
      else
	Firsts[z] = j;
      Lasts[z] = j;
      (NOfSameCl[z])++;
      j = j->Next;
    }
    if (NOfSameCl[AtCl])
      Lasts[AtCl]->Next = NIL;
    if (NOfSameCl[VarCl])
      Lasts[VarCl]->Next = NIL;
    if (NOfSameCl[ListCl])
      Lasts[ListCl]->Next = NIL;
    if (NOfSameCl[ApplCl])
      Lasts[ApplCl]->Next = NIL;
    LLAppl = DealLAppl(Firsts[ApplCl], NOfSameCl[ApplCl], i);
    LLAt = DealLAt(Firsts[AtCl], NOfSameCl[AtCl], i);
    LLList = DealLList(Firsts[ListCl], NOfSameCl[ListCl],
		       LastFoundList | HeadIndex, i);
    fill_switch_slots(WhereToStore, LLList, LLAt, LLAppl, LLVar, 4);
    return (LLFirst);
  }
  if (FinalGr(i))
    VFlags = FirstIndex | LastFoundList | LastGroup;
  else
    VFlags = FirstIndex | LastFoundList;
  labelno += 2;
  emit(label_op, labelno, Zero);
  emit_cp_inst(try_op, j->Code, VFlags, nofalts);
  nofalts--;
  if (indexed_code_for_cut != NIL) {
    return((CELL)indexed_code_for_cut);
  } else {
    j = j->Next;
    for (k = 2; k < NListCl; ++k) {
      emit_cp_inst(retry_op, j->Code, VFlags, nofalts);
      j = j->Next;
      nofalts--;
    }
    emit_cp_inst(trust_op, j->Code, VFlags, nofalts);
    /* emit a jump with the place to jump to after finishing this group */
    if (NGroups > 1 && !(VFlags & LastGroup) && !ExtendedSingle)
      emit(jump_op, (CELL)(Groups[i+1].First), Zero);
    return (labelno);
  }
}


/* Finds the place where to go if we have a flop */
static CELL 
GetFailToGo(int NG)
{
  if (NGroups == 1)
    return (Unsigned(FAILCODE));
  if (FinalGr(NG))
    return (Unsigned(TRUSTFAILCODE));
  if (ExtendedSingle)
    return (Unsigned(Body(Groups[NG + 1].Start->Code)));
  return (Unsigned(FAILCODE));
}


/* Verifies if we have a group and a var first clause (catchall, usually) */
static int 
IsExtendedSingle(int NG)
{
  if (NGroups == 2 && NG == 0 && Groups[1].NCl == 1 &&
      NClauses <= CLAUSES_FOR_EXTENDED_SINGLE)
    return (TRUE);
  return (FALSE);
}

static int
gr_has_cuts(GroupDef *gr)
{
  return(0);
}

static void 
EmitGrSwitch(int Count)
{
  GroupDef        Gr;

  Gr = Groups[Count];
  if (!Gr.Type[ApplCl] && (Gr.SInfo & OnlyNils)) {
    AtomsOnlyNil = TRUE;
    if (FinalGr(Count)) {
      emit_space(switch_ll_op, 3 * CellSize,
		 ((Gr.NCl) << 1)+gr_has_cuts(Groups+Count));
    } else if (NGroups == 1 || ExtendedSingle)
      emit_space(switch_lnl_op, 4 * CellSize, 0);
    else
      emit_space(switch_nvl_op, 3 * CellSize, 0);
  } else {
    AtomsOnlyNil = FALSE;
    if (FinalGr(Count))
      emit_space(switch_l_op, 3 * CellSize, 
		 ((Gr.NCl) << 1)+gr_has_cuts(Groups+Count));
    else if (NGroups == 1 || ExtendedSingle)
      emit_space(switch_t_op, 4 * CellSize, 0);
    else
      emit_space(switch_nv_op, 3 * CellSize, 0);
  }
}

/* Creates the indexation code for a non var group */
static int 
IndexNonVarGr(int Count)
{
  CELL            LVar, LCons, LA, LL;
  GroupDef        Gr;
  CELL           *WhereToStore;

  Gr = Groups[Count];
  ExtendedSingle = IsExtendedSingle(Count);
  FailAddress = (yamop *)GetFailToGo(Count);
  if (NGroups > 1 && !(ExtendedSingle) && Count < NGroups - 1) {
    labelno += 2;
    Groups[Count + 1].First = (yamop *)labelno;
  }
  if (Gr.NCl == 1) {
    yamop * Cl = (Gr.Start)->Code;
    if (Count == 0)
      emit_tr(try_op, Body(Cl), Gr.NofClausesAfter+1, clause_has_cut(Cl));
    else if (FinalGr(Count))
      emit_tr(trust_op, Body(Cl), Gr.NofClausesAfter+1, clause_has_cut(Cl));
    else
      emit_tr(retry_op, Body(Cl), Gr.NofClausesAfter+1, clause_has_cut(Cl));
    return (FALSE);	/* Indexation is not necessary */
  }
  if (NGroups > 1 && !(ExtendedSingle) && Count < NGroups - 1) {
    if (Count == 0)
      emit_tr(tryme_op, (yamop *) labelno,
	      Gr.NofClausesAfter+1, clause_has_cut((Gr.Start)->Code));
    else
      emit_tr(retryme_op, (yamop *) labelno,
	      Gr.NofClausesAfter+1, clause_has_cut((Gr.Start)->Code));
  }
  EmitGrSwitch(Count);
  WhereToStore = StorePoint;
  LCons = DealCons(Count);
  AtomsOnlyNil = FALSE;
  LA = DealAppl(Count);
  LL = DealList(Count);
  if (log_update) {
    LVar = log_update;
  } else
    LVar = Unsigned(FirstCl);
  if (NGroups == 1 || ExtendedSingle)
    fill_switch_slots(WhereToStore, LL, LCons, LA, LVar, 4);
  else
    fill_switch_slots(WhereToStore, LL, LCons, LA, LVar, 3);
  return (TRUE);
}

static void 
IndexVarGr(int Count)
{
  GroupDef       *Gr;
  ClauseDef      *Cla;
  int             j;
  int nofalts;

  Gr = Groups + Count;
  Cla = Gr->Start;
  nofalts = Gr->NofClausesAfter+Gr->NCl;
  if (Count == 0) {
    emit_tr(try_op, Body((Cla)->Code), nofalts, clause_has_cut(Cla->Code));
    Cla++;
    nofalts--;
  } else if (Count == NGroups - 1 && Gr->NCl == 1) {
    emit(label_op, Unsigned(Gr->First), Zero);
    emit_tr(trust_op, Body((Cla)->Code), 1, clause_has_cut(Cla->Code));
    return;
  } else {
    emit(label_op, Unsigned(Gr->First), Zero);
    emit_tr(retry_op, Body((Cla)->Code), nofalts, clause_has_cut(Cla->Code));
    Cla++;
    nofalts--;
  }
  for (j = 2; j < Gr->NCl; ++j) {
    emit_tr(retry_op, Body((Cla)->Code), nofalts, clause_has_cut(Cla->Code));
    Cla++;
    nofalts--;
  }
  if (Gr->NCl > 1) {
    if (Count == NGroups - 1)
      emit_tr(trust_op, Body(Cla->Code), 1, clause_has_cut(Cla->Code));
    else
      emit_tr(retry_op, Body(Cla->Code), nofalts, clause_has_cut(Cla->Code));
  }
}


static int 
SimpleCase(void)
{
  if (Groups[0].Type[VarCl] != 0)
    return (FALSE);
  return (IndexNonVarGr(0));
}

static int 
ComplexCase(void)
{
  int             Indexable = FALSE, i;

  if (IsExtendedSingle(0))
    return (SimpleCase());
  emit(jump_v_op, (CELL) FirstCl, Zero);
  if (Groups[0].Type[VarCl] == 0)
    i = 0;
  else {
    IndexVarGr(0);
    i = 1;
  }
  for (; i < NGroups; i += 2) {
    Indexable |= IndexNonVarGr(i);
    if (i < NGroups - 1)
      IndexVarGr(i + 1);
  }
  return (Indexable);
}


static int 
SpecialCases(void)
{
  CELL LVar;

  if (log_update) {
    LVar = log_update;
  } else {
    LVar = (CELL)(ArOfCl->Code);
  }
  /* Clear what was left before */
  freep = (char *) (ArOfCl + NClauses);
  CodeStart = cpc = NIL;
  /* For now just a special case */
  if (NGroups == 2 && NClauses == 2 && Groups[0].Type[AtCl] == 1 &&
      !RemovedCl && !(Groups[0].SInfo & UsesBips)) {
    emit_if_not(ArOfCl->Name, Unsigned(Body(ArOfCl[1].Code)),
		LVar);
    return (TRUE);
  } else
    return (FALSE);
}

CODEADDR
PredIsIndexable(PredEntry *ap)
{
  int      NGr, Indexable = 0;
  CODEADDR indx_out = NIL;
  log_update = 0;

  if (setjmp(CompilerBotch) == 3) {
    /* just duplicate the stack */
    restore_machine_regs();
    gc(ap->ArityOfPE, ENV, CP);
  }
 restart_index:
  labelno = 1;
  RemovedCl = FALSE;
  FirstCl = (yamop *)(ap->FirstClause);
  CurrentPred = ap;
  if (CurrentPred->PredFlags & ProfiledPredFlag)
    profiling = TRUE;
  else
    profiling = FALSE;
  IPredArity = ap->ArityOfPE;
  /* Store the clauses in the Global */
  ArOfCl = (ClauseDef *) H;
  /* and the groups in the  Auxiliary */
  Groups = (GroupDef *) TR;
  NGr = NGroupsIn(ap);
  CountGroups(Groups, NGr);
  /* store entries after groups */
  Entries = (EntryDef *) (Groups + NGroups);
  CodeStart = cpc = NIL;
  freep = (char *) (ArOfCl + NClauses);
  if (ErrorMessage != NIL) {
    return (NIL);
  }
  if (CurrentPred->PredFlags & LogUpdatePredFlag) {
    log_update = labelno;
    labelno += 2;
  }
  if (NClauses == 0) {
    Indexable = FALSE;
    return(NIL);
  } else {
    if (NGr == 1)
      Indexable = SimpleCase();
    else
      Indexable = ComplexCase();
    if (!Indexable)
      Indexable = SpecialCases();
  }
  if (CellPtr(freep) >= ASP) {
    Error(SYSTEM_ERROR, TermNil, "out of stack space while indexing");
    return(NIL);
  }
  if (log_update && NClauses > 1) {
    int i;
    Clause *cl;

    Indexable = TRUE;
    emit(label_op, log_update, Zero);
    emit(try_op, Unsigned(Body(ArOfCl[0].Code)), Zero);
    cl = ClauseCodeToClause(ArOfCl[0].Code);
    if (cl->ClFlags & LogUpdRuleMask) {
      cl->u2.ClExt->u.EC.ClRefs++;
    } else {
      cl->u2.ClUse++;
    }
    for (i = 1; i < NClauses-1; i++) {
      emit(retry_op, Unsigned(Body(ArOfCl[i].Code)), Zero);
      cl = ClauseCodeToClause(ArOfCl[0].Code);
      if (cl->ClFlags & LogUpdRuleMask) {
	cl->u2.ClExt->u.EC.ClRefs++;
      } else {
	cl->u2.ClUse++;
      }
    }
    emit(trust_op, Unsigned(Body(ArOfCl[i].Code)), Zero);
    cl = ClauseCodeToClause(ArOfCl[i].Code);
    if (cl->ClFlags & LogUpdRuleMask) {
      cl->u2.ClExt->u.EC.ClRefs++;
    } else {
      cl->u2.ClUse++;
    }
  }
  if (!Indexable) {
    return (NIL);
  } else {
#ifdef DEBUG
    if (Option['i' - 'a' + 1]) {
      ShowCode();
    }
#endif
    if ((indx_out = assemble(ASSEMBLING_INDEX)) == NIL) {
      if (!growheap(FALSE)) {
	Error(SYSTEM_ERROR, TermNil, "YAP failed to reserve space in growheap");
	return(FALSE);
      }
      goto restart_index;
    }
  }
  return(indx_out);
}
