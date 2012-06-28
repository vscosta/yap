
#include "Yap.h"
#include "clause.h"
#include "udi.h"


#include "rtree_udi.h"

/* we can have this stactic because it is written once */
static struct udi_control_block RtreeCmd;

/******
       All the info we need to enter user indexed code:
        predicate
	the user control block
	functions used, in case we have different schema (maybe should part of previous)
	right now, this is just a linked list....
******/
typedef struct udi_info
{
  PredEntry *p;
  void *cb;
  UdiControlBlock functions;
  struct udi_info *next;
} *UdiInfo;

/******
      we now have one extra user indexed predicate. We assume these
      are few, so we can do with a linked list.
******/
static int
add_udi_block(void *info, PredEntry *p, UdiControlBlock cmd)
{
  UdiInfo blk = (UdiInfo)Yap_AllocCodeSpace(sizeof(struct udi_info));
  if (!blk)
    return FALSE;
  blk->next = UdiControlBlocks;
  UdiControlBlocks = blk;
  blk->p = p;
  blk->functions = cmd;
  blk->cb = info;
  return TRUE;
}

/******
      new user indexed predicate;
      the type right now is just rtrees, but in the future we'll have more.
      the second argument is the term.
******/
static Int
p_new_udi( USES_REGS1 )
{
  Term spec = Deref(ARG2), udi_type = Deref(ARG1);
  PredEntry *p;
  UdiControlBlock cmd;
  Atom udi_t;
  void *info;

/*  fprintf(stderr,"new pred babe\n");*/
  /* get the predicate from the spec, copied from cdmgr.c */
  if (IsVarTerm(spec)) {
    Yap_Error(INSTANTIATION_ERROR,spec,"new user index/1");
    return FALSE;
  } else if (!IsApplTerm(spec)) {
    Yap_Error(TYPE_ERROR_COMPOUND,spec,"new user index/1");
    return FALSE;
  } else {
    Functor    fun = FunctorOfTerm(spec);
    Term tmod = CurrentModule;

    while (fun == FunctorModule) {
      tmod = ArgOfTerm(1,spec);
      if (IsVarTerm(tmod) ) {
	Yap_Error(INSTANTIATION_ERROR, spec, "new user index/1");
	return FALSE;
      }
      if (!IsAtomTerm(tmod) ) {
	Yap_Error(TYPE_ERROR_ATOM, spec, "new user index/1");
	return FALSE;
      }
      spec = ArgOfTerm(2, spec);
      fun = FunctorOfTerm(spec);
    }
    p = RepPredProp(PredPropByFunc(fun, tmod));
  }
  if (!p)
    return FALSE;
  /* boring, boring, boring! */
  if ((p->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|UserCPredFlag|CArgsPredFlag|NumberDBPredFlag|AtomDBPredFlag|TestPredFlag|AsmPredFlag|CPredFlag|BinaryPredFlag)) ||
      (p->ModuleOfPred == PROLOG_MODULE)) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, spec, "udi/2");
    return FALSE;
  }
  if (p->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|TabledPredFlag)) {
    Yap_Error(PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE, spec, "udi/2");
    return FALSE;
  }
  /* just make sure we're looking at the right user type! */
  if (IsVarTerm(udi_type)) {
    Yap_Error(INSTANTIATION_ERROR,spec,"new user index/1");
    return FALSE;
  } else if (!IsAtomTerm(udi_type)) {
    Yap_Error(TYPE_ERROR_ATOM,spec,"new user index/1");
    return FALSE;
  }
  udi_t = AtomOfTerm(udi_type);
  if (udi_t == AtomRTree) {
    cmd = &RtreeCmd;
  } else {
    Yap_Error(TYPE_ERROR_ATOM,spec,"new user index/1");
    return FALSE;
  }
  /* this is the real work */
  info = cmd->init(spec, (void *)p, p->ArityOfPE);
  if (!info)
    return FALSE;
  /* add to table */
  if (!add_udi_block(info, p, cmd)) {
    Yap_Error(OUT_OF_HEAP_ERROR, spec, "new user index/1");
    return FALSE;
  }
  p->PredFlags |= UDIPredFlag;
  return TRUE;
}

/* just pass info to user, called from cdmgr.c */
int
Yap_new_udi_clause(PredEntry *p, yamop *cl, Term t)
{
  struct udi_info *info = UdiControlBlocks;
  while (info->p != p && info)
    info = info->next;
  if (!info)
    return FALSE;
  info->cb = info->functions->insert(t, info->cb, (void *)cl);
  return TRUE;
}

/* index, called from absmi.c */
yamop *
Yap_udi_search(PredEntry *p)
{
  struct udi_info *info = UdiControlBlocks;
  while (info->p != p && info)
    info = info->next;
  if (!info)
    return NULL;
  return info->functions->search(info->cb);
}

/* index, called from absmi.c */
void
Yap_udi_abolish(PredEntry *p)
{
  /* tell the predicate destroy */
}

void
Yap_udi_init(void)
{
  UdiControlBlocks = NULL;
  /* to be filled in by David */
  RtreeCmd.init = RtreeUdiInit;
  RtreeCmd.insert = RtreeUdiInsert;
  RtreeCmd.search = RtreeUdiSearch;
  RtreeCmd.destroy = RtreeUdiDestroy;
  Yap_InitCPred("$udi_init", 2, p_new_udi, 0);
}

