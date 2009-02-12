
#include "Yap.h"
#include "clause.h"
#include "udi.h"

/* we can have this stactic because it is written once */
static struct udi_control_block RtreeCmd;

typedef struct udi_info
{
  PredEntry *p;
  void *cb;
  UdiControlBlock functions;
  struct udi_info *next;
} *UdiInfo;

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

static Int
p_new_udi(void)
{
  Term spec = Deref(ARG2), udi_type = Deref(ARG1);
  PredEntry *p;
  UdiControlBlock cmd;
  Atom udi_t;
  void *info;

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
    p = RepPredProp(Yap_GetPredPropByFunc(fun, tmod));
  }
  if ((p->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|UserCPredFlag|CArgsPredFlag|NumberDBPredFlag|AtomDBPredFlag|TestPredFlag|AsmPredFlag|CPredFlag|BinaryPredFlag)) ||
      (p->ModuleOfPred == PROLOG_MODULE)) {
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, spec, "udi/2");
    return FALSE;
  }
  if (p->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|TabledPredFlag)) {
    Yap_Error(PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE, spec, "udi/2");
    return FALSE;
  }
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
  info = cmd->init(spec, (void *)p, p->ArityOfPE);
  if (!info)
    return FALSE;
  if (!add_udi_block(info, p, cmd)) {
    Yap_Error(OUT_OF_HEAP_ERROR, spec, "new user index/1");
    return FALSE;
  }
  p->PredFlags |= UDIPredFlag;
  return TRUE;
}

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

void
Yap_udi_init(void)
{
  UdiControlBlocks = NULL;
  /* to be filled in by David */
  RtreeCmd.init = NULL;
  RtreeCmd.insert = NULL;
  RtreeCmd.search = NULL;
  Yap_InitCPred("$init_udi", 2, p_new_udi, 0);
}
