#include <stdio.h>
#include "Yap.h"
#include "YapInterface.h"
#include "clause.h"
#include "clause_list.h"
#include "udi_private.h"

/* to keep a vector of udi indexers */
UT_icd udicb_icd = {sizeof(UdiControlBlock), NULL, NULL, NULL};
UT_array *indexing_structures;

/*
 * New user indexed predicate (used by the public udi interface)
 */
void
Yap_UdiRegister(UdiControlBlock cb){
	/*TODO: check structure integrity and duplicates */
	utarray_push_back(indexing_structures, &cb);
}

/*
 * New user indexed predicate:
 * the first argument is the term.
 */
static YAP_Int
p_new_udi( USES_REGS1 )
{
  Term spec = Deref(ARG1);

  PredEntry *p;
  UdiInfo blk;
  int info;

  //fprintf(stderr,"new pred\n");

  /* get the predicate from the spec, copied from cdmgr.c */
  if (IsVarTerm(spec)) {
	  Yap_Error(INSTANTIATION_ERROR,spec,"new user index/1");
	  return FALSE;
  } else if (!IsApplTerm(spec)) {
	  Yap_Error(TYPE_ERROR_COMPOUND,spec,"new user index/1");
	  return FALSE;
  } else {
	  Functor fun = FunctorOfTerm(spec);
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
  if ((p->PredFlags
		  & (DynamicPredFlag|LogUpdatePredFlag|UserCPredFlag|CArgsPredFlag|NumberDBPredFlag|AtomDBPredFlag|TestPredFlag|AsmPredFlag|CPredFlag|BinaryPredFlag))
		  || (p->ModuleOfPred == PROLOG_MODULE)) {
	  Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, spec, "udi/2");
	  return FALSE;
  }
  if (p->PredFlags & (DynamicPredFlag|LogUpdatePredFlag|TabledPredFlag)) {
	  Yap_Error(PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE, spec, "udi/2");
	  return FALSE;
  }
  /* TODO: remove AtomRTree from atom list */

  /* this is the real work */
  blk = (UdiInfo) Yap_AllocCodeSpace(sizeof(struct udi_info));
  memset((void *) blk,0, sizeof(struct udi_info));

  if (!blk) {
	  Yap_Error(OUT_OF_HEAP_ERROR, spec, "new user index/1");
	  return FALSE;
  }
  utarray_new(blk->clauselist, &ptr_icd);
  utarray_new(blk->args, &arg_icd);
  blk->p = p;
  HASH_ADD_UdiInfo(UdiControlBlocks, p, blk);
  //fprintf(stderr,"PRED %p\n",p);

  info = Yap_udi_args_init(spec, p->ArityOfPE, blk);
  if (!info) /*TODO: clear blk here*/
    return FALSE;

  p->PredFlags |= UDIPredFlag;
  return TRUE;
}

/*
 * Here we initialize the arguments indexing
 */
int
Yap_udi_args_init(Term spec, int arity, UdiInfo blk)
{
	int i;
	Term arg;
	Atom idxtype;
	UdiControlBlock *p;
	struct udi_p_args p_arg;

	//fprintf(stderr,"udi init\n");

	for (i = 1; i <= arity; i++) {
		arg = ArgOfTerm(i,spec);
		if (IsAtomTerm(arg)) {
			idxtype = AtomOfTerm(arg);
//			fprintf(stderr,"%p-%s %p-%s\n",idxtype, YAP_AtomName(idxtype),
//						                   AtomMinus, YAP_AtomName(AtomMinus));
			if (idxtype == AtomMinus)
				continue;
			p_arg.control = NULL;
			p = NULL;
			while ((p = (UdiControlBlock *) utarray_next(indexing_structures, p))) {
				//fprintf(stderr,"cb: %p %p-%s\n", *p, (*p)->decl, YAP_AtomName((*p)->decl));
				if (idxtype == (*p)->decl){
					p_arg.arg = i;
					p_arg.control = *p;
					p_arg.idxstr = (*p)->init(spec, i, arity);
					utarray_push_back(blk->args, &p_arg);
				}
			}
			if (p_arg.control == NULL){ /* not "-" and not found*/
				fprintf(stderr, "Invalid Spec (%s)\n", AtomName(idxtype));
				return FALSE;
			}
		}
	}
	return TRUE;
}

/* just pass info to user, called from cdmgr.c */
int
Yap_new_udi_clause(PredEntry *p, yamop *cl, Term t)
{
	int i;
	UdiPArg parg;
	UdiInfo info;

	/* try to find our structure*/
	HASH_FIND_UdiInfo(UdiControlBlocks,p,info);
	if (!info)
		return FALSE;

	/*insert into clauselist will be used latter*/
	utarray_push_back(info->clauselist,(void *) cl);

	for (i = 0; i < utarray_len(info->args) ; i++) {
		parg = (UdiPArg) utarray_eltptr(info->args,i);
		//fprintf(stderr,"call insert\n");
		parg->idxstr = parg->control->insert(parg->idxstr, t, parg->arg, (void *)cl);
		//info->cb = info->functions->insert(t, info->cb, (void *)cl);
	}
	return TRUE;
}

/* index, called from absmi.c */
yamop *
Yap_udi_search(PredEntry *p)
{
	int i, r = -1;
	struct ClauseList clauselist;
	struct CallbackM cm;
	callback_m_t c;
	UdiPArg parg;
	UdiInfo info;

	/* find our structure*/
	HASH_FIND_UdiInfo(UdiControlBlocks,p,info);
	if (!info)
		return NULL;

	/*TODO: handle intersection*/
	c = &cm;
	c->cl = Yap_ClauseListInit(&clauselist);
	c->pred = p;

	for (i = 0; i < utarray_len(info->args) ; i++) {
		parg = (UdiPArg) utarray_eltptr(info->args,i);
		//fprintf(stderr,"call search %d %p\n", i, (void *)c);
		r = parg->control->search(parg->idxstr, parg->arg, callback, c);
		/*info->functions->search(info->cb);*/
	}
	Yap_ClauseListClose(c->cl);

	if (r >= 0) {
		if (Yap_ClauseListCount(c->cl) == 0)
		{
			Yap_ClauseListDestroy(c->cl);
			return Yap_FAILCODE();
		}

		if (Yap_ClauseListCount(c->cl) == 1)
		{
			return Yap_ClauseListToClause(c->cl);
		}

		return Yap_ClauseListCode(c->cl);
	}
	else
		Yap_ClauseListDestroy(c->cl);

	return NULL;
}

/* index, called from absmi.c */
void
Yap_udi_abolish(PredEntry *p)
{
	/* tell the predicate destroy */
}

/*
 * Init Yap udi interface
 */
void
Yap_udi_init(void)
{
  UdiControlBlocks = NULL;

  /*init indexing structures array*/
  utarray_new(indexing_structures, &udicb_icd);

  Yap_InitCPred("$udi_init", 1, p_new_udi, 0);
  /* TODO: decide if udi.yap should be loaded automaticaly in init.yap */
}
