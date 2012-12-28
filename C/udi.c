#include <stdio.h>
#include <assert.h>
#include <Judy.h>
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
  utarray_new(blk->clauselist, &cl_icd);
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
	int index;
//	yamop **x;

	/* try to find our structure*/
	HASH_FIND_UdiInfo(UdiControlBlocks,p,info);
	if (!info)
		return FALSE;

	/*insert into clauselist will be used latter*/
	utarray_push_back(info->clauselist, &cl);

	for (i = 0; i < utarray_len(info->args) ; i++) {
		parg = (UdiPArg) utarray_eltptr(info->args,i);
		index = utarray_len(info->clauselist);
//		x = (yamop **) utarray_eltptr(info->clauselist, index);
//		fprintf(stderr,"Insert (%p %p %d) %d - %p %p %p\n",
//				info->clauselist,info->clauselist->d,info->clauselist->icd.sz,
//				index,
//				cl, *x, *((yamop **) utarray_eltptr(info->clauselist, index)));
		parg->idxstr = parg->control->insert(parg->idxstr, t,
											 parg->arg,
											 (void *) index);
		//info->cb = info->functions->insert(t, info->cb, (void *)cl);
	}
	return TRUE;
}

static inline int callback(void *key, void *data, void *arg)
{
	int r;
	Pvoid_t *array = (Pvoid_t *) arg;
//	fprintf(stderr,"Found %p %d",data, (int) data);
	J1S(r, *array, (int) data);
	if (r == JERR)
		return FALSE;
	J1C(r, *array, 0 , -1);
//	fprintf(stderr," (%d)\n",r);
	return TRUE;
}

/* index, called from absmi.c */
yamop *
Yap_udi_search(PredEntry *p)
{
	int i, r;
	struct ClauseList clauselist;
//	struct CallbackM cm;
//	callback_m_t c;
	UdiPArg parg;
	UdiInfo info;
	Pvoid_t tmp, result;
	Word_t count;
	Word_t idx_r = 0L;
	yamop **x;

	/* find our structure*/
	HASH_FIND_UdiInfo(UdiControlBlocks,p,info);
	if (!info)
		return NULL;

//	/*TODO: handle intersection*/
//	c = &cm;
//	c->cl = Yap_ClauseListInit(&clauselist);
//	c->pred = p;

	/*
	 * I will start with the simplest case
	 * for each index create a set and intersect it with the
	 * next
	 */
	result = (Pvoid_t) NULL;
	tmp = (Pvoid_t) NULL;
	r = -1;
	for (i = 0; i < utarray_len(info->args) ; i++) {
//		fprintf(stderr,"Start Search\n");
		parg = (UdiPArg) utarray_eltptr(info->args,i);
		r = parg->control->search(parg->idxstr, parg->arg, callback, &tmp);

		if (r == -1) /*this arg does not prune search*/
			continue;

		J1C(count, result, 0, -1);

		if (r == 0)
		{
			if (count > 0) // clear previous result if it exists
				J1FA(count, result);
			fprintf(stderr,"Search Failed");
			return Yap_FAILCODE();
		}

		if (r > 0 && count == 0) // first result_set
		{
			result = tmp;
			tmp = (Pvoid_t) NULL;
		}
		else
		{/*intersection*/
			Word_t idx_tmp = 0L;

			idx_r = 0L;
			J1F(count, result, idx_r);//succeeds one time at least
			J1F(count, tmp, idx_tmp); //succeeds one time at least
			while (count)
			{
				while (idx_r < idx_tmp)
				{
					J1U(count, result, idx_r); //does not belong
					J1N(count, result, idx_r); //next
					if (! count) break;        //end result set
				}
				if(idx_r == idx_tmp)
				{
					J1N(count, result, idx_r); //next
					if (! count) break;        //end result set
					J1N(count, tmp, idx_tmp);  //next tmp
					//if (! count) break;      //end tmp set
				}
				else // (idx_r > idx_tmp)
				{
					idx_tmp = idx_r; // fast forward
					J1F(count, tmp, idx_tmp); // first starting in idx_r
					if (! count) break; //end tmp set
				}
			}
			J1F(count, result, idx_r); // first starting in idx_r
			//clear up the rest
			while (idx_r > idx_tmp && count) //result has more setted values
			{
				J1U(count, result, idx_r); //does not belong
				J1N(count, result, idx_r); //next
			}
			J1FA(count, tmp);
		}

	}


	Yap_ClauseListInit(&clauselist);
	idx_r = 0L;
	J1F(count, result, idx_r);
	while (count)
	{
		x = (yamop **) utarray_eltptr(info->clauselist, idx_r - 1);
//		fprintf(stderr,"Clausule %d of %d: %p\n",
//				idx_r, utarray_len(info->clauselist),
//				*x);
		Yap_ClauseListExtend(
				&clauselist,
				*x,
				info->p);
		J1N(count, result, idx_r);
	}
	J1FA(count,result);
//	fprintf(stderr,"J1 used space %d bytes for %d clausules\n",
//			count, Yap_ClauseListCount(&clauselist));
	Yap_ClauseListClose(&clauselist);

	if (Yap_ClauseListCount(&clauselist) == 0)
	{
		Yap_ClauseListDestroy(&clauselist);
//		fprintf(stderr,"Search Not needed\n");
		return NULL; /*FAIL CODE handled before*/
	}
	if (Yap_ClauseListCount(&clauselist) == 1)
	{
//		fprintf(stderr,"Returning 1 value\n");
		return Yap_ClauseListToClause(&clauselist);
	}
//	fprintf(stderr,"Returning Multiple values (%d)\n", Yap_ClauseListCount(&clauselist));
	return Yap_ClauseListCode(&clauselist);
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
