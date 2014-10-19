#include <stdio.h>
#include <assert.h>
#include "Yap.h"
#include "YapInterface.h"
#include "clause.h"
#include "udi_private.h"

/* to keep an array with the registered udi indexers */
UT_icd udicb_icd = {sizeof(UdiControlBlock), NULL, NULL, NULL};
UT_array *indexing_structures;

/*
 * Register a new user indexer
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

  /*Init UdiInfo */
  utarray_new(blk->args, &arg_icd);
  utarray_new(blk->clauselist, &cl_icd);
  blk->p = p;

  /*Now Init args list*/
  info = p_udi_args_init(spec, p->ArityOfPE, blk);
  if (!info)
  {
	  utarray_free(blk->args);
	  utarray_free(blk->clauselist);
	  Yap_FreeCodeSpace((char *) blk);
	  return FALSE;
  }

  /*Push into the hash*/
  HASH_ADD_UdiInfo(UdiControlBlocks, p, blk);

  p->PredFlags |= UDIPredFlag;

  return TRUE;
}

/*
 * Here we initialize the arguments indexing
 */
YAP_Int
p_udi_args_init(Term spec, int arity, UdiInfo blk)
{
	int i;
	Term arg;
	Atom idxtype;
	UdiControlBlock *cb;
	struct udi_p_args p_arg;

	for (i = 1; i <= arity; i++) {
		arg = ArgOfTerm(i,spec);
		if (IsAtomTerm(arg)) {
			idxtype = AtomOfTerm(arg);
			if (idxtype == AtomMinus) //skip this argument
				continue;
			p_arg.control = NULL;
			cb = NULL;
			while ((cb = (UdiControlBlock *) utarray_next(indexing_structures, cb))) {
				if (idxtype == (*cb)->decl){
					p_arg.arg = i;
					p_arg.control = *cb;
					p_arg.idxstr = (*cb)->init(spec, i, arity);
					utarray_push_back(blk->args, &p_arg);
				}
			}
			if (p_arg.control == NULL){ /* not "-" and not found */
				fprintf(stderr, "Invalid Spec (%s)\n", AtomName(idxtype));
				return FALSE;
			}
		}
	}
	return TRUE;
}

/*
 * From now on this is called in several places of yap
 * when the predicate has the UDIPredFlag
 * and is what actually triggers the insert/search/abolish of indexing structures
 */

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

/* called from cdmgr.c
 *
 * for each assert of a udipredicate
 * to pass info to user structure
 */
int
Yap_new_udi_clause(PredEntry *p, yamop *cl, Term t)
{
	int i;
	UdiPArg parg;
	UdiInfo info;
	YAP_Int index;

	/* try to find our structure */
	HASH_FIND_UdiInfo(UdiControlBlocks,p,info);
	if (!info)
		return FALSE;

	/* insert into clauselist */
	utarray_push_back(info->clauselist, &cl);

	for (i = 0; i < utarray_len(info->args) ; i++) {
		parg = (UdiPArg) utarray_eltptr(info->args,i);
		index = (YAP_Int) utarray_len(info->clauselist);
		parg->idxstr = parg->control->insert(parg->idxstr, t,
											 parg->arg,
											 (void *) index);
	}
	return TRUE;
}

/* index, called from absmi.c
 *
 * Returns:
 *    NULL (yap fallback)       No usable indexing available
 *
 *    Yap_FAILCODE() (fail)     No result found
 *    Yap_CauseListToClause(cl) 1 solution found
 *    Yap_ClauseListCode(cl)    2+ solutions found
 */
yamop *
Yap_udi_search(PredEntry *p)
{
	int r;
	struct ClauseList clauselist;
	UdiPArg parg;
	UdiInfo info;

	/* find our structure*/
	HASH_FIND_UdiInfo(UdiControlBlocks,p,info);
	if (!info || utarray_len(info->args) == 0)
		return NULL;

	if (utarray_len(info->args) == 1){ //simple case no intersection needed
		struct si_callback_h c;

		c.cl = Yap_ClauseListInit(&clauselist);
		c.clauselist = info->clauselist;
		c.pred = info->p;
		if (!c.cl)
			return NULL;

		parg = (UdiPArg) utarray_eltptr(info->args,0);
		r = parg->control->search(parg->idxstr, parg->arg, si_callback, (void *) &c);
		Yap_ClauseListClose(c.cl);

		if (r == -1) {
			Yap_ClauseListDestroy(c.cl);
			return NULL;
		}

		if (Yap_ClauseListCount(c.cl) == 0) {
			Yap_ClauseListDestroy(c.cl);
			return Yap_FAILCODE();
		}
	} else {//intersection needed using Judy1
#ifdef USE_JUDY
		/*TODO: do more tests to this algorithm*/
		int i;
		Pvoid_t tmp = (Pvoid_t) NULL;
		Pvoid_t result = (Pvoid_t) NULL;
		Word_t count = 0L;
		Word_t idx_r = 0L;
		Word_t idx_tmp = 0L;
		int rc = 0;
		yamop **x;

		/*
		 * I will start with the simplest approach
		 * for each index create a set and intersect it with the
		 * next
		 *
		 * In the future it could pay to sort according to index type
		 * to improve intersection part
		 */
		for (i = 0; i < utarray_len(info->args) ; i++) {
			parg = (UdiPArg) utarray_eltptr(info->args,i);
			r = parg->control->search(parg->idxstr, parg->arg, j1_callback, &tmp);
			if (r == -1) /*this arg does not prune search*/
				continue;
			rc ++;
			J1C(count, result, 0, -1);
			if (r == 0) /* this arg gave 0 results -> FAIL*/
			{
				if (count > 0) // clear previous result if they exists
					J1FA(count, result);
				return Yap_FAILCODE();
			}

			if (count == 0) // first result_set
			{
				result = tmp;
				tmp = (Pvoid_t) NULL;
			}
			else /*intersection*/
			{
				idx_tmp = 0L;
				idx_r = 0L;
				J1F(count, result, idx_r); //succeeds one time at least
				assert(count > 0);
				J1F(count, tmp, idx_tmp);  //succeeds one time at least
				assert(count > 0);
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
						//if (! count) break;      //end tmp set will break while
					}
					else // (idx_r > idx_tmp)
					{
						idx_tmp = idx_r; // fast forward
						J1F(count, tmp, idx_tmp); // first starting in idx_r
						//if (! count) break; //end tmp set will break while
					}
				}
				J1F(count, result, idx_r); // first starting in idx_r
				//clear up the rest
				while (idx_r > idx_tmp && count) //result has more setted values
				{
					J1U(count, result, idx_r); //does not belong
					J1N(count, result, idx_r); //next
				}
				J1FA(count, tmp); //free tmp
			}
		}
		if (rc == 0) /*no search performed*/
			return NULL;

		J1C(count, result, 0, -1);
		if (count == 0) { /*result set empty -> FAIL */
			J1FA(count, result);
			return Yap_FAILCODE();
		}

		/*convert Juddy1 to clauselist*/
		Yap_ClauseListInit(&clauselist);
		idx_r = 0L;
		J1F(count, result, idx_r);
		while (count)
		{
			x = (yamop **) utarray_eltptr(info->clauselist, idx_r - 1);
			Yap_ClauseListExtend(
					&clauselist,
					*x,
					info->p);
			J1N(count, result, idx_r);
		}
		J1FA(count,result);
			fprintf(stderr,"J1 used space %ld bytes for %d clausules\n",
					count, Yap_ClauseListCount(&clauselist));
		Yap_ClauseListClose(&clauselist);
#else
		fprintf(stderr,"Without libJudy only one argument indexed is allowed."
				"Falling back to Yap Indexing\n");
		return NULL; //NO Judy Available
#endif
	}

	if (Yap_ClauseListCount(&clauselist) == 1)
		return Yap_ClauseListToClause(&clauselist);
	return Yap_ClauseListCode(&clauselist);
}

/* index, called from absmi.c */
void
Yap_udi_abolish(PredEntry *p)
{
	/* tell the predicate destroy */
}
