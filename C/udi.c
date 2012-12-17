#include <stdio.h>
#include "Yap.h"
#include "YapInterface.h"
#include "clause.h"
#include "udi.h"
#include "utarray.h"

/* to keep a vector of udi indexers */
UT_icd udicb_icd = {sizeof(UdiControlBlock), NULL, NULL, NULL};
UT_array *indexing_structures;

/*
 * New user indexed predicate:
 *   first argument is the decl term.
 *   second argument is the init call with the structure
 */
void
Yap_UdiRegister(UdiControlBlock cb){
	/*TODO: check structure integrity and duplicates */
	utarray_push_back(indexing_structures, &cb);
}

struct udi_p_args {
	void *idxstr; //user indexing structure
	UdiControlBlock control; //user indexing structure functions
};

/* a pointer utarray list
 * This is a hack, becouse I do no know the real type of clauses*/
UT_icd ptr_icd = {sizeof(void *), NULL, NULL, NULL };

#define UDI_MI 10

/******
    All the info we need to enter user indexed code:
	right now, this is just a linked list....
******/
typedef struct udi_info
{
  PredEntry *p;                //predicate (need to identify asserts)
  UT_array *clauselist;        //clause list used on returns
  struct udi_p_args args[UDI_MI];  //indexed args only the first UDI_MI
  struct udi_info *next;
} *UdiInfo;

int Yap_udi_args_init(Term spec, int arity, UdiInfo blk);

/******
      we now have one extra user indexed predicate. We assume these
      are few, so we can do with a linked list.
******/
//static int
//add_udi_block(PredEntry *p, void *info,  UdiControlBlock cmd)
//{
//  UdiInfo blk = (UdiInfo) Yap_AllocCodeSpace(sizeof(struct udi_info));
//  if (!blk)
//    return FALSE;
//  blk->next = UdiControlBlocks;
//  UdiControlBlocks = blk;
//  blk->p = p;
//  blk->functions = cmd;
//  blk->cb = info;
//  return TRUE;
//}

/*
 * New user indexed predicate:
 * the first argument is the term.
 */
static Int
p_new_udi( USES_REGS1 )
{
  Term spec = Deref(ARG1);

  PredEntry *p;
//  UdiControlBlock cmd;
//  Atom udi_t;
  UdiInfo blk;
  int info;

  fprintf(stderr,"new pred\n");

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
  if (!blk) {
	  Yap_Error(OUT_OF_HEAP_ERROR, spec, "new user index/1");
	  return FALSE;
  }
  blk->next = UdiControlBlocks;
  blk->clauselist = NULL;
  blk->p = p;

  info = Yap_udi_args_init(spec, p->ArityOfPE, blk);
  if (!info)
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

	fprintf(stderr,"udi init\n");
	for (i = 1; i <= arity && i <= UDI_MI ; i++) {
		blk->args[i-1].idxstr = NULL;
		blk->args[i-1].control = NULL;
		fprintf(stderr,"%d\n",i);
		arg = ArgOfTerm(i,spec);
		if (IsAtomTerm(arg)) {
			idxtype = AtomOfTerm(arg);
			fprintf(stderr,"%p-%s %p-%s\n",idxtype, YAP_AtomName(idxtype),
						                   AtomMinus, YAP_AtomName(AtomMinus));
			if (idxtype == AtomMinus)
				continue;
			p = NULL;
			while ((p = (UdiControlBlock *) utarray_next(indexing_structures, p))) {
				fprintf(stderr,"cb: %p %p-%s\n", *p, (*p)->decl, YAP_AtomName((*p)->decl));
				if (idxtype == (*p)->decl){
					blk->args[i-1].idxstr = NULL;
					blk->args[i-1].control = *p;
				}
			}
			if (blk->args[i-1].control == NULL){ /* not "-" and not found*/
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
	/* find our structure*/
	struct udi_info *info = UdiControlBlocks;
	while (info->p != p && info)
		info = info->next;
	if (!info)
		return FALSE;

	/* do the actual insertion */
	fprintf(stderr,"udi insert\n");
//	info->cb = info->functions->insert(t, info->cb, (void *)cl);
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
	return NULL; /*info->functions->search(info->cb);*/
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

  /*init indexing structures array*/
  utarray_new(indexing_structures, &udicb_icd);

  Yap_InitCPred("$udi_init", 1, p_new_udi, 0);
  /* TODO: decide if yap.udi should be loaded automaticaly in init.yap */
}

