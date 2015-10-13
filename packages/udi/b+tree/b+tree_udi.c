#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <float.h>

#include "b+tree_udi.h"

static struct udi_control_block BtreeCB;

void udi_btree_init(void) {
	UdiControlBlock cb = &BtreeCB;

	memset((void *) cb,0, sizeof(*cb));

	cb->decl=YAP_LookupAtom(SPEC);

	cb->init=BtreeUdiInit;
	cb->insert=BtreeUdiInsert;
	cb->search=BtreeUdiSearch;
	cb->destroy=BtreeUdiDestroy;

	Yap_UdiRegister(cb);
}
void *BtreeUdiInit (YAP_Term spec, int arg, int arity)
{
	return (void *) BTreeNew();
}

void *BtreeUdiInsert (void *control,
		YAP_Term term, int arg, void *data)
{
  btree_t btree = (btree_t) control;
  assert(control);

  BTreeInsert(&btree,
              YAP_FloatOfTerm(YAP_ArgOfTerm(arg,term)),
              data);

  return (void *) btree;
}

/*ARGS ARE AVAILABLE*/
int BtreeUdiSearch (void *control,
		int arg, Yap_UdiCallback callback, void *args)
{
  int j;
  size_t n;
  YAP_Term Constraints;
  const char * att;

  YAP_Term t = YAP_A(arg);
  if (YAP_IsAttVar(t))
      {
        Constraints = YAP_AttsOfVar(t);
        /* Yap_DebugPlWrite(Constraints); */
        att = YAP_AtomName(YAP_NameOfFunctor(YAP_FunctorOfTerm(Constraints)));
 
        n = sizeof (att_func) / sizeof (struct Att);
        for (j = 0; j < n; j ++)
          if (strcmp(att_func[j].att,att) == 0) /*TODO: Improve this do not need strcmp*/
            return att_func[j].proc_att(control, Constraints, callback, args);
      }
  return -1; /*YAP FALLBACK*/
}

/*Needs to test if tree is not null*/
int BTreeMinAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  void *d;
  d = BTreeMin(tree,NULL,NULL);
  callback(d,d,args);
  /*TODO: test empty tree*/
  return 1;
}

int BTreeMaxAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
	void *d;
	d = BTreeMax(tree,NULL,NULL);
	callback(d,d,args);
	/*TODO: test empty tree*/
	return 1;
}

int BTreeEqAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  node_t n;
  int i;
  double r;
  void * d;

  r = YAP_FloatOfTerm(YAP_ArgOfTerm(2,constraint));

  d = BTreeSearch(tree,r,EQ,&n,&i);
  if (d) {
	callback(d,d,args);
	return 1;
  }
  return 0;
}

int BTreeLtAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  node_t n;
  int i;
  double max;
  void * d;
  int count = 0;

  max = YAP_FloatOfTerm(YAP_ArgOfTerm(2,constraint));

  d = BTreeMin(tree,&n,&i);
  if (d)
	  do {
		  callback(d,d,args);
		  count ++;
	  } while ((d = BTreeSearchNext(max,LT,&n,&i)));

  return count;
}

int BTreeLeAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  node_t n;
  int i;
  double max;
  void * d;
  int count = 0;

  max = YAP_FloatOfTerm(YAP_ArgOfTerm(2,constraint));

  d = BTreeMin(tree,&n,&i);
  if (d)
	  do {
		  callback(d,d,args);
		  count ++;
	  } while ((d = BTreeSearchNext(max,LE,&n,&i)));

  return count;
}

int BTreeGtAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  node_t n;
  int i;
  double min;
  void * d;
  int count = 0;

  min = YAP_FloatOfTerm(YAP_ArgOfTerm(2,constraint));

  d = BTreeSearch(tree,min,GT,&n,&i);
  if (d)
	  do {
		  callback(d,d,args);
		  count ++;
	  }  while ((d = BTreeSearchNext(DBL_MAX,LT,&n,&i)));

  return count;
}

int BTreeGeAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  node_t n;
  int i;
  double min;
  void * d;
  int count = 0;

  min = YAP_FloatOfTerm(YAP_ArgOfTerm(2,constraint));

  d = BTreeSearch(tree,min,GE,&n,&i);
  if (d)
	  do {
	  		  callback(d,d,args);
	  		  count ++;
	  	  }  while ((d = BTreeSearchNext(DBL_MAX,LT,&n,&i)));

  return count;
}

int BTreeRangeAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args)
{
  node_t n;
  int i;
  double min,max;
  int minc,maxc;
  void * d;
  int count = 0;

  min = YAP_FloatOfTerm(YAP_ArgOfTerm(2,constraint));
  minc = strcmp(YAP_AtomName(YAP_AtomOfTerm(YAP_ArgOfTerm(3,constraint))),
                "true") == 0 ? GE: GT;
  max = YAP_FloatOfTerm(YAP_ArgOfTerm(4,constraint));
  maxc = strcmp(YAP_AtomName(YAP_AtomOfTerm(YAP_ArgOfTerm(5,constraint))),
                "true") == 0 ? LE: LT;

  d = BTreeSearch(tree,min,minc,&n,&i);
  if (d)
	  do {
		  callback(d,d,args);
		  count ++;
	  } while ((d = BTreeSearchNext(max,maxc,&n,&i)));

  return count;
}

int BtreeUdiDestroy(void *control)
{
	btree_t btree = (btree_t) control;

	assert(btree);

	BTreeDestroy(btree);

	return TRUE;
}
