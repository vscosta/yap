#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "rtree_udi.h"

static struct udi_control_block RtreeCB;

void udi_rtree_init(void) {
	UdiControlBlock cb = &RtreeCB;

	memset((void *) cb,0, sizeof(*cb));

	cb->decl=YAP_LookupAtom(SPEC);

	cb->init=RtreeUdiInit;
	cb->insert=RtreeUdiInsert;
	cb->search=RtreeUdiSearch;
	cb->destroy=RtreeUdiDestroy;

	Yap_UdiRegister(cb);
}

static int YAP_IsNumberTermToFloat (YAP_Term term, YAP_Float *n)
{
  if (YAP_IsIntTerm (term) != FALSE)
  {
    if (n != NULL)
      *n = (YAP_Float) YAP_IntOfTerm (term);
    return (TRUE);
  }
  if (YAP_IsFloatTerm (term) != FALSE)
  {
    if (n != NULL)
      *n = YAP_FloatOfTerm (term);
    return (TRUE);
  }
  return (FALSE);
}

static rect_t RectOfTerm (YAP_Term term)
{
  YAP_Term tmp;
  rect_t rect;
  int i;
  
  if (!YAP_IsPairTerm(term))
    return (RectInit());
  
  for (i = 0; YAP_IsPairTerm(term) && i < 4; i++)
    {
      tmp = YAP_HeadOfTerm (term);
      if (!YAP_IsNumberTermToFloat(tmp,&(rect.coords[i])))
        return (RectInit());
      term = YAP_TailOfTerm (term);
    }

  return (rect);
}

void *
RtreeUdiInit (YAP_Term spec, int arg, int arity) {
	return (void *) RTreeNew();
}

void *
RtreeUdiInsert (void *control,
		YAP_Term term, int arg, void *data)
{
  rect_t r;
  rtree_t rtree = (rtree_t) control;

  assert(rtree);

  /*TODO: better check of rect, or even not needing it
   * and use the geometry itself */
  r = RectOfTerm(YAP_ArgOfTerm(arg,term));
  RTreeInsert(&rtree, r, data);

  return (void *) rtree;
}

/*ARGS ARE AVAILABLE*/
int RtreeUdiSearch (void *control,
		int arg, Yap_UdiCallback callback, void *args)
{
  int i;
  rtree_t rtree = (rtree_t) control;
  YAP_Term Constraints;
  rect_t r;

  assert(rtree);

  YAP_Term t = YAP_A(arg);
  if (YAP_IsAttVar(t))
  {
	  /*get the constraits rect*/
	  Constraints = YAP_AttsOfVar(t);
//        Yap_DebugPlWrite(Constraints);
	  if (YAP_IsApplTerm(Constraints))
	  {
		  r = RectOfTerm(YAP_ArgOfTerm(2,Constraints));

		  return RTreeSearch(rtree, r, callback, args);
      }
  }

  return -1; /*YAP FALLBACK*/
}

int RtreeUdiDestroy(void *control)
{
  rtree_t rtree = (rtree_t) control;

  assert(rtree);

  RTreeDestroy(rtree);

  return TRUE;
}
