#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <YapInterface.h>

#include "Yap.h"

#include "rtree.h"
#include "clause_list.h"
#include "rtree_udi_i.h"
#include "rtree_udi.h"

static int YAP_IsNumberTermToFloat (Term term, YAP_Float *n)
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

static rect_t RectOfTerm (Term term)
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

control_t *RtreeUdiInit (Term spec,
                         void * pred,
                         int arity){
  control_t *control;
  YAP_Term arg;
  int i, c;
  /*  YAP_Term mod;  */

  /*  spec = Yap_StripModule(spec, &mod); */
  if (! YAP_IsApplTerm(spec))
    return (NULL);

  control = (control_t *) malloc (sizeof(*control));
  assert(control);
  memset((void *) control,0, sizeof(*control));

  c = 0;
  for (i = 1; i <= arity; i ++)
    {
      arg = YAP_ArgOfTerm(i,spec);
      if (YAP_IsAtomTerm(arg)
          && strcmp("+",YAP_AtomName(YAP_AtomOfTerm(arg))) == 0)
        {
          
          (*control)[c].pred = pred;
          (*control)[c++].arg = i;

        }
    }

/*  for (i = 0; i < NARGS; i++)
    printf("%d,%p\t",(*control)[i].arg,(*control)[i].tree);
  printf("\n"); */
  
  return control;
}

control_t *RtreeUdiInsert (Term term,control_t *control,void *clausule)
{
  int i;
  rect_t r;

  assert(control);

  for (i = 0; i < NARGS && (*control)[i].arg != 0 ; i++)
    {
      r = RectOfTerm(YAP_ArgOfTerm((*control)[i].arg,term));
      if (!(*control)[i].tree)
        (*control)[i].tree = RTreeNew();
      RTreeInsert(&(*control)[i].tree,r,clausule);
    }

  /*  printf("insert %p\n", clausule); */

  return (control);
}

static int callback(rect_t r, void *data, void *arg)
{
  callback_m_t x;
  x = (callback_m_t) arg;
  return Yap_ClauseListExtend(x->cl,data,x->pred);
}

/*ARGS ARE AVAILABLE*/
void *RtreeUdiSearch (control_t *control)
{
  rect_t r;
  int i;
  struct ClauseList clauselist;
  struct CallbackM cm;
  callback_m_t c;
  YAP_Term Constraints;

  /*RTreePrint ((*control)[0].tree);*/

  for (i = 0; i < NARGS && (*control)[i].arg != 0 ; i++)
    if (YAP_IsAttVar(YAP_A((*control)[i].arg)))
      {

        /*get the constraits rect*/
        Constraints = YAP_AttsOfVar(YAP_A((*control)[i].arg));
        /*        Yap_DebugPlWrite(Constraints); */
        r = RectOfTerm(YAP_ArgOfTerm(2,Constraints));

        c = &cm;
        c->cl = Yap_ClauseListInit(&clauselist);
        c->pred = (*control)[i].pred;
        if (!c->cl)
          return NULL; /*? or fail*/
        RTreeSearch((*control)[i].tree, r, callback, c);
        Yap_ClauseListClose(c->cl);

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
  
  return NULL; /*YAP FALLBACK*/
}

int RtreeUdiDestroy(control_t *control)
{
  int i;

  assert(control);

  for (i = 0; i < NARGS && (*control)[i].arg != 0; i++)
    {
      if ((*control)[i].tree)
        RTreeDestroy((*control)[i].tree);
    }

  free(control);
  control = NULL;

  return TRUE;
}
