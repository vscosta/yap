#include <string.h>
#include "YapInterface.h"
#include <lbfgs.h>
#include <stdio.h>

/*
  This file is part of YAP-LBFGS.
  Copyright (C) 2009 Bernd Gutmann

  YAP-LBFGS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  YAP-LBFGS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with YAP-LBFGS.  If not, see <http://www.gnu.org/licenses/>.
*/



// These constants describe the internal state
#define OPTIMIZER_STATUS_NONE        0
#define OPTIMIZER_STATUS_INITIALIZED 1
#define OPTIMIZER_STATUS_RUNNING     2
#define OPTIMIZER_STATUS_CB_EVAL     3
#define OPTIMIZER_STATUS_CB_PROGRESS 4

X_API void init_lbfgs_predicates( void ) ;

YAP_Functor fevaluate, fprogress, fmodule, ffloats;
YAP_Term tuser;

static lbfgsfloatval_t evaluate(
    void *instance,
    const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g_tmp,
    const int n,
    const lbfgsfloatval_t step
    )
{
  YAP_Term call;
  YAP_Term v, a1;
  YAP_Bool result;
  YAP_Int s1;

  YAP_Term t[5], t2[2];

  t[0] = v = YAP_MkVarTerm();
  t[1] = YAP_MkIntTerm((YAP_Int)x);
  t[1] = YAP_MkApplTerm(ffloats, 1, t+1);
  t[2] = YAP_MkIntTerm((YAP_Int)g_tmp);
  t[2] = YAP_MkApplTerm(ffloats, 1, t+2);
  t[3] = YAP_MkIntTerm(n);
  t[4] = YAP_MkFloatTerm(step);

  t2[0] = tuser;
  t2[1] = YAP_MkApplTerm(fevaluate, 5, t); 


  call = YAP_MkApplTerm( fmodule, 2, t2 );


  s1 = YAP_InitSlot(v);
  //optimizer_status=OPTIMIZER_STATUS_CB_EVAL;
  result=YAP_RunGoal(call);
  //optimizer_status=OPTIMIZER_STATUS_RUNNING;

  if (result==FALSE) {
    printf("ERROR: the evaluate call failed in YAP.\n");
    // Goal did not succeed
   YAP_ShutdownGoal( TRUE );
   return FALSE;
  }

  a1 = YAP_GetFromSlot( s1 );

  lbfgsfloatval_t  rc;
  if (YAP_IsFloatTerm(a1)) {
      rc =  (lbfgsfloatval_t) YAP_FloatOfTerm(a1);
  } else if (YAP_IsIntTerm(a1)) {
    rc = (lbfgsfloatval_t) YAP_IntOfTerm(a1);
  } else {
    fprintf(stderr, "ERROR: The evaluate call back function did not return a number as first argument.\n");
    rc = false;
  
  }

  YAP_ShutdownGoal( TRUE );
  return rc;
}

static int progress(
   void *instance,
    const lbfgsfloatval_t *local_x,
    const lbfgsfloatval_t *local_g,

    const lbfgsfloatval_t fx,
    const lbfgsfloatval_t xnorm,
    const lbfgsfloatval_t gnorm,
    const lbfgsfloatval_t step,
    int n,
    int k,
    int ls
    )
{
  YAP_Term call;
  YAP_Bool result;
  YAP_Int s1;

  YAP_Term t[9],t2[2], v;
  t[0] = YAP_MkFloatTerm(fx);
  t[1] = YAP_MkIntTerm((YAP_Int)local_x);
  t[1] = YAP_MkApplTerm(ffloats, 1, t+1);
  t[2] = YAP_MkIntTerm((YAP_Int)local_g);
  t[2] = YAP_MkApplTerm(ffloats, 1, t+2);
  t[3] = YAP_MkFloatTerm(xnorm);
  t[4] = YAP_MkFloatTerm(gnorm);
  t[5] = YAP_MkFloatTerm(step);
  t[6] = YAP_MkIntTerm(n);
  t[7] = YAP_MkIntTerm(k);
  t[8] = YAP_MkIntTerm(ls);
  t[9] = v = YAP_MkVarTerm();

  t2[0] = tuser;
  t2[1] = YAP_MkApplTerm( fprogress, 10, t);

  call = YAP_MkApplTerm( fmodule, 2, t2 );
  s1 = YAP_InitSlot(v);

  //optimizer_status=OPTIMIZER_STATUS_CB_PROGRESS;
  result=YAP_RunGoal(call);
  //optimizer_status=OPTIMIZER_STATUS_RUNNING;

  YAP_Term o = YAP_GetFromSlot( s1 );

  if (result==FALSE) {
   printf("ERROR:  the progress call failed in YAP.\n");
    // Goal did not succeed
      YAP_ShutdownGoal( TRUE );
      return -1;
  }

  if (YAP_IsIntTerm(o)) {
    int v = YAP_IntOfTerm(o);
    //YAP_ShutdownGoal( TRUE );
  return (int)v;
  }

  YAP_ShutdownGoal( TRUE );
  fprintf(stderr, "ERROR: The progress call back function did not return an integer as last argument\n");
  return 1;
}

/** @pred optimizer_initialize(+N,+Module,+Evaluate,+Progress)
Create space to optimize a function with _N_ variables (_N_ has to be
integer).

+ _Module</span>_ is the name of the module where the call back
predicates can be found,

+ _Evaluate_ is the call back predicate (arity 3)
to evaluate the function math <span class="math">_F</span>_,

+ _Progress_ is the call back predicate invoked
(arity 8) after every iteration

Example
~~~~
optimizer_initialize(1,user,evaluate,progress,e,g)</span>
~~~~


The evaluate call back predicate has to be of the type
`evaluate(-F,+N,+Step)`. It has to calculate the current function
value _F_. _N_ is the
size of the parameter vector (the value which was used to initialize
LBFGS) and _Step_ is the current state of the
line search. The call back predicate can access the current values of
`x[i]` by calling `optimizer_get_x(+I,-Xi)`. Finally, the call back
predicate has to calculate the gradient of _F</span>_
and set its value by calling `optimizer_set_g(+I,+Gi)` for every `1<=I<=N`.


The progress call back predicate has to be of the type
`progress(+F,+X_Norm,+G_Norm,+Step,+N,+Iteration,+LS,-Continue)`. It
is called after every iteration. The call back predicate can access
the current values of _X_ and of the gradient by calling
`optimizer_get_x(+I,-Xi)` and `optimizer_get_g`(+I,-Gi)`
respectively. However, it must not call the setter predicates for <span
class="code"_X_ or _G_. If it tries to do so, the optimizer will
terminate with an error. If _Continue_ is set to 0 (int) the
optimization process will continue for one more iteration, any other
value will terminate the optimization process.
*/
static YAP_Bool optimizer_initialize(void) {
  YAP_Term t1 = YAP_ARG1;
  int temp_n=0;
  lbfgsfloatval_t *temp_x, *temp_ox;
  lbfgs_parameter_t *temp_p;


  if (! YAP_IsIntTerm(t1)) {
    return false;
  }

  temp_n=YAP_IntOfTerm(t1);

  if (temp_n<1) {
    return FALSE;
  }

  temp_n = 16*(temp_n/16+15);
  lbfgs_parameter_init((temp_p=(lbfgs_parameter_t *)malloc(sizeof(lbfgs_parameter_t))));
  temp_ox = lbfgs_malloc(temp_n);
  YAP_Term tox = YAP_MkIntTerm((YAP_Int)temp_ox);
 temp_x = lbfgs_malloc(temp_n);
  YAP_Term tx = YAP_MkIntTerm((YAP_Int)temp_x);
  tx = YAP_MkApplTerm(ffloats, 1, &tx);
  tox = YAP_MkApplTerm(ffloats, 1, &tox);
  YAP_Term tp = YAP_MkIntTerm((YAP_Int)temp_p);

  
  return YAP_Unify(YAP_ARG2,tx) && YAP_Unify(YAP_ARG3,tox) &&  YAP_Unify(YAP_ARG4,tp) ;
}



/** @pred optimizer_run(-F,-Status)
Runs the optimization, _F is the best (minimal) function value and
Status (int) is the status code returned by libLBFGS. Anything except
0 indicates an error, see the documentation of libLBFGS for the
meaning.
*/
static YAP_Bool optimizer_run(void) {
  int ret = 0;
  int n =  YAP_IntOfTerm(YAP_ARG1);
  YAP_Int s1, s2;
  lbfgsfloatval_t fx;
  lbfgsfloatval_t *temp_x = ( lbfgsfloatval_t *)YAP_IntOfTerm( YAP_ArgOfTerm(1, YAP_ARG2)),
    *temp_ox = ( lbfgsfloatval_t *)  YAP_IntOfTerm(YAP_ArgOfTerm(1,YAP_ARG4));
  lbfgs_parameter_t *temp_p = (lbfgs_parameter_t * ) YAP_IntOfTerm(YAP_ARG6);
  ret = lbfgs(n, temp_x, &fx, evaluate, progress, temp_ox, temp_p);

  return YAP_Unify(YAP_MkIntTerm(ret), YAP_ARG5) &&
    YAP_Unify(YAP_MkFloatTerm(fx), YAP_ARG3);
}



static YAP_Bool optimizer_finalize( void ) {
  /* if (optimizer_status == OPTIMIZER_STATUS_NONE) { */
  /*    printf("Error: Optimizer is not initialized.\n"); */
  /*    return FALSE; */
  /* } */

  /* if (optimizer_status == OPTIMIZER_STATUS_INITIALIZED) { */
  lbfgs_free((void *)YAP_IntOfTerm(YAP_ArgOfTerm(1,YAP_ARG1)));
  lbfgs_free((void *)YAP_IntOfTerm(YAP_ArgOfTerm(1,YAP_ARG2)));
    lbfgs_free((void *)YAP_IntOfTerm(YAP_ARG3));

      return TRUE;
  /* } */

  /* printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n"); */
  /* return FALSE; */
 }



/** @pred  optimizer_set_parameter(+Name,+Value,+Parameters)
Set the parameter Name to Value. Only possible while the optimizer
is not running.
*/
static YAP_Bool optimizer_set_parameter( void ) {
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;
  lbfgs_parameter_t *param = (lbfgs_parameter_t *) YAP_IntOfTerm(YAP_ARG3);
  /* if (optimizer_status != OPTIMIZER_STATUS_NONE && optimizer_status != OPTIMIZER_STATUS_INITIALIZED){ */
  /*   printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n"); */
  /*   return FALSE; */
  /* } */


  if (! YAP_IsAtomTerm(t1)) {
    return FALSE;
  }

  const char* name=YAP_AtomName(YAP_AtomOfTerm(t1));

  if ((strcmp(name, "m") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->m = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "epsilon") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->epsilon=v;
  } else if  ((strcmp(name, "past") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->past = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "delta") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->delta=v;
  } else if  ((strcmp(name, "max_iterations") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->max_iterations = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "linesearch") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->linesearch = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "max_linesearch") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->max_linesearch = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "min_step") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->min_step=v;
  } else if  ((strcmp(name, "max_step") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->max_step=v;
  } else if  ((strcmp(name, "ftol") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->ftol=v;
  } else if  ((strcmp(name, "gtol") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->gtol=v;
  } else if  ((strcmp(name, "xtol") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->xtol=v;
  } else if  ((strcmp(name, "orthantwise_c") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->orthantwise_c=v;
  } else if  ((strcmp(name, "orthantwise_start") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->orthantwise_start = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "orthantwise_end") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param->orthantwise_end = YAP_IntOfTerm(t2);
  } else {
      printf("ERROR: The parameter %s is unknown.\n",name);
      return FALSE;
  }

  return TRUE;
}


/** @pred optimizer_get_parameter(+Name,-Value)</h3>
Get the current Value for Name
*/

static YAP_Bool optimizer_get_parameter( void ) {
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;
  lbfgs_parameter_t *param = (lbfgs_parameter_t *) YAP_IntOfTerm(YAP_ARG3);

  if (! YAP_IsAtomTerm(t1)) {
    return FALSE;
  }

  const char* name=YAP_AtomName(YAP_AtomOfTerm(t1));

  if ((strcmp(name, "m") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->m));
  } else if  ((strcmp(name, "epsilon") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->epsilon));
  } else if  ((strcmp(name, "past") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->past));
  } else if  ((strcmp(name, "delta") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->delta));
  } else if  ((strcmp(name, "max_iterations") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->max_iterations));
  } else if  ((strcmp(name, "linesearch") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->linesearch));
  } else if  ((strcmp(name, "max_linesearch") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->max_linesearch));
  } else if  ((strcmp(name, "min_step") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->min_step));
  } else if  ((strcmp(name, "max_step") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->max_step));
  } else if  ((strcmp(name, "ftol") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->ftol));
  } else if  ((strcmp(name, "gtol") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->gtol));
  } else if  ((strcmp(name, "xtol") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->xtol));
  } else if  ((strcmp(name, "orthantwise_c") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param->orthantwise_c));
  } else if  ((strcmp(name, "orthantwise_start") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->orthantwise_start));
  } else if  ((strcmp(name, "orthantwise_end") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param->orthantwise_end));
  }

  printf("ERROR: The parameter %s is unknown.\n",name);
  return false;
}





X_API void init_lbfgs_predicates( void )
{
  fevaluate = YAP_MkFunctor(YAP_LookupAtom("evaluate"), 5);
  fprogress = YAP_MkFunctor(YAP_LookupAtom("progress"), 10);
  fmodule = YAP_MkFunctor(YAP_LookupAtom(":"), 2);
  ffloats = YAP_MkFunctor(YAP_LookupAtom("floats"), 1);
  tuser = YAP_MkAtomTerm(YAP_LookupAtom("user"));

  //Initialize the parameters for the L-BFGS optimization.
  //  lbfgs_parameter_init(&param);


  YAP_UserCPredicate("optimizer_reserve_memory",optimizer_initialize,4);
  YAP_UserCPredicate("optimizer_run",optimizer_run,6);
  YAP_UserCPredicate("optimizer_free_memory",optimizer_finalize,3);

  YAP_UserCPredicate("optimizer_set_parameter",optimizer_set_parameter,3);
  YAP_UserCPredicate("optimizer_get_parameter",optimizer_get_parameter,3);
}
