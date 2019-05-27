#include "YapInterface.h"
#include <lbfgs.h>
#include <stdio.h>
#include <string.h>

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
#define LBFGS_STATUS_NONE 0
#define LBFGS_STATUS_INITIALIZED 1
#define LBFGS_STATUS_RUNNING 2
#define LBFGS_STATUS_CB_EVAL 3
#define LBFGS_STATUS_CB_PROGRESS 4

static lbfgs_parameter_t parms;

X_API void init_lbfgs_predicates(void);

YAP_Functor fevaluate, fprogress, fmodule, ffloats;
YAP_Term tuser;

lbfgsfloatval_t *x_p, f_x;

static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x,
                                lbfgsfloatval_t *g_tmp, const int n,
                                const lbfgsfloatval_t step) {
  YAP_Term call;
  YAP_Bool result;
  lbfgsfloatval_t rc=0.0;
  YAP_Term t12;
  YAP_Term t[6], t2[2];

  YAP_Term t_0 = YAP_MkIntTerm((YAP_Int)&rc);
  t[0] = YAP_MkApplTerm(ffloats, 1, &t_0);
  YAP_Term t_1 = YAP_MkIntTerm((YAP_Int)x);
  t[1] = YAP_MkApplTerm(ffloats, 1, &t_1);
  t12 = YAP_MkIntTerm((YAP_Int)g_tmp);
  t[2] = YAP_MkApplTerm(ffloats, 1, &t12);
  t[3] = YAP_MkIntTerm(n);
  t[4] = YAP_MkFloatTerm(step);
  t[5] = YAP_MkIntTerm((YAP_Int)instance);

  t2[0] = tuser;
  t2[1] = YAP_MkApplTerm(fevaluate, 6, t);

  call = YAP_MkApplTerm(fmodule, 2, t2);
  // lbfgs_status=LBFGS_STATUS_CB_EVAL;
  result = YAP_RunGoalOnce(call);
  // lbfgs_status=LBFGS_STATUS_RUNNING;

  if (result == FALSE) {
    printf("ERROR: the evaluate call failed in YAP.\n");
    // Goal did not succeed
    return FALSE;
  }
   YAP_ShutdownGoal(true);
  return rc;
}

static int progress(void *instance, const lbfgsfloatval_t *local_x,
                    const lbfgsfloatval_t *local_g,

                    const lbfgsfloatval_t fx, const lbfgsfloatval_t xnorm,
                    const lbfgsfloatval_t gnorm, const lbfgsfloatval_t step,
                    int n, int k, int ls) {
  YAP_Term call;
  YAP_Bool result;
  YAP_Int s1;

  YAP_Term t[10], t2[2], v;
  t[0] = YAP_MkFloatTerm(fx);
  t[1] = YAP_MkIntTerm((YAP_Int)local_x);
  t[1] = YAP_MkApplTerm(ffloats, 1, t + 1);
  t[2] = YAP_MkIntTerm((YAP_Int)local_g);
  t[2] = YAP_MkApplTerm(ffloats, 1, t + 2);
  t[3] = YAP_MkFloatTerm(xnorm);
  t[4] = YAP_MkFloatTerm(gnorm);
  t[5] = YAP_MkFloatTerm(step);
  t[6] = YAP_MkIntTerm(n);
  t[7] = YAP_MkIntTerm(k);
  t[8] = YAP_MkIntTerm(ls);
  t[9] = v = YAP_MkVarTerm();

  t2[0] = tuser;
  t2[1] = YAP_MkApplTerm(fprogress, 10, t);

  call = YAP_MkApplTerm(fmodule, 2, t2);
  s1 = YAP_InitSlot(v);

  // lbfgs_status=LBFGS_STATUS_CB_PROGRESS;
  result = YAP_RunGoalOnce(call);
  // lbfgs_status=LBFGS_STATUS_RUNNING;

  YAP_Term o = YAP_GetFromSlot(s1);

  if (result == FALSE) {
    printf("ERROR:  the progress call failed in YAP.\n");
    // Goal did not succeed
    return -1;
  }

  if (YAP_IsIntTerm(o)) {
    int v = YAP_IntOfTerm(o);
    YAP_ShutdownGoal(true);
   return (int)v;
  }
  YAP_ShutdownGoal(true);
  fprintf(stderr, "ERROR: The progress call back function did not return an "
                  "integer as last argument\n");
  return 1;
}

/** @pred lbfgs_initialize(+N,+Module,+Evaluate,+Progress)
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
lbfgs_initialize(1,user,evaluate,progress,e,g)</span>
~~~~


The evaluate call back predicate has to be of the type
`evaluate(-F,+N,+Step)`. It has to calculate the current function
value _F_. _N_ is the
size of the parameter vector (the value which was used to initialize
LBFGS) and _Step_ is the current state of the
line search. The call back predicate can access the current values of
`x[i]` by calling `lbfgs_get_x(+I,-Xi)`. Finally, the call back
predicate has to calculate the gradient of _F</span>_
and set its value by calling `lbfgs_set_g(+I,+Gi)` for every `1<=I<=N`.


The progress call back predicate has to be of the type
`progress(+F,+X_Norm,+G_Norm,+Step,+N,+Iteration,+LS,-Continue)`. It
is called after every iteration. The call back predicate can access
the current values of _X_ and of the gradient by calling
`lbfgs_get_x(+I,-Xi)` and `lbfgs_get_g`(+I,-Gi)`
respectively. However, it must not call the setter predicates for <span
class="code"_X_ or _G_. If it tries to do so, the lbfgs will
terminate with an error. If _Continue_ is set to 0 (int) the
optimization process will continue for one more iteration, any other
value will terminate the optimization process.
*/
/**
 * @pred lbfgs( N, X, U, FX )
 *
 * @Arg1 N: number of variables in problem
 * @Arg[X0]: input vector
 * @Arg[FX]: function value,
 * @Arg[FX]: parameter
 * @Arg[X0]: user data
 * @Arg[FX]: status
 */
static YAP_Bool p_lbfgs(void) {
  YAP_Term t1 = YAP_ARG1, t;
  int n;
  lbfgsfloatval_t *x;
  lbfgsfloatval_t fx;

  if (!YAP_IsIntTerm(t1)) {
    return false;
  }

  n = YAP_IntOfTerm(t1);

  if (n < 1) {
    return FALSE;
  }

  if (!x_p)
   x_p = lbfgs_malloc(n+1);
  x = x_p;
   t = YAP_MkIntTerm((YAP_Int)x);
   YAP_Unify(YAP_ARG2, YAP_MkApplTerm(ffloats, 1, &t));
  lbfgs_parameter_t *param = &parms;
  void *ui = NULL; //(void *)YAP_IntOfTerm(YAP_ARG4);
  int ret = lbfgs(n, x, &fx, evaluate, progress, ui, param);
  f_x = fx;
if (ret == 0)
    return true;
  fprintf(stderr, "optimization terminated with code %d\n ",ret);
  return true;
}

static YAP_Bool lbfgs_fx(void) {
    return YAP_Unify(YAP_ARG1, YAP_MkFloatTerm(f_x));
}

static YAP_Bool lbfgs_grab(void) {
  int n = YAP_IntOfTerm(YAP_ARG1);

  if (n < 1) {
    return FALSE;
  }
  lbfgsfloatval_t *x = lbfgs_malloc(n);
  YAP_Term t = YAP_MkIntTerm((YAP_Int)x);
  return YAP_Unify(YAP_ARG2, YAP_MkApplTerm(ffloats, 1, &t));
}


static YAP_Bool lbfgs_release(void) {
  /* if (lbfgs_status == LBFGS_STATUS_NONE) { */
  /*    printf("Error: Lbfgs is not initialized.\n"); */
  /*    return FALSE; */
  /* } */

  /* if (lbfgs_status == LBFGS_STATUS_INITIALIZED) { */
  lbfgs_free((lbfgsfloatval_t *)YAP_IntOfTerm(YAP_ArgOfTerm(1, (YAP_ARG1))));

  return TRUE;
  /* return FALSE; */
}
static YAP_Bool lbfgs_defaults(void) {

  lbfgs_parameter_init(&parms);
  return TRUE;
  /* return FALSE; */
}

/** @pred  lbfgs_set_parameter(+Name,+Value,+Parameters)
Set the parameter Name to Value. Only possible while the lbfgs
is not running.
*/
static YAP_Bool lbfgs_set_parameter(void) {
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;
  lbfgs_parameter_t *param = &parms;
  /* if (lbfgs_status != LBFGS_STATUS_NONE && lbfgs_status !=
   * LBFGS_STATUS_INITIALIZED){ */
  /*   printf("ERROR: Lbfgs is running right now. Please wait till it is
   * finished.\n"); */
  /*   return FALSE; */
  /* } */

  if (!YAP_IsAtomTerm(t1)) {
    return FALSE;
  }

  const char *name = YAP_AtomName(YAP_AtomOfTerm(t1));

  if ((strcmp(name, "m") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->m = YAP_IntOfTerm(t2);
  } else if ((strcmp(name, "epsilon") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->epsilon = v;
  } else if ((strcmp(name, "past") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->past = YAP_IntOfTerm(t2);
  } else if ((strcmp(name, "delta") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->delta = v;
  } else if ((strcmp(name, "max_iterations") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->max_iterations = YAP_IntOfTerm(t2);
  } else if ((strcmp(name, "linesearch") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->linesearch = YAP_IntOfTerm(t2);
  } else if ((strcmp(name, "max_linesearch") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->max_linesearch = YAP_IntOfTerm(t2);
  } else if ((strcmp(name, "min_step") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->min_step = v;
  } else if ((strcmp(name, "max_step") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->max_step = v;
  } else if ((strcmp(name, "ftol") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->ftol = v;
  } else if ((strcmp(name, "gtol") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->gtol = v;
  } else if ((strcmp(name, "xtol") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->xtol = v;
  } else if ((strcmp(name, "orthantwise_c") == 0)) {
    lbfgsfloatval_t v;

    if (YAP_IsFloatTerm(t2)) {
      v = YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v = (lbfgsfloatval_t)YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param->orthantwise_c = v;
  } else if ((strcmp(name, "orthantwise_start") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->orthantwise_start = YAP_IntOfTerm(t2);
  } else if ((strcmp(name, "orthantwise_end") == 0)) {
    if (!YAP_IsIntTerm(t2)) {
      return FALSE;
    }
    param->orthantwise_end = YAP_IntOfTerm(t2);
  } else {
    printf("ERROR: The parameter %s is unknown.\n", name);
    return FALSE;
  }

  return TRUE;
}

/** @pred lbfgs_get_parameter(+Name,-Value)</h3>
Get the current Value for Name
*/

static YAP_Bool lbfgs_get_parameter(void) {
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;
   lbfgs_parameter_t *param = &parms;

   if (!YAP_IsAtomTerm(t1)) {
    return FALSE;
  }

  const char *name = YAP_AtomName(YAP_AtomOfTerm(t1));

  if ((strcmp(name, "m") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->m));
  } else if ((strcmp(name, "epsilon") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->epsilon));
  } else if ((strcmp(name, "past") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->past));
  } else if ((strcmp(name, "delta") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->delta));
  } else if ((strcmp(name, "max_iterations") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->max_iterations));
  } else if ((strcmp(name, "linesearch") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->linesearch));
  } else if ((strcmp(name, "max_linesearch") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->max_linesearch));
  } else if ((strcmp(name, "min_step") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->min_step));
  } else if ((strcmp(name, "max_step") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->max_step));
  } else if ((strcmp(name, "ftol") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->ftol));
  } else if ((strcmp(name, "gtol") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->gtol));
  } else if ((strcmp(name, "xtol") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->xtol));
  } else if ((strcmp(name, "orthantwise_c") == 0)) {
    return YAP_Unify(t2, YAP_MkFloatTerm(param->orthantwise_c));
  } else if ((strcmp(name, "orthantwise_start") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->orthantwise_start));
  } else if ((strcmp(name, "orthantwise_end") == 0)) {
    return YAP_Unify(t2, YAP_MkIntTerm(param->orthantwise_end));
  }

  printf("ERROR: The parameter %s is unknown.\n", name);
  return false;
}

X_API void init_lbfgs_predicates(void) {
  fevaluate = YAP_MkFunctor(YAP_LookupAtom("evaluate"), 6);
  fprogress = YAP_MkFunctor(YAP_LookupAtom("progress"), 10);
  fmodule = YAP_MkFunctor(YAP_LookupAtom(":"), 2);
  ffloats = YAP_MkFunctor(YAP_LookupAtom("floats"), 1);
  tuser = YAP_MkAtomTerm(YAP_LookupAtom("user"));

  // Initialize the parameters for the L-BFGS optimization.
  lbfgs_parameter_init(&parms);

  YAP_UserCPredicate("lbfgs_grab", lbfgs_grab, 2);
  YAP_UserCPredicate("lbfgs", p_lbfgs, 2);
    YAP_UserCPredicate("lbfgs_release", lbfgs_release, 1);
    YAP_UserCPredicate("lbfgs_fx", lbfgs_fx, 1);

  YAP_UserCPredicate("lbfgs_defaults", lbfgs_defaults, 0);
  
  YAP_UserCPredicate("lbfgs_set_parameter", lbfgs_set_parameter, 2);
  YAP_UserCPredicate("lbfgs_get_parameter", lbfgs_get_parameter, 2);
}
