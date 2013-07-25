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



int optimizer_status=OPTIMIZER_STATUS_NONE;   // the internal state
int n;                                        // the size of the parameter vector
lbfgsfloatval_t *x;                           // pointer to the parameter vector x[0],...,x[n-1]
lbfgsfloatval_t *g;                           // pointer to the gradient vector g[0],...,g[n-1]
lbfgs_parameter_t param;                      // the parameters used for lbfgs
char buffer [2048];                            // this buffer is used for creating the atoms to call

YAP_Functor fcall3, fprogress8, fmod2;

static lbfgsfloatval_t evaluate(
    void *instance,
    const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g_tmp,
    const int n,
    const lbfgsfloatval_t step
    )
{
  YAP_Term call;
  YAP_Bool result;
  YAP_Int s1;

  YAP_Term t[3];

  t[0] = YAP_MkVarTerm();
  t[1] = YAP_MkIntTerm(n);
  t[2] = YAP_MkFloatTerm(step);

  call = YAP_MkApplTerm(fcall3, 3, t);
  g=g_tmp;  

  
  s1 = YAP_InitSlot(call);
  optimizer_status=OPTIMIZER_STATUS_CB_EVAL;
  result=YAP_CallProlog(call);
  optimizer_status=OPTIMIZER_STATUS_RUNNING;

  if (result==FALSE) {
    printf("ERROR: Calling the evaluate call back function in YAP.\n");
    // Goal did not succeed
    return FALSE;
  }

  call = YAP_GetFromSlot( s1 );
  if (YAP_IsFloatTerm(YAP_ArgOfTerm(1,call))) {
    return (lbfgsfloatval_t) YAP_FloatOfTerm(YAP_ArgOfTerm(1,call));
  } else if (YAP_IsIntTerm(YAP_ArgOfTerm(1,call))) {
    return (lbfgsfloatval_t) YAP_IntOfTerm(YAP_ArgOfTerm(1,call));
  } 

  YAP_ShutdownGoal( TRUE );
  fprintf(stderr, "ERROR: The evaluate call back function did not return a number as first argument.\n");
  return 0;
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

  YAP_Term t[8];
  t[0] = YAP_MkFloatTerm(fx);
  t[1] = YAP_MkFloatTerm(xnorm);
  t[2] = YAP_MkFloatTerm(gnorm);
  t[3] = YAP_MkFloatTerm(step);
  t[4] = YAP_MkIntTerm(n);
  t[5] = YAP_MkIntTerm(k);
  t[6] = YAP_MkIntTerm(ls);
  t[7] = YAP_MkVarTerm();

  call = YAP_MkApplTerm( fprogress8, 8, t);

  optimizer_status=OPTIMIZER_STATUS_CB_PROGRESS;
  result=YAP_CallProlog(call);
  optimizer_status=OPTIMIZER_STATUS_RUNNING;

  if (result==FALSE) {
   printf("ERROR: Calling the progress call back function in YAP.\n");
    // Goal did not succeed
    return FALSE;
  }

  if (YAP_IsIntTerm(YAP_ArgOfTerm(8,call))) {
    return YAP_IntOfTerm(YAP_ArgOfTerm(8,call));
  }

  YAP_ShutdownGoal( TRUE );
  fprintf(stderr, "ERROR: The progress call back function did not return an integer as last argument\n");
  return 1;
}


static int set_x_value(void) {
  YAP_Term t1=YAP_ARG1;
  YAP_Term t2=YAP_ARG2;
  int i=0;

  if (optimizer_status!=OPTIMIZER_STATUS_INITIALIZED) {
    printf("ERROR: set_x_value/2 can be called only when the optimizer is initialized and not running.\n");
    return FALSE;
  }
  
  if (YAP_IsIntTerm(t1)) {
    i=YAP_IntOfTerm(t1);
  } else {
    return FALSE;
  }

  if (i<0 || i>=n) {
    printf("ERROR: invalid index for set_x_value/2.\n");
    return FALSE;
  }

  if (YAP_IsFloatTerm(t2)) {
    x[i]=(lbfgsfloatval_t) YAP_FloatOfTerm(t2);
  } else if (YAP_IsIntTerm(t2)) {
    x[i]=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
  } else {
    return FALSE;
  }


  return TRUE;
}

static int get_x_value(void) {
  YAP_Term t1=YAP_ARG1;
  YAP_Term t2=YAP_ARG2;
  int i=0;
  
  if (optimizer_status==OPTIMIZER_STATUS_NONE) {
    printf("ERROR: set_x_value/2 can be called only when the optimizer is initialized.\n");
    return FALSE;
  }
  
  if (YAP_IsIntTerm(t1)) {
    i=YAP_IntOfTerm(t1);
  } else {
    return FALSE;
  }

  if (i<0 || i>=n) {
    printf("ERROR: invalid index for set_x_value/2.\n");
    return FALSE;
  }

  return YAP_Unify(t2,YAP_MkFloatTerm(x[i]));
}




static int set_g_value(void) {
  YAP_Term t1=YAP_ARG1;
  YAP_Term t2=YAP_ARG2;
  int i=0;

  if (optimizer_status != OPTIMIZER_STATUS_CB_EVAL) {
    printf("ERROR: optimizer_set_g/2 can only be called by the evaluation call back function.\n");
    return FALSE;
  }
  
  if (YAP_IsIntTerm(t1)) {
    i=YAP_IntOfTerm(t1);
  } else {
    return FALSE;
  }

  if (i<0 || i>=n) {
    return FALSE;
  }


  if (YAP_IsFloatTerm(t2)) {
    g[i]=(lbfgsfloatval_t) YAP_FloatOfTerm(t2);
  } else if (YAP_IsIntTerm(t2)) {
    g[i]=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
  } else {
    return FALSE;
  }


  return TRUE;
}

static int get_g_value(void) {
  YAP_Term t1=YAP_ARG1;
  YAP_Term t2=YAP_ARG2;
  int i=0;

  if (optimizer_status != OPTIMIZER_STATUS_RUNNING && optimizer_status != OPTIMIZER_STATUS_CB_EVAL && optimizer_status != OPTIMIZER_STATUS_CB_PROGRESS) {
    printf("ERROR: optimizer_get_g/2 can only be called while the optimizer is running.\n");
    return FALSE;
  }
  
  if (YAP_IsIntTerm(t1)) {
    i=YAP_IntOfTerm(t1);
  } else {
    return FALSE;
  }

  if (i<0 || i>=n) {
    return FALSE;
  }

  return YAP_Unify(t2,YAP_MkFloatTerm(g[i]));
}


static int optimizer_initialize(void) {
  YAP_Term t1 = YAP_ARG1;
  int temp_n=0;
  
  if (optimizer_status!=OPTIMIZER_STATUS_NONE) {
    printf("ERROR: Optimizer has already been initialized. Please call optimizer_finalize/0 first.\n");
    return FALSE;
  }


  if (! YAP_IsIntTerm(t1)) {
    return FALSE;
  }

  temp_n=YAP_IntOfTerm(t1);

  if (temp_n<1) {
    return FALSE;
  }

  x = lbfgs_malloc(temp_n);

  if (x == NULL) {
        printf("ERROR: Failed to allocate a memory block for variables.\n");
        return FALSE;
  }

  n=temp_n;

  optimizer_status=OPTIMIZER_STATUS_INITIALIZED;

  return TRUE;
}

static int optimizer_run(void) {
  int ret = 0;
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;
  YAP_Int s1, s2;
  lbfgsfloatval_t fx;
  lbfgsfloatval_t * tmp_x=x;

 if (optimizer_status == OPTIMIZER_STATUS_NONE) {
    printf("ERROR: Memory for parameter vector not initialized, please call optimizer_initialize/1 first.\n");
    return FALSE;
  }
  
  if (optimizer_status != OPTIMIZER_STATUS_INITIALIZED) {
    printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n");
    return FALSE;
  }

  
  // both arguments have to be variables
  if (! YAP_IsVarTerm(t1) || ! YAP_IsVarTerm(t2)) {
    return FALSE;
  }
  s1 = YAP_InitSlot(t1);
  s2 = YAP_InitSlot(t2);
  optimizer_status = OPTIMIZER_STATUS_RUNNING;
  ret = lbfgs(n, x, &fx, evaluate, progress, NULL, &param);
  x=tmp_x;
  optimizer_status = OPTIMIZER_STATUS_INITIALIZED;

  YAP_Unify(YAP_GetFromSlot(s1),YAP_MkFloatTerm(fx));
  YAP_Unify(YAP_GetFromSlot(s2),YAP_MkIntTerm(ret));

  return TRUE;
}



static int optimizer_finalize( void ) {
  if (optimizer_status == OPTIMIZER_STATUS_NONE) {
     printf("Error: Optimizer is not initialized.\n");
     return FALSE;
  }

  if (optimizer_status == OPTIMIZER_STATUS_INITIALIZED) {
      lbfgs_free(x);
      x=NULL;
      n=0;
      optimizer_status = OPTIMIZER_STATUS_NONE;

      return TRUE;
  }

  printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n");
  return FALSE;
 }



static int optimizer_set_parameter( void ) {
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;

  if (optimizer_status != OPTIMIZER_STATUS_NONE && optimizer_status != OPTIMIZER_STATUS_INITIALIZED){
    printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n");
    return FALSE;
  }


  if (! YAP_IsAtomTerm(t1)) {
    return FALSE;
  }

  const char* name=YAP_AtomName(YAP_AtomOfTerm(t1));

  if ((strcmp(name, "m") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.m = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "epsilon") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.epsilon=v;
  } else if  ((strcmp(name, "past") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.past = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "delta") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.delta=v;
  } else if  ((strcmp(name, "max_iterations") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.max_iterations = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "linesearch") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.linesearch = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "max_linesearch") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.max_linesearch = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "min_step") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.min_step=v;
  } else if  ((strcmp(name, "max_step") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.max_step=v;
  } else if  ((strcmp(name, "ftol") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.ftol=v;
  } else if  ((strcmp(name, "gtol") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.gtol=v;
  } else if  ((strcmp(name, "xtol") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.xtol=v;
  } else if  ((strcmp(name, "orthantwise_c") == 0)) {
    lbfgsfloatval_t v;
      
    if (YAP_IsFloatTerm(t2)) {
      v=YAP_FloatOfTerm(t2);
    } else if (YAP_IsIntTerm(t2)) {
      v=(lbfgsfloatval_t) YAP_IntOfTerm(t2);
    } else {
      return FALSE;
    }

    param.orthantwise_c=v;
  } else if  ((strcmp(name, "orthantwise_start") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.orthantwise_start = YAP_IntOfTerm(t2);
  } else if  ((strcmp(name, "orthantwise_end") == 0)) {
    if (! YAP_IsIntTerm(t2)) {
	return FALSE;
    }
    param.orthantwise_end = YAP_IntOfTerm(t2);
  } else {
      printf("ERROR: The parameter %s is unknown.\n",name);
      return FALSE;
  }
  
  return TRUE;
}

static int optimizer_get_parameter( void ) {
  YAP_Term t1 = YAP_ARG1;
  YAP_Term t2 = YAP_ARG2;

  if (! YAP_IsAtomTerm(t1)) {
    return FALSE;
  }

  const char* name=YAP_AtomName(YAP_AtomOfTerm(t1));

  if ((strcmp(name, "m") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.m));
  } else if  ((strcmp(name, "epsilon") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.epsilon));
  } else if  ((strcmp(name, "past") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.past));
  } else if  ((strcmp(name, "delta") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.delta));
  } else if  ((strcmp(name, "max_iterations") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.max_iterations));
  } else if  ((strcmp(name, "linesearch") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.linesearch));
  } else if  ((strcmp(name, "max_linesearch") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.max_linesearch));
  } else if  ((strcmp(name, "min_step") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.min_step));
  } else if  ((strcmp(name, "max_step") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.max_step));
  } else if  ((strcmp(name, "ftol") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.ftol));
  } else if  ((strcmp(name, "gtol") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.gtol));
  } else if  ((strcmp(name, "xtol") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.xtol));
  } else if  ((strcmp(name, "orthantwise_c") == 0)) {
    return YAP_Unify(t2,YAP_MkFloatTerm(param.orthantwise_c));
  } else if  ((strcmp(name, "orthantwise_start") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.orthantwise_start));
  } else if  ((strcmp(name, "orthantwise_end") == 0)) {
    return YAP_Unify(t2,YAP_MkIntTerm(param.orthantwise_end));
  }

  printf("ERROR: The parameter %s is unknown.\n",name);
  return FALSE;
}





void init_lbfgs_predicates( void ) 
{ 
  fcall3 = YAP_MkFunctor(YAP_LookupAtom("$lbfgs_callback_evaluate"), 3);
  fprogress8 = YAP_MkFunctor(YAP_LookupAtom("$lbfgs_callback_progress"), 8);
  fmod2 = YAP_MkFunctor(YAP_LookupAtom(":"), 2);

  //Initialize the parameters for the L-BFGS optimization.
  lbfgs_parameter_init(&param);


  YAP_UserCPredicate("optimizer_reserve_memory",optimizer_initialize,1);
  YAP_UserCPredicate("optimizer_run",optimizer_run,2);
  YAP_UserCPredicate("optimizer_free_memory",optimizer_finalize,0);

  YAP_UserCPredicate("optimizer_set_x",set_x_value,2);
  YAP_UserCPredicate("optimizer_get_x",get_x_value,2);
  YAP_UserCPredicate("optimizer_set_g",set_g_value,2);
  YAP_UserCPredicate("optimizer_get_g",get_g_value,2);

  YAP_UserCPredicate("optimizer_set_parameter",optimizer_set_parameter,2);
  YAP_UserCPredicate("optimizer_get_parameter",optimizer_get_parameter,2);
}  
