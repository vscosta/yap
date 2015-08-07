/*************************************************************************
*									 *
*	 Extension for YAP Prolog 					 *
*									 *
* Copyright G.S.Oliveira, A.F.Silva and Universidade Estadual de Maringá *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		jit_configpreds.c					 *
* comments:	JIT Compiler Configuration predicates			 *
*									 *
* Last rev:     2013-10-18                               		 *
*************************************************************************/

#define JIT_CODE 1

#include "jit_predicates.hpp"
#include <math.h>

// Enable any (passed by argument) execution mode
static Int  p_execution_mode( USES_REGS1 );

// Enable 'just interpreted' mode.
static Int  p_interpreted_mode( USES_REGS1 );

// Enable 'smart jit' mode.
static Int  p_smartjit_mode( USES_REGS1 );

// Enable 'continuous compilation' mode.
static Int  p_continuouscompilation_mode( USES_REGS1 );

// Enable 'just compiled' mode.
static Int  p_justcompiled_mode( USES_REGS1 );

// Enable one (passed by argument) of all available frequency types: counter or time. Frequency bound is default.
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_frequencyty1( USES_REGS1 );

// Enable one (1st argument) of all available frequency types: counter and time. Frequency bound is 2nd argument
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_frequencyty2( USES_REGS1 );

// Enable frequency bound
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_frequency_bound( USES_REGS1 );

// Enable value for starting profiling
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_profiling_start_point( USES_REGS1 );

// Choose type of clause that can be main on traces
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_main_clause_ty( USES_REGS1 );

// Choose amount of compilation threads
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_compilation_threads( USES_REGS1 );

// Enable recompilation
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_enable_recompilation( USES_REGS1 );

// Disable recompilation
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_disable_recompilation( USES_REGS1 );

// Just code interpretation. Don't compile
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_only_profiled_interpreter( USES_REGS1 );

// Disable 'p_only_profiled_interpreter'
// Just for 'smart jit' or 'continuous compilation' mode
static Int  p_noonly_profiled_interpreter( USES_REGS1 );

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

static Int
p_execution_mode( USES_REGS1 )
{
  enumExecModes mode;
  // valid values for ARG1 are 'integer' and 'atom'
  Term t = Deref(ARG1);
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < 0 || v > 3) {
      // value passed by argument is out of known range (valid values are: 0 -- interpreted; 1 -- smart jit; 2 -- continuous compilation; 3 -- just compiled)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // storing mode
    mode = (enumExecModes)v;
  }
  else if (IsAtomTerm(t)) {
    // ARG1 is atom
    int i = 0, j = 0;
    char *tmp;
    // gets string from atom and stores it on 'str'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    // Makes upper characters of 'str' (for comparison)
    UPPER_ENTRY(str);

    // Detecting mode according to 'str'
    if (strcmp(str, "INTERPRETED") == 0) mode = JUST_INTERPRETED;
    else if (strcmp(str, "SMARTJIT") == 0) mode = SMART_JIT;
    else if (strcmp(str, "CONTINUOUSCOMPILATION") == 0) mode = CONTINUOUS_COMPILATION;
    else if (strcmp(str, "JUSTCOMPILED") == 0) mode = JUST_COMPILED;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Execution mode");
    return FALSE;
  }

  // setting execution mode
  ExpEnv.config_struc.execution_mode = mode;

  /* setting execution mode parameters */
  switch (mode) {
  case JUST_INTERPRETED:
    {
      if (Yap_ExecutionMode == INTERPRETED) {
        // execution mode only can be 'JUST_INTERPRETED' if 'Yap_ExecutionMode == INTERPRETED' (passing -J0 on command line)
        // 'JUST_INTERPRETED' does not use these parameters
        ExpEnv.config_struc.frequency_type = NO_FREQ;
        ExpEnv.config_struc.frequency_bound = 0.0;
        ExpEnv.config_struc.profiling_startp = 0.0;
        ExpEnv.config_struc.mainclause_ty = UNUSED;
        ExpEnv.config_struc.compilation_threads = 0;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
          fprintf(stderr,"      YAP Execution mode changed to INTERPRETED!!\n");
#endif
      }
      else {
        // 'Yap_ExecutionMode' is not compatible
        Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"INTERPRETED");
        return FALSE;
      }
    }
    break;
  case SMART_JIT:
    {
      if (Yap_ExecutionMode == MIXED_MODE) {
        // execution mode only can be 'SMART_JIT' if 'Yap_ExecutionMode == MIXED_MODE' (passing -J1 on command line)
        ExpEnv.config_struc.frequency_type = COUNTER;
        ExpEnv.config_struc.frequency_bound = 1024.0;
        ExpEnv.config_struc.profiling_startp = 0.72;
        ExpEnv.config_struc.mainclause_ty = HOT_AND_CALLEE;
         ExpEnv.config_struc.compilation_threads = 0;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
          fprintf(stderr,"      YAP Execution mode changed to SMART JIT!!\n");
#endif
      }
      else {
        // 'Yap_ExecutionMode' is not compatible
        Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"SMART JIT");
        return FALSE;
      }
    }
    break;
  case CONTINUOUS_COMPILATION:
    {
      if (Yap_ExecutionMode == MIXED_MODE) {
        // execution mode only can be 'CONTINUOUS_COMPILATION' if 'Yap_ExecutionMode == MIXED_MODE' (passing -J1 on command line)
        ExpEnv.config_struc.frequency_type = COUNTER;
        ExpEnv.config_struc.frequency_bound = 1024.0;
        ExpEnv.config_struc.profiling_startp = 0.72;
        ExpEnv.config_struc.mainclause_ty = HOT_AND_CALLEE;
        ExpEnv.config_struc.compilation_threads = ExpEnv.config_struc.ncores-1;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
          fprintf(stderr,"      YAP Execution mode changed to CONTINUOUS COMPILATION!!\n");
#endif
      }
      else {
        // 'Yap_ExecutionMode' is not compatible
        Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"CONTINUOUS COMPILATION");
        return FALSE;
      }
    }
    break;
  case JUST_COMPILED:
    {
      if (Yap_ExecutionMode == COMPILED) {
        // execution mode only can be 'JUST_COMPILED' if 'Yap_ExecutionMode == COMPILED' (passing -J2 on command line)
        // 'JUST_COMPILED' does not use these parameters
        ExpEnv.config_struc.frequency_type = NO_FREQ;
        ExpEnv.config_struc.frequency_bound = 0.0;
        ExpEnv.config_struc.profiling_startp = 0.0;
        ExpEnv.config_struc.mainclause_ty = UNUSED;
        ExpEnv.config_struc.compilation_threads = 0;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
          fprintf(stderr,"      YAP Execution mode changed to JUST COMPILED!!\n");
#endif
      }
      else {
        // 'Yap_ExecutionMode' is not compatible
        Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"JUST COMPILED");
        return FALSE;
      }
    }
    break;
  }
  /***/
  return TRUE;
}

static Int
p_interpreted_mode( USES_REGS1 )
{
  // Same as 'execution_mode(0)' or 'execution_mode(interpreted)'
  if (Yap_ExecutionMode == INTERPRETED) {
    // execution mode only can be 'JUST_INTERPRETED' if 'Yap_ExecutionMode == INTERPRETED' (passing -J0 on command line)
    ExpEnv.config_struc.execution_mode = JUST_INTERPRETED; // setting mode
    ExpEnv.config_struc.frequency_type = NO_FREQ; // does not use frequency type
    ExpEnv.config_struc.frequency_bound = 0.0;  // does not use frequency bound
    ExpEnv.config_struc.profiling_startp = 0.0; // does not use profiling startp
    ExpEnv.config_struc.mainclause_ty = UNUSED; // does not use mainclause ty
    ExpEnv.config_struc.compilation_threads = 0; // does not use compilation threads
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
      fprintf(stderr,"      YAP Execution mode changed to INTERPRETED!!\n");
#endif
    return TRUE;
  }
  else {
    // 'Yap_ExecutionMode' is not compatible
    Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"INTERPRETED");
    return FALSE;
  }
}

static Int
p_smartjit_mode( USES_REGS1 )
{
  // Same as 'execution_mode(1)' or 'execution_mode(smartjit)'
  if (Yap_ExecutionMode == MIXED_MODE) {
    // execution mode only can be 'SMART_JIT' if 'Yap_ExecutionMode == MIXED_MODE' (passing -J1 on command line)
    ExpEnv.config_struc.execution_mode = SMART_JIT; // setting mode
    ExpEnv.config_struc.frequency_type = COUNTER; // default value
    ExpEnv.config_struc.frequency_bound = 1024.0; // default value
    ExpEnv.config_struc.profiling_startp = 0.72; // default value
    ExpEnv.config_struc.mainclause_ty = HOT_AND_CALLEE; // default value
    /* does not use compilation threads */
    ExpEnv.config_struc.compilation_threads = 0;
    ExpEnv.config_struc.threaded_compiler_threads = NULL;
    ExpEnv.config_struc.posthreads = NULL;
    /***/
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
      fprintf(stderr,"      YAP Execution mode changed to SMART JIT!!\n");
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_file)
      strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.file_name), "trace");
#endif
    return TRUE;
  }
  else {
    // 'Yap_ExecutionMode' is not compatible
    Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"SMART JIT");
    return FALSE;
  }
}

static Int
p_continuouscompilation_mode( USES_REGS1 )
{
  // Same as 'execution_mode(2)' or 'execution_mode(continuouscompilation)'
  if (Yap_ExecutionMode == MIXED_MODE) {
    // execution mode only can be 'CONTINUOUS_COMPILATION' if 'Yap_ExecutionMode == MIXED_MODE' (passing -J1 on command line)
    ExpEnv.config_struc.execution_mode = CONTINUOUS_COMPILATION; // setting mode
    ExpEnv.config_struc.frequency_type = COUNTER; // default value
    ExpEnv.config_struc.frequency_bound = 1024.0; // default value
    ExpEnv.config_struc.profiling_startp = 0.72; // default value
    ExpEnv.config_struc.mainclause_ty = HOT_AND_CALLEE; // default value
    ExpEnv.config_struc.compilation_threads = ExpEnv.config_struc.ncores-1; // default value for this mode
    /* initializing structures which will handle compilation threads */
    {
      if (ExpEnv.config_struc.threaded_compiler_threads) free(ExpEnv.config_struc.threaded_compiler_threads);
      if (ExpEnv.config_struc.posthreads) free(ExpEnv.config_struc.posthreads);
      ExpEnv.config_struc.threaded_compiler_threads = (pthread_t*)malloc(ExpEnv.config_struc.compilation_threads*sizeof(pthread_t));
      ExpEnv.config_struc.posthreads = (CELL*)malloc(ExpEnv.config_struc.compilation_threads*sizeof(CELL));
      int i;
      for (i = 0; i < ExpEnv.config_struc.compilation_threads; i++) ExpEnv.config_struc.posthreads[i] = 0;
    }
    /***/
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
      fprintf(stderr,"      YAP Execution mode changed to CONTINUOUS COMPILATION!!\n");
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_file)
      strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.file_name), "trace");
#endif
    return TRUE;
  }
  else {
    // 'Yap_ExecutionMode' is not compatible
    Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"CONTINUOUS COMPILATION");
    return FALSE;
  }
}

static Int
p_justcompiled_mode( USES_REGS1 )
{
  // Same as 'execution_mode(3)' or 'execution_mode(justcompiled)'
  if (Yap_ExecutionMode == COMPILED) {
    // execution mode only can be 'JUST_COMPILED' if 'Yap_ExecutionMode == COMPILED' (passing -J2 on command line)
    ExpEnv.config_struc.execution_mode = JUST_COMPILED; // setting mode
    ExpEnv.config_struc.frequency_type = NO_FREQ; // does not use frequency type
    ExpEnv.config_struc.frequency_bound = 0.0; // does not use frequency bound
    ExpEnv.config_struc.profiling_startp = 0.0; // does not use profiling startp
    ExpEnv.config_struc.mainclause_ty = UNUSED; // does not use mainclause ty
    ExpEnv.config_struc.compilation_threads = 0; // does not use compilation threads
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs)
      fprintf(stderr,"      YAP Execution mode changed to JUST COMPILED!!\n");
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_file)
      strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.file_name), "clause");
#endif
    return TRUE;
  }
  else {
    // 'Yap_ExecutionMode' is not compatible
    Yap_NilError(INCOMPATIBLE_CODEMODE_WARNING,"JUST COMPILED");
    return FALSE;
  }
}

static Int
p_frequencyty1( USES_REGS1 )
{
  // this predicate works only 'SMART_JIT' and 'CONTINUOUS_COMPILATION' modes
  if (ExpEnv.config_struc.execution_mode == SMART_JIT || ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
    Term t = Deref(ARG1);
    // valid value for ARG1 is just 'atom'
    if (IsAtomTerm(t)) {
      // ARG1 is atom
      int i = 0, j = 0;
      char *tmp;
      // gets string from atom and stores it on 'str'
      char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
      strcpy(str, AtomName(AtomOfTerm(t)));
      // Makes upper characters of 'str' (for comparison)
      UPPER_ENTRY(str);

      // Detectng frequency type according to 'str'
      if (strcmp(str, "COUNTER") == 0 || strcmp(str, "COUNT") == 0) {
        ExpEnv.config_struc.frequency_type = COUNTER; // setting frequency type to 'counter'
 	ExpEnv.config_struc.frequency_bound = 1024.0; // if 'counter', frequency bound is '1024.0'
 	return TRUE;
      }
      else if (strcmp(str, "TIME") == 0 || strcmp(str, "TIMING") == 0) {
        ExpEnv.config_struc.frequency_type = TIME; // setting frequency type to 'time'
 	ExpEnv.config_struc.frequency_bound = 0.02; // if 'time', frequency bound is '0.02'
	return TRUE;
      }
      else {
        // value passed by argument is out of known range
        Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
        return FALSE;
      }
    }
    else {
      // ARG1 is not an atom
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Frequency type");
      return FALSE;
    }
  }
  else {
    // current execution mode differs of 'SMART_JIT' and 'CONTINUOUS_COMPILATION'
    Yap_NilError(INCOMPATIBLEMODE_WARNING,"");
    return FALSE;
  }
}

static Int
p_frequencyty2( USES_REGS1 )
{
  // this predicate works only 'SMART_JIT' and 'CONTINUOUS_COMPILATION' modes
  if (ExpEnv.config_struc.execution_mode == SMART_JIT || ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
    Term t = Deref(ARG1);
    // valid value for ARG1 is just 'atom'
    if (IsAtomTerm(t)) {
      Term u = Deref(ARG2);
      // valid values for ARG2 are 'integer' and 'float'
      if (IsIntTerm(u) || IsFloatTerm(u)) {
        // ARG1 is atom and ARG2 is integer or float
        int i = 0, j = 0;
        char *tmp;
        // getting string from atom and stores it on 'str'
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(t)));
        // Making upper characters of 'str' (for comparison)
        UPPER_ENTRY(str);

        // getting ARG2 value
        Float v;
        if (IsIntTerm(u)) v = (Float)IntOfTerm(u);
        if (IsFloatTerm(u)) v = FloatOfTerm(u);

        // setting 'frequency type' and 'frequency bound' if 'COUNTER'
        if (strcmp(str, "COUNTER") == 0 || strcmp(str, "COUNT") == 0) {
	  if (v < 20.0) {
            // Very low frequency bound to apply on 'COUNTER'
	    fprintf(stderr,"%.2f is a very low value for the active frequency type. Reconsider its value...\n", v);
	    return FALSE;
	  }
	  ExpEnv.config_struc.frequency_type = COUNTER;
	  ExpEnv.config_struc.frequency_bound = roundf(v);
	  return TRUE;
        }
        // setting 'frequency type' and 'frequency bound' if 'TIME'
        else if (strcmp(str, "TIME") == 0 || strcmp(str, "TIMING") == 0) {
	  if (v <= 0.0 || v > 0.49) {
            // Very low frequency bound to apply on 'COUNTER'
	    fprintf(stderr,"%.2f is an invalid or a very high value for the active frequency type. Reconsider its value...\n", v);
	    return FALSE;
	  }
	  ExpEnv.config_struc.frequency_type = TIME;
	  ExpEnv.config_struc.frequency_bound = v;
	  return TRUE;
        }
        else {
          // value passed by argument (ARG1) is out of known range
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
          return FALSE;
        }
      }
      else {
        // ARG2 is not an 'integer' or 'float'
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"frequencyty/2 (2nd arg)");
        return FALSE;
      }
    }
    else {
      // ARG1 is not an atom
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"frequencyty/2 (1st arg)");
      return FALSE;
    }
  }
  else {
    // current execution mode differs of 'SMART_JIT' and 'CONTINUOUS_COMPILATION'
    Yap_NilError(INCOMPATIBLEMODE_WARNING,"");
    return FALSE;
  }
}

static Int
p_frequency_bound( USES_REGS1 )
{
  // this predicate works only 'SMART_JIT' and 'CONTINUOUS_COMPILATION' modes
  if (ExpEnv.config_struc.execution_mode == SMART_JIT || ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
    Term t = Deref(ARG1);
    // valid values for ARG1 are 'integer' and 'float'
    if (IsIntTerm(t) || IsFloatTerm(t)) {
      // ARG1 is integer or float
      // getting ARG1 value
      Float v;
      if (IsIntTerm(t)) v = (Float)IntOfTerm(t);
      if (IsFloatTerm(t)) v = FloatOfTerm(t);

      // setting 'frequency bound' if 'frequency type' is 'COUNTER'
      if (ExpEnv.config_struc.frequency_type == COUNTER) {
        if (v < 20.0) {
          fprintf(stderr,"%.2f is a very low value for the active frequency type. Reconsider its value...\n", v);
	  return FALSE;
	}
	ExpEnv.config_struc.frequency_bound = roundf(v);
	return TRUE;
      }
      // setting 'frequency bound' if 'frequency type' is 'TIME'
      else {
        if (v <= 0.0 || v > 0.49) {
	  fprintf(stderr,"%.2f is an invalid or a very high value for the active frequency type. Reconsider its value...\n", v);
	  return FALSE;
	}
	ExpEnv.config_struc.frequency_bound = v;
	return TRUE;
      }
    }
    else {
      // ARG1 is not an 'integer' or 'float'
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"frequency_bound/1 (1st arg)");
      return FALSE;
    }
  }
  else {
    // current execution mode differs of 'SMART_JIT' and 'CONTINUOUS_COMPILATION'
    Yap_NilError(INCOMPATIBLEMODE_WARNING,"");
    return FALSE;
  }
}

static Int
p_profiling_start_point( USES_REGS1 )
{
  // this predicate works only 'SMART_JIT' and 'CONTINUOUS_COMPILATION' modes
  if (ExpEnv.config_struc.execution_mode == SMART_JIT || ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
    Term t = Deref(ARG1);
    Float v;
    // valid value for ARG1 is just 'float'
    if (IsFloatTerm(t)) {
      v = FloatOfTerm(t);
      if (v < 0.0 || v >= 1.0) {
        // value passed by argument is out of known range
        Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
        return FALSE;
      }
      ExpEnv.config_struc.profiling_startp = v;
      return TRUE;
    }
    else {
      // ARG1 is not float
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"profiling_start_point/1 (1st arg)");
      return FALSE;
    }
  }
  else {
    // current execution mode differs of 'SMART_JIT' and 'CONTINUOUS_COMPILATION'
    Yap_NilError(INCOMPATIBLEMODE_WARNING,"");
    return FALSE;
  }
}

static Int
p_main_clause_ty( USES_REGS1 )
{
  // this predicate works only 'SMART_JIT' and 'CONTINUOUS_COMPILATION' modes
  if (ExpEnv.config_struc.execution_mode == SMART_JIT || ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
    Term t = Deref(ARG1);
    // valid values for ARG1 are 'integer' and 'atom'
    if (IsIntTerm(t)) {
      // ARG1 is integer
      Int v;
      v = IntOfTerm(t);
      if (v < 0 || v > 3) {
        // value passed by argument is out of known range
        Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
        return FALSE;
      }
#if YAP_DBG_PREDS
      if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs) {
        switch(v) {
          case 0:
            fprintf(stderr,"      Type of main clause was changed to JUST HOT!!\n");
            break;
          case 1:
            fprintf(stderr,"      Type of main clause was changed to HOT AND CALLEE!!\n");
            break;
          case 2:
            fprintf(stderr,"      Type of main clause was changed to HOT AND GREATER!!\n");
            break;
          case 3:
            fprintf(stderr,"      Type of main clause was changed to HOT AND FEWER!!\n");
            break;
        }
      }
#endif
      // setting 'mainclause_ty' -- I should de add '1' because the first enum of 'enumMainClauseType' is 'UNUSED', used just for control
      ExpEnv.config_struc.mainclause_ty = (enumMainClauseType)(v+1);
      return TRUE;
    }
    else if (IsAtomTerm(t)) {
      // ARG1 is atom
      enumMainClauseType v;
      int i = 0, j = 0;
      char *tmp;
      // gets string from atom and stores it on 'str'
      char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
      strcpy(str, AtomName(AtomOfTerm(t)));
      // Makes upper characters of 'str' (for comparison)
      UPPER_ENTRY(str);

      // Detecting mainclause type chosen by user according to 'str'
      if (strcmp(str, "JUSTHOT") == 0) {
        v = JUST_HOT;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs) fprintf(stderr,"      Type of main clause was changed to JUST HOT!!\n");
#endif
      }
      else if (strcmp(str, "HOTANDCALLEE") == 0) {
        v = HOT_AND_CALLEE;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs) fprintf(stderr,"      Type of main clause was changed to HOT AND CALLEE!!\n");
#endif
      }
      else if (strcmp(str, "HOTANDGREATER") == 0) {
        v = HOT_AND_GREATER;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs) fprintf(stderr,"      Type of main clause was changed to HOT AND GREATER!!\n");
#endif
      }
      else if (strcmp(str, "HOTANDFEWER") == 0) {
        v = HOT_AND_FEWER;
#if YAP_DBG_PREDS
        if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs) fprintf(stderr,"      Type of main clause was changed to HOT AND FEWER!!\n");
#endif
      }
      else {
        // value passed by argument is out of known range
        Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
        return FALSE;
      }
      ExpEnv.config_struc.mainclause_ty = v;
      return TRUE;
    }
    else {
      // ARG1 is not an integer or atom
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"main_clause_ty/1 (1st arg)");
      return FALSE;
    }
  }
  else {
    // current execution mode differs of 'SMART_JIT' and 'CONTINUOUS_COMPILATION'
    Yap_NilError(INCOMPATIBLEMODE_WARNING,"");
    return FALSE;
  }
}

static Int
p_compilation_threads( USES_REGS1 )
{
  // this predicate works only 'CONTINUOUS_COMPILATION' mode
  if (ExpEnv.config_struc.execution_mode == SMART_JIT || ExpEnv.config_struc.execution_mode == CONTINUOUS_COMPILATION) {
    Term t = Deref(ARG1);
    Int v;
    // valid value for ARG1 is 'integer' (because it defines number of threads)
    if (IsIntTerm(t)) {
    // ARG1 is integer
      v = IntOfTerm(t);
      if (v < 1) {
        // ERROR: number of threads is negative!!
        Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
        return FALSE;
      }
      if (v >= ExpEnv.config_struc.ncores) {
        // WARNING: number of threads is not ideal -- real parallelism won't occur!!
        fprintf(stderr,
          "      It was detected %ld cores on this computer, therefore it is ideally to set just %ld compilation thread. Reconsider its value...\n",
          ExpEnv.config_struc.ncores, ExpEnv.config_struc.ncores-1);
      }
      // setting compilation threads
      ExpEnv.config_struc.compilation_threads = v;

      /* initializing structures which will handle compilation threads */
      {
        if (ExpEnv.config_struc.threaded_compiler_threads) free(ExpEnv.config_struc.threaded_compiler_threads);
        if (ExpEnv.config_struc.posthreads) free(ExpEnv.config_struc.posthreads);
        ExpEnv.config_struc.threaded_compiler_threads = (pthread_t*)malloc(v*sizeof(pthread_t));
        ExpEnv.config_struc.posthreads = (CELL*)malloc(v*sizeof(CELL));
        int i;
        for (i = 0; i < v; i++) ExpEnv.config_struc.posthreads[i] = 0;
      }
      /***/
#if YAP_DBG_PREDS
      if (ExpEnv.debug_struc.act_predicate_msgs.success_msgs) fprintf(stderr,"      Type of main clause was changed to HOT AND FEWER!!\n");
#endif
      return TRUE;
    }
    else {
      // ARG1 is not an integer
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"compilation_threads/1 (1st arg)");
      return FALSE;
    }
  }
  else {
    // current execution mode differs of 'CONTINUOUS_COMPILATION'
    Yap_NilError(INCOMPATIBLEMODE_WARNING,"");
    return FALSE;
  }
}

static Int
p_enable_recompilation( USES_REGS1 )
{
  ExpEnv.config_struc.torecompile = 1;
  return TRUE;
}

static Int
p_disable_recompilation( USES_REGS1 )
{
  ExpEnv.config_struc.torecompile = 0;
  return TRUE;
}

static Int
p_only_profiled_interpreter( USES_REGS1 )
{
  ExpEnv.config_struc.useonlypi = 1;
  return TRUE;
}

static Int
p_noonly_profiled_interpreter( USES_REGS1 )
{
  ExpEnv.config_struc.useonlypi = 0;
  return TRUE;
}

#pragma GCC diagnostic pop

void
Yap_InitJitConfigPreds( void )
{
  Yap_InitCPred("execution_mode", 1, p_execution_mode, SafePredFlag);
  Yap_InitCPred("interpreted_mode", 0, p_interpreted_mode, SafePredFlag);
  Yap_InitCPred("smartjit_mode", 0, p_smartjit_mode, SafePredFlag);
  Yap_InitCPred("continuouscompilation_mode", 0, p_continuouscompilation_mode, SafePredFlag);
  Yap_InitCPred("justcompiled_mode", 0, p_justcompiled_mode, SafePredFlag);
  Yap_InitCPred("frequencyty1", 1, p_frequencyty1, SafePredFlag);
  Yap_InitCPred("frequencyty2", 2, p_frequencyty2, SafePredFlag);
  Yap_InitCPred("frequency_bound", 1, p_frequency_bound, SafePredFlag);
  Yap_InitCPred("profiling_start_point", 1, p_profiling_start_point, SafePredFlag);
  Yap_InitCPred("main_clause_ty", 1, p_main_clause_ty, SafePredFlag);
  Yap_InitCPred("compilation_threads", 1, p_compilation_threads, SafePredFlag);
  Yap_InitCPred("enable_recompilation", 0, p_enable_recompilation, SafePredFlag);
  Yap_InitCPred("disable_recompilation", 0, p_disable_recompilation, SafePredFlag);
  Yap_InitCPred("only_profiled_interpreter", 0, p_only_profiled_interpreter, SafePredFlag);
  Yap_InitCPred("noonly_profiled_interpreter", 0, p_noonly_profiled_interpreter, SafePredFlag);
}

