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
* File:		jit_codegenpreds.c					 *
* comments:	JIT Compiler Codegen Options predicates			 *
*									 *
* Last rev:     2013-10-18                               		 *
*************************************************************************/

#include "jit_predicates.hpp"

/* Predicates for LLVM Target Options configuration */
static Int  p_enable_framepointer_elimination( USES_REGS1 );
static Int  p_more_precise_fp_mad_option( USES_REGS1 );
static Int  p_excess_fp_precision( USES_REGS1 );
static Int  p_safe_fp_math( USES_REGS1 );
static Int  p_rounding_mode_not_changed( USES_REGS1 );
static Int  p_no_use_soft_float( USES_REGS1 );
static Int  p_disable_jit_exception_handling( USES_REGS1 );
static Int  p_disable_jit_emit_debug_info( USES_REGS1 );
static Int  p_disable_jit_emit_debug_info_to_disk( USES_REGS1 );
static Int  p_no_guaranteed_tail_call_opt( USES_REGS1 );
static Int  p_enable_tail_calls( USES_REGS1 );
static Int  p_disable_fast_isel( USES_REGS1 );
static Int  p_disable_framepointer_elimination( USES_REGS1 );
static Int  p_less_precise_fp_mad_option( USES_REGS1 );
static Int  p_no_excess_fp_precision( USES_REGS1 );
static Int  p_unsafe_fp_math( USES_REGS1 );
static Int  p_rounding_mode_dynamically_changed( USES_REGS1 );
static Int  p_use_soft_float( USES_REGS1 );
static Int  p_enable_jit_exception_handling( USES_REGS1 );
static Int  p_enable_jit_emit_debug_info( USES_REGS1 );
static Int  p_enable_jit_emit_debug_info_to_disk( USES_REGS1 );
static Int  p_guaranteed_tail_call_opt( USES_REGS1 );
static Int  p_disable_tail_calls( USES_REGS1 );
static Int  p_enable_fast_isel( USES_REGS1 );
static Int  p_fp_abitype( USES_REGS1 );
static Int  p_default_fp_abitype( USES_REGS1 );

// LLVM Execution Engine level
static Int  p_engine_opt_level( USES_REGS1 );
static Int  p_reset_engine_opt_level( USES_REGS1 );

// LLVM Execution Engine reloc model
static Int  p_relocmodel( USES_REGS1 );
static Int  p_reset_relocmodel( USES_REGS1 );

// LLVM Execution Engine code model
static Int  p_codemodel( USES_REGS1 );
static Int  p_reset_codemodel( USES_REGS1 );

// Enable MC JIT (experimental)
static Int  p_enable_mcjit( USES_REGS1 );

// Disable MC JIT (experimental)
static Int  p_disable_mcjit( USES_REGS1 );

// LLVM Register Allocator (not implemented -- for some reason, LLVM crashes when I use it on 'JIT_Compiler.cpp')
static Int  p_register_allocator( USES_REGS1 );
static Int  p_reset_register_allocator( USES_REGS1 );

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

static Int
p_enable_framepointer_elimination( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.noframepointerelim = FALSE;
  return TRUE;
}

static Int
p_more_precise_fp_mad_option( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.lessprecisefpmadoption = FALSE;
  return TRUE;
}

static Int
p_excess_fp_precision( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.noexcessfpprecision = FALSE;
  return TRUE;
}

static Int
p_safe_fp_math( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.unsafefpmath = FALSE;
  return TRUE;
}

static Int
p_rounding_mode_not_changed( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.honorsigndependentroundingfpmathoption = FALSE;
  return TRUE;
}

static Int
p_no_use_soft_float( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.usesoftfloat = FALSE;
  return TRUE;
}

static Int
p_disable_jit_exception_handling( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.jitexceptionhandling = FALSE;
  return TRUE;
}

static Int
p_disable_jit_emit_debug_info( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfo = FALSE;
  return TRUE;
}

static Int
p_disable_jit_emit_debug_info_to_disk( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfotodisk = FALSE;
  return TRUE;
}

static Int
p_no_guaranteed_tail_call_opt( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.guaranteedtailcallopt = FALSE;
  return TRUE;
}

static Int
p_enable_tail_calls( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.disabletailcalls = FALSE;
  return TRUE;
}

static Int
p_disable_fast_isel( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.fastisel = FALSE;
  return TRUE;
}

static Int
p_disable_framepointer_elimination( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.noframepointerelim = TRUE;
  return TRUE;
}

static Int
p_less_precise_fp_mad_option( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.lessprecisefpmadoption = TRUE;
  return TRUE;
}

static Int
p_no_excess_fp_precision( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.noexcessfpprecision = TRUE;
  return TRUE;
}

static Int
p_unsafe_fp_math( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.unsafefpmath = TRUE;
  return TRUE;
}

static Int
p_rounding_mode_dynamically_changed( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.honorsigndependentroundingfpmathoption = TRUE;
  return TRUE;
}

static Int
p_use_soft_float( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.usesoftfloat = TRUE;
  return TRUE;
}

static Int
p_enable_jit_exception_handling( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.jitexceptionhandling = TRUE;
  return TRUE;
}

static Int
p_enable_jit_emit_debug_info( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfo = TRUE;
  return TRUE;
}

static Int
p_enable_jit_emit_debug_info_to_disk( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfotodisk = TRUE;
  return TRUE;
}

static Int
p_guaranteed_tail_call_opt( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.guaranteedtailcallopt = TRUE;
  return TRUE;
}

static Int
p_disable_tail_calls( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.disabletailcalls = TRUE;
  return TRUE;
}

static Int
p_enable_fast_isel( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.fastisel = TRUE;
  return TRUE;
}

static Int
p_fp_abitype( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Int v;
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    v = IntOfTerm(t);
    if (v < 0 || v > 2) {
      // value passed by argument is out of known range (0 = default; 1 = soft; 2 = hard)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_targetopt.floatabitype = v; // setting 'float abi type'
    return TRUE;
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

    // Detects float abi type chosen by user
    if (strcmp(str, "DEFAULT") == 0) v = 0;
    else if (strcmp(str, "SOFT") == 0) v = 1;
    else if (strcmp(str, "HARD") == 0) v = 2;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_targetopt.floatabitype = v; // setting 'float abi type'
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"fp_abitype");
    return FALSE;
  }
}

static Int
p_default_fp_abitype( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_targetopt.floatabitype = 0;
  return TRUE;
}

static Int
p_engine_opt_level( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Int v;
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    v = IntOfTerm(t);
    if (v < 0 || v > 3) {
      // value passed by argument is out of known range (0 = none; 1 = less; 2 = default; 3 = aggressive)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.engineoptlevel = v; // setting 'engine opt level'
    return TRUE;
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

    // Detects engine opt level chosen by user
    if (strcmp(str, "NONE") == 0) v = 0;
    else if (strcmp(str, "LESS") == 0) v = 1;
    else if (strcmp(str, "DEFAULT") == 0) v = 2;
    else if (strcmp(str, "AGGRESSIVE") == 0) v = 3;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.engineoptlevel = v; // setting 'engine opt level'
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"engine_opt_level");
    return FALSE;
  }
}

static Int
p_reset_engine_opt_level( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_enginebuilder.engineoptlevel = 3;
  return TRUE;
}

static Int
p_relocmodel( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Int v;
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    v = IntOfTerm(t);
    if (v < 0 || v > 3) {
      // value passed by argument is out of known range (0 = default; 1 = static; 2 = PIC; 3 = DynamicNoPIC)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.relocmodel = v; // setting 'reloc model'
    return TRUE;
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

    // Detects reloc model chosen by user
    if (strcmp(str, "DEFAULT") == 0) v = 0;
    else if (strcmp(str, "STATIC") == 0) v = 1;
    else if (strcmp(str, "PIC") == 0) v = 2;
    else if (strcmp(str, "DYNAMICNOPIC") == 0) v = 3;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.relocmodel = v; // setting 'reloc model'
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"relocmodel");
    return FALSE;
  }
}

static Int
p_reset_relocmodel( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_enginebuilder.relocmodel = 0;
  return TRUE;
}

static Int
p_codemodel( USES_REGS1 )
{
  Term t = Deref(ARG1);
  Int v;
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    v = IntOfTerm(t);
    if (v < 0 || v > 5) {
      // value passed by argument is out of known range (0 = default; 1 = JITDefault; 2 = small; 3 = kernel; 4 = medium; 5 = large)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.codemodel = v; // setting 'code model'
    return TRUE;
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

    // Detects code chosen by user
    if (strcmp(str, "DEFAULT") == 0) v = 0;
    else if (strcmp(str, "JITDEFAULT") == 0) v = 1;
    else if (strcmp(str, "SMALL") == 0) v = 2;
    else if (strcmp(str, "KERNEL") == 0) v = 3;
    else if (strcmp(str, "MEDIUM") == 0) v = 4;
    else if (strcmp(str, "LARGE") == 0) v = 5;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.codemodel = v; // setting 'code model'
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"codemodel");
    return FALSE;
  }
}

static Int
p_reset_codemodel( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_enginebuilder.codemodel = 1;
  return TRUE;
}

static Int
p_enable_mcjit( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_enginebuilder.usemcjit = 1;
  return TRUE;
}

static Int
p_disable_mcjit( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_enginebuilder.usemcjit = 0;
  return TRUE;
}

static Int
p_register_allocator( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < 0 || v > 3) {
      // value passed by argument is out of known range (0 = basic; 1 = fast; 2 = greedy; 3 = PBQP)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.regallocator = (enumRegAllocator)v; // setting 'register allocator'
    return TRUE;
  }
  else if (IsAtomTerm(t)) {
    // ARG1 is atom
    enumRegAllocator v;
    int i = 0, j = 0;
    char *tmp;
    // gets string from atom and stores it on 'str'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    // Makes upper characters of 'str' (for comparison)
    UPPER_ENTRY(str);

    // Detects register allocator chosen by user
    if (strcmp(str, "BASIC") == 0) v = REG_ALLOC_BASIC;
    else if (strcmp(str, "FAST") == 0) v = REG_ALLOC_FAST;
    else if (strcmp(str, "GREEDY") == 0) v = REG_ALLOC_GREEDY;
    else if (strcmp(str, "PBQP") == 0) v = REG_ALLOC_PBQP;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.codegen_struc.struc_enginebuilder.regallocator = v; // setting 'register allocator'
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"register_allocator");
    return FALSE;
  }
}

static Int
p_reset_register_allocator( USES_REGS1 )
{
  ExpEnv.codegen_struc.struc_enginebuilder.regallocator = REG_ALLOC_GREEDY;
  return TRUE;
}

#pragma GCC diagnostic pop

void
Yap_InitJitCodegenPreds( void )
{
  Yap_InitCPred("enable_framepointer_elimination", 0, p_enable_framepointer_elimination, SafePredFlag);
  Yap_InitCPred("more_precise_fp_mad_option", 0, p_more_precise_fp_mad_option, SafePredFlag);
  Yap_InitCPred("excess_fp_precision", 0, p_excess_fp_precision, SafePredFlag);
  Yap_InitCPred("safe_fp_math", 0, p_safe_fp_math, SafePredFlag);
  Yap_InitCPred("rounding_mode_not_changed", 0, p_rounding_mode_not_changed, SafePredFlag);
  Yap_InitCPred("no_use_soft_float", 0, p_no_use_soft_float, SafePredFlag);
  Yap_InitCPred("disable_jit_exception_handling", 0, p_disable_jit_exception_handling, SafePredFlag);
  Yap_InitCPred("disable_jit_emit_debug_info", 0, p_disable_jit_emit_debug_info, SafePredFlag);
  Yap_InitCPred("disable_jit_emit_debug_info_to_disk", 0, p_disable_jit_emit_debug_info_to_disk, SafePredFlag);
  Yap_InitCPred("no_guaranteed_tail_call_opt", 0, p_no_guaranteed_tail_call_opt, SafePredFlag);
  Yap_InitCPred("enable_tail_calls", 0, p_enable_tail_calls, SafePredFlag);
  Yap_InitCPred("disable_fast_isel", 0, p_disable_fast_isel, SafePredFlag);
  Yap_InitCPred("disable_framepointer_elimination", 0, p_disable_framepointer_elimination, SafePredFlag);
  Yap_InitCPred("less_precise_fp_mad_option", 0, p_less_precise_fp_mad_option, SafePredFlag);
  Yap_InitCPred("no_excess_fp_precision", 0, p_no_excess_fp_precision, SafePredFlag);
  Yap_InitCPred("unsafe_fp_math", 0, p_unsafe_fp_math, SafePredFlag);
  Yap_InitCPred("rounding_mode_dynamically_changed", 0, p_rounding_mode_dynamically_changed, SafePredFlag);
  Yap_InitCPred("use_soft_float", 0, p_use_soft_float, SafePredFlag);
  Yap_InitCPred("enable_jit_exception_handling", 0, p_enable_jit_exception_handling, SafePredFlag);
  Yap_InitCPred("enable_jit_emit_debug_info", 0, p_enable_jit_emit_debug_info, SafePredFlag);
  Yap_InitCPred("enable_jit_emit_debug_info_to_disk", 0, p_enable_jit_emit_debug_info_to_disk, SafePredFlag);
  Yap_InitCPred("guaranteed_tail_call_opt", 0, p_guaranteed_tail_call_opt, SafePredFlag);
  Yap_InitCPred("disable_tail_calls", 0, p_disable_tail_calls, SafePredFlag);
  Yap_InitCPred("enable_fast_isel", 0, p_enable_fast_isel, SafePredFlag);
  Yap_InitCPred("fp_abitype", 1, p_fp_abitype, SafePredFlag);
  Yap_InitCPred("default_fp_abitype", 0, p_default_fp_abitype, SafePredFlag);
  Yap_InitCPred("engine_opt_level", 1, p_engine_opt_level, SafePredFlag);
  Yap_InitCPred("reset_engine_opt_level", 0, p_reset_engine_opt_level, SafePredFlag);
  Yap_InitCPred("relocmodel", 1, p_relocmodel, SafePredFlag);
  Yap_InitCPred("reset_relocmodel", 0, p_reset_relocmodel, SafePredFlag);
  Yap_InitCPred("codemodel", 1, p_codemodel, SafePredFlag);
  Yap_InitCPred("reset_codemodel", 0, p_reset_codemodel, SafePredFlag);
  Yap_InitCPred("enable_mcjit", 0, p_enable_mcjit, SafePredFlag);
  Yap_InitCPred("disable_mcjit", 0, p_disable_mcjit, SafePredFlag);
  Yap_InitCPred("register_allocator", 1, p_register_allocator, SafePredFlag);
  Yap_InitCPred("reset_register_allocator", 0, p_reset_register_allocator, SafePredFlag);
}
