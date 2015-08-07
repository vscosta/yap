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
* File:		jit_transformpreds.c					 *
* comments:	JIT Compiler Optimizations predicates			 *
*									 *
* Last rev:     2013-10-18                               		 *
*************************************************************************/

#define JIT_CODE 1

#include "jit_predicates.hpp"

#define N_TRANSFORM_PASSES 69

// Disable one (passed by argument) LLVM transform pass
static Int  p_disable_transform_pass( USES_REGS1 );

// Enable one (passed by argument) LLVM transform pass
static Int  p_transform_pass( USES_REGS1 );

// Enable one (passed by argument) LLVM transform pass
static Int  p_enable_transform_pass( USES_REGS1 );

// Enable a list (passed by argument) of LLVM transform passes
static Int  p_transform_passes( USES_REGS1 );

// Enable all available LLVM transform passes
static Int  p_enable_all_transform_passes( USES_REGS1 );

// Disable all available LLVM transform passes
static Int  p_disable_all_transform_passes( USES_REGS1 );

// Enable n LLVM transform passes (randomly)
static Int  p_n_transform_passes( USES_REGS1 );

// Enable a transform level
static Int  p_transform_level( USES_REGS1 );

// Max element of Argument Promotion Pass
static Int  p_argument_promotion_max_elements( USES_REGS1 );

// Threshold of Scalar Repl Aggregates Pass
static Int  p_scalar_replace_aggregates_threshold( USES_REGS1 );

// Threshold of Loop Unroll Pass
static Int  p_loop_unroll_threshold( USES_REGS1 );

// Threshold of Function Inlining Pass
static Int  p_inline_threshold( USES_REGS1 );

// Eliminates (or not) only debugging information on Strip Symbols Pass
static Int  p_strip_symbols_pass_type( USES_REGS1 );

// Optimizes (or not) for size on Loop Unswitch Pass
static Int  p_loop_unswitch_optimize_for_size( USES_REGS1 );

/*
 * Default value of 'max elements on Argument Promotion Pass *
 *                  'threshold of Scalar Repl Aggregates Pass *
 *                  'threshold of Loop Unroll Pass *
 *                  'threshold of Function Inlining Pass *
 *                  'Strip Symbols Pass *
 *                  'Loop Unswitch Pass *
*/
static Int  p_default_optimization_args( USES_REGS1 );

// Same as 'p_default_optimization_args'
static Int  p_reset_optimization_args( USES_REGS1 );

// Enable IPO by LLVM
static Int  p_enable_unit_at_time( USES_REGS1 );

// Enable libcalls simplification by LLVM
static Int  p_enable_simplify_libcalls( USES_REGS1 );

// Disable IPO by LLVM
static Int  p_disable_unit_at_time( USES_REGS1 );

// Disable libcalls simplification by LLVM
static Int  p_disable_simplify_libcalls( USES_REGS1 );

// Enable (or not) link-time optimization
static Int  p_link_time_opt1( USES_REGS1 );

/*
 * Same as 'p_link_time_opt1', but accepts 3 arguments: *
 *   1 -- Should I apply this opt?                      *
 *   2 -- Should I run internalize?                     *
 *   3 -- Should I run inliner?                         *
*/
static Int  p_link_time_opt3( USES_REGS1 );

// Enable link-time optimization. Same as 'link_time_opt(true)'
static Int  p_enable_link_time_opt( USES_REGS1 );

/*
 * Same as 'p_enable_link_time_opt', but accepts 2 arguments: *
 *   1 -- Should I run internalize?                     *
 *   2 -- Should I run inliner?                         *
*/
static Int  p_enable_link_time_opt2( USES_REGS1 );

// Disable link-time optimization.
static Int  p_disable_link_time_opt( USES_REGS1 );

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

static Int
p_disable_transform_pass( USES_REGS1 )
{
  // First: stores what transform pass should be disabled

  Term t = Deref(ARG1);
  enumTransformPasses f;
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < 0 || v >= N_TRANSFORM_PASSES) {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    f = (enumTransformPasses)v;
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
    if (strcmp(str, "ALL") == 0) {
      // atom is 'all' -- this will disable all passes
      if (ExpEnv.transform_struc.act_tr) free(ExpEnv.transform_struc.act_tr);
      ExpEnv.transform_struc.act_tr = NULL;
      ExpEnv.transform_struc.n = 0;
      return TRUE;
    }

    // Detects one pass according to 'str' -- store it
    if (strcmp(str, "AGGRESSIVEDCE") == 0 || strcmp(str, "ADCE") == 0) f = t_createAggressiveDCEPass;
    else if (strcmp(str, "ARGUMENTPROMOTION") == 0 || strcmp(str, "ARGPROMOTION") == 0) f = t_createArgumentPromotionPass;
    else if (strcmp(str, "BBVECTORIZE") == 0) f = t_createBBVectorizePass;
    else if (strcmp(str, "BLOCKEXTRACTOR") == 0 || strcmp(str, "EXTRACTBLOCKS") == 0) f = t_createBlockExtractorPass;
    else if (strcmp(str, "BLOCKPLACEMENT") == 0) f = t_createBlockPlacementPass;
    else if (strcmp(str, "BREAKCRITICALEDGES") == 0 || strcmp(str, "BREAKCRITEDGES") == 0) f = t_createBreakCriticalEdgesPass;
    else if (strcmp(str, "CFGSIMPLIFICATION") == 0 || strcmp(str, "SIMPLIFYCFG") == 0) f = t_createCFGSimplificationPass;
    else if (strcmp(str, "CODEGENPREPARE") == 0) f = t_createCodeGenPreparePass;
    else if (strcmp(str, "CONSTANTMERGE") == 0 || strcmp(str, "CONSTMERGE") == 0) f = t_createConstantMergePass;
    else if (strcmp(str, "CONSTANTPROPAGATION") == 0 || strcmp(str, "CONSTPROP") == 0) f = t_createConstantPropagationPass;
    else if (strcmp(str, "CORRELATEDVALUEPROPAGATION") == 0 || strcmp(str, "CORRELATEDPROPAGATION") == 0) f = t_createCorrelatedValuePropagationPass;
    else if (strcmp(str, "DEADARGELIMINATION") == 0 || strcmp(str, "DEADARGELIM") == 0) f = t_createDeadArgEliminationPass;
    else if (strcmp(str, "DEADARGHACKING") == 0 || strcmp(str, "DEADARGHAX0R") == 0) f = t_createDeadArgHackingPass;
    else if (strcmp(str, "DEADCODEELIMINATION") == 0 || strcmp(str, "DCE") == 0) f = t_createDeadCodeEliminationPass;
    else if (strcmp(str, "DEADINSTELIMINATION") == 0 || strcmp(str, "DIE") == 0) f = t_createDeadInstEliminationPass;
    else if (strcmp(str, "DEADSTOREELIMINATION") == 0 || strcmp(str, "DSE") == 0) f = t_createDeadStoreEliminationPass;
    else if (strcmp(str, "DEMOTEREGISTERTOMEMORY") == 0 || strcmp(str, "REG2MEM") == 0) f = t_createDemoteRegisterToMemoryPass;
    else if (strcmp(str, "EARLYCSE") == 0) f = t_createEarlyCSEPass;
    else if (strcmp(str, "FUNCTIONATTRS") == 0) f = t_createFunctionAttrsPass;
    else if (strcmp(str, "FUNCTIONINLINING") == 0 || strcmp(str, "INLINE") == 0) f = t_createFunctionInliningPass;
    else if (strcmp(str, "GLOBALDCE") == 0) f = t_createGlobalDCEPass;
    else if (strcmp(str, "GLOBALOPTIMIZER") == 0 || strcmp(str, "GLOBALOPT") == 0) f = t_createGlobalOptimizerPass;
    else if (strcmp(str, "GVEXTRACTION") == 0) f = t_createGVExtractionPass;
    else if (strcmp(str, "GVN") == 0) f = t_createGVNPass;
    else if (strcmp(str, "INDVARSIMPLIFY") == 0 || strcmp(str, "INDVARS") == 0) f = t_createIndVarSimplifyPass;
    else if (strcmp(str, "INSTRUCTIONCOMBINING") == 0 || strcmp(str, "INSTCOMBINE") == 0) f = t_createInstructionCombiningPass;
    else if (strcmp(str, "INSTRUCTIONNAMER") == 0 || strcmp(str, "INSTNAMER") == 0) f = t_createInstructionNamerPass;
    else if (strcmp(str, "INSTRUCTIONSIMPLIFIER") == 0 || strcmp(str, "INSTSIMPLIFY") == 0) f = t_createInstructionSimplifierPass;
    else if (strcmp(str, "INTERNALIZE") == 0) f = t_createInternalizePass;
    else if (strcmp(str, "IPCONSTANTPROPAGATION") == 0 || strcmp(str, "IPCONSTPROP") == 0) f = t_createIPConstantPropagationPass;
    else if (strcmp(str, "IPSCCP") == 0) f = t_createIPSCCPPass;
    else if (strcmp(str, "JUMPTHREADING") == 0) f = t_createJumpThreadingPass;
    else if (strcmp(str, "LCSSA") == 0) f = t_createLCSSAPass;
    else if (strcmp(str, "LICM") == 0) f = t_createLICMPass;
    else if (strcmp(str, "LOOPDELETION") == 0) f = t_createLoopDeletionPass;
    else if (strcmp(str, "LOOPEXTRACTOR") == 0 || strcmp(str, "LOOPEXTRACT") == 0) f = t_createLoopExtractorPass;
    else if (strcmp(str, "LOOPIDIOM") == 0) f = t_createLoopIdiomPass;
    else if (strcmp(str, "LOOPINSTSIMPLIFY") == 0) f = t_createLoopInstSimplifyPass;
    else if (strcmp(str, "LOOPROTATE") == 0) f = t_createLoopRotatePass;
    else if (strcmp(str, "LOOPSIMPLIFY") == 0) f = t_createLoopSimplifyPass;
    else if (strcmp(str, "LOOPSTRENGTHREDUCE") == 0 || strcmp(str, "LOOPREDUCE") == 0) f = t_createLoopStrengthReducePass;
    else if (strcmp(str, "LOOPUNROLL") == 0) f = t_createLoopUnrollPass;
    else if (strcmp(str, "LOOPUNSWITCH") == 0) f = t_createLoopUnswitchPass;
    else if (strcmp(str, "LOWERATOMIC") == 0) f = t_createLowerAtomicPass;
    else if (strcmp(str, "LOWEREXPECTINTRINSIC") == 0 || strcmp(str, "LOWEREXPECT") == 0) f = t_createLowerExpectIntrinsicPass;
    else if (strcmp(str, "LOWERINVOKE") == 0) f = t_createLowerInvokePass;
    else if (strcmp(str, "LOWERSWITCH") == 0) f = t_createLowerSwitchPass;
    else if (strcmp(str, "MEMCPYOPT") == 0) f = t_createMemCpyOptPass;
    else if (strcmp(str, "MERGEFUNCTIONS") == 0 || strcmp(str, "MERGEFUNC") == 0) f = t_createMergeFunctionsPass;
    else if (strcmp(str, "OBJCARCAPELIM") == 0) f = t_createObjCARCAPElimPass;
    else if (strcmp(str, "OBJCARCCONTRACT") == 0) f = t_createObjCARCContractPass;
    else if (strcmp(str, "OBJCARCEXPAND") == 0) f = t_createObjCARCExpandPass;
    else if (strcmp(str, "OBJCARCOPT") == 0 || strcmp(str, "OBJCARC") == 0) f = t_createObjCARCOptPass;
    else if (strcmp(str, "PARTIALINLINING") == 0 || strcmp(str, "PARTIALINLINER") == 0) f = t_createPartialInliningPass;
    else if (strcmp(str, "PROMOTEMEMORYTOREGISTER") == 0 || strcmp(str, "MEM2REG") == 0) f = t_createPromoteMemoryToRegisterPass;
    else if (strcmp(str, "PRUNEEH") == 0) f = t_createPruneEHPass;
    else if (strcmp(str, "REASSOCIATE") == 0) f = t_createReassociatePass;
    else if (strcmp(str, "SCALARREPLAGGREGATES") == 0 || strcmp(str, "SCALARREPL") == 0) f = t_createScalarReplAggregatesPass;
    else if (strcmp(str, "SCCP") == 0) f = t_createSCCPPass;
    else if (strcmp(str, "SIMPLIFYLIBCALLS") == 0) f = t_createSimplifyLibCallsPass;
    else if (strcmp(str, "SINGLELOOPEXTRACTOR") == 0 || strcmp(str, "LOOPEXTRACTSINGLE") == 0) f = t_createSingleLoopExtractorPass;
    else if (strcmp(str, "SINKING") == 0 || strcmp(str, "SINK") == 0) f = t_createSinkingPass;
    else if (strcmp(str, "STRIPDEADDEBUGINFO") == 0) f = t_createStripDeadDebugInfoPass;
    else if (strcmp(str, "STRIPDEADPROTOTYPES") == 0) f = t_createStripDeadPrototypesPass;
    else if (strcmp(str, "STRIPDEBUGDECLARE") == 0) f = t_createStripDebugDeclarePass;
    else if (strcmp(str, "STRIPNONDEBUGSYMBOLS") == 0) f = t_createStripNonDebugSymbolsPass;
    else if (strcmp(str, "STRIPSYMBOLS") == 0 || strcmp(str, "STRIP") == 0) f = t_createStripSymbolsPass;
    else if (strcmp(str, "TAILCALLELIMINATION") == 0 || strcmp(str, "TAILCALLELIM") == 0) f = t_createTailCallEliminationPass;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Transform pass");
    return FALSE;
  }

  // Second: creates a new list with all transform on 'ExpEnv.analysis_struc.act_tr' but that transform stored on first step
  enumTransformPasses *tmplist = NULL;
  COUNT tmpn = 0;
  int i = 0;
  while (i < ExpEnv.transform_struc.n) {
    if (ExpEnv.transform_struc.act_tr[i] != f) {
      tmpn += 1;
      tmplist = (enumTransformPasses*)realloc(tmplist, tmpn*sizeof(enumTransformPasses));
      tmplist[tmpn-1] = ExpEnv.transform_struc.act_tr[i];
    }
    i += 1;
  }

  // Third: makes 'ExpEnv.analysis_struc.act_tr' to point to new list created on second step
  free(ExpEnv.transform_struc.act_tr);
  ExpEnv.transform_struc.n = tmpn;
  ExpEnv.transform_struc.act_tr = tmplist;
  return TRUE;
}

static Int
p_transform_pass( USES_REGS1 )
{
  // First: disables analysis pass (if be active)
  p_disable_transform_pass( PASS_REGS1 );

  // Second: valids argument and inserts new transform pass
  // valid values for ARG1 are 'integer' and 'atom'
  Term t = Deref(ARG1);
  Int v;
  if (IsIntTerm(t)) {
    // ARG1 is integer
    v = IntOfTerm(t);
    if (v < 0 || v >= N_TRANSFORM_PASSES) {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // creates a new slot in 'ExpEnv.analysis_struc.act_tr' and appends the pass in it
    ExpEnv.transform_struc.n += 1;
    ExpEnv.transform_struc.act_tr = (enumTransformPasses*)
                                   realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n * sizeof(enumTransformPasses));
    ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = (enumTransformPasses)v;
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

    // Detects one pass according to 'str'
    if (strcmp(str, "AGGRESSIVEDCE") == 0 || strcmp(str, "ADCE") == 0) v = t_createAggressiveDCEPass;
    else if (strcmp(str, "ARGUMENTPROMOTION") == 0 || strcmp(str, "ARGPROMOTION") == 0) v = t_createArgumentPromotionPass;
    else if (strcmp(str, "BBVECTORIZE") == 0) v = t_createBBVectorizePass;
    else if (strcmp(str, "BLOCKEXTRACTOR") == 0 || strcmp(str, "EXTRACTBLOCKS") == 0) v = t_createBlockExtractorPass;
    else if (strcmp(str, "BLOCKPLACEMENT") == 0) v = t_createBlockPlacementPass;
    else if (strcmp(str, "BREAKCRITICALEDGES") == 0 || strcmp(str, "BREAKCRITEDGES") == 0) v = t_createBreakCriticalEdgesPass;
    else if (strcmp(str, "CFGSIMPLIFICATION") == 0 || strcmp(str, "SIMPLIFYCFG") == 0) v = t_createCFGSimplificationPass;
    else if (strcmp(str, "CODEGENPREPARE") == 0) v = t_createCodeGenPreparePass;
    else if (strcmp(str, "CONSTANTMERGE") == 0 || strcmp(str, "CONSTMERGE") == 0) v = t_createConstantMergePass;
    else if (strcmp(str, "CONSTANTPROPAGATION") == 0 || strcmp(str, "CONSTPROP") == 0) v = t_createConstantPropagationPass;
    else if (strcmp(str, "CORRELATEDVALUEPROPAGATION") == 0 || strcmp(str, "CORRELATEDPROPAGATION") == 0) v = t_createCorrelatedValuePropagationPass;
    else if (strcmp(str, "DEADARGELIMINATION") == 0 || strcmp(str, "DEADARGELIM") == 0) v = t_createDeadArgEliminationPass;
    else if (strcmp(str, "DEADARGHACKING") == 0 || strcmp(str, "DEADARGHAX0R") == 0) v = t_createDeadArgHackingPass;
    else if (strcmp(str, "DEADCODEELIMINATION") == 0 || strcmp(str, "DCE") == 0) v = t_createDeadCodeEliminationPass;
    else if (strcmp(str, "DEADINSTELIMINATION") == 0 || strcmp(str, "DIE") == 0) v = t_createDeadInstEliminationPass;
    else if (strcmp(str, "DEADSTOREELIMINATION") == 0 || strcmp(str, "DSE") == 0) v = t_createDeadStoreEliminationPass;
    else if (strcmp(str, "DEMOTEREGISTERTOMEMORY") == 0 || strcmp(str, "REG2MEM") == 0) v = t_createDemoteRegisterToMemoryPass;
    else if (strcmp(str, "EARLYCSE") == 0) v = t_createEarlyCSEPass;
    else if (strcmp(str, "FUNCTIONATTRS") == 0) v = t_createFunctionAttrsPass;
    else if (strcmp(str, "FUNCTIONINLINING") == 0 || strcmp(str, "INLINE") == 0) v = t_createFunctionInliningPass;
    else if (strcmp(str, "GLOBALDCE") == 0) v = t_createGlobalDCEPass;
    else if (strcmp(str, "GLOBALOPTIMIZER") == 0 || strcmp(str, "GLOBALOPT") == 0) v = t_createGlobalOptimizerPass;
    else if (strcmp(str, "GVEXTRACTION") == 0) v = t_createGVExtractionPass;
    else if (strcmp(str, "GVN") == 0) v = t_createGVNPass;
    else if (strcmp(str, "INDVARSIMPLIFY") == 0 || strcmp(str, "INDVARS") == 0) v = t_createIndVarSimplifyPass;
    else if (strcmp(str, "INSTRUCTIONCOMBINING") == 0 || strcmp(str, "INSTCOMBINE") == 0) v = t_createInstructionCombiningPass;
    else if (strcmp(str, "INSTRUCTIONNAMER") == 0 || strcmp(str, "INSTNAMER") == 0) v = t_createInstructionNamerPass;
    else if (strcmp(str, "INSTRUCTIONSIMPLIFIER") == 0 || strcmp(str, "INSTSIMPLIFY") == 0) v = t_createInstructionSimplifierPass;
    else if (strcmp(str, "INTERNALIZE") == 0) v = t_createInternalizePass;
    else if (strcmp(str, "IPCONSTANTPROPAGATION") == 0 || strcmp(str, "IPCONSTPROP") == 0) v = t_createIPConstantPropagationPass;
    else if (strcmp(str, "IPSCCP") == 0) v = t_createIPSCCPPass;
    else if (strcmp(str, "JUMPTHREADING") == 0) v = t_createJumpThreadingPass;
    else if (strcmp(str, "LCSSA") == 0) v = t_createLCSSAPass;
    else if (strcmp(str, "LICM") == 0) v = t_createLICMPass;
    else if (strcmp(str, "LOOPDELETION") == 0) v = t_createLoopDeletionPass;
    else if (strcmp(str, "LOOPEXTRACTOR") == 0 || strcmp(str, "LOOPEXTRACT") == 0) v = t_createLoopExtractorPass;
    else if (strcmp(str, "LOOPIDIOM") == 0) v = t_createLoopIdiomPass;
    else if (strcmp(str, "LOOPINSTSIMPLIFY") == 0) v = t_createLoopInstSimplifyPass;
    else if (strcmp(str, "LOOPROTATE") == 0) v = t_createLoopRotatePass;
    else if (strcmp(str, "LOOPSIMPLIFY") == 0) v = t_createLoopSimplifyPass;
    else if (strcmp(str, "LOOPSTRENGTHREDUCE") == 0 || strcmp(str, "LOOPREDUCE") == 0) v = t_createLoopStrengthReducePass;
    else if (strcmp(str, "LOOPUNROLL") == 0) v = t_createLoopUnrollPass;
    else if (strcmp(str, "LOOPUNSWITCH") == 0) v = t_createLoopUnswitchPass;
    else if (strcmp(str, "LOWERATOMIC") == 0) v = t_createLowerAtomicPass;
    else if (strcmp(str, "LOWEREXPECTINTRINSIC") == 0 || strcmp(str, "LOWEREXPECT") == 0) v = t_createLowerExpectIntrinsicPass;
    else if (strcmp(str, "LOWERINVOKE") == 0) v = t_createLowerInvokePass;
    else if (strcmp(str, "LOWERSWITCH") == 0) v = t_createLowerSwitchPass;
    else if (strcmp(str, "MEMCPYOPT") == 0) v = t_createMemCpyOptPass;
    else if (strcmp(str, "MERGEFUNCTIONS") == 0 || strcmp(str, "MERGEFUNC") == 0) v = t_createMergeFunctionsPass;
    else if (strcmp(str, "OBJCARCAPELIM") == 0) v = t_createObjCARCAPElimPass;
    else if (strcmp(str, "OBJCARCCONTRACT") == 0) v = t_createObjCARCContractPass;
    else if (strcmp(str, "OBJCARCEXPAND") == 0) v = t_createObjCARCExpandPass;
    else if (strcmp(str, "OBJCARCOPT") == 0 || strcmp(str, "OBJCARC") == 0) v = t_createObjCARCOptPass;
    else if (strcmp(str, "PARTIALINLINING") == 0 || strcmp(str, "PARTIALINLINER") == 0) v = t_createPartialInliningPass;
    else if (strcmp(str, "PROMOTEMEMORYTOREGISTER") == 0 || strcmp(str, "MEM2REG") == 0) v = t_createPromoteMemoryToRegisterPass;
    else if (strcmp(str, "PRUNEEH") == 0) v = t_createPruneEHPass;
    else if (strcmp(str, "REASSOCIATE") == 0) v = t_createReassociatePass;
    else if (strcmp(str, "SCALARREPLAGGREGATES") == 0 || strcmp(str, "SCALARREPL") == 0) v = t_createScalarReplAggregatesPass;
    else if (strcmp(str, "SCCP") == 0) v = t_createSCCPPass;
    else if (strcmp(str, "SIMPLIFYLIBCALLS") == 0) v = t_createSimplifyLibCallsPass;
    else if (strcmp(str, "SINGLELOOPEXTRACTOR") == 0 || strcmp(str, "LOOPEXTRACTSINGLE") == 0) v = t_createSingleLoopExtractorPass;
    else if (strcmp(str, "SINKING") == 0 || strcmp(str, "SINK") == 0) v = t_createSinkingPass;
    else if (strcmp(str, "STRIPDEADDEBUGINFO") == 0) v = t_createStripDeadDebugInfoPass;
    else if (strcmp(str, "STRIPDEADPROTOTYPES") == 0) v = t_createStripDeadPrototypesPass;
    else if (strcmp(str, "STRIPDEBUGDECLARE") == 0) v = t_createStripDebugDeclarePass;
    else if (strcmp(str, "STRIPNONDEBUGSYMBOLS") == 0) v = t_createStripNonDebugSymbolsPass;
    else if (strcmp(str, "STRIPSYMBOLS") == 0 || strcmp(str, "STRIP") == 0) v = t_createStripSymbolsPass;
    else if (strcmp(str, "TAILCALLELIMINATION") == 0 || strcmp(str, "TAILCALLELIM") == 0) v = t_createTailCallEliminationPass;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }

    // creates a new slot in 'ExpEnv.analysis_struc.act_tr' and appends the pass in it
    ExpEnv.transform_struc.n += 1;
    ExpEnv.transform_struc.act_tr = (enumTransformPasses*)
                                   realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n * sizeof(enumTransformPasses));
    ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = v;
    ExpEnv.transform_struc.optlevel = -1; // using this, optlevel won't be used
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Transform pass");
    return FALSE;
  }
}

static Int
p_enable_transform_pass( USES_REGS1 )
{
  return p_transform_pass( PASS_REGS1 );
}

static Int
p_transform_passes( USES_REGS1 )
{
  int i = 0, j = 0;
  char *tmp;
  // valid values for ARG1 are 'atom' and 'list (pair)'
  Term t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    // ARG1 is atom
    // gets string from atom and stores it on 'str'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    // Makes upper characters of 'str' (for comparison)
    UPPER_ENTRY(str);

    // If ARG1 is atom, 'all' is the only valid value
    if (strcmp(str, "ALL") == 0) {
      // First: disables all transform passes
      free(ExpEnv.transform_struc.act_tr);
      ExpEnv.transform_struc.act_tr = NULL;
      ExpEnv.transform_struc.n = 0;

      // Second, insert all transform passes on 'ExpEnv.analysis_struc.act_tr'
      int i;
      for (i = 0; i < N_TRANSFORM_PASSES; i++) {
        ExpEnv.transform_struc.n += 1;
    	ExpEnv.transform_struc.act_tr = (enumTransformPasses*)realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n * sizeof(enumTransformPasses));
	ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = i;
      }
      return TRUE;
    }
    // value passed by argument is out of known range (ARG1 differs of 'all')
    Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
    return FALSE;
  }
  else if (IsPairTerm(t)) {
    // ARG1 is list
    // First: disables all transform passes
    if (ExpEnv.transform_struc.act_tr) free(ExpEnv.transform_struc.act_tr);
    ExpEnv.transform_struc.act_tr = NULL;
    ExpEnv.transform_struc.n = 0;

    // Second: scrolls over the list treating each element individually
    Term u = HeadOfTermCell(t); // get head of list 't'
    u = Deref(u);
    while (1) {
      Int v;
      enumTransformPasses w;

      // valid values for head are 'integer' and 'atom' (the list can contain both)
      if (IsIntTerm(u)) {
        // head is integer
        v = IntOfTerm(u);
        if (v < 0 || v >= N_TRANSFORM_PASSES) {
          // head's value is out of known range
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
        }

        // insert transform pass defined by 'head' on 'ExpEnv.analysis_struc.act_tr'
	ExpEnv.transform_struc.n += 1;
        ExpEnv.transform_struc.act_tr = (enumTransformPasses*)realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n * sizeof(enumTransformPasses));
        ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = (enumTransformPasses)v;
      }
      else if (IsAtomTerm(u)) {
        // head is atom
        int i = 0, j = 0;
        // gets string from atom and stores it on 'str'
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
        // Makes upper characters of 'str' (for comparison)
	UPPER_ENTRY(str);

        // Detects one pass according to 'str'
        if (strcmp(str, "AGGRESSIVEDCE") == 0 || strcmp(str, "ADCE") == 0) w = t_createAggressiveDCEPass;
        else if (strcmp(str, "ARGUMENTPROMOTION") == 0 || strcmp(str, "ARGPROMOTION") == 0) w = t_createArgumentPromotionPass;
        else if (strcmp(str, "BBVECTORIZE") == 0) w = t_createBBVectorizePass;
        else if (strcmp(str, "BLOCKEXTRACTOR") == 0 || strcmp(str, "EXTRACTBLOCKS") == 0) w = t_createBlockExtractorPass;
        else if (strcmp(str, "BLOCKPLACEMENT") == 0) w = t_createBlockPlacementPass;
        else if (strcmp(str, "BREAKCRITICALEDGES") == 0 || strcmp(str, "BREAKCRITEDGES") == 0) w = t_createBreakCriticalEdgesPass;
        else if (strcmp(str, "CFGSIMPLIFICATION") == 0 || strcmp(str, "SIMPLIFYCFG") == 0) w = t_createCFGSimplificationPass;
        else if (strcmp(str, "CODEGENPREPARE") == 0) w = t_createCodeGenPreparePass;
        else if (strcmp(str, "CONSTANTMERGE") == 0 || strcmp(str, "CONSTMERGE") == 0) w = t_createConstantMergePass;
        else if (strcmp(str, "CONSTANTPROPAGATION") == 0 || strcmp(str, "CONSTPROP") == 0) w = t_createConstantPropagationPass;
        else if (strcmp(str, "CORRELATEDVALUEPROPAGATION") == 0 || strcmp(str, "CORRELATEDPROPAGATION") == 0) w = t_createCorrelatedValuePropagationPass;
        else if (strcmp(str, "DEADARGELIMINATION") == 0 || strcmp(str, "DEADARGELIM") == 0) w = t_createDeadArgEliminationPass;
        else if (strcmp(str, "DEADARGHACKING") == 0 || strcmp(str, "DEADARGHAX0R") == 0) w = t_createDeadArgHackingPass;
        else if (strcmp(str, "DEADCODEELIMINATION") == 0 || strcmp(str, "DCE") == 0) w = t_createDeadCodeEliminationPass;
        else if (strcmp(str, "DEADINSTELIMINATION") == 0 || strcmp(str, "DIE") == 0) w = t_createDeadInstEliminationPass;
        else if (strcmp(str, "DEADSTOREELIMINATION") == 0 || strcmp(str, "DSE") == 0) w = t_createDeadStoreEliminationPass;
        else if (strcmp(str, "DEMOTEREGISTERTOMEMORY") == 0 || strcmp(str, "REG2MEM") == 0) w = t_createDemoteRegisterToMemoryPass;
        else if (strcmp(str, "EARLYCSE") == 0) w = t_createEarlyCSEPass;
        else if (strcmp(str, "FUNCTIONATTRS") == 0) w = t_createFunctionAttrsPass;
        else if (strcmp(str, "FUNCTIONINLINING") == 0 || strcmp(str, "INLINE") == 0) w = t_createFunctionInliningPass;
        else if (strcmp(str, "GLOBALDCE") == 0) w = t_createGlobalDCEPass;
        else if (strcmp(str, "GLOBALOPTIMIZER") == 0 || strcmp(str, "GLOBALOPT") == 0) w = t_createGlobalOptimizerPass;
        else if (strcmp(str, "GVEXTRACTION") == 0) w = t_createGVExtractionPass;
        else if (strcmp(str, "GVN") == 0) w = t_createGVNPass;
        else if (strcmp(str, "INDVARSIMPLIFY") == 0 || strcmp(str, "INDVARS") == 0) w = t_createIndVarSimplifyPass;
        else if (strcmp(str, "INSTRUCTIONCOMBINING") == 0 || strcmp(str, "INSTCOMBINE") == 0) w = t_createInstructionCombiningPass;
        else if (strcmp(str, "INSTRUCTIONNAMER") == 0 || strcmp(str, "INSTNAMER") == 0) w = t_createInstructionNamerPass;
        else if (strcmp(str, "INSTRUCTIONSIMPLIFIER") == 0 || strcmp(str, "INSTSIMPLIFY") == 0) w = t_createInstructionSimplifierPass;
        else if (strcmp(str, "INTERNALIZE") == 0) w = t_createInternalizePass;
        else if (strcmp(str, "IPCONSTANTPROPAGATION") == 0 || strcmp(str, "IPCONSTPROP") == 0) w = t_createIPConstantPropagationPass;
        else if (strcmp(str, "IPSCCP") == 0) w = t_createIPSCCPPass;
        else if (strcmp(str, "JUMPTHREADING") == 0) w = t_createJumpThreadingPass;
        else if (strcmp(str, "LCSSA") == 0) w = t_createLCSSAPass;
        else if (strcmp(str, "LICM") == 0) w = t_createLICMPass;
        else if (strcmp(str, "LOOPDELETION") == 0) w = t_createLoopDeletionPass;
        else if (strcmp(str, "LOOPEXTRACTOR") == 0 || strcmp(str, "LOOPEXTRACT") == 0) w = t_createLoopExtractorPass;
        else if (strcmp(str, "LOOPIDIOM") == 0) w = t_createLoopIdiomPass;
        else if (strcmp(str, "LOOPINSTSIMPLIFY") == 0) w = t_createLoopInstSimplifyPass;
        else if (strcmp(str, "LOOPROTATE") == 0) w = t_createLoopRotatePass;
        else if (strcmp(str, "LOOPSIMPLIFY") == 0) w = t_createLoopSimplifyPass;
        else if (strcmp(str, "LOOPSTRENGTHREDUCE") == 0 || strcmp(str, "LOOPREDUCE") == 0) w = t_createLoopStrengthReducePass;
        else if (strcmp(str, "LOOPUNROLL") == 0) w = t_createLoopUnrollPass;
        else if (strcmp(str, "LOOPUNSWITCH") == 0) w = t_createLoopUnswitchPass;
        else if (strcmp(str, "LOWERATOMIC") == 0) w = t_createLowerAtomicPass;
        else if (strcmp(str, "LOWEREXPECTINTRINSIC") == 0 || strcmp(str, "LOWEREXPECT") == 0) w = t_createLowerExpectIntrinsicPass;
        else if (strcmp(str, "LOWERINVOKE") == 0) w = t_createLowerInvokePass;
        else if (strcmp(str, "LOWERSWITCH") == 0) w = t_createLowerSwitchPass;
        else if (strcmp(str, "MEMCPYOPT") == 0) w = t_createMemCpyOptPass;
        else if (strcmp(str, "MERGEFUNCTIONS") == 0 || strcmp(str, "MERGEFUNC") == 0) w = t_createMergeFunctionsPass;
        else if (strcmp(str, "OBJCARCAPELIM") == 0) w = t_createObjCARCAPElimPass;
        else if (strcmp(str, "OBJCARCCONTRACT") == 0) w = t_createObjCARCContractPass;
        else if (strcmp(str, "OBJCARCEXPAND") == 0) w = t_createObjCARCExpandPass;
        else if (strcmp(str, "OBJCARCOPT") == 0 || strcmp(str, "OBJCARC") == 0) w = t_createObjCARCOptPass;
        else if (strcmp(str, "PARTIALINLINING") == 0 || strcmp(str, "PARTIALINLINER") == 0) w = t_createPartialInliningPass;
        else if (strcmp(str, "PROMOTEMEMORYTOREGISTER") == 0 || strcmp(str, "MEM2REG") == 0) w = t_createPromoteMemoryToRegisterPass;
        else if (strcmp(str, "PRUNEEH") == 0) w = t_createPruneEHPass;
        else if (strcmp(str, "REASSOCIATE") == 0) w = t_createReassociatePass;
        else if (strcmp(str, "SCALARREPLAGGREGATES") == 0 || strcmp(str, "SCALARREPL") == 0) w = t_createScalarReplAggregatesPass;
        else if (strcmp(str, "SCCP") == 0) w = t_createSCCPPass;
        else if (strcmp(str, "SIMPLIFYLIBCALLS") == 0) w = t_createSimplifyLibCallsPass;
        else if (strcmp(str, "SINGLELOOPEXTRACTOR") == 0 || strcmp(str, "LOOPEXTRACTSINGLE") == 0) w = t_createSingleLoopExtractorPass;
        else if (strcmp(str, "SINKING") == 0 || strcmp(str, "SINK") == 0) w = t_createSinkingPass;
        else if (strcmp(str, "STRIPDEADDEBUGINFO") == 0) w = t_createStripDeadDebugInfoPass;
        else if (strcmp(str, "STRIPDEADPROTOTYPES") == 0) w = t_createStripDeadPrototypesPass;
        else if (strcmp(str, "STRIPDEBUGDECLARE") == 0) w = t_createStripDebugDeclarePass;
        else if (strcmp(str, "STRIPNONDEBUGSYMBOLS") == 0) w = t_createStripNonDebugSymbolsPass;
        else if (strcmp(str, "STRIPSYMBOLS") == 0 || strcmp(str, "STRIP") == 0) w = t_createStripSymbolsPass;
        else if (strcmp(str, "TAILCALLELIMINATION") == 0 || strcmp(str, "TAILCALLELIM") == 0) w = t_createTailCallEliminationPass;
        else {
          // head's value is out of known range
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
        }

        // insert transform pass defined by 'head' on 'ExpEnv.analysis_struc.act_tr'
	ExpEnv.transform_struc.n += 1;
	ExpEnv.transform_struc.act_tr = (enumTransformPasses*)realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n * sizeof(enumTransformPasses));
	ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = w;
      }
      else {
        // head's value is not an integer or atom
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Transform pass");
	return FALSE;
      }

      // here, 'u' is the current head of list (just been treated) and 't' is our list itself
      t = TailOfTermCell(t); // 't' is now our list without 'u' (tail of 't')
      t = Deref(t);
      if (IsAtomTerm(t)) break; // if 't' is an empty list (which is treated as atom by Prolog), we finish the loop
      u = HeadOfTermCell(t); // else, 'u' is now next head which will be treated
      u = Deref(u);
    }
    ExpEnv.transform_struc.optlevel = -1; // using this, optlevel won't be used
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Transform pass");
    return FALSE;
  }
}

static Int
p_enable_all_transform_passes( USES_REGS1 )
{
  // Same as 'transform_passes(all)'
  // First, disable all analysis passes
  if (ExpEnv.transform_struc.act_tr) free(ExpEnv.transform_struc.act_tr);
  ExpEnv.transform_struc.act_tr = NULL;
  ExpEnv.transform_struc.n = 0;
  // Second, insert all transform passes
  int i;
  for (i = 0; i < N_TRANSFORM_PASSES; i++) {
    ExpEnv.transform_struc.n += 1;
    ExpEnv.transform_struc.act_tr = (enumTransformPasses*)
                                   realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n * sizeof(enumTransformPasses));
    ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = (enumTransformPasses)i;
  }
  ExpEnv.transform_struc.optlevel = -1; // using this, optlevel won't be used
  return TRUE;
}

static Int
p_disable_all_transform_passes( USES_REGS1 )
{
  // Just empty 'ExpEnv.analysis_struc.act_tr'
  if (ExpEnv.transform_struc.act_tr) free(ExpEnv.transform_struc.act_tr);
  ExpEnv.transform_struc.act_tr = NULL;
  ExpEnv.transform_struc.n = 0;
  return TRUE;
}

static Int
p_n_transform_passes( USES_REGS1 )
{
  // valid value for ARG1 is just 'integer' (number of transform passes added randomly)
  Term t = Deref(ARG1);
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int n = IntOfTerm(t);
    if (n < 0 || n >= N_TRANSFORM_PASSES) {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // First: disables all transform passes
    free(ExpEnv.transform_struc.act_tr);
    ExpEnv.transform_struc.act_tr = NULL;
    ExpEnv.transform_struc.n = 0;

    /* Second: adds n transform passes randomly */
    srand (time(NULL));
    while (ExpEnv.transform_struc.n < n) {
      CELL v = rand() % (n);
      if (
	 ((enumTransformPasses)v == t_createBlockExtractorPass) ||
	 ((enumTransformPasses)v == t_createDeadArgHackingPass) ||
	 ((enumTransformPasses)v == t_createGVExtractionPass) ||
	 ((enumTransformPasses)v == t_createLoopExtractorPass) ||
	 ((enumTransformPasses)v == t_createStripDeadDebugInfoPass) ||
	 ((enumTransformPasses)v == t_createStripDeadPrototypesPass) ||
	 ((enumTransformPasses)v == t_createStripDebugDeclarePass) ||
	 ((enumTransformPasses)v == t_createStripNonDebugSymbolsPass) ||
	 ((enumTransformPasses)v == t_createStripSymbolsPass)
	 ) {
           // I can't add these passes (they are used just for debugging)
	   n -= 1;
	   continue;
      }
      COUNT i = 0;

      /* I must ensure all steps are different from one another */
      while (i < ExpEnv.transform_struc.n) {
        if (ExpEnv.transform_struc.act_tr[i] == (enumTransformPasses)v) break;
        i++;
      }
      if (i == ExpEnv.transform_struc.n) {
      /***/

        // If pass is not specific for debugging and not yet added so I add it */
        ExpEnv.transform_struc.n += 1;
        ExpEnv.transform_struc.act_tr = (enumTransformPasses*)
		                                 realloc(ExpEnv.transform_struc.act_tr, ExpEnv.transform_struc.n*sizeof(enumTransformPasses));
        ExpEnv.transform_struc.act_tr[ExpEnv.transform_struc.n-1] = (enumTransformPasses)v;
      }
    }
    ExpEnv.transform_struc.optlevel = -1; // using this, optlevel won't be used
    return TRUE;
  }
  else {
    // ARG1 is not an integer
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"N transform passes");
    return FALSE;
  }
}

static Int
p_transform_level( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 is just 'integer'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int lvl = IntOfTerm(t);
    if (lvl < -1 || lvl > 3) {
      // value passed by argument is out of known range (valid values are: 0, 1, 2, and 3)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    ExpEnv.transform_struc.optlevel = lvl; // if 'transform_pass' or similars are used before, they won't be considered

    /* Setting function inlining threshold (for createFunctionInliningPass) -- level 0 does not apply this */
    if (ExpEnv.transform_struc.optlevel == 3)
      ExpEnv.transform_struc.opt_args.inline_threshold = 225;
    else if (ExpEnv.transform_struc.optlevel == 2)
      ExpEnv.transform_struc.opt_args.inline_threshold = 100;
    else
      ExpEnv.transform_struc.opt_args.inline_threshold = 40;
    return TRUE;
  }
  else {
    // ARG1 is not an integer
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Transform level");
    return FALSE;
  }
}

static Int
p_argument_promotion_max_elements( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 is just 'integer'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
#if SIZEOF_INT_P==4
    if (v < 0 || v > 0xffffffff) { // 32 bits
#else /* SIZEOF_INT_P==8 */
    if (v < 0 || v > 0x1999999999999999) { // 64 bits
#endif
      // value passed by argument is out of known range (max value is machine-dependent)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // Setting max elements for Argument Promotion Pass
    ExpEnv.transform_struc.opt_args.arg_promotion_max_elements = v;
    return TRUE;
  }
  else {
    // ARG1 is not an integer
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Argument Promotion Max Elements");
    return FALSE;
  }
}

static Int
p_scalar_replace_aggregates_threshold( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 is just 'integer'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < -1 || v > 0xffff) {
      // value passed by argument is out of known range (max value is 65535)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // Setting threshold for Scalar Repl Aggregates Pass
    ExpEnv.transform_struc.opt_args.scalar_replace_aggregates_threshold = v;
    return TRUE;
  }
  else {
    // ARG1 is not an integer
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Scalar Replace Aggregates Threshold");
    return FALSE;
  }
}

static Int
p_loop_unroll_threshold( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 is just 'integer'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < -1 || v > 0xffff) {
      // value passed by argument is out of known range (max value is 65535)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // Setting threshold for Loop Unroll Pass
    ExpEnv.transform_struc.opt_args.loop_unroll_threshold = v;
    return TRUE;
  }
  else {
    // ARG1 is not an integer
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Loop Unroll Threshold");
    return FALSE;
  }
}

static Int
p_inline_threshold( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 is just 'integer'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < -1 || v > 0xffff) {
      // value passed by argument is out of known range (max value is 65535)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // Setting threshold for Function Inlining Pass
    ExpEnv.transform_struc.opt_args.inline_threshold = v;
    return TRUE;
  }
  else {
    // ARG1 is not an integer
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Inline Threshold");
    return FALSE;
  }
}

static Int
p_strip_symbols_pass_type( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    // Setting Strip Symbols Pass condition. '0' is false; Any other value is true
    ExpEnv.transform_struc.opt_args.strip_symbols_pass_type = IntOfTerm(t);
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

    // Detecting value according to 'str'
    if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.opt_args.strip_symbols_pass_type = 0;
    else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.opt_args.strip_symbols_pass_type = 1;
    else {
      // value passed by argument is out of known range (only 'true' or 'false' are allowed)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Strip Symbols Pass Type");
    return FALSE;
  }
}

static Int
p_loop_unswitch_optimize_for_size( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    // Setting Loop Unswitch Pass condition. '0' is false; Any other value is true
    ExpEnv.transform_struc.opt_args.loop_unswitch_optimize_for_size = IntOfTerm(t);
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

    // Detecting value according to 'str'
    if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.opt_args.loop_unswitch_optimize_for_size = 0;
    else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.opt_args.loop_unswitch_optimize_for_size = 1;
    else {
      // value passed by argument is out of known range (only 'true' or 'false' are allowed)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Loop Unswitch Optimize for Size");
    return FALSE;
  }
}

static Int
p_default_optimization_args( USES_REGS1 )
{
  /* resetting arguments used in some passes */
  ExpEnv.transform_struc.opt_args.arg_promotion_max_elements = 3; // Argument Promotion Pass
  ExpEnv.transform_struc.opt_args.strip_symbols_pass_type = 0; // Strip Symbols Pass
  ExpEnv.transform_struc.opt_args.scalar_replace_aggregates_threshold = -1; // Scalar Repl Aggregates Pass
  ExpEnv.transform_struc.opt_args.loop_unswitch_optimize_for_size = 0; // Loop Unswitch Pass
  ExpEnv.transform_struc.opt_args.loop_unroll_threshold = -1; // Loop Unroll Pass

  // Function Inlining Pass (according current 'optlevel')
  if (ExpEnv.transform_struc.optlevel == 3)
    ExpEnv.transform_struc.opt_args.inline_threshold = 225;
  else if (ExpEnv.transform_struc.optlevel == 2)
    ExpEnv.transform_struc.opt_args.inline_threshold = 100;
  else if (ExpEnv.transform_struc.optlevel == 1)
    ExpEnv.transform_struc.opt_args.inline_threshold = 40;
  else // 0
    ExpEnv.transform_struc.opt_args.inline_threshold = 0;
  /***/

  // Link-time optimization
  ExpEnv.transform_struc.link_time_opt.enabled = 0;
  ExpEnv.transform_struc.link_time_opt.internalize = 0;
  ExpEnv.transform_struc.link_time_opt.runinliner = 0;
  return TRUE;
}

static Int
p_reset_optimization_args( USES_REGS1 )
{
  // Same as 'p_default_optimization_args'
  p_default_optimization_args( PASS_REGS1 );
  return TRUE;
}

static Int
p_enable_unit_at_time( USES_REGS1 )
{
  // Enable IPO
  ExpEnv.transform_struc.unit_at_time_enabled = 1;
  return TRUE;
}

static Int
p_enable_simplify_libcalls( USES_REGS1 )
{
  // Enable libcalls simplification
  ExpEnv.transform_struc.simplify_libcalls_enabled = 1;
  return TRUE;
}

static Int
p_disable_unit_at_time( USES_REGS1 )
{
  // Disable IPO
  ExpEnv.transform_struc.unit_at_time_enabled = 0;
  return TRUE;
}

static Int
p_disable_simplify_libcalls( USES_REGS1 )
{
  // Disable libcalls simplification
  ExpEnv.transform_struc.simplify_libcalls_enabled = 0;
  return TRUE;
}

static Int
p_link_time_opt1( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // 'ARG1' is integer
    // enabling link-time optimization ('internalize' = 1 and 'runinliner' = 0 are default)
    ExpEnv.transform_struc.link_time_opt.enabled = IntOfTerm(t);
    ExpEnv.transform_struc.link_time_opt.internalize = 1;
    ExpEnv.transform_struc.link_time_opt.runinliner = 0;
    return TRUE;
  }
  else if (IsAtomTerm(t)) {
    // 'ARG1' is atom
    int i = 0, j = 0;
    char *tmp;
    // gets string from atom and stores it on 'str'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    // Makes upper characters of 'str' (for comparison)
    UPPER_ENTRY(str);

    // Detecting value according to 'str'
    if (strcmp(str, "FALSE") == 0) {
      // disabling link-time optimization ('internalize' and 'runinliner' does not work if 'enabled' = 0)
      ExpEnv.transform_struc.link_time_opt.enabled = 0;
      ExpEnv.transform_struc.link_time_opt.internalize = 0;
      ExpEnv.transform_struc.link_time_opt.runinliner = 0;
    }
    else if (strcmp(str, "TRUE") == 0) {
      // enabling link-time optimization ('internalize' = 1 and 'runinliner' = 0 are default)
      ExpEnv.transform_struc.link_time_opt.enabled = 1;
      ExpEnv.transform_struc.link_time_opt.internalize = 1;
      ExpEnv.transform_struc.link_time_opt.runinliner = 0;
    }
    else {
      // value passed by argument is out of known range (only 'true' or 'false' are allowed)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Link-time optimization");
    return FALSE;
  }
}

static Int
p_link_time_opt3( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t) || IsAtomTerm(t)) {
    Term u = Deref(ARG2);
    // valid value for ARG2 are 'integer' and 'atom'
    if (IsIntTerm(u) || IsAtomTerm(u)) {
      Term v = Deref(ARG3);
      // valid value for ARG3 are 'integer' and 'atom'
      if (IsIntTerm(v) || IsAtomTerm(v)) {

        /* setting 1st argument (link-time optimization itself) */
        if (IsIntTerm(t)) {
          // ARG1 is integer
          ExpEnv.transform_struc.link_time_opt.enabled = IntOfTerm(t);
        }
        else /* atom */ {
          // ARG1 is atom
          int i = 0, j = 0;
	  char *tmp;
          // gets string from atom and stores it on 'str'
          char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
          strcpy(str, AtomName(AtomOfTerm(t)));
          // Makes upper characters of 'str' (for comparison)
          UPPER_ENTRY(str);

          // Detecting value according to 'str'
          if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.link_time_opt.enabled = 0; // disabling link-time opt
          else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.link_time_opt.enabled = 1; // enabling link-time opt
          else {
            // value passed by argument is out of known range (only 'true' or 'false' are allowed)
            Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
            return FALSE;
          }
        }

        /* setting 2nd argument (internalize) */
        if (IsIntTerm(u)) {
          // ARG2 is integer
          ExpEnv.transform_struc.link_time_opt.internalize = IntOfTerm(u);
        }
        else /* atom */ {
          // ARG2 is atom
          int i = 0, j = 0;
	  char *tmp;
          // gets string from atom and stores it on 'str'
          char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
          strcpy(str, AtomName(AtomOfTerm(u)));
          // Makes upper characters of 'str' (for comparison)
          UPPER_ENTRY(str);

          // Detecting value according to 'str'
          if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.link_time_opt.internalize = 0; // disabling internalize
          else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.link_time_opt.internalize = 1; // enabling internalize
          else {
            // value passed by argument is out of known range (only 'true' or 'false' are allowed)
            Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
            return FALSE;
          }
        }

        /* setting 3rd argument (runinliner) */
        if (IsIntTerm(v)) {
          // ARG3 is integer
          ExpEnv.transform_struc.link_time_opt.runinliner = IntOfTerm(v);
          return TRUE;
        }
        else /* atom */ {
          // ARG3 is atom
          int i = 0, j = 0;
	  char *tmp;
          // gets string from atom and stores it on 'str'
          char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(v))*sizeof(char));
          strcpy(str, AtomName(AtomOfTerm(v)));
          // Makes upper characters of 'str' (for comparison)
          UPPER_ENTRY(str);

          // Detecting value according to 'str'
          if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.link_time_opt.runinliner = 0; // disabling runinliner
          else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.link_time_opt.runinliner = 1; // enabling runinliner
          else {
            // value passed by argument is out of known range (only 'true' or 'false' are allowed)
            Yap_Error(OUT_OF_KNOWNRANGE_ERROR,v,"");
            return FALSE;
          }
          return TRUE;
        }
      }
      else {
        // ARG3 is not an integer or atom
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Link-time optimization (3rd arg)");
        return FALSE;
      }
    }
    else {
      // ARG2 is not an integer or atom
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Link-time optimization (2nd arg)");
      return FALSE;
    }
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Link-time optimization (1st arg)");
    return FALSE;
  }
}

static Int
p_enable_link_time_opt( USES_REGS1 )
{
  // Same as 'link_time_opt(true)'
  ExpEnv.transform_struc.link_time_opt.enabled = 1;
  ExpEnv.transform_struc.link_time_opt.internalize = 1;
  ExpEnv.transform_struc.link_time_opt.runinliner = 0;
  return TRUE;
}

static Int
p_enable_link_time_opt2( USES_REGS1 )
{
  Term t = Deref(ARG1);
  // valid value for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t) || IsAtomTerm(t)) {
    // valid value for ARG2 are 'integer' and 'atom'
    Term u = Deref(ARG2);
    if (IsIntTerm(u) || IsAtomTerm(u)) {

      /* setting 1st argument (internalize) */
      if (IsIntTerm(t)) {
        // ARG1 is integer
        ExpEnv.transform_struc.link_time_opt.internalize = IntOfTerm(t);
      }
      else /* atom */ {
        // ARG1 is atom
        int i = 0, j = 0;
	char *tmp;
        // gets string from atom and stores it on 'str'
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(t)));
        // Makes upper characters of 'str' (for comparison)
        UPPER_ENTRY(str);

        // Detecting value according to 'str'
        if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.link_time_opt.internalize = 0; // disabling internalize
        else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.link_time_opt.internalize = 1; // enabling internalize
        else {
          // value passed by argument is out of known range (only 'true' or 'false' are allowed)
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
          return FALSE;
        }
      }

      /* setting 2nd argument (runinliner) */
      if (IsIntTerm(u)) {
        // ARG2 is integer
        ExpEnv.transform_struc.link_time_opt.runinliner = IntOfTerm(u);
        ExpEnv.transform_struc.link_time_opt.enabled = 1;
        return TRUE;
      }
      else /* atom */ {
        // ARG2 is atom
        int i = 0, j = 0;
	char *tmp;
        // gets string from atom and stores it on 'str'
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
        // Makes upper characters of 'str' (for comparison)
        UPPER_ENTRY(str);

        // Detecting value according to 'str'
        if (strcmp(str, "FALSE") == 0) ExpEnv.transform_struc.link_time_opt.runinliner = 0; // disabling runinliner
        else if (strcmp(str, "TRUE") == 0) ExpEnv.transform_struc.link_time_opt.runinliner = 1; // enabling runinliner
        else {
          // value passed by argument is out of known range (only 'true' or 'false' are allowed)
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
        }
        ExpEnv.transform_struc.link_time_opt.enabled = 1; // enabling link-time opt
        return TRUE;
      }
    }
    else {
      // ARG2 is not an integer or atom
      Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Link-time optimization (2nd arg)");
      return FALSE;
    }
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Link-time optimization (1st arg)");
    return FALSE;
  }
}

static Int
p_disable_link_time_opt( USES_REGS1 )
{
  ExpEnv.transform_struc.link_time_opt.enabled = 0;
  ExpEnv.transform_struc.link_time_opt.internalize = 0;
  ExpEnv.transform_struc.link_time_opt.runinliner = 0;
  return TRUE;
}

#pragma GCC diagnostic pop

void
Yap_InitJitTransformPreds( void )
{
  Yap_InitCPred("disable_transform_pass", 1, p_disable_transform_pass, SafePredFlag);
  Yap_InitCPred("transform_pass", 1, p_transform_pass, SafePredFlag);
  Yap_InitCPred("enable_transform_pass", 1, p_enable_transform_pass, SafePredFlag);
  Yap_InitCPred("transform_passes", 1, p_transform_passes, SafePredFlag);
  Yap_InitCPred("enable_all_transform_passes", 0, p_enable_all_transform_passes, SafePredFlag);
  Yap_InitCPred("disable_all_transform_passes", 1, p_disable_all_transform_passes, SafePredFlag);
  Yap_InitCPred("n_transform_passes", 1, p_n_transform_passes, SafePredFlag);
  Yap_InitCPred("transform_level", 1, p_transform_level, SafePredFlag);
  Yap_InitCPred("argument_promotion_max_elements", 1, p_argument_promotion_max_elements, SafePredFlag);
  Yap_InitCPred("scalar_replace_aggregates_threshold", 1, p_scalar_replace_aggregates_threshold, SafePredFlag);
  Yap_InitCPred("loop_unroll_threshold", 1, p_loop_unroll_threshold, SafePredFlag);
  Yap_InitCPred("inline_threshold", 1, p_inline_threshold, SafePredFlag);
  Yap_InitCPred("strip_symbols_pass_type", 1, p_strip_symbols_pass_type, SafePredFlag);
  Yap_InitCPred("loop_unswitch_optimize_for_size", 1, p_loop_unswitch_optimize_for_size, SafePredFlag);
  Yap_InitCPred("default_optimization_args", 0, p_default_optimization_args, SafePredFlag);
  Yap_InitCPred("reset_optimization_args", 0, p_reset_optimization_args, SafePredFlag);
  Yap_InitCPred("enable_unit_at_time", 0, p_enable_unit_at_time, SafePredFlag);
  Yap_InitCPred("enable_simplify_libcalls", 0, p_enable_simplify_libcalls, SafePredFlag);
  Yap_InitCPred("disable_unit_at_time", 0, p_disable_unit_at_time, SafePredFlag);
  Yap_InitCPred("disable_simplify_libcalls", 0, p_disable_simplify_libcalls, SafePredFlag);
  Yap_InitCPred("link_time_opt", 1, p_link_time_opt1, SafePredFlag);
  Yap_InitCPred("link_time_opt", 3, p_link_time_opt3, SafePredFlag);
  Yap_InitCPred("enable_link_time_opt", 0, p_enable_link_time_opt, SafePredFlag);
  Yap_InitCPred("enable_link_time_opt", 2, p_enable_link_time_opt2, SafePredFlag);
  Yap_InitCPred("disable_link_time_opt", 0, p_disable_link_time_opt, SafePredFlag);
}
