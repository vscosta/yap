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
* File:		jit_analysispreds.c					 *
* comments:	JIT Compiler Analysis predicates			 *
*									 *
* Last rev:     2013-10-18                               		 *
*************************************************************************/

#include "jit_predicates.hpp"
#include <string.h>

#define N_ANALYSIS_PASSES 33

// Disable one (passed by argument) LLVM analysis pass
static Int  p_disable_analysis_pass( USES_REGS1 );

// Enable one (passed by argument) LLVM analysis pass
static Int  p_analysis_pass( USES_REGS1 );

// Enable one (passed by argument) LLVM analysis pass
static Int  p_enable_analysis_pass( USES_REGS1 );

// Enable a list (passed by argument) of LLVM analysis passes
static Int  p_analysis_passes( USES_REGS1 );

// Enable all available LLVM analysis passes
static Int  p_enable_all_analysis_passes( USES_REGS1 );

// Disable all available LLVM analysis passes
static Int  p_disable_all_analysis_passes( USES_REGS1 );

// Enable LLVM statistics
static Int  p_enable_stats( USES_REGS1 );

// Enable elapsed time of each LLVM's task
static Int  p_enable_time_passes( USES_REGS1 );

// Checks generated modules are correct (before optimize it). Use only if you suspect that any module has been generated incorrectly.
static Int  p_enable_module_correctness( USES_REGS1 );

// Same as 'p_enable_module_correctness', but accepts one argument, which defines when modules are checked.
// Valid values are those defined by 'enumPointToVerifiy' on 'amidefs.h'
static Int  p_enable_module_correctness1( USES_REGS1 );

// Same as 'p_enable_module_correctness' with ARG1 = NOPOINT
static Int  p_verify_module_nopoint( USES_REGS1 );

// Same as 'p_enable_module_correctness' with ARG1 = BEFORE
static Int  p_verify_module_before( USES_REGS1 );

// Same as 'p_enable_module_correctness' with ARG1 = AFTER
static Int  p_verify_module_after( USES_REGS1 );

// Same as 'p_enable_module_correctness' with ARG1 = BOTH
static Int  p_verify_module_both( USES_REGS1 );

// Disable LLVM statistics
static Int  p_disable_stats( USES_REGS1 );

// Disable elapsed time of each LLVM's task
static Int  p_disable_time_passes( USES_REGS1 );

// Don't check generated modules are correct
static Int  p_disable_module_correctness( USES_REGS1 );

// Set output file where analysis results are emitted. 'stderr' and 'stdout' are valid values
static Int  p_analysis_output_file( USES_REGS1 );

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

#define JIT_CODE 1

static Int
p_disable_analysis_pass( USES_REGS1 )
{
  // First: stores what analysis pass should be disabled

  Term t = Deref(ARG1);
  enumAnalysisPasses f;
  // valid values for ARG1 are 'integer' and 'atom'
  if (IsIntTerm(t)) {
    // ARG1 is integer
    Int v = IntOfTerm(t);
    if (v < 0 || v >= N_ANALYSIS_PASSES) {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    f = (enumAnalysisPasses)v;
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
      if (ExpEnv.analysis_struc.act_an) free(ExpEnv.analysis_struc.act_an);
      ExpEnv.analysis_struc.act_an = NULL;
      ExpEnv.analysis_struc.n = 0;
      return TRUE;
    }

    // Detects one pass according to 'str' -- store it
    if (strcmp(str, "AAEVAL") == 0) f = e_createAAEvalPass;
    else if (strcmp(str, "ALIASANALYSISCOUNTER") == 0 || strcmp(str, "COUNTAA") == 0) f = e_createAliasAnalysisCounterPass;
    else if (strcmp(str, "BASICALIASANALYSIS") == 0 || strcmp(str, "BASICAA") == 0) f = e_createBasicAliasAnalysisPass;
    else if (strcmp(str, "CFGONLYPRINTER") == 0 || strcmp(str, "DOTCFGONLY") == 0) f = e_createCFGOnlyPrinterPass;
    else if (strcmp(str, "CFGPRINTER") == 0 || strcmp(str, "DOTCFG") == 0) f = e_createCFGPrinterPass;
    else if (strcmp(str, "DBGINFOPRINTER") == 0 || strcmp(str, "PRINTDBGINFO") == 0) f = e_createDbgInfoPrinterPass;
    else if (strcmp(str, "DOMONLYPRINTER") == 0 || strcmp(str, "DOTDOMONLY") == 0) f = e_createDomOnlyPrinterPass;
    else if (strcmp(str, "DOMPRINTER") == 0 || strcmp(str, "DOTDOM") == 0) f = e_createDomPrinterPass;
    else if (strcmp(str, "GLOBALSMODREF") == 0 || strcmp(str, "GLOBALSMODREFAA") == 0) f = e_createGlobalsModRefPass;
    else if (strcmp(str, "INSTCOUNT") == 0) f = e_createInstCountPass;
    else if (strcmp(str, "IVUSERS") == 0) f = e_createIVUsersPass;
    else if (strcmp(str, "LAZYVALUEINFO") == 0) f = e_createLazyValueInfoPass;
    else if (strcmp(str, "LIBCALLALIASANALYSIS") == 0 || strcmp(str, "LIBCALLAA") == 0) f = e_createLibCallAliasAnalysisPass;
    else if (strcmp(str, "LINT") == 0) f = e_createLintPass;
    else if (strcmp(str, "LOOPDEPENDENCEANALYSIS") == 0 || strcmp(str, "LDA") == 0) f = e_createLoopDependenceAnalysisPass;
    else if (strcmp(str, "MEMDEPPRINTER") == 0 || strcmp(str, "MEMDEP") == 0) f = e_createMemDepPrinter;
    else if (strcmp(str, "MODULEDEBUGINFOPRINTER") == 0 || strcmp(str, "MODULEDEBUGINFO") == 0) f = e_createModuleDebugInfoPrinterPass;
    else if (strcmp(str, "NOAA") == 0) f = e_createNoAAPass;
    else if (strcmp(str, "NOPATHPROFILEINFO") == 0 || strcmp(str, "NOPATHPROFILE") == 0) f = e_createNoPathProfileInfoPass;
    else if (strcmp(str, "NOPROFILEINFO") == 0 || strcmp(str, "NOPROFILE") == 0) f = e_createNoProfileInfoPass;
    else if (strcmp(str, "OBJCARCALIASANALYSIS") == 0 || strcmp(str, "OBJCARCAA") == 0) f = e_createObjCARCAliasAnalysisPass;
    else if (strcmp(str, "PATHPROFILELOADER") == 0) f = e_createPathProfileLoaderPass;
    else if (strcmp(str, "PATHPROFILEVERIFIER") == 0) f = e_createPathProfileVerifierPass;
    else if (strcmp(str, "POSTDOMONLYPRINTER") == 0 || strcmp(str, "DOTPOSTDOMONLY") == 0) f = e_createPostDomOnlyPrinterPass;
    else if (strcmp(str, "POSTDOMPRINTER") == 0 || strcmp(str, "DOTPOSTDOM") == 0) f = e_createPostDomPrinterPass;
    else if (strcmp(str, "PROFILEESTIMATOR") == 0) f = e_createProfileEstimatorPass;
    else if (strcmp(str, "PROFILELOADER") == 0) f = e_createProfileLoaderPass;
    else if (strcmp(str, "PROFILEVERIFIER") == 0) f = e_createProfileVerifierPass;
    else if (strcmp(str, "REGIONINFO") == 0) f = e_createRegionInfoPass;
    else if (strcmp(str, "REGIONONLYPRINTER") == 0 || strcmp(str, "DOTREGIONSONLY") == 0) f = e_createRegionOnlyPrinterPass;
    else if (strcmp(str, "REGIONPRINTER") == 0 || strcmp(str, "DOTREGIONS") == 0) f = e_createRegionPrinterPass;
    else if (strcmp(str, "SCALAREVOLUTIONALIASANALYSIS") == 0 || strcmp(str, "SCEVAA") == 0) f = e_createScalarEvolutionAliasAnalysisPass;
    else if (strcmp(str, "TYPEBASEDALIASANALYSIS") == 0 || strcmp(str, "TBAA") == 0) f = e_createTypeBasedAliasAnalysisPass;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Analysis pass");
    return FALSE;
  }

  // Second: creates a new list with all analysis on 'ExpEnv.analysis_struc.act_an' but that analysis stored on first step
  enumAnalysisPasses *tmplist = NULL;
  COUNT tmpn = 0;
  int i = 0;
  while (i < ExpEnv.analysis_struc.n) {
    if (ExpEnv.analysis_struc.act_an[i] != f) {
      tmpn += 1;
      tmplist = (enumAnalysisPasses*)realloc(tmplist, tmpn*sizeof(enumAnalysisPasses));
      tmplist[tmpn-1] = ExpEnv.analysis_struc.act_an[i];
    }
    i += 1;
  }

  // Third: makes 'ExpEnv.analysis_struc.act_an' to point to new list created on second step
  free(ExpEnv.analysis_struc.act_an);
  ExpEnv.analysis_struc.n = tmpn;
  ExpEnv.analysis_struc.act_an = tmplist;
  return TRUE;
}

static Int
p_analysis_pass( USES_REGS1 )
{
  // First: disables analysis pass (if be active)
  p_disable_analysis_pass( PASS_REGS1 );

  // Second: valids argument and inserts new analysis pass
  // valid values for ARG1 are 'integer' and 'atom'
  Term t = Deref(ARG1);
  Int v;
  if (IsIntTerm(t)) {
    // ARG1 is integer
    v = IntOfTerm(t);
    if (v < 0 || v >= N_ANALYSIS_PASSES) {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // creates a new slot in 'ExpEnv.analysis_struc.act_an' and appends the pass in it
    ExpEnv.analysis_struc.n += 1;
    ExpEnv.analysis_struc.act_an = (enumAnalysisPasses*)
                                   realloc(ExpEnv.analysis_struc.act_an, ExpEnv.analysis_struc.n * sizeof(enumAnalysisPasses));
    ExpEnv.analysis_struc.act_an[ExpEnv.analysis_struc.n-1] = (enumAnalysisPasses)v;
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
    if (strcmp(str, "AAEVAL") == 0) v = e_createAAEvalPass;
    else if (strcmp(str, "ALIASANALYSISCOUNTER") == 0 || strcmp(str, "COUNTAA") == 0) v = e_createAliasAnalysisCounterPass;
    else if (strcmp(str, "BASICALIASANALYSIS") == 0 || strcmp(str, "BASICAA") == 0) v = e_createBasicAliasAnalysisPass;
    else if (strcmp(str, "CFGONLYPRINTER") == 0 || strcmp(str, "DOTCFGONLY") == 0) v = e_createCFGOnlyPrinterPass;
    else if (strcmp(str, "CFGPRINTER") == 0 || strcmp(str, "DOTCFG") == 0) v = e_createCFGPrinterPass;
    else if (strcmp(str, "DBGINFOPRINTER") == 0 || strcmp(str, "PRINTDBGINFO") == 0) v = e_createDbgInfoPrinterPass;
    else if (strcmp(str, "DOMONLYPRINTER") == 0 || strcmp(str, "DOTDOMONLY") == 0) v = e_createDomOnlyPrinterPass;
    else if (strcmp(str, "DOMPRINTER") == 0 || strcmp(str, "DOTDOM") == 0) v = e_createDomPrinterPass;
    else if (strcmp(str, "GLOBALSMODREF") == 0 || strcmp(str, "GLOBALSMODREFAA") == 0) v = e_createGlobalsModRefPass;
    else if (strcmp(str, "INSTCOUNT") == 0) v = e_createInstCountPass;
    else if (strcmp(str, "IVUSERS") == 0) v = e_createIVUsersPass;
    else if (strcmp(str, "LAZYVALUEINFO") == 0) v = e_createLazyValueInfoPass;
    else if (strcmp(str, "LIBCALLALIASANALYSIS") == 0 || strcmp(str, "LIBCALLAA") == 0) v = e_createLibCallAliasAnalysisPass;
    else if (strcmp(str, "LINT") == 0) v = e_createLintPass;
    else if (strcmp(str, "LOOPDEPENDENCEANALYSIS") == 0 || strcmp(str, "LDA") == 0) v = e_createLoopDependenceAnalysisPass;
    else if (strcmp(str, "MEMDEPPRINTER") == 0 || strcmp(str, "MEMDEP") == 0) v = e_createMemDepPrinter;
    else if (strcmp(str, "MODULEDEBUGINFOPRINTER") == 0 || strcmp(str, "MODULEDEBUGINFO") == 0) v = e_createModuleDebugInfoPrinterPass;
    else if (strcmp(str, "NOAA") == 0) v = e_createNoAAPass;
    else if (strcmp(str, "NOPATHPROFILEINFO") == 0 || strcmp(str, "NOPATHPROFILE") == 0) v = e_createNoPathProfileInfoPass;
    else if (strcmp(str, "NOPROFILEINFO") == 0 || strcmp(str, "NOPROFILE") == 0) v = e_createNoProfileInfoPass;
    else if (strcmp(str, "OBJCARCALIASANALYSIS") == 0 || strcmp(str, "OBJCARCAA") == 0) v = e_createObjCARCAliasAnalysisPass;
    else if (strcmp(str, "PATHPROFILELOADER") == 0) v = e_createPathProfileLoaderPass;
    else if (strcmp(str, "PATHPROFILEVERIFIER") == 0) v = e_createPathProfileVerifierPass;
    else if (strcmp(str, "POSTDOMONLYPRINTER") == 0 || strcmp(str, "DOTPOSTDOMONLY") == 0) v = e_createPostDomOnlyPrinterPass;
    else if (strcmp(str, "POSTDOMPRINTER") == 0 || strcmp(str, "DOTPOSTDOM") == 0) v = e_createPostDomPrinterPass;
    else if (strcmp(str, "PROFILEESTIMATOR") == 0) v = e_createProfileEstimatorPass;
    else if (strcmp(str, "PROFILELOADER") == 0) v = e_createProfileLoaderPass;
    else if (strcmp(str, "PROFILEVERIFIER") == 0) v = e_createProfileVerifierPass;
    else if (strcmp(str, "REGIONINFO") == 0) v = e_createRegionInfoPass;
    else if (strcmp(str, "REGIONONLYPRINTER") == 0 || strcmp(str, "DOTREGIONSONLY") == 0) v = e_createRegionOnlyPrinterPass;
    else if (strcmp(str, "REGIONPRINTER") == 0 || strcmp(str, "DOTREGIONS") == 0) v = e_createRegionPrinterPass;
    else if (strcmp(str, "SCALAREVOLUTIONALIASANALYSIS") == 0 || strcmp(str, "SCEVAA") == 0) v = e_createScalarEvolutionAliasAnalysisPass;
    else if (strcmp(str, "TYPEBASEDALIASANALYSIS") == 0 || strcmp(str, "TBAA") == 0) v = e_createTypeBasedAliasAnalysisPass;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    // creates a new slot in 'ExpEnv.analysis_struc.act_an' and appends the pass in it
    ExpEnv.analysis_struc.n += 1;
    ExpEnv.analysis_struc.act_an = (enumAnalysisPasses*)
                                   realloc(ExpEnv.analysis_struc.act_an, ExpEnv.analysis_struc.n * sizeof(enumAnalysisPasses));
    ExpEnv.analysis_struc.act_an[ExpEnv.analysis_struc.n-1] = v;
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Analysis pass");
    return FALSE;
  }
}

static Int
p_enable_analysis_pass( USES_REGS1 )
{
  return p_analysis_pass( PASS_REGS1 );
}

static Int
p_analysis_passes( USES_REGS1 )
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
      // First: disables all analysis passes
      free(ExpEnv.analysis_struc.act_an);
      ExpEnv.analysis_struc.act_an = NULL;
      ExpEnv.analysis_struc.n = 0;

      // Second, insert all analysis passes on 'ExpEnv.analysis_struc.act_an'
      int i;
      for (i = 0; i < N_ANALYSIS_PASSES; i++) {
        ExpEnv.analysis_struc.n += 1;
    	ExpEnv.analysis_struc.act_an = (enumAnalysisPasses*)realloc(ExpEnv.analysis_struc.act_an, ExpEnv.analysis_struc.n * sizeof(enumAnalysisPasses));
	ExpEnv.analysis_struc.act_an[ExpEnv.analysis_struc.n-1] = i;
      }
      return TRUE;
    }
    // value passed by argument is out of known range (ARG1 differs of 'all')
    Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
    return FALSE;
  }
  else if (IsPairTerm(t)) {
    // ARG1 is list
    // First: disables all analysis passes
    if (ExpEnv.analysis_struc.act_an) free(ExpEnv.analysis_struc.act_an);
    ExpEnv.analysis_struc.act_an = NULL;
    ExpEnv.analysis_struc.n = 0;

    // Second: scrolls over the list treating each element individually
    Term u = HeadOfTermCell(t); // get head of list 't'
    u = Deref(u);
    while (1) {
      Int v;
      enumAnalysisPasses w;

      // valid values for head are 'integer' and 'atom' (the list can contain both)
      if (IsIntTerm(u)) {
        // head is integer
        v = IntOfTerm(u);
        if (v < 0 || v >= N_ANALYSIS_PASSES) {
          // head's value is out of known range
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
        }

        // insert analysis pass defined by 'head' on 'ExpEnv.analysis_struc.act_an'
	ExpEnv.analysis_struc.n += 1;
        ExpEnv.analysis_struc.act_an = (enumAnalysisPasses*)realloc(ExpEnv.analysis_struc.act_an, ExpEnv.analysis_struc.n * sizeof(enumAnalysisPasses));
        ExpEnv.analysis_struc.act_an[ExpEnv.analysis_struc.n-1] = (enumAnalysisPasses)v;
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
        if (strcmp(str, "AAEVAL") == 0) w = e_createAAEvalPass;
        else if (strcmp(str, "ALIASANALYSISCOUNTER") == 0 || strcmp(str, "COUNTAA") == 0) w = e_createAliasAnalysisCounterPass;
        else if (strcmp(str, "BASICALIASANALYSIS") == 0 || strcmp(str, "BASICAA") == 0) w = e_createBasicAliasAnalysisPass;
        else if (strcmp(str, "CFGONLYPRINTER") == 0 || strcmp(str, "DOTCFGONLY") == 0) w = e_createCFGOnlyPrinterPass;
        else if (strcmp(str, "CFGPRINTER") == 0 || strcmp(str, "DOTCFG") == 0) w = e_createCFGPrinterPass;
        else if (strcmp(str, "DBGINFOPRINTER") == 0 || strcmp(str, "PRINTDBGINFO") == 0) w = e_createDbgInfoPrinterPass;
        else if (strcmp(str, "DOMONLYPRINTER") == 0 || strcmp(str, "DOTDOMONLY") == 0) w = e_createDomOnlyPrinterPass;
        else if (strcmp(str, "DOMPRINTER") == 0 || strcmp(str, "DOTDOM") == 0) w = e_createDomPrinterPass;
        else if (strcmp(str, "GLOBALSMODREF") == 0 || strcmp(str, "GLOBALSMODREFAA") == 0) w = e_createGlobalsModRefPass;
        else if (strcmp(str, "INSTCOUNT") == 0) w = e_createInstCountPass;
        else if (strcmp(str, "IVUSERS") == 0) w = e_createIVUsersPass;
        else if (strcmp(str, "LAZYVALUEINFO") == 0) w = e_createLazyValueInfoPass;
        else if (strcmp(str, "LIBCALLALIASANALYSIS") == 0 || strcmp(str, "LIBCALLAA") == 0) w = e_createLibCallAliasAnalysisPass;
        else if (strcmp(str, "LINT") == 0) w = e_createLintPass;
        else if (strcmp(str, "LOOPDEPENDENCEANALYSIS") == 0 || strcmp(str, "LDA") == 0) w = e_createLoopDependenceAnalysisPass;
        else if (strcmp(str, "MEMDEPPRINTER") == 0 || strcmp(str, "MEMDEP") == 0) w = e_createMemDepPrinter;
        else if (strcmp(str, "MODULEDEBUGINFOPRINTER") == 0 || strcmp(str, "MODULEDEBUGINFO") == 0) w = e_createModuleDebugInfoPrinterPass;
        else if (strcmp(str, "NOAA") == 0) w = e_createNoAAPass;
        else if (strcmp(str, "NOPATHPROFILEINFO") == 0 || strcmp(str, "NOPATHPROFILE") == 0) w = e_createNoPathProfileInfoPass;
        else if (strcmp(str, "NOPROFILEINFO") == 0 || strcmp(str, "NOPROFILE") == 0) w = e_createNoProfileInfoPass;
        else if (strcmp(str, "OBJCARCALIASANALYSIS") == 0 || strcmp(str, "OBJCARCAA") == 0) w = e_createObjCARCAliasAnalysisPass;
        else if (strcmp(str, "PATHPROFILELOADER") == 0) w = e_createPathProfileLoaderPass;
        else if (strcmp(str, "PATHPROFILEVERIFIER") == 0) w = e_createPathProfileVerifierPass;
        else if (strcmp(str, "POSTDOMONLYPRINTER") == 0 || strcmp(str, "DOTPOSTDOMONLY") == 0) w = e_createPostDomOnlyPrinterPass;
        else if (strcmp(str, "POSTDOMPRINTER") == 0 || strcmp(str, "DOTPOSTDOM") == 0) w = e_createPostDomPrinterPass;
        else if (strcmp(str, "PROFILEESTIMATOR") == 0) w = e_createProfileEstimatorPass;
        else if (strcmp(str, "PROFILELOADER") == 0) w = e_createProfileLoaderPass;
        else if (strcmp(str, "PROFILEVERIFIER") == 0) w = e_createProfileVerifierPass;
        else if (strcmp(str, "REGIONINFO") == 0) w = e_createRegionInfoPass;
        else if (strcmp(str, "REGIONONLYPRINTER") == 0 || strcmp(str, "DOTREGIONSONLY") == 0) w = e_createRegionOnlyPrinterPass;
        else if (strcmp(str, "REGIONPRINTER") == 0 || strcmp(str, "DOTREGIONS") == 0) w = e_createRegionPrinterPass;
        else if (strcmp(str, "SCALAREVOLUTIONALIASANALYSIS") == 0 || strcmp(str, "SCEVAA") == 0) w = e_createScalarEvolutionAliasAnalysisPass;
        else if (strcmp(str, "TYPEBASEDALIASANALYSIS") == 0 || strcmp(str, "TBAA") == 0) w = e_createTypeBasedAliasAnalysisPass;
        else {
          // head's value is out of known range
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
        }

        // insert analysis pass defined by 'head' on 'ExpEnv.analysis_struc.act_an'
	ExpEnv.analysis_struc.n += 1;
	ExpEnv.analysis_struc.act_an = (enumAnalysisPasses*)realloc(ExpEnv.analysis_struc.act_an, ExpEnv.analysis_struc.n * sizeof(enumAnalysisPasses));
	ExpEnv.analysis_struc.act_an[ExpEnv.analysis_struc.n-1] = w;
      }
      else {
        // head's value is not an integer or atom
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Analysis pass");
	return FALSE;
      }

      // here, 'u' is the current head of list (just been treated) and 't' is our list itself
      t = TailOfTermCell(t); // 't' is now our list without 'u' (tail of 't')
      t = Deref(t);
      if (IsAtomTerm(t)) break; // if 't' is an empty list (which is treated as atom by Prolog), we finish the loop
      u = HeadOfTermCell(t); // else, 'u' is now next head which will be treated
      u = Deref(u);
    }
    return TRUE;
  }
  else {
    // ARG1 is not an integer or atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Analysis pass");
    return FALSE;
  }
}

static Int
p_enable_all_analysis_passes( USES_REGS1 )
{
  // Same as 'analysis_passes(all)'
  // First, disable all analysis passes
  if (ExpEnv.analysis_struc.act_an) free(ExpEnv.analysis_struc.act_an);
  ExpEnv.analysis_struc.act_an = NULL;
  ExpEnv.analysis_struc.n = 0;
  // Second, insert all analysis passes
  int i;
  for (i = 0; i < N_ANALYSIS_PASSES; i++) {
    ExpEnv.analysis_struc.n += 1;
    ExpEnv.analysis_struc.act_an = (enumAnalysisPasses*)
                                   realloc(ExpEnv.analysis_struc.act_an, ExpEnv.analysis_struc.n * sizeof(enumAnalysisPasses));
    ExpEnv.analysis_struc.act_an[ExpEnv.analysis_struc.n-1] = (enumAnalysisPasses)i;
  }
  return TRUE;
}

static Int
p_disable_all_analysis_passes( USES_REGS1 )
{
  // Just empty 'ExpEnv.analysis_struc.act_an'
  if (ExpEnv.analysis_struc.act_an) free(ExpEnv.analysis_struc.act_an);
  ExpEnv.analysis_struc.act_an = NULL;
  ExpEnv.analysis_struc.n = 0;
  return TRUE;
}

static Int
p_enable_stats( USES_REGS1 )
{
  ExpEnv.analysis_struc.stats_enabled = 1;
  return TRUE;
}

static Int
p_enable_time_passes( USES_REGS1 )
{
  ExpEnv.analysis_struc.time_pass_enabled = 1;
  return TRUE;
}

static Int
p_enable_module_correctness( USES_REGS1 )
{
  ExpEnv.analysis_struc.pointtoverifymodule = AFTER;
  return TRUE;
}

static Int
p_enable_module_correctness1( USES_REGS1 )
{
  Term t = Deref(ARG1);
  char *tmp;
  // valid value for ARG1 is just 'atom'
  if (IsAtomTerm(t)) {
    // 'ARG1' is atom
    int i = 0, j = 0;
    // gets string from atom and stores it on 'str'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    // Makes upper characters of 'str' (for comparison)
    UPPER_ENTRY(str);

    // Detects one pass according to 'str'
    if (strcmp(str, "NOPOINT") == 0) ExpEnv.analysis_struc.pointtoverifymodule = NOPOINT;
    else if (strcmp(str, "BEFORE") == 0) ExpEnv.analysis_struc.pointtoverifymodule = BEFORE;
    else if (strcmp(str, "AFTER") == 0) ExpEnv.analysis_struc.pointtoverifymodule = AFTER;
    else if (strcmp(str, "BOTH") == 0) ExpEnv.analysis_struc.pointtoverifymodule = BOTH;
    else {
      // value passed by argument is out of known range
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }
    return TRUE;
  }
  else {
    // ARG1 is not an atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Analysis pass");
    return FALSE;
  }
}

static Int
p_verify_module_nopoint( USES_REGS1 )
{
  // Same as 'enable_module_correctness(nopoint)'
  ExpEnv.analysis_struc.pointtoverifymodule = NOPOINT;
  return TRUE;
}

static Int
p_verify_module_before( USES_REGS1 )
{
  // Same as 'enable_module_correctness(before)'
  ExpEnv.analysis_struc.pointtoverifymodule = BEFORE;
  return TRUE;
}

static Int
p_verify_module_after( USES_REGS1 )
{
  // Same as 'enable_module_correctness(after)'
  ExpEnv.analysis_struc.pointtoverifymodule = AFTER;
  return TRUE;
}

static Int
p_verify_module_both( USES_REGS1 )
{
  // Same as 'enable_module_correctness(both)'
  ExpEnv.analysis_struc.pointtoverifymodule = BOTH;
  return TRUE;
}

static Int
p_disable_stats( USES_REGS1 )
{
  ExpEnv.analysis_struc.stats_enabled = 0;
  return TRUE;
}

static Int
p_disable_time_passes( USES_REGS1 )
{
  ExpEnv.analysis_struc.time_pass_enabled = 0;
  return TRUE;
}

static Int
p_disable_module_correctness( USES_REGS1 )
{
  ExpEnv.analysis_struc.pointtoverifymodule = NOPOINT;
  return TRUE;
}

static Int
p_analysis_output_file( USES_REGS1 )
{
  Term t = Deref(ARG1);
  char *tmp;
  // valid value for ARG1 is just 'atom'
  if (IsAtomTerm(t)) {
    // 'ARG1' is atom
    int i = 0, j = 0;

    // allocates memory to 'ExpEnv.analysis_struc.outfile'
    ExpEnv.analysis_struc.outfile = (CELL)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));

    // gets string from atom and stores it on 'str' and 'tmpstr'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    char *tmpstr = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    strcpy(tmpstr, str);

    // here, both 'str' and 'tmpstr' contain the file name... I need to verify if this name is 'stdout' or 'stderr', so...
    // makes upper characters of 'tmpstr' (for comparison) and...
    UPPER_ENTRY(tmpstr);
    // verify if tmpstr is 'stdout' or 'stderr'. Note here that 'tmpstr' is the same of 'str', but capitalized
    if (strcmp(tmpstr, "STDOUT") == 0 || strcmp(tmpstr, "STDERR") == 0)
      strcpy(((char*)ExpEnv.analysis_struc.outfile), tmpstr);
    else
      // if not 'stdout' or 'stderr', 'ExpEnv.analysis_struc.outfile' will be the real string of ARG1, ie., 'str'
      strcpy(((char*)ExpEnv.analysis_struc.outfile), str);
    return TRUE;
  }
  else {
    // ARG1 is not an atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"Analysis pass");
    return FALSE;
  }
}

#pragma GCC diagnostic pop

void 
Yap_InitJitAnalysisPreds(void)
{
  Yap_InitCPred("disable_analysis_pass", 1, p_disable_analysis_pass, SafePredFlag);
  Yap_InitCPred("analysis_pass", 1, p_analysis_pass, SafePredFlag);
  Yap_InitCPred("enable_analysis_pass", 1, p_enable_analysis_pass, SafePredFlag);
  Yap_InitCPred("analysis_passes", 1, p_analysis_passes, SafePredFlag);
  Yap_InitCPred("enable_all_analysis_passes", 0, p_enable_all_analysis_passes, SafePredFlag);
  Yap_InitCPred("disable_all_analysis_passes", 0, p_disable_all_analysis_passes, SafePredFlag);
  Yap_InitCPred("enable_stats", 0, p_enable_stats, SafePredFlag);
  Yap_InitCPred("enable_time_passes", 0, p_enable_time_passes, SafePredFlag);
  Yap_InitCPred("enable_module_correctness", 0, p_enable_module_correctness, SafePredFlag);
  Yap_InitCPred("enable_module_correctness", 1, p_enable_module_correctness1, SafePredFlag);
  Yap_InitCPred("verify_module_nopoint", 0, p_verify_module_nopoint, SafePredFlag);
  Yap_InitCPred("verify_module_before", 0, p_verify_module_before, SafePredFlag);
  Yap_InitCPred("verify_module_after", 0, p_verify_module_after, SafePredFlag);
  Yap_InitCPred("verify_module_both", 0, p_verify_module_both, SafePredFlag);
  Yap_InitCPred("disable_stats", 0, p_disable_stats, SafePredFlag);
  Yap_InitCPred("disable_time_passes", 0, p_disable_time_passes, SafePredFlag);
  Yap_InitCPred("disable_module_correctness", 0, p_disable_module_correctness, SafePredFlag);
  Yap_InitCPred("analysis_output_file", 1, p_analysis_output_file, SafePredFlag);
}
