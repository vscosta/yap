#include "JIT_Compiler.hpp"
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
//using namespace std;
//using namespace llvm;

#include <llvm/Pass.h>
#include <llvm/PassAnalysisSupport.h>
#include <llvm/PassInfo.h>
#include <llvm/PassManager.h>
#include <llvm/PassRegistry.h>
#include <llvm/PassSupport.h>
#include <llvm/Analysis/CallGraphSCCPass.h>
#include <llvm/CodeGen/Passes.h>

#include "PassPrinters.hh"

#define FREE_ALLOCATED() \
  free(buffer); \
  free(p->y_u.J.jh->cmd); \
  free(outputfilename); \
  free(optoutputfilename); \
  free(cmd1); \
  free(cmd2);

#define ADD_PASS_ACCORDING_TO_KIND()		\
	switch (Kind) { \
        case PT_BasicBlock: \
	  fprintf(stderr, "Oops -- basicblock printer\n"); \
	  exit(1); \
        case PT_Region: \
	  Pass.add(new RegionPassPrinter(PInfo));	\
          break; \
        case PT_Loop: \
          Pass.add(new LoopPassPrinter(PInfo)); \
          break; \
        case PT_Function: \
          Pass.add(new FunctionPassPrinter(PInfo)); \
          break; \
        case PT_CallGraphSCC: \
          Pass.add(new CallGraphSCCPassPrinter(PInfo)); \
          break; \
        default: \
	  Pass.add(new ModulePassPrinter(PInfo));	\
          break; \
        }

#define TREAT_CASE_FOR(PASS) \
	    PInfo = PassRegistry::getPassRegistry()->getPassInfo(PASS->getPassID()); \
	    Kind = PASS->getPassKind(); \
	    ADD_PASS_ACCORDING_TO_KIND();

void JIT_Compiler::analyze_module(llvm::Module* &M)
{
  PassManager Pass; // 'Pass' stores analysis passes to be applied
  TargetLibraryInfo *TLI = new TargetLibraryInfo(Triple(M->getTargetTriple()));

  const PassInfo *PInfo;
  PassKind Kind;

  Pass.add(TLI); // First, I add on 'Pass' the Target Info of Module
  Pass.add(new DataLayoutPass()); // Second, I must add Target Data on 'Pass'
  for (int i = 0; i < ExpEnv.analysis_struc.n; i++) {
    /*
     * 'ExpEnv.analysis_struc.act_an' contains sorted analysis passes *
     * 'ExpEnv.analysis_struc.act_an' is filled by analysis predicates *
     * What must I do? *
     *    1. Pass over 'ExpEnv.analysis_struc.act_an' (by previous 'for') *
     *    2. For each unity within 'ExpEnv.analysis_struc.act_an' *
     *        2.1. Check its type *
     *        2.2. Add analysis pass on 'Pass' accordingly the type checked *
    */
    switch (ExpEnv.analysis_struc.act_an[i]) {
    case e_createAAEvalPass:
      TREAT_CASE_FOR(createAAEvalPass());
      break;
    case e_createBasicAliasAnalysisPass:
      TREAT_CASE_FOR(createBasicAliasAnalysisPass());
      break;
    case e_createAliasAnalysisCounterPass:
      TREAT_CASE_FOR(createAliasAnalysisCounterPass());
      break;
    case e_createGlobalsModRefPass:
      TREAT_CASE_FOR(createGlobalsModRefPass());
      break;
    case e_createInstCountPass:
      TREAT_CASE_FOR(createInstCountPass());
      break;
    case e_createIVUsersPass:
      TREAT_CASE_FOR(createIVUsersPass());
      break;
    case e_createLazyValueInfoPass:
      TREAT_CASE_FOR(createLazyValueInfoPass());
      break;
    //CHANGED FOR LLVM 3.5
    case e_createLoopDependenceAnalysisPass:
      TREAT_CASE_FOR(createDependenceAnalysisPass());
      break;
    case e_createLibCallAliasAnalysisPass:
      TREAT_CASE_FOR(createLibCallAliasAnalysisPass(NULL));
      break;
    case e_createLintPass:
      TREAT_CASE_FOR(createLintPass());
      break;
    case e_createMemDepPrinter:
      TREAT_CASE_FOR(createMemDepPrinter());
      break;
    case e_createModuleDebugInfoPrinterPass:
      TREAT_CASE_FOR(createModuleDebugInfoPrinterPass());
      break;
    case e_createNoAAPass:
      TREAT_CASE_FOR(createNoAAPass());
      break;
    //NOT IN LLVM 3.5
    //case e_createNoPathProfileInfoPass:
    //  TREAT_CASE_FOR(createNoPathProfileInfoPass());
    //  break;
    //NOT IN LLVM 3.5
    //case e_createNoProfileInfoPass:
    //  TREAT_CASE_FOR(createNoProfileInfoPass());
    //  break;
    case e_createObjCARCAliasAnalysisPass:
      TREAT_CASE_FOR(createObjCARCAliasAnalysisPass());
      break;
    //NOT IN LLVM 3.5
    //case e_createProfileEstimatorPass:
    //  TREAT_CASE_FOR(createProfileEstimatorPass());
    //  break;
    //CHANGED FOR LLVM 3.5
    case e_createProfileLoaderPass:
      TREAT_CASE_FOR(createSampleProfileLoaderPass());
      break;
    //NOT IN LLVM 3.5
    //case e_createProfileVerifierPass:
    //  TREAT_CASE_FOR(createProfileVerifierPass());
    //  break;
    case e_createRegionInfoPass:
      TREAT_CASE_FOR(createRegionInfoPass());
      break;
    case e_createScalarEvolutionAliasAnalysisPass:
      TREAT_CASE_FOR(createScalarEvolutionAliasAnalysisPass());
      break;
    case e_createTypeBasedAliasAnalysisPass:
      TREAT_CASE_FOR(createTypeBasedAliasAnalysisPass());
      break;
    //CHANGED FOR LLVM 3.5
    case e_createDbgInfoPrinterPass:
      TREAT_CASE_FOR(createDebugInfoVerifierPass());
      break;
    case e_createCFGPrinterPass:
      TREAT_CASE_FOR(createCFGPrinterPass());
      break;
    case e_createCFGOnlyPrinterPass:
      TREAT_CASE_FOR(createCFGOnlyPrinterPass());
      break;
    case e_createDomPrinterPass:
      TREAT_CASE_FOR(createDomPrinterPass());
      break;
    case e_createDomOnlyPrinterPass:
      TREAT_CASE_FOR(createDomOnlyPrinterPass());
      break;
    case e_createPostDomPrinterPass:
      TREAT_CASE_FOR(createPostDomPrinterPass());
      break;
    case e_createPostDomOnlyPrinterPass:
      TREAT_CASE_FOR(createPostDomOnlyPrinterPass());
      break;
    case e_createRegionPrinterPass:
      TREAT_CASE_FOR(createRegionPrinterPass());
      break;
    case e_createRegionOnlyPrinterPass:
      TREAT_CASE_FOR(createRegionOnlyPrinterPass());
      break;
    //NOT IN LLVM 3.5
    //case e_createPathProfileLoaderPass:
    //  TREAT_CASE_FOR(createPathProfileLoaderPass());
    //  break;
    //NOT IN LLVM 3.5
    //case e_createPathProfileVerifierPass:
    //  TREAT_CASE_FOR(createPathProfileVerifierPass());
    //  break;
    default:;
    }
  }
   
  /* if 'llvm::TimePassesIsEnabled' is 'true', llvm time passes are printed on 'shutdown_llvm()' (p_halt -- stdpreds.c) */
  llvm::TimePassesIsEnabled = ExpEnv.analysis_struc.time_pass_enabled;
  /* Use 'llvm::EnableStatistics()' so that llvm stats are printed on 'shutdown_llvm()' (p_halt -- stdpreds.c) */
  if (ExpEnv.analysis_struc.stats_enabled) llvm::EnableStatistics();
  
  /*
   * Here, I configure resulting analysis output -- default: stderr *
   * Use analysis_output_file/1 to change *
  */
  if (strcmp(((char*)ExpEnv.analysis_struc.outfile), "STDERR")) { // print to file that is not stderr
    int stderrcopy = dup(2); // stderr backup
    if (strcmp(((char*)ExpEnv.analysis_struc.outfile), "STDOUT") == 0) { // print to stdout
      dup2(1, 2); // 2 is stderr; 1 is stdout -- dup2(1,2) redirects stderr output to stdout
      Pass.run(*M); // Run passes (results will be printed on stdout)
      dup2(stderrcopy, 2); // Recovers stderr
    }
    else {
      int Outputfile = open(((char*)ExpEnv.analysis_struc.outfile), O_CREAT | O_TRUNC | O_WRONLY, 0777);
      // Openning Output file, whose name is on 'ExpEnv.analysis_struc.outfile'
      if (Outputfile < 0) {
        fprintf(stderr, "Error:: I can not write analysis passes's output on %s...\n", ((char*)ExpEnv.analysis_struc.outfile));
        fprintf(stderr, "        %s...\n", strerror(errno));
        errno = 0;
        exit(1);
      }
      dup2(Outputfile, 2); // 2 is stderr; Outputfile is any other file -- dup2(Outputfile,2) redirects stderr output to Outputfile
      Pass.run(*M); // Run passes (results will be printed on Outputfile)
      close(Outputfile);
      dup2(stderrcopy, 2); // Recovers stderr
    }
    close(stderrcopy);
  }
  else // print to stderr
    Pass.run(*M); // Run passes (results will be printed on stderr)
}

void JIT_Compiler::optimize_module(llvm::Module* &M)
{
  if (ExpEnv.transform_struc.optlevel > -1) { /* Do I need to apply transform level? */
    /* Yes, I do, so... */

    /* Initializes PassManager for Function */
    std::shared_ptr<FunctionPassManager> FPM;
    FPM.reset(new legacy::FunctionPassManager(M));
    FPM->add(new DataLayoutPass());
    PassManagerBuilder Builder; // aid to 'FPM' and 'MPM'
	
    /* Initializes PassManager for Function */
    PassManager MPM;
    TargetLibraryInfo *TLI = new TargetLibraryInfo(Triple(M->getTargetTriple()));
    MPM.add(TLI);
    MPM.add(new DataLayoutPass());
	
    /* Populates 'Builder' */
    Builder.OptLevel = ExpEnv.transform_struc.optlevel;
    Builder.DisableUnitAtATime = !ExpEnv.transform_struc.unit_at_time_enabled;
    //NOT IN LLVM 3.5
    //Builder.DisableSimplifyLibCalls = !ExpEnv.transform_struc.simplify_libcalls_enabled;
      /* inline and unrool only be enabled if 'ExpEnv.transform_struc.optlevel' > 0 */
      if (ExpEnv.transform_struc.optlevel) Builder.Inliner =
          createFunctionInliningPass(ExpEnv.transform_struc.opt_args.inline_threshold);
      Builder.DisableUnrollLoops = (ExpEnv.transform_struc.optlevel == 0);
      /***/
    /***/

    /* Populates 'FPM' from 'Builder' */
    Builder.populateFunctionPassManager(*FPM);
    /* Populates 'MPM' from 'Builder' */
    Builder.populateModulePassManager(MPM);
    
    /*
     * Enabling link-time optimizations -- default is no *
     * Use 'link_time_opt/1', 'link_time_opt/3', 'enable_link_time_opt/0', or 'enable_link_time_opt/2' to change *
    */
    if (ExpEnv.transform_struc.link_time_opt.enabled)
      Builder.populateLTOPassManager(MPM /*,
                                     (bool)ExpEnv.transform_struc.link_time_opt.internalize,
                                     (bool)ExpEnv.transform_struc.link_time_opt.runinliner
					 */);
    /***/

    //set_regalloc_pass(MPM);

    /* Pass over all functions within Module and run passes */
    FPM->doInitialization();
    for (Module::iterator F = M->begin(), E = M->end(); F != E; ++F)
      FPM->run(*F);
    FPM->doFinalization();
    /***/

    MPM.run(*M); // Run passes on module
  }
  else {
    /* No, I need to apply transform passes accordingly transform predicates but transform_level/1 */
    std::vector<GlobalValue*> GVvector; // 'createGVExtractionPass' argument
    PassManager Pass; // 'Pass' stores transform passes to be applied
    TargetLibraryInfo *TLI = new TargetLibraryInfo(Triple(M->getTargetTriple()));
    Pass.add(TLI); // First, I add on 'Pass' the Target Info of Module
    Pass.add(new DataLayoutPass()); // Second, I must add Target Data on 'Pass'
    for (int i = 0; i < ExpEnv.transform_struc.n; i++) {
      /*
       * 'ExpEnv.transform_struc.act_tr' contains sorted transform passes *
       * 'ExpEnv.transform_struc.act_tr' is filled by transform predicates *
       * What must I do? *
       *    1. Pass over 'ExpEnv.transform_struc.act_tr' (by previous 'for') *
       *    2. For each unity within 'ExpEnv.analysis_struc.act_an', add transform pass on 'Pass' *
      */
      switch (ExpEnv.transform_struc.act_tr[i]) {
      case t_createAggressiveDCEPass:
        Pass.add(createAggressiveDCEPass());
	break;
      case t_createArgumentPromotionPass:
        Pass.add(createArgumentPromotionPass((Int)ExpEnv.transform_struc.opt_args.arg_promotion_max_elements));
        break;
      case t_createBBVectorizePass:
        Pass.add(createBBVectorizePass());
        break;
      case t_createBlockExtractorPass:
        Pass.add(createBlockExtractorPass());
        break;
      //NOT IN LLVM 3.5
      //case t_createBlockPlacementPass:
      //  Pass.add(createBlockPlacementPass());
      //  break;
      case t_createBreakCriticalEdgesPass:
        Pass.add(createBreakCriticalEdgesPass());
        break;
      case t_createCFGSimplificationPass:
        Pass.add(createCFGSimplificationPass());
        break;
      case t_createCodeGenPreparePass:
        Pass.add(createCodeGenPreparePass());
        break;
      case t_createConstantMergePass:
        Pass.add(createConstantMergePass());
        break;
      case t_createConstantPropagationPass:
        Pass.add(createConstantPropagationPass());
        break;
      case t_createCorrelatedValuePropagationPass:
        Pass.add(createCorrelatedValuePropagationPass());
        break;
      case t_createDeadArgEliminationPass:
        Pass.add(createDeadArgEliminationPass());
        break;
      case t_createDeadArgHackingPass:
        Pass.add(createDeadArgHackingPass());
        break;
      case t_createDeadCodeEliminationPass:
        Pass.add(createDeadCodeEliminationPass());
        break;
      case t_createDeadInstEliminationPass:
        Pass.add(createDeadInstEliminationPass());
        break;
      case t_createDeadStoreEliminationPass:
        Pass.add(createDeadStoreEliminationPass());
        break;
      case t_createDemoteRegisterToMemoryPass:
        Pass.add(createDemoteRegisterToMemoryPass());
        break;
      case t_createEarlyCSEPass:
        Pass.add(createEarlyCSEPass());
        break;
      case t_createFunctionAttrsPass:
        Pass.add(createFunctionAttrsPass());
        break;
      case t_createFunctionInliningPass:
        Pass.add(createFunctionInliningPass(ExpEnv.transform_struc.opt_args.inline_threshold));
        break;
      case t_createGlobalDCEPass:
        Pass.add(createGlobalDCEPass());
        break;
      case t_createGlobalOptimizerPass:
        Pass.add(createGlobalOptimizerPass());
        break;
      case t_createGVExtractionPass:
        Pass.add(createGVExtractionPass(GVvector));
        break;
      case t_createGVNPass:
        Pass.add(createGVNPass());
        break;
      case t_createIndVarSimplifyPass:
        Pass.add(createIndVarSimplifyPass());
        break;
      case t_createInstructionCombiningPass:
        Pass.add(createInstructionCombiningPass());
        break;
      case t_createInstructionNamerPass:
        Pass.add(createInstructionNamerPass());
        break;
      case t_createInstructionSimplifierPass:
        Pass.add(createInstructionSimplifierPass());
        break;
      case t_createInternalizePass:
        Pass.add(createInternalizePass());
        break;
      case t_createIPConstantPropagationPass:
        Pass.add(createIPConstantPropagationPass());
        break;
      case t_createIPSCCPPass:
        Pass.add(createIPSCCPPass());
        break;
      case t_createJumpThreadingPass:
        Pass.add(createJumpThreadingPass());
        break;
      case t_createLCSSAPass:
        Pass.add(createLCSSAPass());
        break;
      case t_createLICMPass:
        Pass.add(createLICMPass());
        break;
      case t_createLoopDeletionPass:
        Pass.add(createLoopDeletionPass());
        break;
      case t_createLoopExtractorPass:
        Pass.add(createLoopExtractorPass());
        break;
      case t_createLoopIdiomPass:
        Pass.add(createLoopIdiomPass());
        break;
      case t_createLoopInstSimplifyPass:
        Pass.add(createLoopInstSimplifyPass());
        break;
      case t_createLoopRotatePass:
        Pass.add(createLoopRotatePass());
        break;
      case t_createLoopSimplifyPass:
        Pass.add(createLoopSimplifyPass());
        break;
      case t_createLoopStrengthReducePass:
        Pass.add(createLoopStrengthReducePass());
        break;
      case t_createLoopUnrollPass:
        Pass.add(createLoopUnrollPass((Int)ExpEnv.transform_struc.opt_args.loop_unroll_threshold));
        break;
      case t_createLoopUnswitchPass:
        Pass.add(createLoopUnswitchPass((bool)ExpEnv.transform_struc.opt_args.loop_unswitch_optimize_for_size));
        break;
      case t_createLowerAtomicPass:
        Pass.add(createLowerAtomicPass());
        break;
      case t_createLowerExpectIntrinsicPass:
        Pass.add(createLowerExpectIntrinsicPass());
        break;
      case t_createLowerInvokePass:
        Pass.add(createLowerInvokePass());
        break;
      case t_createLowerSwitchPass:
        Pass.add(createLowerSwitchPass());
        break;
      case t_createMemCpyOptPass:
        Pass.add(createMemCpyOptPass());
        break;
      case t_createMergeFunctionsPass:
        Pass.add(createMergeFunctionsPass());
        break;
      case t_createObjCARCAPElimPass:
        Pass.add(createObjCARCAPElimPass());
        break;
      case t_createObjCARCContractPass:
        Pass.add(createObjCARCContractPass());
        break;
      case t_createObjCARCExpandPass:
        Pass.add(createObjCARCExpandPass());
        break;
      case t_createObjCARCOptPass:
        Pass.add(createObjCARCOptPass());
        break;
      case t_createPartialInliningPass:
        Pass.add(createPartialInliningPass());
        break;
      case t_createPromoteMemoryToRegisterPass:
        Pass.add(createPromoteMemoryToRegisterPass());
        break;
      case t_createPruneEHPass:
        Pass.add(createPruneEHPass());
        break;
      case t_createReassociatePass:
        Pass.add(createReassociatePass());
        break;
      case t_createScalarReplAggregatesPass:
        Pass.add(createScalarReplAggregatesPass((Int)ExpEnv.transform_struc.opt_args.scalar_replace_aggregates_threshold));
        break;
      case t_createSCCPPass:
        Pass.add(createSCCPPass());
        break;
      //NOT IN LLVM 3.5
      //case t_createSimplifyLibCallsPass:
      //  Pass.add(createSimplifyLibCallsPass());
      //  break;
      case t_createSingleLoopExtractorPass:
        Pass.add(createSingleLoopExtractorPass());
        break;
      case t_createSinkingPass:
        Pass.add(createSinkingPass());
        break;
      case t_createStripDeadDebugInfoPass:
        Pass.add(createStripDeadDebugInfoPass());
        break;
      case t_createStripDeadPrototypesPass:
        Pass.add(createStripDeadPrototypesPass());
        break;
      case t_createStripDebugDeclarePass:
        Pass.add(createStripDebugDeclarePass());
        break;
      case t_createStripNonDebugSymbolsPass:
        Pass.add(createStripNonDebugSymbolsPass());
        break;
      case t_createStripSymbolsPass:
        Pass.add(createStripSymbolsPass((bool)ExpEnv.transform_struc.opt_args.strip_symbols_pass_type));
        break;
      case t_createTailCallEliminationPass:
        Pass.add(createTailCallEliminationPass());
        break;
      default:;
      }
    }
    //set_regalloc_pass(Pass);
    Pass.run(*M); // Run passes
  }
}

void JIT_Compiler::set_regalloc_pass(PassManager &PM) {
  // 'ExpEnv.codegen_struc.struc_enginebuilder.regallocator' contains the active register allocator to be used
  switch(ExpEnv.codegen_struc.struc_enginebuilder.regallocator) {
  case REG_ALLOC_BASIC:
    PM.add(createBasicRegisterAllocator());
    break;
  case REG_ALLOC_FAST:
    PM.add(createFastRegisterAllocator());
    break;
  case REG_ALLOC_GREEDY:
    PM.add(createGreedyRegisterAllocator());
    break;
  case REG_ALLOC_PBQP:
    PM.add(createDefaultPBQPRegisterAllocator());
    break;
  }
}

void* JIT_Compiler::compile_all(LLVMContext* &Context, yamop* p)
{
  /* Init llvm code generation, analysis and transform passes */
  InitializeNativeTarget();
  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeCore(Registry);
  initializeScalarOpts(Registry);
  initializeVectorization(Registry);
  initializeIPO(Registry);
  initializeAnalysis(Registry);
  initializeIPA(Registry);
  initializeTransformUtils(Registry);
  initializeInstCombine(Registry);
  initializeInstrumentation(Registry);
  initializeTarget(Registry);
  sys::Process::PreventCoreFiles();

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
  /* CLANG arguments */
  char *clang_args[] =
{
    "clang", "-fomit-frame-pointer", "-O0",
#if CUT_C
    "-DCUT_C=1",
#endif
#if COROUTINING
    "-DCOROUTINING=1",
#endif
#if RATIONAL_TREES
    "-DRATIONAL_TREES=1",
#endif
#if DEBUG
    "-DDEBUG=1",
#endif
#if DEPTH_LIMIT
    "-DDEPTH_LIMIT=1",
#endif
#if YAP_DBG_PREDS
    "-DYAP_DBG_PREDS=1",
#endif
#if YAP_STAT_PREDS
    "-DYAP_STAT_PREDS=1",
#endif
#if TABLING
    "-DTABLING=1",
#endif
#if _YAP_NOT_INSTALLED_
    "-D_YAP_NOT_INSTALLED_=1",
#endif
#ifdef HAVE_CONFIG_H
    "-DHAVE_CONFIG_H",
#endif
    "-DYAP_JIT", "-D_NATIVE=1", "-I.", "-I./H", "-I./include", "-I./os", "-I./OPTYap", "-I./BEAM", "-I./MYDDAS", "-I./HPP", "-xc", "-c", "-", "-o", "-", "-emit-llvm", NULL
};
#pragma GCC diagnostic pop
  
  std::string errStr;
  error_code e;

  /* Pipe to communicate 'echo' with 'clang' */
  int pipe1[2];
  int pid_echo, pid_clang ;
  
  /* 'clang' out file */
  char* outputfilename = (char*)malloc(33*sizeof(char));
  sprintf(outputfilename, "%lx.bc", (CELL)p);
  int Output = open(outputfilename, O_CREAT | O_RDWR, 0644);
  
  /* Creating pipes */
  if (pipe(pipe1)<0) {
    perror("      ERROR!!\n      ERROR") ;
    exit(1);
  }
	
  /* Calls echo. */
  pid_echo = fork() ;
  if (pid_echo < 0) {
    perror("      ERROR!!\n      ERROR") ;
    exit(1);
  }
  if (!pid_echo) {
    /* Setting echo's output to 1st pipe */
    dup2(pipe1[1], 1);
	
    /* Closing pipes */
    close(pipe1[0]);
    close(pipe1[1]);
    
    execlp("echo", "echo", p->y_u.J.jh->tcc.cmd, NULL);
  }
  else {
    /* Calls clang. */
    pid_clang = fork() ;
    if (pid_clang < 0) {
      perror("      ERROR!!\n      ERROR") ;
      exit(1);
    }
    if (!pid_clang) {
      /* Setting clang's input from 1st pipe */
      dup2(pipe1[0], 0) ;

      /* Setting clang's output to Output */
      dup2(Output, 1) ;
		  
      /* Closing pipes */
      close(pipe1[0]);
      close(pipe1[1]);
		  
      execvp(*clang_args, clang_args);
    }
  }
  /* Closing pipes */
  close(pipe1[0]);
  close(pipe1[1]);

  /* waiting for completion of processes */
  int i;
  int *status = NULL;
  // 2 means two processes: 'echo' and 'clang'
  for (i = 0; i < 2; i++) wait(status);
  /***/
  
  /*
   * At this point, the compiled code (O0) is on 'Output' *
   * I need to read it to main memory *
   * for this, I'll use 'MemoryBuffer::getOpenFile' *
  */
  lseek(Output, 0, SEEK_SET);
  ErrorOr<std::unique_ptr<MemoryBuffer>> em = MemoryBuffer::getOpenFile(Output, outputfilename, -1);
  e = em.getError();
  if (e) {
    errs() << "ERROR::Unable to MemoryBuffer from " << outputfilename << " -- " << e.message() << "\n";
    exit(1);
  }

  /*
   * At this point, the compiled code (O0) is on main memory *
   * I need to read it to Module *
   * for this, I'll use 'parseBitcodeFile' *
  */
  ErrorOr<Module *> ModuleOrErr =
    parseBitcodeFile(em.get()->getMemBufferRef(), *Context);

  std::unique_ptr<Module> M;
  
  if (std::error_code ec = ModuleOrErr.getError()) 
    errs() <<  ec.message();
  /* at last, get M */
  M.reset(ModuleOrErr.get());
  /*
   * verify module correctness *
   * predicates: *
   *   enable_module_correctness/1 *
   *   verify_module_before/0 *
   *   verify_module_both/0 *
  */
  if (ExpEnv.analysis_struc.pointtoverifymodule == BEFORE || ExpEnv.analysis_struc.pointtoverifymodule == BOTH) {
    if (verifyModule(*M)) {
      errs() << "ERROR:: Module not built correctly!\n";
      exit(1);
    }
  }
  /***/

#if YAP_DBG_PREDS
  /* for debug... print module before optimizing it */
  if (ExpEnv.debug_struc.pprint_llva.print_llva_before)
    errs() << "Module before optimization::\n" << *Mod;
#endif

  llvm::Module *Mod = M.get();
  /* Analyze module -- analysis predicates */
  analyze_module(Mod);
  /* Optimize module -- transform predicates */
  optimize_module(Mod);
  
  /* Computing size of optimized module */
  {
    std::error_code  ErrorInfo;
    /* Open file 'tmp.bc' which will be filled by optimized Module */
    std::unique_ptr<tool_output_file> Out;
    Out.reset(new tool_output_file("tmp.bc", ErrorInfo, llvm::sys::fs::F_None));
    if (ErrorInfo) {
      errs() << ErrorInfo.message() << '\n';
      exit(1);
    }
    /* 'createPrintModulePass(arg)' will print Module (now optimized) to on file represented by 'arg' */
    PassManager Pass;
    Pass.add(createPrintModulePass(Out->os()));
    Pass.run(*M);
    /* 'Out->keep()' will keep printed module to file and will close file */
    Out->keep();
	
    /* Open file 'tmp.bc' */
    int Outtmp = open("tmp.bc", O_CREAT | O_RDWR, 0644);
#if YAP_STAT_PREDS
    /* for statistics... compute file size and store value on 'NativeArea->area.native_size_bytes' */
    NativeArea->area.native_size_bytes[p->y_u.jhc.jh->caa.naddress][NativeArea->area.nrecomp[p->y_u.jhc.jh->caa.naddress]-1] = lseek(Outtmp, 0, SEEK_END);
#endif
    close(Outtmp);
    remove("tmp.bc");
  }
  /***/\
  
#if YAP_DBG_PREDS
  /* for debug... print module after optimizing it */
  if (ExpEnv.debug_struc.pprint_llva.print_llva_after)
    errs() << "Module after optimization::\n" << *Mod;
#endif
	
  /*
   * verify module correctness *
   * predicates: *
   *   enable_module_correctness/0 *
   *   enable_module_correctness/1 *
   *   verify_module_after/0 *
   *   verify_module_both/0 *
  */
  if (ExpEnv.analysis_struc.pointtoverifymodule == AFTER || ExpEnv.analysis_struc.pointtoverifymodule == BOTH) {
    if (verifyModule(*Mod)) {
      errs() << "ERROR:: Module not built correctly!\n";
      exit(1);
    }
  }
  /***/
  
  // materializeAllPermanently -- Make sure all GlobalValues in this Module are fully read
  error_code materialize_error = Mod->materializeAllPermanently();
  if (materialize_error.value() != 0) {
    errs() <<"Error:: bitcode didn't read correctly. -- " << materialize_error.message() << "\n";
    exit(1);
  }

  /* Creating EngineBuilder -- called 'builder' */
  Function *EntryFn;
   
  llvm::CodeGenOpt::Level level;
  switch(ExpEnv.codegen_struc.struc_enginebuilder.engineoptlevel) {
    case 0:
      level = CodeGenOpt::None;
      break;
    case 1:
      level = CodeGenOpt::Less;
      break;
    case 2:
      level = CodeGenOpt::Default;
      break;
    case 3:
      level = CodeGenOpt::Aggressive;
      break;
  }
  // codegen predicate 'relocmodel/1'
  llvm::Reloc::Model relocmodel;
  switch(ExpEnv.codegen_struc.struc_enginebuilder.relocmodel) {
    case 0:
      relocmodel = Reloc::Default;
      break;
    case 1:
      relocmodel = Reloc::Static;
      break;
    case 2:
      relocmodel = Reloc::PIC_;
      break;
    case 3:
      relocmodel = Reloc::DynamicNoPIC;
      break;
  }
  // codegen predicate 'codemodel/1'
  llvm::CodeModel::Model codemodel;
  switch(ExpEnv.codegen_struc.struc_enginebuilder.codemodel) {
    case 0:
      codemodel = CodeModel::Default;
      break;
    case 1:
      codemodel = CodeModel::JITDefault;
      break;
    case 2:
      codemodel = CodeModel::Small;
      break;
    case 3:
      codemodel = CodeModel::Kernel;
      break;
    case 4:
      codemodel = CodeModel::Medium;
      break;
    case 5:
      codemodel = CodeModel::Large;
      break;
  }
  // MCJIT is default from 3.6
  // codegen predicates 'enable_mcjit/0' or 'disable_mcjit/0'
  //  builder.setUseMCJIT((bool)ExpEnv.codegen_struc.struc_enginebuilder.usemcjit);
  llvm::TargetOptions Options;
  {
    /* codegen predicates 'enable_framepointer_elimination/0' or 'disable_framepointer_elimination/0' */
    Options.NoFramePointerElim = (bool)ExpEnv.codegen_struc.struc_targetopt.noframepointerelim;
	//NOT IN LLVM 3.5
    //Options.NoFramePointerElimNonLeaf = (bool)ExpEnv.codegen_struc.struc_targetopt.noframepointerelim;
    /***/
    // codegen predicates 'less_precise_fp_mad_option/0' or 'more_precise_fp_mad_option/0'
    Options.LessPreciseFPMADOption = (bool)ExpEnv.codegen_struc.struc_targetopt.lessprecisefpmadoption;
    // codegen predicates 'no_excess_fp_precision/0' or 'excess_fp_precision/0'
    //NOT IN LLVM 3.5
    //Options.NoExcessFPPrecision = (bool)ExpEnv.codegen_struc.struc_targetopt.noexcessfpprecision;
    // codegen predicates 'unsafe_fp_math/0' or 'safe_fp_math/0'
    Options.UnsafeFPMath = (bool)ExpEnv.codegen_struc.struc_targetopt.unsafefpmath;
    // codegen predicates 'rounding_mode_dynamically_changed/0' or 'rounding_mode_not_changed/0'
    Options.HonorSignDependentRoundingFPMathOption =
        (bool)ExpEnv.codegen_struc.struc_targetopt.honorsigndependentroundingfpmathoption;
    // codegen predicates 'no_use_soft_float/0' or 'use_soft_float/0'
    Options.UseSoftFloat = (bool)ExpEnv.codegen_struc.struc_targetopt.usesoftfloat;
    // codegen predicates 'enable_jit_exception_handling/0' or 'disable_jit_exception_handling/0'
	//NOT IN LLVM 3.5
    //Options.JITExceptionHandling = (bool)ExpEnv.codegen_struc.struc_targetopt.jitexceptionhandling;
    // codegen predicates 'enable_jit_emit_debug_info/0' or 'disable_jit_emit_debug_info/0'
    Options.JITEmitDebugInfo = (bool)ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfo;
    // codegen predicates 'enable_jit_emit_debug_info_to_disk/0' or 'disable_jit_emit_debug_info_to_disk/0'
    Options.JITEmitDebugInfoToDisk = (bool)ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfotodisk;
    // codegen predicates 'guaranteed_tail_call_opt/0' or 'no_guaranteed_tail_call_opt/0'
    Options.GuaranteedTailCallOpt = (bool)ExpEnv.codegen_struc.struc_targetopt.guaranteedtailcallopt;
    // codegen predicates 'enable_tail_calls/0' or 'disable_tail_calls/0'
    Options.DisableTailCalls = (bool)ExpEnv.codegen_struc.struc_targetopt.disabletailcalls;
    // codegen predicates 'enable_fast_isel/0' or 'disable_fast_isel/0'
    Options.EnableFastISel = (bool)ExpEnv.codegen_struc.struc_targetopt.fastisel;
  }
  // codegen predicates 'fp_abitype/1' or 'default_fp_abitype/0'
  switch(ExpEnv.codegen_struc.struc_targetopt.floatabitype) {
    case 0:
      Options.FloatABIType = FloatABI::Default;
      break;
    case 1:
      Options.FloatABIType = FloatABI::Soft;
      break;
    case 2:
      Options.FloatABIType = FloatABI::Hard;
      break;
  }
   
  // codegen predicate 'engine_opt_level/1'

  /* Creating ExecutionEngine from EngineBuilder (builder) */
  ExecutionEngine *EE =
    EngineBuilder(std::move(M))
    .setErrorStr(&errStr)
    .setOptLevel( level )
    .setRelocationModel( relocmodel )
    .setCodeModel( codemodel )
    .setEngineKind(EngineKind::JIT)
       .setTargetOptions(Options)
       // check class in Kaleidoscope example.
       //    .setMCJITMemoryManager(std::unique_ptr<HelpingMemoryManager>(
       //								 new HelpingMemoryManager(this)))
    .setMCJITMemoryManager(std::unique_ptr<SectionMemoryManager>(new SectionMemoryManager()))
    .create();
  if (!EE) {
    if (!errStr.empty())
      errs() << "Error creating Execution Engine: " << errStr << "\n";
    else
      errs() << "Unknown error creating Execution Engine!\n";
    exit(1);
  }
  /***/

  // 'clause' is our function -- getting it from Module
  EntryFn = Mod->getFunction("clause");
  if (!EntryFn) {
    /*
     * Theoretically, every Module is correct, but *
     * for some reason, llvm can not compile some of them *
    */
    close(Output);
    remove(outputfilename);
    free(p->y_u.J.jh->tcc.cmd);
    free(outputfilename);
    return NULL;
  }

  /* Here, we know that Module was be successfully compiled, so... */
  // 1. execute all of the static constructors or destructors for program
  EE->runStaticConstructorsDestructors(false);
  // global++; what is this?
  close(Output);
  remove(outputfilename);
  free(p->y_u.J.jh->tcc.cmd);
  free(outputfilename);
  // 2. get native pointer from 'clause' (our function within Module) and return it
  return EE->getPointerToFunction(EntryFn);
}

void* JIT_Compiler::compile(yamop* p) {
  /* LLVMContext must be declared here, otherwise LLVM will crash on x86_64 machines */
  LLVMContext *Context = new LLVMContext();
  return compile_all(Context, p);
}
