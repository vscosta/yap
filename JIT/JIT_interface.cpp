/*
 * amijit.h
 *
 *  Created on: Jan 9, 2015
 *      Author: vsc
 */

#ifndef AMIJIT_H_
#define AMIJIT_H_

#if YAP_JIT
/* Available LLVM (v. 3.1) Analysis Passes */
typedef enum {
  e_createAAEvalPass, //Exhaustive Alias Analysis Precision Evaluator
  e_createAliasAnalysisCounterPass, //Count Alias Analysis Query Responses
  e_createBasicAliasAnalysisPass, //Basic Alias Analysis (stateless AA impl)
  e_createCFGOnlyPrinterPass, //Print CFG of function to 'dot' file (with no function bodies)
  e_createCFGPrinterPass, //Print CFG of function to 'dot' file
  e_createDbgInfoPrinterPass, //Print debug info in human readable form
  e_createDomOnlyPrinterPass, //Print dominance tree of function to 'dot' file (with no function bodies)
  e_createDomPrinterPass, //Print dominance tree of function to 'dot' file
  e_createGlobalsModRefPass, //Simple mod/ref analysis for globals
  e_createInstCountPass, //Counts the various types of Instructions
  e_createIVUsersPass, //Induction Variable Users
  e_createLazyValueInfoPass, //Lazy Value Information Analysis
  e_createLibCallAliasAnalysisPass, //LibCall Alias Analysis
  e_createLintPass, //Statically lint-checks LLVM IR
  e_createLoopDependenceAnalysisPass, //Loop Dependence Analysis
  e_createMemDepPrinter, //Memory Dependence Analysis
  e_createModuleDebugInfoPrinterPass, //Decodes module-level debug info
  e_createNoAAPass, //No Alias Analysis (always returns 'may' alias)
  e_createNoPathProfileInfoPass, //No Path Profile Information
  e_createNoProfileInfoPass, //No Profile Information
  e_createObjCARCAliasAnalysisPass, //ObjC-ARC-Based Alias Analysis
  e_createPathProfileLoaderPass, //Loads information from a path profile dump file
  e_createPathProfileVerifierPass, //Verifies path profiling information
  e_createPostDomOnlyPrinterPass, //Print postdominance tree of function to 'dot' file (with no function bodies)
  e_createPostDomPrinterPass, //Print postdominance tree of function to 'dot' file
  e_createProfileEstimatorPass, //Estimate profiling information
  e_createProfileLoaderPass, //Load profile information from llvmprof.out
  e_createProfileVerifierPass, //Verify profiling information
  e_createRegionInfoPass, //Detect single entry single exit regions
  e_createRegionOnlyPrinterPass, //Print regions of function to 'dot' file (with no function bodies)
  e_createRegionPrinterPass, //Print regions of function to 'dot' file
  e_createScalarEvolutionAliasAnalysisPass, //ScalarEvolution-based Alias Analysis
  e_createTypeBasedAliasAnalysisPass //Type-Based Alias Analysis
} enumAnalysisPasses;

/* Available LLVM (v. 3.1) Transform Passes */
typedef enum{
  t_createAggressiveDCEPass, //This pass uses the SSA based Aggressive DCE algorithm
  t_createArgumentPromotionPass, //Promotes "by reference" arguments to be passed by value if the number of elements passed is smaller or equal to maxElement
  t_createBBVectorizePass, //A basic-block vectorization pass
  t_createBlockExtractorPass, //Extracts all blocks (except those specified in the argument list) from the functions in the module
  t_createBlockPlacementPass, //This pass reorders basic blocks in order to increase the number of fall-through conditional branches
  t_createBreakCriticalEdgesPass, //Break all of the critical edges in the CFG by inserting a dummy basic block
  t_createCFGSimplificationPass, //Merge basic blocks, eliminate unreachable blocks, simplify terminator instructions, etc...
  t_createCodeGenPreparePass, //Prepares a function for instruction selection
  t_createConstantMergePass, //Returns a new pass that merges duplicate global constants together into a single constant that is shared
  t_createConstantPropagationPass, //A worklist driven constant propagation pass
  t_createCorrelatedValuePropagationPass, //Propagate CFG-derived value information
  t_createDeadArgEliminationPass, //This pass removes arguments from functions which are not used by the body of the function
  t_createDeadArgHackingPass, //Same as DAE, but delete arguments of external functions as well
  t_createDeadCodeEliminationPass, //This pass is more powerful than DeadInstElimination
  t_createDeadInstEliminationPass, //Removes trivially dead instructions without modifying the CFG of the function
  t_createDeadStoreEliminationPass, //Deletes stores that are post-dominated by must-aliased stores and are not loaded used between the stores
  t_createDemoteRegisterToMemoryPass, //This pass is used to demote registers to memory references
  t_createEarlyCSEPass, //This pass performs a simple and fast CSE pass over the dominator tree
  t_createFunctionAttrsPass, //Discovers functions that do not access memory, or only read memory, and gives them the readnone/readonly attribute
  t_createFunctionInliningPass,
  t_createGlobalDCEPass, //This transform is designed to eliminate unreachable internal globals (functions or global variables)
  t_createGlobalOptimizerPass, //Returns a new pass that optimizes non-address taken internal globals
  t_createGVExtractionPass, //Deletes as much of the module as possible, except for the global values specified
  t_createGVNPass, //Performs global value numbering and redundant load elimination cotemporaneously
  t_createIndVarSimplifyPass, //Transform induction variables in a program to all use a single canonical induction variable per loop
  t_createInstructionCombiningPass, //Combine instructions to form fewer, simple instructions
  t_createInstructionNamerPass, //Give any unnamed non-void instructions "tmp" names
  t_createInstructionSimplifierPass, //Remove redundant instructions
  t_createInternalizePass, //Loops over all of the functions in the input module, internalizing all globals
  t_createIPConstantPropagationPass, //Propagates constants from call sites into the bodies of functions
  t_createIPSCCPPass, //Propagates constants from call sites into the bodies of functions, and keeps track of whether basic blocks are executable in the process
  t_createJumpThreadingPass, //Thread control through mult-pred/multi-succ blocks where some preds always go to some succ
  t_createLCSSAPass, //This pass inserts phi nodes at loop boundaries to simplify other loop optimizations
  t_createLICMPass, //Loop invariant code motion and memory promotion pass
  t_createLoopDeletionPass, //Performs DCE of non-infinite loops that it can prove are dead
  t_createLoopExtractorPass, //Extracts all natural loops from the program into a function if it can
  t_createLoopIdiomPass, //Recognizes and replaces idioms in loops
  t_createLoopInstSimplifyPass, //Simplifies instructions in a loop's body
  t_createLoopRotatePass, //Simple loop rotating pass
  t_createLoopSimplifyPass, //Insert Pre-header blocks into the CFG for every function in the module
  t_createLoopStrengthReducePass, //This pass is strength reduces GEP instructions that use a loop's canonical induction variable as one of their indices
  t_createLoopUnrollPass, //Simple loop unrolling pass
  t_createLoopUnswitchPass, //Simple loop unswitching pass
  t_createLowerAtomicPass, //Lower atomic intrinsics to non-atomic form
  t_createLowerExpectIntrinsicPass, //Removes llvm.expect intrinsics and creates "block_weights" metadata
  t_createLowerInvokePass, //Converts invoke and unwind instructions to use sjlj exception handling mechanisms
  t_createLowerSwitchPass, //Converts SwitchInst instructions into a sequence of chained binary branch instructions
  t_createMemCpyOptPass, //Performs optimizations related to eliminating memmove calls and/or combining multiple stores into memset's
  t_createMergeFunctionsPass, //Discovers identical functions and collapses them
  t_createObjCARCAPElimPass, //ObjC ARC autorelease pool elimination
  t_createObjCARCContractPass, //Late ObjC ARC cleanups
  t_createObjCARCExpandPass, //ObjC ARC preliminary simplifications
  t_createObjCARCOptPass, //ObjC ARC optimization
  t_createPartialInliningPass, //Inlines parts of functions
  t_createPromoteMemoryToRegisterPass, //This pass is used to promote memory references to be register references
  t_createPruneEHPass, //Return a new pass object which transforms invoke instructions into calls, if the callee can _no  t_ unwind the stack
  t_createReassociatePass, //This pass reassociates commutative expressions in an order that is designed to promote better constant propagation, GCSE, LICM, PRE...
  t_createScalarReplAggregatesPass, //Break up alloca's of aggregates into multiple allocas if possible.
  t_createSCCPPass, //Sparse conditional constant propagation
  t_createSimplifyLibCallsPass, //Optimizes specific calls to specific well-known (library) functions
  t_createSingleLoopExtractorPass, //Extracts one natural loop from the program into a function if it can
  t_createSinkingPass, //Code Sinking
  t_createStripDeadDebugInfoPass, //Removes unused symbols' debug info
  t_createStripDeadPrototypesPass, //Removes any function declarations (prototypes) that are not used
  t_createStripDebugDeclarePass, //Removes llvm.dbg.declare intrinsics
  t_createStripNonDebugSymbolsPass, //Strips symbols from functions and modules
  t_createStripSymbolsPass, //Removes symbols from functions and modules
  t_createTailCallEliminationPass //Eliminates call instructions to the current function which occur immediately before return instructions
} enumTransformPasses;

/* Enumeration for points to verify module correctness */
typedef enum {
  NOPOINT, // no point -- module will not be verified
  BEFORE,  // before optimize -- module will be verified before transform passes
  AFTER,   // after optimize -- module will be verified after transform passes
  BOTH     // both -- module will be verified both before and after transform passes
} enumPointToVerifiy;

/* Enumeration for available execution modes */
typedef enum {
  JUST_INTERPRETED,
  SMART_JIT,
  CONTINUOUS_COMPILATION,
  JUST_COMPILED
} enumExecModes;

/* Enumerations for available parameters for frequency measurement */
typedef enum{
  NO_FREQ, // without frequency (used on 'JUST_INTERPRETED' and 'JUST_COMPILED' modes)
  COUNTER, // unity counters
  TIME     // unity execution times
} enumFrequencyType;

/* Enumerations for types of clauses that can be head on traces */
typedef enum{
  UNUSED,          // not used
  JUST_HOT,        // enumFrequencyType associated to clause must reach threshold
  HOT_AND_CALLEE,  // JUST_HOT + clause must contain a callee opcode (fcall or call)
  HOT_AND_GREATER, // JUST_HOT + clause size must be greater than others
  HOT_AND_FEWER    // JUST_HOT + clause's backtracks must be smaller than others
} enumMainClauseType;

/* Enumerations for available llvm registers allocators */
typedef enum{
  REG_ALLOC_BASIC,  // Basic
  REG_ALLOC_FAST,   // Fast
  REG_ALLOC_GREEDY, // Greedy
  REG_ALLOC_PBQP    // PBQP
} enumRegAllocator;

#include <pthread.h> // for 'CONTINUOUS_COMPILATION' mode

/* Enumeration for points to apply debug predicates */
typedef enum{
  NO_PLACE = 0,                 // no place
  ON_INTERPRETER = 1,           // on interpreted opcodes
  ON_PROFILED_INTERPRETER = 2,  // on profiled opcodes
  ON_NATIVE = 4                 // on native code
} enumPlace;

/* This struct is used by debug predicates --
   usually associated to yaam opcode, basic blocks or clauses */
typedef struct printt_struc {
 Int print;       // Should I print?
 CELL msg_before; // If I print, what message should come before?
 CELL msg_after;  // If I print, what message should come after?
} PrinttStruc;


/* This struct represents our experimental environment for YAP */
typedef struct environment {
  // struct for analysis predicates -- all fields are modified by analysis predicates (JIT_AnalysisPreds.c)
  struct {
    CELL outfile; // Where will analysis results be printed?
    Int stats_enabled; // Should llvm stats be printed on 'shutdown_llvm()'?
    Int time_pass_enabled; // Should llvm time passes be printed on 'shutdown_llvm()'?
    enumPointToVerifiy pointtoverifymodule; // What point of code will llvm modules be verified?
    COUNT n; // Number of elements on 'act_an'
    enumAnalysisPasses *act_an; // List of analysis passes
  } analysis_struc;

  // struct for transform predicates -- all fields are modified by transform predicates (JIT_TransformPreds.c)
  struct {
    Int optlevel; // Optimization level -- 'act_tr' only will be used if 'optlevel' is '-1'
    COUNT n; // Number of elements on 'act_tr'
    enumTransformPasses *act_tr; // List of transform passes
    struct {
      CELL arg_promotion_max_elements; // Max elements on 'Argument Promotion Pass'
      CELL strip_symbols_pass_type;    // Argument for 'Strip Symbols Pass' -- if true, only debugging information is removed from the module
      CELL scalar_replace_aggregates_threshold; // Threshold for 'Scalar Repl Aggregates Pass'
      CELL loop_unswitch_optimize_for_size; // Argument for 'Loop Unswitch Pass' -- Should I optimize for size?
      CELL loop_unroll_threshold; // Threshold for 'Loop Unroll Pass'
      CELL inline_threshold; // Threshold for 'Function Inlining Pass'
    } opt_args;
    Int unit_at_time_enabled; // Should I enable IPO?
    Int simplify_libcalls_enabled; // Should I simplify libcalls?
    struct {
      Int enabled; // Should I enable link-time optimizations?
      CELL internalize; // Should I run 'Internalize Pass' on link-time optimization?
      CELL runinliner; // Should I run 'Inline Pass' on link-time optimization?
    } link_time_opt;
  } transform_struc;

  // struct for codegen predicates -- all fields are modified by codegen predicates (JIT_CodegenPreds.c)
  struct {
    struct {
      Int noframepointerelim; // Should I use frame pointer elimination opt?
      Int lessprecisefpmadoption; // Should I allow to generate multiply add if the result is "less precise"?
      Int noexcessfpprecision; // Should I enable excess fp precision?
      Int unsafefpmath; // Should I allow to produce results that are "less precise" than IEEE allows?
      Int honorsigndependentroundingfpmathoption; // Which rounding mode should I use?
      Int usesoftfloat; // Should I use libcalls or FP instructions to treat floating point libraries?
      Int jitexceptionhandling; // Should JIT emit exception handling info?
      Int jitemitdebuginfo; // Should JIT emit debug information?
      Int jitemitdebuginfotodisk; // Should JIT write debug information to disk?
      Int guaranteedtailcallopt; // Should I perform tail call optimization on calls which use fastcc calling convention?
      Int disabletailcalls; // Should I use tail calls?
      Int fastisel; // Should I use 'fast-path instruction selection' to reduce compilation time? If 'yes' native code won't have best quality
      Int floatabitype; /* 0 = Default, 1 = Soft, 2 = Hard */
    } struc_targetopt;
    struct {
      Int engineoptlevel; /* 0 = None, 1 = Less, 2 = Default, 3 = Agressive */
      Int relocmodel; /* 0 = Default, 1 = Static, 2 = PIC, 3 = DynamicNoPIC */
      Int codemodel; /* 0 = Default, 1 = JITDefault, 2 = Small, 3 = Kernel, 4 = Medium, 5 = Large */
      Int usemcjit; // Should I use MC-JIT implementation (experimental)?
      enumRegAllocator regallocator; // Active register allocator (predicate register_allocator/1)
    } struc_enginebuilder;
  } codegen_struc;

  // struct for configuration predicates -- all fields are modified by configuration predicates (JIT_ConfigPreds.c)
  struct {
    enumExecModes execution_mode; // Active execution mode
    enumFrequencyType frequency_type; // Active frequency type
    Float frequency_bound; // Bound to become clauses as hot
    Float profiling_startp; // Bound to init monitoring and trace building
    enumMainClauseType mainclause_ty; // Types of clauses that can be head on traces
    COUNT ncores; // Number of cores on processor -- used to determine default 'compilation_threads' (compilation_threads = ncores - 1)
    COUNT compilation_threads; // Number of compilation threads (used only if 'execution_mode' is 'CONTINUOUS_COMPILATION')
    pthread_t* threaded_compiler_threads; // List of threads (size = 'compilation_threads'). Used by function 'pthread_create'. Used on 'CONTINUOUS_COMPILATION' mode
    CELL* posthreads; // Used to determine which threads are free/busy
    Int torecompile; // Should I recompile traces?
    Int current_displacement; // Jump displacement to run yaam opcodes on absmi. Zero is the default value and will make standard yaam opcodes are executed. 'TOTAL_OF_OPCODES' is the value after any clause become critical and will make 'traced_' yaam opcodes are executed.
    COUNT TOTAL_OF_OPCODES; // Total of yaam opcodes. I must determine this dynamically due to several '#define' statements. Used to determine 'current_displacement' after any clause become critical.
    Int useonlypi; // Execute only 'traced_' yaam opcodes. Don't compile. WARNING: if you enable this field (predicate only_profiled_interpreter/0), the system performance will decrease considerably. Use only to determine the actual cost of running such opcodes.
  } config_struc;

#if YAP_STAT_PREDS
  // struct for statistic predicates -- all fields are modified by statistic predicates (JIT_StatisticPreds.c)
  struct {
    int papi_initialized; // Was PAPI initialized? Used on predicate 'statistics_jit/0' -- if 1, PAPI results will be emitted
    int papi_eventset; // PAPI event set
    long long *papi_values; // List of collected performance counter values
    short *papi_valid_values; // List of performance counters that will be collected
    int papi_event_type; // Type of event that will be collected -- 'papi_event_type' involves what performance counters will be within 'papi_valid_values'
  } stats_struc;
#endif
  
#if YAP_DBG_PREDS
  // struct for debug predicates -- all fields are modified by debug predicates (JIT_DebugPreds.c)
  struct {
    /* Here, one 'PrinttStruc' for each yaam opcode on 'YapAppliedOpcodes.h' */
    #define OPCODE(OP,TYPE) \
    PrinttStruc pyaam_##OP;
    #include "YapAppliedOpcodes.h"
    #undef OPCODE

    /* Here, one 'PrinttStruc' for each basic block on 'AppliedBasicBlocks.h' */
    #define BBLOCK(BB) \
    PrinttStruc pbbs_##BB;
    #include "Yap_AppliedBasicBlocks.h"
    #undef BBLOCK

    /* This 'PrinttStruc' inform system whether head-clauses on traces should be printed */
    PrinttStruc pmainclause_on_head;

    /* Fields to treat intermediate code */
    struct{
      Int print_to_std; // Should intermediate code be printed on std (stdout, stderr)? Default:: print_to_std = 0 (don't print to stdout nor stderr)
      Int print_to_file; // Should intermediate code be printed on file? Default:: print_to_file = 0 (don't print to file)
      CELL std_name; // if 'print_to_std' = 'yes', where should intermediate code be printed?
      CELL file_name; // if 'print_to_file' = 'yes', what file should intermediate code be printed?
    } pprint_intermediate;

    /* Fields to treat llva code */
    struct {
      Int print_llva_before; // Should llva code be printed before optimizing module?
      Int print_llva_after; // Shoud llva code be printed after optimizing module?
    } pprint_llva;

    /* Fields for predicate print_me/2 */
    struct {
      CELL interpreted_backtrack; // msg to print when backtrack on standard yaam opcodes occur
      CELL profiled_interpreted_backtrack; // msg to print when backtrack on 'traced_' yaam opcodes occur
      CELL native_backtrack; // msg to print when backtrack on native code occur
      CELL interpreted_treat_heap; // msg to print when heap is treated on interpreter (standard and 'traced_' yaam opcodes)
      CELL native_treat_heap; // msg to print when heap is treated on native code
      CELL interpreted_treat_trail; // msg to print when trail is treated on interpreter (standard and 'traced_' yaam opcodes)
      CELL native_treat_trail; // msg to print when trail is treated on native code
      CELL criticals; // msg to print when any clause becomes critical
      CELL at_compilation; // msg to print before compilation
      CELL at_recompilation; // msg to print before recompilation
      CELL nativerun_init; // msg to print when native code is about to be run
      CELL nativerun_exit_by_success; // msg to print when native code exits by success, ie., basic block nonexistent in native code (allows rebuilding and recompilation of traces)
      CELL nativerun_exit_by_fail; // msg to print when native code exits byfail, ie., exits to treat trail or heap (don't allow rebuilding and recompilation of traces)
    } pprint_me;

    /* Messages on all predicates */
    struct {
      Int info_msgs; // Should I allow info messages?
      Int success_msgs; // Should I allow success messages?
      Int warning_msgs; // Should I allow warning messages?
      Int error_msgs; // Should I allow error messages?
    } act_predicate_msgs;

    /* Actions on all predicates */
    struct {
      Int exit_on_warning; // Should I exit when any warning occur?
      Int disable_on_warning; // Shouldn't I adjust appropriate values when any warning occur (implies 'exit_on_warning' = 'false')
      Int exit_on_error; // Should I exit when any error occur?
    } act_predicate_actions;
  } debug_struc;
#endif

} Environment;

/* Enumeration for types of basic blocks -- used on trace construction */
typedef enum block_try {
  NONE,               // untyped
  SIMPLE_ENTRY,       // first basic block of any yaam opcode
  SIMPLE,             // any other basic block of any yaam opcode
  CONDITIONAL_HEADER, // basic block of any 'if' statement of any yaam opcode
  MULTIPLE_DESTINY    // basic block of many destinations (elementary block). Returns from native code to interpreter always will occur here
} BlockTy;

/* Struct to represent individual basic blocks within traces */
typedef struct blocks_context {
  union {
    /* Fields for SIMPLE_ENTRY blocks */
    struct {
      UInt id; // block identifier (Yap_BasicBlocks.h)
      char *label_entry; // entry label -- destinations of jumps from others 'SIMPLE_ENTRY' or 'SIMPLE' blocks
      char *label_destiny; // destiny label -- where should I jump after I run?
    } eb;

    /* Fields for SIMPLE blocks */
    struct {
      UInt id; // block identifier (Yap_BasicBlocks.h)
      char *label_destiny; // destiny label -- where should I jump after I run?
    } sb;

    /* Fields for CONDITIONAL_HEADER blocks */
    struct {
      char *exp; // expression of 'if' statement
      struct blocks_context *_if; // destination if 'exp' is true
      struct blocks_context *_else; // destination if 'exp' is false
    } kb;

    /* Fields for MULTIPLE_DESTINY blocks */
    struct {
      UInt id; // block identifier (Yap_BasicBlocks.h)
      COUNT nfaillabels; // number of destinations caused by backtrack on native code
      struct {
        UInt *p;
        char **labels;
      } faildestiny; // destinations caused by backtrack on native code
      COUNT ndest; // number of destinations that not be backtrack
      struct {
        UInt *p;
        char **labels;
      } destiny; // destinations that not be backtrack
    } mdb;

    /* Fields for untyped blocks */
    struct {
      CELL header; // just for aiding after treating 'CONDITIONAL_HEADER' blocks
    } xb;
  } u;
  BlockTy blockty; // Basic block type
  CELL thisp; // Value of PREG. Inside traces, basic blocks are only different from each other if 'id' and 'blockty' are different
  CELL prev; // Previous basic block
  CELL next; // Next basic block
} BlocksContext;

/* Struct to represent fully traces */
typedef struct trace_context {
  COUNT n; // number of basic blocks
  CELL tracesize; // For statistics... list of size (bytes) of each trace
  BlocksContext *bc; // basic blocks context
} TraceContext;

/* Struct to represent Intermediatecode Area */
typedef struct intermediatecode_context {
  COUNT n; // Total of traces stored
  struct {
    TraceContext** t; // List of pointers to traces -- total of 'n'
    COUNT* ok; // List of traces ok (traces constructed and compiled at least once)
    COUNT* isactive; // List of active traces (traces under construction). Initialized to zero
    BlocksContext** lastblock; // List of last block added on each trace
#if YAP_STAT_PREDS
    double* profiling_time; // For statistics... list of profiling time of each trace
#endif
  } area;
} IntermediatecodeContext;

/* Struct to represent Nativecode Area */
typedef struct native_context {
  COUNT n; // Total of traces compiled (is not necessarily equal to 'IntermediatecodeContext.n')
  struct {
    void** p; // List of pointers to compiled codes -- total of 'n'
    COUNT* ok;  // List of compiled codes ok (traces constructed and compiled at least once)
    CELL* pc; // List of first heads of each compiled code
#if YAP_STAT_PREDS
    COUNT *nrecomp; // For statistics... number of recompilations of each compiled code (max '1' if recompilation is disabled)
    double **compilation_time; // For statistics... list of compilation time of each compiled code on each recompilation
    CELL **native_size_bytes; // For statistics... list of native size (bytes) of each compiled code on each recompilation
    CELL **trace_size_bytes; // For statistics... list of trace size (bytes) of each trace on each recompilation
#endif
  } area;
#if YAP_STAT_PREDS
  COUNT *runs; // List of calls of each compiled code
  double *t_runs; // List of execution time of each compiled code
  COUNT *success; // List of exit by success of each compiled code
#endif
} NativeContext;

/*
 *   Control flags for managing intermediate code *
 * from traces stored on Intermediatecode Area *
 *   Intermediatecode Area stores traces represented as *
 * basic blocks sequence *
*/
typedef struct control_flags_context {
  COUNT nemited;
  char** emited_blocks;
  short emit;
  short leastonce;
  char* clabel;
  COUNT labelidx;
  COUNT printlabel;
  char** entries;
  COUNT nentries;
} ControlFlagsContext;


/* Struct associated to 'jit_handler' opcode */
typedef struct jit_handl_context {
  /* Main yaam opcode features -- used by predicate 'main_clause_ty/1' */
  struct {
    CELL isground; // Does clause whose head is this 'jit_handler' have calls ('fcall' or 'call' opcode)? If 'yes', isground is '0'. Used when 'main_clause_ty(hot_and_callee)'
    CELL clausesize;  // Is clause whose head is this 'jit_handler' greater than other clauses? Used when ''main_clause_ty(hot_and_greater)'
    CELL backtrack_counter; // Does clause whose head is this 'jit_handler' have fewer backtracks on history than other clauses? Used when ''main_clause_ty(hot_and_fewer)'
  } mf;

  /* Frequency Instrumenters -- only one is used */
  struct {
    union {
      COUNT c; // counter
      CELL t;  // time
    } bcst; // balance, counter, crossover, time
  } fi;

  /* Reverse pointers to code areas ('Intermediatecode Area' and 'Native Area') */
  struct {
    COUNT taddress; // intermediatecode area
    COUNT naddress; // native area
  } caa;

  /* Fields for aiding trace construction and compilation */
  struct {
    char *cmd; // Argument to program 'echo' (called by fork on JIT_Compiler.cpp). Its value is C code that represent traces that will be compiled. Its value is freed after compilation.
    ControlFlagsContext* cf; // Pointer to ControlFlagsContext. Just used here.
  } tcc;

  /* Fields for managing JIT -- recompilation and threads */
  struct {
    CELL used_thread;
    CELL torecomp;
  }jitman;
} JitHandlContext;

extern void **Yap_ABSMI_ControlLabels;

#endif /* YAP_JIT */



#endif /* _AMIJIT_H_ */
#if YAP_JIT
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

typedef void *(*call_jitc_t)(struct JIT_Compiler*, yamop *);

void *call_JIT_Compiler(struct JIT_Compiler*, yamop *);



static void
initJit(void)
{
  extern void shutdown_llvm(void);
  extern call_jitc_t Yap_JITCall;
  
  Yap_InitJitAnalysisPreds();
  Yap_InitJitTransformPreds();
  Yap_InitJitCodegenPreds();
  Yap_InitJitConfigPreds();
#if YAP_STAT_PREDS
  Yap_InitJitStatisticPreds();
#endif
#if YAP_DBG_PREDS
  Yap_InitJitDebugPreds();
#endif
  GLOBAL_JIT_finalizer = shutdown_llvm;
  Yap_JITCall = call_JIT_Compiler;  
  Yap_llvmShutdown = llvm_shutdown;
  Yap_ExpEnvP = &Yap_ExpEnv;;  

}

// export JIT as DLL
void
init_jit(void) {
  initJit();
}  

Environment Yap_ExpEnv;

#pragma GCC diagnostic pop
#endif /* YAP_JIT */

