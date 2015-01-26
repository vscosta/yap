#if YAP_STAT_PREDS
#include <papi.h>
#endif

#ifndef X_API
#if (defined(_MSC_VER) || defined(__MINGW32__)) && defined(PL_KERNEL)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif
#endif

extern Environment ExpEnv;

extern Environment ExpEnv;

#if YAP_JIT
Int           Get_N_Cores(void);
X_API Int     Init_Analysis_Struc(void);
X_API Int     Init_Transform_Struc(void);
X_API Int     Init_Codegen_Struc(void);
X_API Int     Init_Config_Struc(void);
#if YAP_STAT_PREDS
X_API Int     Init_Stats_Struc(void);
#endif
#endif /* YAP_JIT */
#if YAP_DBG_PREDS
X_API Int     Init_Debug_Struc(void);
#endif
X_API Int     YAP_Init_ExpEnv(void);

#if YAP_JIT
IntGet_N_Cores() {
#ifdef WIN32
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
#elif MACOS
    int nm[2];
    size_t len = 4;
    uint32_t count;

    nm[0] = CTL_HW; nm[1] = HW_AVAILCPU;
    sysctl(nm, 2, &count, &len, NULL, 0);

    if(count < 1) {
        nm[1] = HW_NCPU;
        sysctl(nm, 2, &count, &len, NULL, 0);
        if(count < 1) { count = 1; }
    }
    return count;
#else
    return sysconf(_SC_NPROCESSORS_ONLN);
#endif
}

X_API Int
Init_Analysis_Struc()
{
  ExpEnv.analysis_struc.stats_enabled = 0;
  ExpEnv.analysis_struc.time_pass_enabled = 0;
  ExpEnv.analysis_struc.pointtoverifymodule = NOPOINT;
  ExpEnv.analysis_struc.n = 0;
  ExpEnv.analysis_struc.act_an = NULL;
  ExpEnv.analysis_struc.outfile = (CELL)malloc(7*sizeof(char));
  strcpy(((char*)ExpEnv.analysis_struc.outfile), "STDERR");
  return TRUE;
}

X_API Int
Init_Transform_Struc()
{
  ExpEnv.transform_struc.optlevel = 3;
  ExpEnv.transform_struc.n = 0;
  ExpEnv.transform_struc.act_tr = NULL;
  ExpEnv.transform_struc.opt_args.arg_promotion_max_elements = 3;
  ExpEnv.transform_struc.opt_args.strip_symbols_pass_type = 0;
  ExpEnv.transform_struc.opt_args.scalar_replace_aggregates_threshold = -1;
  ExpEnv.transform_struc.opt_args.loop_unswitch_optimize_for_size = 0;
  ExpEnv.transform_struc.opt_args.loop_unroll_threshold = -1;
  ExpEnv.transform_struc.opt_args.inline_threshold = 225;
  ExpEnv.transform_struc.unit_at_time_enabled = 1;
  ExpEnv.transform_struc.simplify_libcalls_enabled = 1;
  ExpEnv.transform_struc.link_time_opt.enabled = 0;
  ExpEnv.transform_struc.link_time_opt.internalize = 0;
  ExpEnv.transform_struc.link_time_opt.runinliner = 0;
  return TRUE;
}

X_API Int
Init_Codegen_Struc()
{
  ExpEnv.codegen_struc.struc_targetopt.noframepointerelim = 0;
  ExpEnv.codegen_struc.struc_targetopt.lessprecisefpmadoption = 0;
  ExpEnv.codegen_struc.struc_targetopt.noexcessfpprecision = 0;
  ExpEnv.codegen_struc.struc_targetopt.unsafefpmath = 0;
  ExpEnv.codegen_struc.struc_targetopt.honorsigndependentroundingfpmathoption = 0;
  ExpEnv.codegen_struc.struc_targetopt.usesoftfloat = 0;
  ExpEnv.codegen_struc.struc_targetopt.jitexceptionhandling = 0;
  ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfo = 0;
  ExpEnv.codegen_struc.struc_targetopt.jitemitdebuginfotodisk = 0;
  ExpEnv.codegen_struc.struc_targetopt.guaranteedtailcallopt = 0;
  ExpEnv.codegen_struc.struc_targetopt.disabletailcalls = 0;
  ExpEnv.codegen_struc.struc_targetopt.fastisel = 0;
  ExpEnv.codegen_struc.struc_targetopt.floatabitype = 0;

  ExpEnv.codegen_struc.struc_enginebuilder.engineoptlevel = 3;
  ExpEnv.codegen_struc.struc_enginebuilder.relocmodel = 0;
  ExpEnv.codegen_struc.struc_enginebuilder.codemodel = 1;
  ExpEnv.codegen_struc.struc_enginebuilder.usemcjit = 0;
  ExpEnv.codegen_struc.struc_enginebuilder.regallocator = REG_ALLOC_GREEDY;
  return TRUE;
}

X_API Int
Init_Config_Struc()
{
  if (Yap_ExecutionMode == MIXED_MODE) {
    ExpEnv.config_struc.execution_mode = SMART_JIT;
    ExpEnv.config_struc.frequency_type = COUNTER;
    ExpEnv.config_struc.frequency_bound = 1024.0;
    ExpEnv.config_struc.profiling_startp = 0.72;
    ExpEnv.config_struc.mainclause_ty = HROT_AND_CALLEE;
    ExpEnv.config_struc.torecompile = 1;
  }
  else {
    if (Yap_ExecutionMode == COMPILED)
      ExpEnv.config_struc.execution_mode = JUST_COMPILED;
    else
      ExpEnv.config_struc.execution_mode = JUST_INTERPRETED;
    ExpEnv.config_struc.frequency_type = NO_FREQ;
    ExpEnv.config_struc.frequency_bound = 0.0;
    ExpEnv.config_struc.profiling_startp = 0.0;
    ExpEnv.config_struc.mainclause_ty = UNUSED;
    ExpEnv.config_struc.torecompile = 0;
  }
  ExpEnv.config_struc.ncores = Get_N_Cores();
  ExpEnv.config_struc.useonlypi = 0;
  ExpEnv.config_struc.compilation_threads = 0;
  ExpEnv.config_struc.threaded_compiler_threads = NULL;
  ExpEnv.config_struc.posthreads = NULL;
  return TRUE;
}

#if YAP_STAT_PREDS
X_API Int
Init_Stats_Struc()
{
  ExpEnv.stats_struc.papi_initialized = 0;
  ExpEnv.stats_struc.papi_eventset = PAPI_NULL;
  return TRUE;
}
#endif

#endif /* YAP_JIT */

#if YAP_DBG_PREDS
X_API Int
Init_Debug_Struc()
{
  #define OPCODE(OP,TYPE) \
  ExpEnv.debug_struc.pyaam_##OP.print = (Int)NO_PLACE; \
  ExpEnv.debug_struc.pyaam_##OP.msg_before = 0; \
  ExpEnv.debug_struc.pyaam_##OP.msg_after = 0;
  #include "YapAppliedOpcodes.h"
  #undef OPCODE

  #define BBLOCK(BB) \
  ExpEnv.debug_struc.pbbs_##BB.print = (Int)NO_PLACE; \
  ExpEnv.debug_struc.pbbs_##BB.msg_before = 0; \
  ExpEnv.debug_struc.pbbs_##BB.msg_after = 0;
  #include "Yap_AppliedBasicBlocks.h"
  #undef BBLOCK

  ExpEnv.debug_struc.pmainclause_on_head.print = (Int)NO_PLACE;
  ExpEnv.debug_struc.pmainclause_on_head.msg_before = 0;
  ExpEnv.debug_struc.pmainclause_on_head.msg_after = 0;

  ExpEnv.debug_struc.pprint_intermediate.print_to_std = 0;
  ExpEnv.debug_struc.pprint_intermediate.print_to_file = 0;
  ExpEnv.debug_struc.pprint_intermediate.std_name = 0;
  ExpEnv.debug_struc.pprint_intermediate.file_name = 0;

  ExpEnv.debug_struc.pprint_llva.print_llva_before = 0;
  ExpEnv.debug_struc.pprint_llva.print_llva_after = 0;

  ExpEnv.debug_struc.pprint_me.interpreted_backtrack = 0;
  ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack = 0;
  ExpEnv.debug_struc.pprint_me.native_backtrack = 0;
  ExpEnv.debug_struc.pprint_me.interpreted_treat_heap = 0;
  ExpEnv.debug_struc.pprint_me.native_treat_heap = 0;
  ExpEnv.debug_struc.pprint_me.interpreted_treat_trail = 0;
  ExpEnv.debug_struc.pprint_me.native_treat_trail = 0;
  ExpEnv.debug_struc.pprint_me.criticals = 0;
  ExpEnv.debug_struc.pprint_me.at_compilation = 0;
  ExpEnv.debug_struc.pprint_me.at_recompilation = 0;
  ExpEnv.debug_struc.pprint_me.nativerun_init = 0;
  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success = 0;
  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail = 0;

  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;

  ExpEnv.debug_struc.act_predicate_actions.exit_on_warning = 0;
  ExpEnv.debug_struc.act_predicate_actions.disable_on_warning = 1;
  ExpEnv.debug_struc.act_predicate_actions.exit_on_error = 1;

  return TRUE;
}
#endif

X_API Int
YAP_Init_ExpEnv()
{
  //ExpEnv.in = (char*)malloc(1024*sizeof(char));
  //strcpy(ExpEnv.in, fin);
#if YAP_JIT
  Init_Analysis_Struc();
  Init_Transform_Struc();
  Init_Codegen_Struc();
  Init_Config_Struc();
#if YAP_STAT_PREDS
  Init_Stats_Struc();
#endif
#endif /* YAP_JIT */
#if YAP_DBG_PREDS
  Init_Debug_Struc();
#endif
  return TRUE;
}
