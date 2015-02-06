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

}

// export JIT as DLL
void
init_jit(void) {
  initJit();
}  
#pragma GCC diagnostic pop
#endif /* YAP_JIT */

