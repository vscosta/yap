
#include "JIT_Compiler.hpp"

using namespace std;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

extern "C" void Yap_InitJitAnalysisPreds(void);
extern "C" void Yap_InitJitTransformPreds(void);
extern "C" void Yap_InitJitCodegenPreds(void);
extern "C" void Yap_InitJitConfigPreds(void);
#if YAP_STAT_PREDS
extern "C" void Yap_InitJitStatisticPreds(void);
#endif
#if YAP_DBG_PREDS
extern "C" void Yap_InitJitDebugPreds(void);
#endif

// global variables with intercae
extern "C" void* (*Yap_JitCall)(JIT_Compiler* jc, yamop* p);
extern "C" void (* Yap_llvmShutdown)( ) ;
extern "C" Int  (* Yap_traced_absmi)( ) ;

void init_jit();

extern "C" void* call_JIT_Compiler(JIT_Compiler* jc, yamop* p) {
  return jc->compile(p); }

extern "C" void shutdown_llvm() { llvm_shutdown(); }

extern "C" Int traced_absmi();

#define JIT_CODE 1

static void
initJit(void)
{
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
  Yap_JitCall = call_JIT_Compiler;
  Yap_llvmShutdown = llvm_shutdown;
  Yap_traced_absmi = traced_absmi;
}

// export JIT as DLL
void
init_jit() {
  initJit();
}  
#pragma GCC diagnostic pop

