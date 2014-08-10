#ifndef JIT_COMPILER_HPP
#define JIT_COMPILER_HPP

#ifdef __cplusplus

#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/Linker.h"
#include "llvm/PassManager.h"
#include "llvm/CallGraphSCCPass.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineCodeInfo.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/JITMemoryManager.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/Support/DataStream.h"
#include "llvm/Support/IRReader.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/PathV1.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/IVUsers.h"
#include "llvm/Analysis/Lint.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/RegionPass.h"
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/Analysis/DomPrinter.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/RegionPrinter.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/Process.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/Cloning.h"

using namespace llvm;

#include <fcntl.h>
#include <errno.h>
#include "JIT.hpp"

using namespace std;

extern short global;
extern Environment ExpEnv;
extern NativeContext *NativeArea;

class JIT_Compiler {
  private:
    /* main method of JIT Compiler: compiles by clang, analyzes, optimizs and generates code accordingly the user choices */
    void*       compile_all(LLVMContext* &Context, yamop* p);

    /* aid method to 'compile_all': adds register allocator pass to be used.
       WARNING: don't use! For some reasons llvm crashes when I use it */
    void        set_regalloc_pass(PassManager &PM);

    /* aid method to 'compile_all': optimizes module by individual transform passes or transform level */
    void        optimize_module(llvm::Module* &M);

    /* aid method to 'compile_all': analyzes module by individual analysis passes */
    void        analyze_module(llvm::Module* &M);
  public:
    /* method invoked by wrapper 'call_JIT_Compiler' */
    void*       compile(yamop*);
};
#else

struct JIT_Compiler{}; // Doing this, I can call class 'JIT_Compiler' from C code

#endif

#ifdef __cplusplus

extern "C" void* call_JIT_Compiler(JIT_Compiler* jc, yamop* p) { return jc->compile(p); }
extern "C" void shutdown_llvm() { llvm_shutdown(); }

#endif //#ifdef __cplusplus

#endif
