#ifndef JIT_COMPILER_HPP
#define JIT_COMPILER_HPP

#ifdef __cplusplus

#include "llvm/Linker/Linker.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/CallGraphSCCPass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineBranchProbabilityInfo.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineBranchProbabilityInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegionInfo.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_os_ostream.h"
//#include "llvm/Support/PathV1.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/IVUsers.h"
#include "llvm/Analysis/Lint.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/RegionPass.h"
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/Analysis/DomPrinter.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/RegionPrinter.h"
#include "llvm/Support/MemoryBuffer.h"
//#include "llvm/Support/SYSTEM_ERROR_INTERNAL.h"
#include "llvm/Support/Process.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/ObjCARC.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Pass.h"

using namespace llvm;

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

 extern "C" void* call_JIT_Compiler(JIT_Compiler* jc, yamop* p);

 extern "C" void shutdown_llvm();

#else

typedef struct jit_compiler JIT_Compiler;

EXTERN void* (*Yap_JitCall)(JIT_Compiler* jc, yamop* p);

INLINE_ONLY void* call_JIT_Compiler(JIT_Compiler* jc, yamop* p);

INLINE_ONLY void* call_JIT_Compiler(JIT_Compiler* jc, yamop* p) {
  return Yap_JitCall (jc,p); }

INLINE_ONLY void shutdown_llvm(void ) ;

EXTERN void (* Yap_llvmShutdown)(void ) ;

INLINE_ONLY void shutdown_llvm(void ) {  Yap_llvmShutdown (); }

#endif

#endif
