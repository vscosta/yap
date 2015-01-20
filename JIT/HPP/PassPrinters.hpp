/**
* The code below is adapted from opt tool according to clearance and conditions established in NCSA license as follows:

* Copyright (c) 2003 University of Illinois. All rights reserved.

* Developed by: LLVM Developer Group
* University of Illinois
* http://llvm.org/

*     Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
* files (the "Software"), to deal with the Software without restriction, including without limitation the rights to use, copy,
* modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software
* is furnished to do so, subject to the following conditions:

*     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers.
*     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimers
* in the documentation and/or other materials provided with the distribution.
*     Neither the names of LLVM Developer Group, University of Illinois, nor the names of its contributors may be used to endorse or
* promote products derived from this Software without specific prior written permission.

*     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
* OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS
* BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
* IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.

**/

struct CallGraphSCCPassPrinter : public CallGraphSCCPass {
  static char ID;
  const PassInfo *PassToPrint;
  std::string PassName;

  CallGraphSCCPassPrinter(const PassInfo *PInfo) :
    CallGraphSCCPass(ID), PassToPrint(PInfo) {
      std::string PassToPrintName =  PassToPrint->getPassName();
      PassName = "CallGraphSCCPass Printer: " + PassToPrintName;
    }

  virtual bool runOnSCC(CallGraphSCC &SCC) {
    errs() << "Printing analysis '" << PassToPrint->getPassName() << "':\n";

    // Get and print pass...
    for (CallGraphSCC::iterator I = SCC.begin(), E = SCC.end(); I != E; ++I) {
      Function *F = (*I)->getFunction();
      if (F)
        getAnalysisID<Pass>(PassToPrint->getTypeInfo()).print(errs(),
                                                              F->getParent());
    }
    return false;
  }

  virtual const char *getPassName() const { return PassName.c_str(); }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(PassToPrint->getTypeInfo());
    AU.setPreservesAll();
  }
};

char CallGraphSCCPassPrinter::ID = 0;

struct ModulePassPrinter : public ModulePass {
  static char ID;
  const PassInfo *PassToPrint;
  std::string PassName;

  ModulePassPrinter(const PassInfo *PInfo)
    : ModulePass(ID), PassToPrint(PInfo) {
      std::string PassToPrintName =  PassToPrint->getPassName();
      PassName = "ModulePass Printer: " + PassToPrintName;
    }

  virtual bool runOnModule(Module &M) {
    errs() << "Printing analysis '" << PassToPrint->getPassName() << "':\n";

    // Get and print pass...
    getAnalysisID<Pass>(PassToPrint->getTypeInfo()).print(errs(), &M);
    return false;
  }

  virtual const char *getPassName() const { return PassName.c_str(); }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(PassToPrint->getTypeInfo());
    AU.setPreservesAll();
  }
};

char ModulePassPrinter::ID = 0;

struct FunctionPassPrinter : public FunctionPass {
  const PassInfo *PassToPrint;
  static char ID;
  std::string PassName;

  FunctionPassPrinter(const PassInfo *PInfo)
    : FunctionPass(ID), PassToPrint(PInfo) {
      std::string PassToPrintName =  PassToPrint->getPassName();
      PassName = "FunctionPass Printer: " + PassToPrintName;
    }

  virtual bool runOnFunction(Function &F) {
    errs() << "Printing analysis '" << PassToPrint->getPassName()
          << "' for function '" << F.getName() << "':\n";

    // Get and print pass...
    getAnalysisID<Pass>(PassToPrint->getTypeInfo()).print(errs(),
            F.getParent());
    return false;
  }

  virtual const char *getPassName() const { return PassName.c_str(); }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(PassToPrint->getTypeInfo());
    AU.setPreservesAll();
  }
};

char FunctionPassPrinter::ID = 0;

struct LoopPassPrinter : public LoopPass {
  static char ID;
  const PassInfo *PassToPrint;
  std::string PassName;

  LoopPassPrinter(const PassInfo *PInfo) :
    LoopPass(ID), PassToPrint(PInfo) {
      std::string PassToPrintName =  PassToPrint->getPassName();
      PassName = "LoopPass Printer: " + PassToPrintName;
    }


  virtual bool runOnLoop(Loop *L, LPPassManager &LPM) {
    errs() << "Printing analysis '" << PassToPrint->getPassName() << "':\n";

    // Get and print pass...
    getAnalysisID<Pass>(PassToPrint->getTypeInfo()).print(errs(),
                        L->getHeader()->getParent()->getParent());
    return false;
  }

  virtual const char *getPassName() const { return PassName.c_str(); }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(PassToPrint->getTypeInfo());
    AU.setPreservesAll();
  }
};

char LoopPassPrinter::ID = 0;

struct RegionPassPrinter : public RegionPass {
  static char ID;
  const PassInfo *PassToPrint;
  std::string PassName;

  RegionPassPrinter(const PassInfo *PInfo) : RegionPass(ID),
    PassToPrint(PInfo) {
    std::string PassToPrintName =  PassToPrint->getPassName();
    PassName = "RegionPass Printer: " + PassToPrintName;
  }

  virtual bool runOnRegion(Region *R, RGPassManager &RGM) {
    errs() << "Printing analysis '" << PassToPrint->getPassName() << "' for "
        << "region: '" << R->getNameStr() << "' in function '"
        << R->getEntry()->getParent()->getName() << "':\n";

    // Get and print pass...
   getAnalysisID<Pass>(PassToPrint->getTypeInfo()).print(errs(),
                       R->getEntry()->getParent()->getParent());
    return false;
  }

  virtual const char *getPassName() const { return PassName.c_str(); }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(PassToPrint->getTypeInfo());
    AU.setPreservesAll();
  }
};

char RegionPassPrinter::ID = 0;
