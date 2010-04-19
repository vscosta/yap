# Microsoft Developer Studio Generated NMAKE File, Based on yapdll.dsp
!IF "$(CFG)" == ""
CFG=yapdll - Win32 Debug
!MESSAGE No configuration specified. Defaulting to yapdll - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "yapdll - Win32 Release" && "$(CFG)" != "yapdll - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "yapdll.mak" CFG="yapdll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "yapdll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "yapdll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "yapdll - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\yapdll.dll"


CLEAN :
	-@erase "$(INTDIR)\absmi.obj"
	-@erase "$(INTDIR)\adtdefs.obj"
	-@erase "$(INTDIR)\alloc.obj"
	-@erase "$(INTDIR)\amasm.obj"
	-@erase "$(INTDIR)\analyst.obj"
	-@erase "$(INTDIR)\arith0.obj"
	-@erase "$(INTDIR)\arith1.obj"
	-@erase "$(INTDIR)\arith2.obj"
	-@erase "$(INTDIR)\arrays.obj"
	-@erase "$(INTDIR)\attvar.obj"
	-@erase "$(INTDIR)\bb.obj"
	-@erase "$(INTDIR)\bignum.obj"
	-@erase "$(INTDIR)\c_interface.obj"
	-@erase "$(INTDIR)\cdmgr.obj"
	-@erase "$(INTDIR)\cmppreds.obj"
	-@erase "$(INTDIR)\compiler.obj"
	-@erase "$(INTDIR)\computils.obj"
	-@erase "$(INTDIR)\corout.obj"
	-@erase "$(INTDIR)\dbase.obj"
	-@erase "$(INTDIR)\depth_bound.obj"
	-@erase "$(INTDIR)\errors.obj"
	-@erase "$(INTDIR)\eval.obj"
	-@erase "$(INTDIR)\exec.obj"
	-@erase "$(INTDIR)\grow.obj"
	-@erase "$(INTDIR)\heapgc.obj"
	-@erase "$(INTDIR)\index.obj"
	-@erase "$(INTDIR)\init.obj"
	-@erase "$(INTDIR)\iopreds.obj"
	-@erase "$(INTDIR)\load_aix.obj"
	-@erase "$(INTDIR)\load_aout.obj"
	-@erase "$(INTDIR)\load_coff.obj"
	-@erase "$(INTDIR)\load_dl.obj"
	-@erase "$(INTDIR)\load_dld.obj"
	-@erase "$(INTDIR)\load_dll.obj"
	-@erase "$(INTDIR)\load_foreign.obj"
	-@erase "$(INTDIR)\load_none.obj"
	-@erase "$(INTDIR)\load_shl.obj"
	-@erase "$(INTDIR)\mavar.obj"
	-@erase "$(INTDIR)\modules.obj"
	-@erase "$(INTDIR)\opt.init.obj"
	-@erase "$(INTDIR)\opt.memory.obj"
	-@erase "$(INTDIR)\opt.preds.obj"
	-@erase "$(INTDIR)\or.cowengine.obj"
	-@erase "$(INTDIR)\or.cut.obj"
	-@erase "$(INTDIR)\or.engine.obj"
	-@erase "$(INTDIR)\or.sbaengine.obj"
	-@erase "$(INTDIR)\or.scheduler.obj"
	-@erase "$(INTDIR)\other.obj"
	-@erase "$(INTDIR)\parser.obj"
	-@erase "$(INTDIR)\save.obj"
	-@erase "$(INTDIR)\scanner.obj"
	-@erase "$(INTDIR)\sort.obj"
	-@erase "$(INTDIR)\stdpreds.obj"
	-@erase "$(INTDIR)\sysbits.obj"
	-@erase "$(INTDIR)\tab.completion.obj"
	-@erase "$(INTDIR)\tab.tries.obj"
	-@erase "$(INTDIR)\tracer.obj"
	-@erase "$(INTDIR)\unify.obj"
	-@erase "$(INTDIR)\userpreds.obj"
	-@erase "$(INTDIR)\utilpreds.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\write.obj"
	-@erase "$(INTDIR)\ypsocks.obj"
	-@erase "$(INTDIR)\ypstdio.obj"
	-@erase "$(OUTDIR)\yapdll.dll"
	-@erase "$(OUTDIR)\yapdll.exp"
	-@erase "$(OUTDIR)\yapdll.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "..\include" /I "..\H" /I "..\OPTYap" /I "..\VC\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "YAPDLL_EXPORTS" /Fp"$(INTDIR)\yapdll.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\yapdll.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\yapdll.pdb" /machine:I386 /out:"$(OUTDIR)\yapdll.dll" /implib:"$(OUTDIR)\yapdll.lib" 
LINK32_OBJS= \
	"$(INTDIR)\absmi.obj" \
	"$(INTDIR)\adtdefs.obj" \
	"$(INTDIR)\alloc.obj" \
	"$(INTDIR)\amasm.obj" \
	"$(INTDIR)\analyst.obj" \
	"$(INTDIR)\arith0.obj" \
	"$(INTDIR)\arith1.obj" \
	"$(INTDIR)\arith2.obj" \
	"$(INTDIR)\arrays.obj" \
	"$(INTDIR)\attvar.obj" \
	"$(INTDIR)\bb.obj" \
	"$(INTDIR)\bignum.obj" \
	"$(INTDIR)\c_interface.obj" \
	"$(INTDIR)\cdmgr.obj" \
	"$(INTDIR)\cmppreds.obj" \
	"$(INTDIR)\compiler.obj" \
	"$(INTDIR)\computils.obj" \
	"$(INTDIR)\corout.obj" \
	"$(INTDIR)\dbase.obj" \
	"$(INTDIR)\depth_bound.obj" \
	"$(INTDIR)\errors.obj" \
	"$(INTDIR)\eval.obj" \
	"$(INTDIR)\exec.obj" \
	"$(INTDIR)\grow.obj" \
	"$(INTDIR)\heapgc.obj" \
	"$(INTDIR)\index.obj" \
	"$(INTDIR)\init.obj" \
	"$(INTDIR)\iopreds.obj" \
	"$(INTDIR)\load_aix.obj" \
	"$(INTDIR)\load_aout.obj" \
	"$(INTDIR)\load_coff.obj" \
	"$(INTDIR)\load_dl.obj" \
	"$(INTDIR)\load_dld.obj" \
	"$(INTDIR)\load_dll.obj" \
	"$(INTDIR)\load_foreign.obj" \
	"$(INTDIR)\load_none.obj" \
	"$(INTDIR)\load_shl.obj" \
	"$(INTDIR)\mavar.obj" \
	"$(INTDIR)\modules.obj" \
	"$(INTDIR)\opt.init.obj" \
	"$(INTDIR)\opt.memory.obj" \
	"$(INTDIR)\opt.preds.obj" \
	"$(INTDIR)\or.cowengine.obj" \
	"$(INTDIR)\or.cut.obj" \
	"$(INTDIR)\or.engine.obj" \
	"$(INTDIR)\or.sbaengine.obj" \
	"$(INTDIR)\or.scheduler.obj" \
	"$(INTDIR)\other.obj" \
	"$(INTDIR)\parser.obj" \
	"$(INTDIR)\save.obj" \
	"$(INTDIR)\scanner.obj" \
	"$(INTDIR)\sort.obj" \
	"$(INTDIR)\stdpreds.obj" \
	"$(INTDIR)\sysbits.obj" \
	"$(INTDIR)\tab.completion.obj" \
	"$(INTDIR)\tab.tries.obj" \
	"$(INTDIR)\tracer.obj" \
	"$(INTDIR)\unify.obj" \
	"$(INTDIR)\userpreds.obj" \
	"$(INTDIR)\utilpreds.obj" \
	"$(INTDIR)\write.obj" \
	"$(INTDIR)\ypsocks.obj" \
	"$(INTDIR)\ypstdio.obj"

"$(OUTDIR)\yapdll.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "yapdll - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\yapdll.dll"


CLEAN :
	-@erase "$(INTDIR)\absmi.obj"
	-@erase "$(INTDIR)\adtdefs.obj"
	-@erase "$(INTDIR)\alloc.obj"
	-@erase "$(INTDIR)\amasm.obj"
	-@erase "$(INTDIR)\analyst.obj"
	-@erase "$(INTDIR)\arith0.obj"
	-@erase "$(INTDIR)\arith1.obj"
	-@erase "$(INTDIR)\arith2.obj"
	-@erase "$(INTDIR)\arrays.obj"
	-@erase "$(INTDIR)\attvar.obj"
	-@erase "$(INTDIR)\bb.obj"
	-@erase "$(INTDIR)\bignum.obj"
	-@erase "$(INTDIR)\c_interface.obj"
	-@erase "$(INTDIR)\cdmgr.obj"
	-@erase "$(INTDIR)\cmppreds.obj"
	-@erase "$(INTDIR)\compiler.obj"
	-@erase "$(INTDIR)\computils.obj"
	-@erase "$(INTDIR)\corout.obj"
	-@erase "$(INTDIR)\dbase.obj"
	-@erase "$(INTDIR)\depth_bound.obj"
	-@erase "$(INTDIR)\errors.obj"
	-@erase "$(INTDIR)\eval.obj"
	-@erase "$(INTDIR)\exec.obj"
	-@erase "$(INTDIR)\grow.obj"
	-@erase "$(INTDIR)\heapgc.obj"
	-@erase "$(INTDIR)\index.obj"
	-@erase "$(INTDIR)\init.obj"
	-@erase "$(INTDIR)\iopreds.obj"
	-@erase "$(INTDIR)\load_aix.obj"
	-@erase "$(INTDIR)\load_aout.obj"
	-@erase "$(INTDIR)\load_coff.obj"
	-@erase "$(INTDIR)\load_dl.obj"
	-@erase "$(INTDIR)\load_dld.obj"
	-@erase "$(INTDIR)\load_dll.obj"
	-@erase "$(INTDIR)\load_foreign.obj"
	-@erase "$(INTDIR)\load_none.obj"
	-@erase "$(INTDIR)\load_shl.obj"
	-@erase "$(INTDIR)\mavar.obj"
	-@erase "$(INTDIR)\modules.obj"
	-@erase "$(INTDIR)\opt.init.obj"
	-@erase "$(INTDIR)\opt.memory.obj"
	-@erase "$(INTDIR)\opt.preds.obj"
	-@erase "$(INTDIR)\or.cowengine.obj"
	-@erase "$(INTDIR)\or.cut.obj"
	-@erase "$(INTDIR)\or.engine.obj"
	-@erase "$(INTDIR)\or.sbaengine.obj"
	-@erase "$(INTDIR)\or.scheduler.obj"
	-@erase "$(INTDIR)\other.obj"
	-@erase "$(INTDIR)\parser.obj"
	-@erase "$(INTDIR)\save.obj"
	-@erase "$(INTDIR)\scanner.obj"
	-@erase "$(INTDIR)\sort.obj"
	-@erase "$(INTDIR)\stdpreds.obj"
	-@erase "$(INTDIR)\sysbits.obj"
	-@erase "$(INTDIR)\tab.completion.obj"
	-@erase "$(INTDIR)\tab.tries.obj"
	-@erase "$(INTDIR)\tracer.obj"
	-@erase "$(INTDIR)\unify.obj"
	-@erase "$(INTDIR)\userpreds.obj"
	-@erase "$(INTDIR)\utilpreds.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\write.obj"
	-@erase "$(INTDIR)\ypsocks.obj"
	-@erase "$(INTDIR)\ypstdio.obj"
	-@erase "$(OUTDIR)\yapdll.dll"
	-@erase "$(OUTDIR)\yapdll.exp"
	-@erase "$(OUTDIR)\yapdll.ilk"
	-@erase "$(OUTDIR)\yapdll.lib"
	-@erase "$(OUTDIR)\yapdll.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "YAPDLL_EXPORTS" /U "..\include" /U "..\H" /U "..\OPTYap" /U "..\VC\include" /Fp"$(INTDIR)\yapdll.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\yapdll.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\yapdll.pdb" /debug /machine:I386 /out:"$(OUTDIR)\yapdll.dll" /implib:"$(OUTDIR)\yapdll.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\absmi.obj" \
	"$(INTDIR)\adtdefs.obj" \
	"$(INTDIR)\alloc.obj" \
	"$(INTDIR)\amasm.obj" \
	"$(INTDIR)\analyst.obj" \
	"$(INTDIR)\arith0.obj" \
	"$(INTDIR)\arith1.obj" \
	"$(INTDIR)\arith2.obj" \
	"$(INTDIR)\arrays.obj" \
	"$(INTDIR)\attvar.obj" \
	"$(INTDIR)\bb.obj" \
	"$(INTDIR)\bignum.obj" \
	"$(INTDIR)\c_interface.obj" \
	"$(INTDIR)\cdmgr.obj" \
	"$(INTDIR)\cmppreds.obj" \
	"$(INTDIR)\compiler.obj" \
	"$(INTDIR)\computils.obj" \
	"$(INTDIR)\corout.obj" \
	"$(INTDIR)\dbase.obj" \
	"$(INTDIR)\depth_bound.obj" \
	"$(INTDIR)\errors.obj" \
	"$(INTDIR)\eval.obj" \
	"$(INTDIR)\exec.obj" \
	"$(INTDIR)\grow.obj" \
	"$(INTDIR)\heapgc.obj" \
	"$(INTDIR)\index.obj" \
	"$(INTDIR)\init.obj" \
	"$(INTDIR)\iopreds.obj" \
	"$(INTDIR)\load_aix.obj" \
	"$(INTDIR)\load_aout.obj" \
	"$(INTDIR)\load_coff.obj" \
	"$(INTDIR)\load_dl.obj" \
	"$(INTDIR)\load_dld.obj" \
	"$(INTDIR)\load_dll.obj" \
	"$(INTDIR)\load_foreign.obj" \
	"$(INTDIR)\load_none.obj" \
	"$(INTDIR)\load_shl.obj" \
	"$(INTDIR)\mavar.obj" \
	"$(INTDIR)\modules.obj" \
	"$(INTDIR)\opt.init.obj" \
	"$(INTDIR)\opt.memory.obj" \
	"$(INTDIR)\opt.preds.obj" \
	"$(INTDIR)\or.cowengine.obj" \
	"$(INTDIR)\or.cut.obj" \
	"$(INTDIR)\or.engine.obj" \
	"$(INTDIR)\or.sbaengine.obj" \
	"$(INTDIR)\or.scheduler.obj" \
	"$(INTDIR)\other.obj" \
	"$(INTDIR)\parser.obj" \
	"$(INTDIR)\save.obj" \
	"$(INTDIR)\scanner.obj" \
	"$(INTDIR)\sort.obj" \
	"$(INTDIR)\stdpreds.obj" \
	"$(INTDIR)\sysbits.obj" \
	"$(INTDIR)\tab.completion.obj" \
	"$(INTDIR)\tab.tries.obj" \
	"$(INTDIR)\tracer.obj" \
	"$(INTDIR)\unify.obj" \
	"$(INTDIR)\userpreds.obj" \
	"$(INTDIR)\utilpreds.obj" \
	"$(INTDIR)\write.obj" \
	"$(INTDIR)\ypsocks.obj" \
	"$(INTDIR)\ypstdio.obj"

"$(OUTDIR)\yapdll.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("yapdll.dep")
!INCLUDE "yapdll.dep"
!ELSE 
!MESSAGE Warning: cannot find "yapdll.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "yapdll - Win32 Release" || "$(CFG)" == "yapdll - Win32 Debug"
SOURCE="\Yap\Yap-4.3.17\C\absmi.c"

"$(INTDIR)\absmi.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\adtdefs.c"

"$(INTDIR)\adtdefs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\alloc.c"

"$(INTDIR)\alloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\amasm.c"

"$(INTDIR)\amasm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\analyst.c"

"$(INTDIR)\analyst.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\arith0.c"

"$(INTDIR)\arith0.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\arith1.c"

"$(INTDIR)\arith1.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\arith2.c"

"$(INTDIR)\arith2.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\arrays.c"

"$(INTDIR)\arrays.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\attvar.c"

"$(INTDIR)\attvar.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\bb.c"

"$(INTDIR)\bb.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\bignum.c"

"$(INTDIR)\bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\c_interface.c"

"$(INTDIR)\c_interface.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\cdmgr.c"

"$(INTDIR)\cdmgr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\cmppreds.c"

"$(INTDIR)\cmppreds.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\compiler.c"

"$(INTDIR)\compiler.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\computils.c"

"$(INTDIR)\computils.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\corout.c"

"$(INTDIR)\corout.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\dbase.c"

"$(INTDIR)\dbase.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\depth_bound.c"

"$(INTDIR)\depth_bound.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\errors.c"

"$(INTDIR)\errors.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\eval.c"

"$(INTDIR)\eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\exec.c"

"$(INTDIR)\exec.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\grow.c"

"$(INTDIR)\grow.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\heapgc.c"

"$(INTDIR)\heapgc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\index.c"

"$(INTDIR)\index.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\init.c"

"$(INTDIR)\init.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\iopreds.c"

"$(INTDIR)\iopreds.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_aix.c"

"$(INTDIR)\load_aix.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_aout.c"

"$(INTDIR)\load_aout.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_coff.c"

"$(INTDIR)\load_coff.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_dl.c"

"$(INTDIR)\load_dl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_dld.c"

"$(INTDIR)\load_dld.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_dll.c"

"$(INTDIR)\load_dll.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_foreign.c"

"$(INTDIR)\load_foreign.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_none.c"

"$(INTDIR)\load_none.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\load_shl.c"

"$(INTDIR)\load_shl.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\mavar.c"

"$(INTDIR)\mavar.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\modules.c"

"$(INTDIR)\modules.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\opt.init.c"

"$(INTDIR)\opt.init.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\opt.memory.c"

"$(INTDIR)\opt.memory.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\opt.preds.c"

"$(INTDIR)\opt.preds.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\or.cowengine.c"

"$(INTDIR)\or.cowengine.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\or.cut.c"

"$(INTDIR)\or.cut.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\or.engine.c"

"$(INTDIR)\or.engine.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\or.sbaengine.c"

"$(INTDIR)\or.sbaengine.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\or.scheduler.c"

"$(INTDIR)\or.scheduler.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\other.c"

"$(INTDIR)\other.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\parser.c"

"$(INTDIR)\parser.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\save.c"

"$(INTDIR)\save.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\scanner.c"

"$(INTDIR)\scanner.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\sort.c"

"$(INTDIR)\sort.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\stdpreds.c"

"$(INTDIR)\stdpreds.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\sysbits.c"

"$(INTDIR)\sysbits.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\tab.completion.c"

"$(INTDIR)\tab.completion.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\OPTYap\tab.tries.c"

"$(INTDIR)\tab.tries.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\tracer.c"

"$(INTDIR)\tracer.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\unify.c"

"$(INTDIR)\unify.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\userpreds.c"

"$(INTDIR)\userpreds.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\utilpreds.c"

"$(INTDIR)\utilpreds.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\write.c"

"$(INTDIR)\write.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\ypsocks.c"

"$(INTDIR)\ypsocks.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE="\Yap\Yap-4.3.17\C\ypstdio.c"

"$(INTDIR)\ypstdio.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

