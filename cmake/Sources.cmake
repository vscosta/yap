#
# Sources Section
#


set (ABSMI_SOURCES
  C/absmi.c
  C/absmi_insts.h
  C/fli_absmi_insts.h
  C/or_absmi_insts.h
  C/control_absmi_insts.h
  C/index_absmi_insts.h
  C/prim_absmi_insts.h
  C/cp_absmi_insts.h
  C/lu_absmi_insts.h
  C/unify_absmi_insts.h
  C/fail_absmi_insts.h
  C/meta_absmi_insts.h
  )

set (ENGINE_SOURCES
  ${ABSMI_SOURCES}
  C/agc.c
  C/adtdefs.c
  C/alloc.c
  C/amasm.c
  C/analyst.c
  C/args.c
  C/arrays.c
  C/arith0.c
  C/arith1.c
  C/arith2.c
  C/atomic.c
  C/attvar.c
  C/bignum.c
  C/bb.c
  C/blobs.c
  C/cdmgr.c
  C/cmppreds.c
  C/compiler.c
  C/computils.c
  C/corout.c
  C/cut_c.c
  C/dbase.c
  C/dlmalloc.c
  C/errors.c
  C/eval.c
  C/exec.c
  C/exo.c
  C/exo_udi.c
  C/flags.c
  C/globals.c
  C/gmp_support.c
  C/gprof.c
  C/grow.c
  C/heapgc.c
  C/index.c
  C/init.c
  C/inlines.c
  C/depth_bound.c
  C/mavar.c
  C/modules.c
  C/other.c
  C/parser.c
  C/qlyr.c
  C/qlyw.c
  C/range.c
  C/save.c
  C/scanner.c
  C/signals.c
  C/sort.c
  C/stdpreds.c
  C/stack.c
  C/text.c
  C/threads.c
  C/tracer.c
  C/unify.c
  C/userpreds.c
  C/utilpreds.c
  C/yap-args.c
  C/write.c
  C/udi.c
  #packages/udi/rtree.c
  #packages/udi/rtree_udi.c

  #  ${IOLIB_SOURCES}
  #  MPI_SOURCES
  )

set(C_INTERFACE_SOURCES
    ${PROJECT_BINARY_DIR}/GitSHA1.c
 C/load_foreign.c
  C/load_dl.c
  C/load_dld.c
  C/load_dyld.c
  C/load_none.c
  C/load_aout.c
  C/load_aix.c
  C/load_dll.c
  C/load_shl.c
  C/c_interface.c
  C/clause_list.c
  )

list(APPEND LIBYAP_SOURCES ${C_INTERFACE_SOURCES} ${ENGINE_SOURCES})

  set (HEADERS
  	H/Atoms.h
  	H/sshift.h
  	H/Yap.h
  	H/Yatom.h
  	H/YapHeap.h
  	H/Regs.h
  	H/Yapproto.h
  	H/absmi.h
  	H/absmi-switch.h
  	H/absmi-threaded.h
  	H/absmi-traced.h
  	H/alloc.h
  	H/amidefs.h
  	H/amiops.h
  	H/arrays.h
  	H/arith2.h
  	H/attvar.h
      H/blobs.h
  	H/clause.h
  	H/compile.h
  	H/corout.h
  	H/dlmalloc.h
  	H/generated/dglobals.h
  	H/generated/dlocals.h
  	H/generated/dhstruct.h
  	H/eval.h
  	H/heapgc.h
  	H/generated/hglobals.h
  	H/generated/hlocals.h
  	H/generated/hstruct.h
  	H/generated/iglobals.h
  	H/generated/ihstruct.h
  	H/generated/ilocals.h
  	H/index.h
  	H/inline-only.h
  	H/qly.h
  	H/rclause.h
  	H/generated/rglobals.h
  	H/generated/rlocals.h
  	H/rheap.h
  	H/generated/rhstruct.h
  	H/threads.h
  	H/tracer.h
  	H/trim_trail.h
  	H/YapSignals.h
    H/YapGFlagInfo.h
    H/YapFlags.h
    H/YapLFlagInfo.h
  	H/YapText.h
  	H/cut_c.h
  	H/generated/iatoms.h
    H/generated/ratoms.h
    H/generated/tatoms.h
  	CXX/yapdb.hh
  	CXX/yapi.hh
  	BEAM/eam.h BEAM/eamamasm.h
	)



set(STATIC_SOURCES
  #NOT INCLUDED FOR NOW
  )

set(CONSOLE_SOURCES
console/yap.c)

#MPI STUFF
# library/mpi/mpi.c library/mpi/mpe.c
# library/lammpi/yap_mpi.c library/lammpi/hash.c library/lammpi/prologterms2c.c
# )

#WIN STUFF
# SET(PLCONS_SOURCES
#   console/LGPL/pl-nt.c
#   console/LGPL/pl-ntcon.c
#   console/LGPL/pl-ntconsole.c
#   console/LGPL/pl-ntmain.c
# )
