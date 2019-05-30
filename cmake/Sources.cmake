#
# Sources Section
#


set (ABSMI_SOURCES
  C/absmi.c
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
        C/dbload.c
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
  C/terms.c
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

set( BEAM_HEADERS
BEAM/beam.h
)
  set (CORE_HEADERS
      ${CMAKE_SOURCE_DIR}/H/Atoms.h
    ${CMAKE_SOURCE_DIR}/H/Foreign.h
    ${CMAKE_SOURCE_DIR}/H/Regs.h
    ${CMAKE_SOURCE_DIR}/H/ScannerTypes.h
    ${CMAKE_SOURCE_DIR}/H/Tags_24bits.h
    ${CMAKE_SOURCE_DIR}/H/Tags_32LowTag.h
    ${CMAKE_SOURCE_DIR}/H/Tags_32Ops.h
    ${CMAKE_SOURCE_DIR}/H/Tags_32bits.h
    ${CMAKE_SOURCE_DIR}/H/Tags_64bits.h
    ${CMAKE_SOURCE_DIR}/H/TermExt.h
    ${CMAKE_SOURCE_DIR}/H/Yap.h
    ${CMAKE_SOURCE_DIR}/H/YapAppliedOpcodes.h
    ${CMAKE_SOURCE_DIR}/H/YapCompile.h
    ${CMAKE_SOURCE_DIR}/H/YapCompoundTerm.h
    ${CMAKE_SOURCE_DIR}/H/YapEval.h
    ${CMAKE_SOURCE_DIR}/H/YapFlags.h
    ${CMAKE_SOURCE_DIR}/H/YapGFlagInfo.h
    ${CMAKE_SOURCE_DIR}/H/YapHandles.h
    ${CMAKE_SOURCE_DIR}/H/YapHeap.h
    ${CMAKE_SOURCE_DIR}/H/YapLFlagInfo.h
    ${CMAKE_SOURCE_DIR}/H/YapOpcodes.h
    ${CMAKE_SOURCE_DIR}/H/YapSignals.h
    ${CMAKE_SOURCE_DIR}/H/YapTags.h
    ${CMAKE_SOURCE_DIR}/H/YapText.h
    ${CMAKE_SOURCE_DIR}/H/Yapproto.h
    ${CMAKE_SOURCE_DIR}/H/Yatom.h
    ${CMAKE_SOURCE_DIR}/H/absmi-interpretrer.h
    ${CMAKE_SOURCE_DIR}/H/absmi-switch.h
    ${CMAKE_SOURCE_DIR}/H/absmi-threaded.h
    ${CMAKE_SOURCE_DIR}/H/absmi-traced.h
    ${CMAKE_SOURCE_DIR}/H/absmi.h
    ${CMAKE_SOURCE_DIR}/H/alloc.h
    ${CMAKE_SOURCE_DIR}/H/amidefs.h
    ${CMAKE_SOURCE_DIR}/H/amijit.h
    ${CMAKE_SOURCE_DIR}/H/amiops.h
    ${CMAKE_SOURCE_DIR}/H/arith2.h
    ${CMAKE_SOURCE_DIR}/H/arrays.h
    ${CMAKE_SOURCE_DIR}/H/attvar.h
    ${CMAKE_SOURCE_DIR}/H/clause.h
    ${CMAKE_SOURCE_DIR}/H/corout.h
    ${CMAKE_SOURCE_DIR}/H/cut_c.h
    ${CMAKE_SOURCE_DIR}/H/dlmalloc.h
    ${CMAKE_SOURCE_DIR}/H/fields.h
    ${CMAKE_SOURCE_DIR}/H/findclause.h
    ${CMAKE_SOURCE_DIR}/H/generated/dglobals.h
	${CMAKE_SOURCE_DIR}/H/generated/dhstruct.h
	${CMAKE_SOURCE_DIR}/H/generated/h0globals.h
	${CMAKE_SOURCE_DIR}/H/generated/h0struct.h
	${CMAKE_SOURCE_DIR}/H/generated/hglobals.h
	${CMAKE_SOURCE_DIR}/H/generated/hlocals.h
	${CMAKE_SOURCE_DIR}/H/generated/hstruct.h
	${CMAKE_SOURCE_DIR}/H/generated/i0globals.h
	${CMAKE_SOURCE_DIR}/H/generated/iatoms.h
	${CMAKE_SOURCE_DIR}/H/generated/iglobals.h
	${CMAKE_SOURCE_DIR}/H/generated/ihstruct.h
	${CMAKE_SOURCE_DIR}/H/generated/ilocals.h
	${CMAKE_SOURCE_DIR}/H/generated/ratoms.h
	${CMAKE_SOURCE_DIR}/H/generated/rglobals.h
	${CMAKE_SOURCE_DIR}/H/generated/rhstruct.h
	${CMAKE_SOURCE_DIR}/H/generated/rlocals.h
	${CMAKE_SOURCE_DIR}/H/generated/tatoms.h
    ${CMAKE_SOURCE_DIR}/H/globals.h
    ${CMAKE_SOURCE_DIR}/H/headclause.h
    ${CMAKE_SOURCE_DIR}/H/heap.h
    ${CMAKE_SOURCE_DIR}/H/heapgc.h
    ${CMAKE_SOURCE_DIR}/H/index.h
    ${CMAKE_SOURCE_DIR}/H/inline-only.h
    ${CMAKE_SOURCE_DIR}/H/iswiatoms.h
    ${CMAKE_SOURCE_DIR}/H/locals.h
    ${CMAKE_SOURCE_DIR}/H/nolocks.h
    ${CMAKE_SOURCE_DIR}/H/qly.h
    ${CMAKE_SOURCE_DIR}/H/rclause.h
    ${CMAKE_SOURCE_DIR}/H/rheap.h
    ${CMAKE_SOURCE_DIR}/H/saveclause.h
    ${CMAKE_SOURCE_DIR}/H/sig.h
    ${CMAKE_SOURCE_DIR}/H/sshift.h
    ${CMAKE_SOURCE_DIR}/H/threads.h
    ${CMAKE_SOURCE_DIR}/H/tracer.h
    ${CMAKE_SOURCE_DIR}/H/trim_trail.h
    ${CMAKE_SOURCE_DIR}/H/udi_private.h
    ${CMAKE_SOURCE_DIR}/H/utarray.h
    ${CMAKE_SOURCE_DIR}/H/uthash.h
    ${CMAKE_SOURCE_DIR}/H/walkclause.h
    ${CMAKE_SOURCE_DIR}/H/yerror.h
	)

set (INCLUDE_HEADERS
    ${CMAKE_SOURCE_DIR}/include/GitSHA1.h
    ${CMAKE_SOURCE_DIR}/include/SWI-Prolog.h
    ${CMAKE_SOURCE_DIR}/include/VFS.h
		${CMAKE_SOURCE_DIR}/include/YapBlobs.h
		${CMAKE_SOURCE_DIR}/include/YapDefs.h
		${CMAKE_SOURCE_DIR}/include/YapEncoding.h
    ${CMAKE_SOURCE_DIR}/include/YapError.h
    ${CMAKE_SOURCE_DIR}/include/YapErrors.h
    ${CMAKE_SOURCE_DIR}/include/YapFormat.h
		${CMAKE_SOURCE_DIR}/include/YapInit.h
		${CMAKE_SOURCE_DIR}/include/YapInterface.h
    ${CMAKE_SOURCE_DIR}/include/YapRegs.h
    ${CMAKE_SOURCE_DIR}/include/YapStreams.h
    ${CMAKE_SOURCE_DIR}/include/YapUTF8.h
		${CMAKE_SOURCE_DIR}/include/YapTerm.h
    ${CMAKE_SOURCE_DIR}/include/c_interface.h
    ${CMAKE_SOURCE_DIR}/include/clause_list.h
    ${CMAKE_SOURCE_DIR}/include/pl-types.h
    ${CMAKE_SOURCE_DIR}/include/udi.h
)

set (CONFIGURATION_HEADERS
${CMAKE_BINARY_DIR}/YapConfig.h
    ${CMAKE_BINARY_DIR}/YapTermConfig.h
    ${CMAKE_BINARY_DIR}/dlocals.h
    ${CMAKE_BINARY_DIR}/YapIOConfig.h
)


set (CXX_HEADERS
    ${CMAKE_SOURCE_DIR}/CXX/yapa.hh
    ${CMAKE_SOURCE_DIR}/CXX/yapdb.hh
    ${CMAKE_SOURCE_DIR}/CXX/yapi.hh
    ${CMAKE_SOURCE_DIR}/CXX/yapie.hh
    ${CMAKE_SOURCE_DIR}/CXX/yapq.hh
    ${CMAKE_SOURCE_DIR}/CXX/yapt.hh

)
set (PYTHON_HEADERS  ${CMAKE_SOURCE_DIR}/packages/python/py4yap.h)

set (OPTYap_HEADERS
	${CMAKE_SOURCE_DIR}/OPTYap/opt.config.h
	${CMAKE_SOURCE_DIR}/OPTYap/opt.proto.h
	${CMAKE_SOURCE_DIR}/OPTYap/opt.structs.h
	${CMAKE_SOURCE_DIR}/OPTYap/opt.macros.h
	${CMAKE_SOURCE_DIR}/OPTYap/or.macros.h
	${CMAKE_SOURCE_DIR}/OPTYap/or.sba_amiops.h
	${CMAKE_SOURCE_DIR}/OPTYap/or.sba_unify.h
	${CMAKE_SOURCE_DIR}/OPTYap/tab.structs.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_x86.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_sparc.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_mips.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_mips_funcs.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_alpha.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_alpha_funcs.h
	${CMAKE_SOURCE_DIR}/OPTYap/locks_pthread.h

  )

set (YAPOS_HEADERS
	${CMAKE_SOURCE_DIR}/os/format.h
	${CMAKE_SOURCE_DIR}/os/getw.h
	${CMAKE_SOURCE_DIR}/os/iopreds.h
	${CMAKE_SOURCE_DIR}/os/sysbits.h
	${CMAKE_SOURCE_DIR}/os/yapio.h
)

list( APPEND c_headers ${CMAKE_SOURCE_DIR}/utf8proc/utf8proc.h )
list( APPEND c_headers ${YAPOS_HEADERS} )
list( APPEND c_headers ${OPTYap_HEADERS} ) 
list( APPEND c_headers ${INCLUDE_HEADERS} )
list( APPEND c_headers ${CORE_HEADERS} )
list (APPEND c_headers ${PYTHON_HEADERS})
#list( APPEND c_headers ${CONFIGURATION_HEADERS} )
list( APPEND cxx_headers ${CXX_HEADERS} )


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
