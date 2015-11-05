
set (OPTYap_SOURCES
	opt.config.h
	opt.proto.h
	opt.structs.h
	opt.macros.h
	or.macros.h
	or.sba_amiops.h
	or.sba_unify.h
	tab.structs.h
	locks_x86.h
	locks_sparc.h
	locks_mips.h
	locks_mips_funcs.h
	locks_alpha.h
	locks_alpha_funcs.h
	locks_pthread.h
  	or.memory.c
  	opt.init.c
  	opt.preds.c
  	or.copy_engine.c
  	or.cow_engine.c
  	or.sba_engine.c
  	or.thread_engine.c
  	or.scheduler.c
	or.cut.c
	tab.tries.c
	tab.completion.c
  )

option (WITH_TABLING "Support tabling" ON)
if (WITH_TABLING)
  #this depends on DEPTH_LIMIT define it after

  #this macro should realy be in config.h or other like it
  #and it is used across several files outside OPTYap
  set_property(DIRECTORY ..   APPEND PROPERTY COMPILE_DEFINITIONS  TABLING=1)
  include_directories (OPTYap)
endif(WITH_TABLING)


option (WITH_YAPOR "Experimental Support for Or-parallelism" OFF)

CMAKE_DEPENDENT_OPTION (WITH_YAPOR_COPY "Copy-based Or-parallelism" ON
  "WITH_YAPOR" OFF)
if (WITH_YAPOR_COPY)
  set_property(DIRECTORY ..  APPEND PROPERTY COMPILE_DEFINITIONS  YAPOR_COPY=1)
endif (WITH_YAPOR_COPY)
  
CMAKE_DEPENDENT_OPTION (WITH_YAPOR_THOR "Thread-based Or-parallelism" OFF
  "WITH_OR_PARALLELISM;WITH_THREADS;NOT WITH_YAPOR" OFF)
if (WITH_YAPOR_THOR)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS YAPOR_THREADS=1)
endif (WITH_YAPOR_THOR)

CMAKE_DEPENDENT_OPTION (WITH_YAPOR_ACOW "Process-based Copy-On-Write Or-parallelism" OFF
  "WITH_OR_PARALLELISM;NOT WITH_YAPOR;NOT WITH_THOR" OFF)
if (WITH_YAPOR_ACOW)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  YAPOR_COW=1)
endif (WITH_YAPOR_ACOW)

CMAKE_DEPENDENT_OPTION (WITH_YAPOR_SBA "Sparse Bind Array-based Or-parallelism" OFF
  "WITH_OR_PARALLELISM;NOT WITH_YAPOR;NOT WITH_THOR;NOT WITH_ACOW" OFF)
if (WITH_YAPOR_SBA)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  YAPOR_SBA=1)
endif (WITH_YAPOR_SBA)

set (POSITION_INDEPENDENT_CODE TRUE)

add_library (libOPTYap  OBJECT
  ${OPTYap_SOURCES} )

set_target_properties(libOPTYap
  PROPERTIES
  #  RPATH ${libdir} VERSION ${LIBYAPTAI_FULL_VERSION}
  #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME OPTYap
  )
