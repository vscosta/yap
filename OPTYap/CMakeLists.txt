

  set (OPTYap_SOURCES
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
  include_directories (OPTYap )
  set(YAP_SYSTEM_OPTIONS "tabling " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
endif(WITH_TABLING)

option (WITH_YAPOR "Experimental Support for Or-parallelism" OFF)

OPTION (WITH_YAPOR_COPY "Copy-based Or-parallelism" OFF)
if (WITH_YAPOR_COPY)
  set_property(DIRECTORY ..  APPEND PROPERTY COMPILE_DEFINITIONS  YAPOR_COPY=1;USE_DL_MALLOC=1)
  set(WITH_YAPOR ON)
  set(YAP_SYSTEM_OPTIONS "or_parallelism " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
 endif (WITH_YAPOR_COPY)

OPTION (WITH_YAPOR_THOR "Thread-based Or-parallelism" OFF)
if (WITH_YAPOR_THOR)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS YAPOR_THREADS=1)
  set(WITH_YAPOR ON)
  set(YAP_SYSTEM_OPTIONS "or_parallelism " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
endif (WITH_YAPOR_THOR)

OPTION (WITH_YAPOR_ACOW "Process-based Copy-On-Write Or-parallelism" OFF)
if (WITH_YAPOR_ACOW)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  YAPOR_COW=1)
  set(WITH_YAPOR ON)
  set(YAP_SYSTEM_OPTIONS "or_parallelism " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
endif (WITH_YAPOR_ACOW)

OPTION (WITH_YAPOR_SBA "Sparse Bind Array-based Or-parallelism" OFF)
if (WITH_YAPOR_SBA)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  YAPOR_SBA=1)
  set(WITH_YAPOR ON)
  set(YAP_SYSTEM_OPTIONS "or_parallelism, " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
endif (WITH_YAPOR_SBA)



if (WITH_YAPOR)
  set_property(DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS YAPOR=1;YAP_KERNEL=1)
endif ()

set (POSITION_INDEPENDENT_CODE TRUE)
SET_PROPERTY(DIRECTORY PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

add_component (libOPTYap
		${OPTYap_SOURCES} )


My_set_target_properties(libOPTYap
  PROPERTIES
  #  RPATH ${CMAKE_INSTALL_LIBDIR} VERSION ${LIBYAPTAI_FULL_VERSION}
  #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME OPTYap
  )
