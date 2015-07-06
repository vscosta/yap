
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

include_directories (../H ../include .  ${GMP_INCLUDE_DIR}  ${PROJECT_BINARY_DIR})


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
