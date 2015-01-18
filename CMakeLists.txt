# organised as follows:
#  main variables
#  sources
#  system core
#  libraries

cmake_minimum_required(VERSION 2.8)

project(YAP C CXX)

set(YAP_MAJOR_VERSION 6)
set(YAP_MINOR_VERSION 3)
set(YAP_PATCH_VERSION 4)

if (WIN32)
  set (YAP_ARCH $ENV{PROCESSOR_ARCHITECTURE})
endif()
if (UNIX)
  find_program (UNAME uname)
  execute_process (
    COMMAND ${UNAME} -m
    OUTPUT_VARIABLE YAP_ARCH OUTPUT_STRIP_TRAILING_WHITESPACE )
endif()

set(YAP_FULL_VERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_FVERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_NUMERIC_VERSION
  ${YAP_MAJOR_VERSION}*10000+${YAP_MINOR_VERSION}*100+${YAP_PATCH_VERSION})
set(MYDDAS_VERSION MYDDAS-0.9.1)

site_name( YAP_SITE )


message(STATUS "Building YAP version ${YAP_VERSION}")

# set path to additional CMake modules
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

set(CMAKE_TOP_BINARY_DIR ${CMAKE_BINARY_DIR})
set(YAP_PL_SRCDIR ${CMAKE_SOURCE_DIR}/pl)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  cmake_policy( SET CMP0042 NEW)
endif()

# Compatibility vars with autotols
set ( prefix "${CMAKE_INSTALL_PREFIX}")
set ( exec_prefix "${prefix}")
set ( libdir "${exec_prefix}/lib")
set ( dlls "${exec_prefix}/lib/Yap")
set ( includedir "${prefix}/include")
set ( datarootdir "${prefix}/share")
set ( libpl  "${prefix}/share/Yap")
set ( datadir "${datarootdir}")
set ( mandir "${datarootdir}/man")
set ( bindir "${exec_prefix}/bin")
set ( docdir "${exec_prefix}/share/doc/Yap")

set(YAP_ROOTDIR ${prefix})

# erootdir -> rootdir
# bindir defined above
# libdir defined above
set(YAP_LIBDIR "${dlls}")  
set(YAP_SHAREDIR "${datarootdir}")  
set(YAP_BINDIR "${bindir}")  
set(YAP_ROOTDIR "${prefix}")  

set(YAP_YAPLIB libYap${CMAKE_SHARED_LIBRARY_SUFFIX})
set(YAP_STARTUP startup.yss)
string(TIMESTAMP YAP_TIMESTAMP)
string( SUBSTRING ${CMAKE_SHARED_LIBRARY_SUFFIX} 1 -1 SO_EXT )
#
include_directories (H include os ${CMAKE_BINARY_DIR})

# Optional libraries that affect compilation
#
include (ConfigureChecks)
include (MacroOptionalAddSubdirectory)
include (MacroOptionalFindPackage)
include (MacroLogFeature)

#
# Optional Components
#

include (CMakeDependentOption)
include (CheckSymbolExists)

set ( MALLOC_T void *)
set ( MIN_STACKSPACE 1024*SIZEOF_INT_P )
set ( MIN_HEAPSPACE 2*1024*SIZEOF_INT_P )
set ( MIN_TRAILSPACE 512*SIZEOF_INT_P )
set ( DEF_STACKSPACE 0 )
set ( DEF_HEAPSPACE 0 )
set ( DEF_TRAILSPACE 0 )

# option (WITH_RATIONAL_TREES "support infinite rational trees" ON) 
add_definitions (-DRATIONAL_TREES=1)

# option (WITH_COROUTINING 
#   "support co-routining, attributed variables and constraints" ON)
add_definitions (-DCOROUTINING=1)

# if (WITH_RATIONAL_TREES and WITH_COROUTINING)
#TODO: install (cd LGPL/clp ; $(MAKE) install)
# endif (WITH_RATIONAL_TREES and WITH_COROUTINING)

# option (WITH_DEPTH_LIMIT
#   "support depth-bound computation" ON)
add_definitions (-DDEPTH_LIMIT=1)

#ensure cells are properly aligned in code
set (ALIGN_LONGS 1)

#ensure best access to slots in environments
set (MSHIFTOFFS 1)

set (C_COMPILER CMAKE_C_COMPILER_ID)

if ( ${C_COMPILER} MATCHES "GNU")
  set (HAVE_GCC 1)
endif()

# compatible compilers
if ( ${C_COMPILER} MATCHES "Clang")
  set (HAVE_GCC 1)
endif()

if ( ${C_COMPILER} MATCHES "Intel")
  set (HAVE_GCC 1)
endif()

if (HAVE_GCC)
  # replace instructions codes by the address of their code
  option (WITH_THREADED_CODE "Use threaded code" ON)
  if (WITH_THREADED_CODE)
    set (USE_THREADED_CODE 1)
  endif (WITH_THREADED_CODE)
endif (HAVE_GCC)

# 
set (BUILD_SHARED_LIBS ON)

#
# include libtai package as an independent library
#
add_subdirectory (os/libtai)

option (WITH_TABLING "Support tabling" ON)
if (WITH_TABLING) 
  #this depends on DEPTH_LIMIT define it after

  #this macro should realy be in config.h or other like it
  #and it is used across several files outside OPTYap
  add_definitions (-DTABLING=1)  
  include_directories (OPTYap)
  add_subdirectory (OPTYap)
endif(WITH_TABLING)

option (WITH_EAM "enable EAM (Extended Andorra Model)" OFF)
if (WITH_EAM)
  #this macro should realy be in config.h or other like it
  add_definitions (-DBEAM=1)
  include_directories (BEAM)
  add_subdirectory (BEAM)
  set (EXTRALIBS ${EXTRALIBS} beam)
endif (WITH_EAM)

#TODO: option (or-parallelism)


# option (WITH_WAM_Profile
#   "support low level profiling of abstract machine" OFF)

option (WITH_Low_Level_Tracer
  "support for procedure-call tracing" OFF)
#TODO:

option (System_Threads
  "support system threads" OFF)
#TODO:

#
# Sources Section
#

set(IOLIB_SOURCES
  os/pl-buffer.c 
  os/pl-codelist.c 
  os/pl-ctype.c 
  os/pl-dtoa.c  
  os/pl-error.c  
  os/pl-file.c 
  os/pl-files.c  
  os/pl-fmt.c  
  os/pl-glob.c  
  os/pl-option.c  
  os/pl-nt.c
  os/pl-os.c 
  os/pl-privitf.c 
  os/pl-prologflag.c 
  os/pl-read.c 
  os/pl-rl.c  
  os/pl-stream.c
  os/pl-string.c 
  os/pl-table.c 
  os/pl-tai.c  
  os/pl-text.c 
  os/pl-utf8.c
  os/pl-write.c 
  C/pl-yap.c
  )

if (WIN32)
  set(IOLIBS_SOURCES
    ${IOLIBS_SOURCES}
    os/windows/uxnt.c
    )
endif (WIN32)

set (ENGINE_SOURCES 
  C/agc.c 
  C/absmi.c
  C/adtdefs.c 
  C/alloc.c 
  C/amasm.c
  C/analyst.c 
  C/arrays.c   
  C/arith0.c
  C/arith1.c
  C/arith2.c
  C/atomic.c
  C/attvar.c
  C/bignum.c 
  C/bb.c
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
  C/globals.c
  C/gmp_support.c 
  C/gprof.c
  C/grow.c 
  C/heapgc.c
  C/index.c        
  C/init.c
  C/inlines.c      
  C/iopreds.c
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
  C/sysbits.c
  C/text.c
  C/threads.c 
  C/tracer.c 
  C/unify.c
  C/userpreds.c  
  C/utilpreds.c
  C/yap-args.c 
  C/write.c 
  C/ypstdio.c 
  library/dialect/swi/fli/swi.c 
  library/dialect/swi/fli/blobs.c
  C/udi.c 
  #packages/udi/rtree.c 
  #packages/udi/rtree_udi.c 

  #  ${IOLIB_SOURCES}
  #  MPI_SOURCES
  )

set(C_INTERFACE_SOURCES
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

SET(OPTYAP_SOURCES
  OPTYap/or.memory.c
  OPTYap/opt.init.c 
  OPTYap/opt.preds.c
  OPTYap/or.copy_engine.c 
  OPTYap/or.cow_engine.c
  OPTYap/or.sba_engine.c 
  OPTYap/or.thread_engine.c 
  OPTYap/or.scheduler.c
  OPTYap/or.cut.c 
  OPTYap/tab.tries.c
  OPTYap/tab.completion.c
  )

set(STATIC_SOURCES
  #NOT INCLUDED FOR NOW
  )

set(CONSOLE_SOURCES console/yap.c)   

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

## define system

add_library(libYap SHARED
  ${ENGINE_SOURCES} 
  ${IOLIB_SOURCES}
  ${C_INTERFACE_SOURCES} 
  ${STATIC_SOURCES}  
  ${OPTYAP_SOURCES}
  $<TARGET_OBJECTS:libyaptai> )

target_link_libraries(libYap
  m resolv stdc++ )

set_target_properties(libYap
  PROPERTIES VERSION ${YAP_FULL_VERSION}
  SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
  OUTPUT_NAME Yap
  )


option (WITH_Dynamic_BDD
  "dynamic bdd library" OFF)
#TODO:

#TODO:

#
# Arch checks
#
#include(ConfigureChecks)
include (Config)

macro_optional_find_package (GMP ON)
macro_log_feature (GMP_FOUND "libgmp" 
  "GNU Multiple Precision" 
  "http://gmplib.org")
if (GMP_FOUND)
  include_directories (${GMP_INCLUDE_DIR})
  target_link_libraries(libYap ${GMP_LIBRARIES})  
  #config.h needs this (TODO: change in code latter)
  set (USE_GMP 1)
  set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIR} )
  set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GMP_LIBRARIES} )
endif (GMP_FOUND)

macro_optional_find_package (Readline ON)
macro_log_feature (READLINE_FOUND "libreadline" 
  "GNU Readline Library (or similar)" 
  "http://www.gnu.org/software/readline")
if (READLINE_FOUND)
  include_directories (${READLINE_INCLUDE_DIR})
  target_link_libraries(libYap ${READLINE_LIBRARIES})  
  set (HAVE_READLINE_READLINE_H 1)
  set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${READLINE_INCLUDE_DIR} )
  set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${READLINE_LIBRARIES} )
  
  check_function_exists( add_history  HAVE_ADD_HISTORY )
  check_function_exists( rl_begin_undo_group HAVE_RL_BEGIN_UNDO_GROUP)  
  check_function_exists( rl_clear_pending_input HAVE_RL_CLEAR_PENDING_INPUT)
  check_function_exists( rl_discard_argument HAVE_RL_DISCARD_ARGUMENT)
  check_function_exists( rl_filename_completion_function HAVE_RL_FILENAME_COMPLETION_FUNCTION)  
  check_function_exists( rl_free_line_state HAVE_RL_FREE_LINE_STATE )  
  check_function_exists( rl_insert_close  HAVE_RL_INSERT_CLOSE )      
  check_function_exists( rl_reset_after_signal  HAVE_RL_RESET_AFTER_SIGNAL )
  check_function_exists( rl_set_keyboard_input_timeout  HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT )      
  check_function_exists( rl_set_prompt  HAVE_RL_SET_PROMPT)
  check_symbol_exists( rl_catch_signals stdio.h;readline/readline.h  HAVE_DECL_RL_CATCH_SIGNALS )  
  check_symbol_exists( rl_completion_func_t stdio.h;readline/readline.h  HAVE_DECL_RL_COMPLETION_FUNC_T )      
  check_symbol_exists( rl_done stdio.h;readline/readline.h  HAVE_DECL_RL_DONE )
  check_symbol_exists( rl_hook_func_t stdio.h;readline/readline.h  HAVE_DECL_RL_HOOK_FUNC_T )      
  check_symbol_exists( rl_event_hook stdio.h;readline/readline.h HAVE_DECL_RL_EVENT_HOOK )
  check_symbol_exists( rl_readline_state stdio.h;readline/readline.h HAVE_DECL_RL_READLINE_STATE )
endif (READLINE_FOUND)


macro_optional_find_package (Threads ON)
macro_log_feature (THREADS_FOUND "Threads Support" 
  "GNU Threads Library (or similar)" 
  "http://www.gnu.org/software/threads")
if (THREADS_FOUND)
  #
  #   CMAKE_THREAD_LIBS_INIT     - the thread library
  #   CMAKE_USE_SPROC_INIT       - are we using sproc?
  #   CMAKE_USE_WIN32_THREADS_INIT - using WIN32 threads?
  #   CMAKE_USE_PTHREADS_INIT    - are we using pthreads
  #   CMAKE_HP_PTHREADS_INIT     - are we using hp pthreads
  #
  # The following import target is created
  #
  # ::
  #
  #   Threads::Threads
  #
  # For systems with multiple thread libraries, caller can set
  #
  # ::
  #
  #   CMAKE_THREAD_PREFER_PTHREAD
  #
  # If the use of the -pthread compiler and linker flag is prefered then the
  # caller can set
  #
  # ::
  #
  set( THREADS_PREFER_PTHREAD_FLAG ON)
  
  if (CMAKE_USE_PTHREADS_INIT)
    target_link_libraries(libYap pthread)  
    set (HAVE_READLINE_READLINE_H 1)
    set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} )
    check_function_exists( pthread_mutexattr_setkind_np HAVE_PTHREAD_MUTEXATTR_SETKIND_NP )
    check_function_exists( pthread_mutexattr_settype HAVE_PTHREAD_MUTEXATTR_SETTYPE )
    check_function_exists( pthread_setconcurrency HAVE_PTHREAD_SETCONCURRENCY )  
  endif (CMAKE_USE_PTHREADS_INIT)
  #
  # Please note that the compiler flag can only be used with the imported
  # target. Use of both the imported target as well as this switch is highly
  # recommended for new code.
endif (THREADS_FOUND)

cmake_dependent_option (WITH_PThreadLocking
  "use pthread locking primitives for internal locking" ON
  System_Threads OFF)

ADD_SUBDIRECTORY(library)
ADD_SUBDIRECTORY(swi/library)
# ADD_SUBDIRECTORY(os)
# ADD_SUBDIRECTORY(packages)
# ADD_SUBDIRECTORY(packages)
ADD_SUBDIRECTORY(pl)

macro_optional_add_subdirectory (packages/cuda)  

macro_optional_add_subdirectory (packages/real)  

macro_optional_add_subdirectory (packages/python)  

macro_optional_add_subdirectory (packages/raptor)  

#macro_optional_add_subdirectory (packages/archive)

#macro_optional_add_subdirectory (packages/jpl)

#macro_optional_add_subdirectory (packages/prosqlite)

#macro_optional_add_subdirectory (packages/zlib)

#todo: use cmake target builds
# option (MAXPERFORMANCE
#   "try using the best flags for specific architecture" OFF)

# option (MAXMEMORY
#   "try using the best flags for using the memory to the most" ON)
#TODO: check MAXMEMORY

#TODO: use cmake target builds
# option (DEBUGYAP
#   "enable C-debugging for YAP" OFF)

#TODO: use cmake arch/compiler
# option (CYGWIN
#   "use cygwin library in WIN32" OFF)

option (WITH_PRISM
  "use PRISM system in YAP" ON)
#TODO:

option (WITH_Dynamic_Loading
  "compile YAP as a DLL" ON)
#TODO:

option (WITH_Static_compilation
  "compile YAP statically" OFF)
#TODO:

option (Use_MALLOC
  "use malloc to allocate memory" ON)
if (Use_MALLOC)
  # use default allocator
  set ( USE_SYSTEM_MALLOC 1 )
endif()

option(CONDOR
  "allow YAP to be used from condor" OFF)
#TODO:

#TODO: detect arch before allow this option
# OPTION(WIN64
#   "compile YAP for win64" OFF)

# option (WITH_APRIL
#   "compile Yap to support April ILP system" OFF)
# option (DLCOMPAT
#   "use dlcompat library for dynamic loading on Mac OS X" OFF)

# find_package(R)
# if (R_FOUND)
#   MESSAGE(STATUS "RFOUND ${R_LIBRARIES} ${R_DEFINITIONS} ${R_EXECUTABLE}")
# endif (R_FOUND)
#TODO: check REAL_TARGET ENABLE_REAL

#TODO: Switch to feature detection
# OPTION(CHR
#   "install chr library" ON)
# OPTION(CLPQR
#   "install clpqr library" ON)
# OPTION(CPLINT
#   "enable the cplint library using the CUDD library in DIR/lib" OFF)
# OPTION(yap_cv_clpbn_bp
#   "enable belief propagation solver in CLPBN." OFF)
# OPTION( 

# SHARED PACKAGES with SWI
# swi packages have both Makefile.in which we will use and
# Makefile.mak, we will use the later to identify this packages
# while we keep both autotools amd cmake working side by side
# Search for available packages which all have a Makefile.mak
#file (GLOB PACKAGES packages/*/Makefile.mak)

# needed by this packages
#configure_file(packages/Makefile.defs.cmake packages/Makefile.defs)
#configure_file(packages/Dialect.defs.cmake packages/Dialect.defs)


#
# include subdirectories configuration
## after we have all functionality in
#

# Compilation Flags
if (CMAKE_COMPILER_IS_GNUCC)
  message(STATUS "Original CMAKE_C_FLAGS ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wstrict-prototypes -Wmissing-prototypes")
  message(STATUS "Running with CMAKE_C_FLAGS ${CMAKE_C_FLAGS}")
  message(STATUS "Original CMAKE_C_FLAGS_DEBUG ${CMAKE_C_FLAGS_DEBUG}")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -g ")
  message(STATUS "Runing with CMAKE_C_FLAGS_DEBUG ${CMAKE_C_FLAGS_DEBUG}")
  message(STATUS "Original CMAKE_C_FLAGS_RELEASE ${CMAKE_C_FLAGS_RELEASE}")
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS} -O3 -fomit-frame-pointer -fstrict-aliasing -freorder-blocks -fsched-interblock")
  message(STATUS "Running with CMAKE_C_FLAGS_RELEASE ${CMAKE_C_FLAGS_RELEASE}")
endif()
add_definitions( -Wall)    
add_definitions(-D_YAP_NOT_INSTALLED_=1 -DHAVE_CONFIG_H  -DLOW_LEVEL_TRACER=1 -DDEBUG=1)
add_definitions(-DTHREADEAD_CODE=1)            
add_definitions(-D_GNU_SOURCE=1 -D_XOPEN_SOURCE=700)            

configure_file ("${PROJECT_SOURCE_DIR}/config.h.cmake"
  "${PROJECT_BINARY_DIR}/config.h" )
configure_file ("${PROJECT_SOURCE_DIR}/YapTermConfig.h.cmake"
  "${PROJECT_BINARY_DIR}/YapTermConfig.h" )

add_executable (yap-bin ${CONSOLE_SOURCES})

set_target_properties (yap-bin PROPERTIES OUTPUT_NAME yap)

target_link_libraries(yap-bin libYap )

add_custom_target (main ALL DEPENDS ${YAP_STARTUP} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )

install (
    TARGETS yap-bin libYap
    RUNTIME DESTINATION ${bindir}
    ARCHIVE  DESTINATION ${libdir}
    LIBRARY  DESTINATION ${libdir}
  )

macro_display_feature_log()

