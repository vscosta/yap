# organised as follows:
#  main variables
#  sources
#  system core
#  libraries

project(YAP)

cmake_minimum_required(VERSION 2.8)

# where we have most scripts
# set path to additional CMake modules
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

set(CMAKE_PREFIX_PATH ~/Qt/5.4/clang_64/ ${CMAKE_PREFIX_PATH})

# set(CMAKE_BUILD_TYPE  Debug)

if(POLICY CMP0042)
cmake_policy(SET CMP0042 NEW) # Set MACOSX_RPATH=YES by default
endif(POLICY CMP0042)
if(POLICY CMP0043)
cmake_policy(SET CMP0058 NEW)
endif(POLICY CMP0043)


set(YAP_FOUND ON)

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

#
# Optional Components
#
include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include (CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckFunctionExists)
include(CheckIncludeFiles)
include(CheckFunctionExists)
include(CheckPrototypeExists)
include(CheckTypeSize)
include(CheckCXXSourceCompiles)
include(TestBigEndian)
include (CMakeDependentOption)
include (MacroOptionalAddSubdirectory)
include (MacroOptionalFindPackage)
include (MacroLogFeature)
include(GetGitRevisionDescription)

# Test signal handler return type (mimics AC_TYPE_SIGNAL)
include(TestSignalType) #check if this is really needed as c89 defines this as void

#                                   Test standard headers (mimics AC_HEADER_STDC)
include(TestSTDC)

set(bitness 32)
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(bitness 64)
endif()

get_git_head_revision(GIT_HEAD GIT_SHA1)
git_describe(GIT_DESCRIBE)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  cmake_policy( SET CMP0042 NEW)
  #cmake_policy( NO_POLICY_SCOPE )
endif()


## define system

include (Sources)


add_library(libYap SHARED
  ${ENGINE_SOURCES}
  ${C_INTERFACE_SOURCES}
  ${STATIC_SOURCES}
  ${OPTYAP_SOURCES}
  ${HEADERS}
  $<TARGET_OBJECTS:libYAPOs>
  $<TARGET_OBJECTS:libOPTYap>
#  $<TARGET_OBJECTS:myddas>
  )

set_property(DIRECTORY PROPERTY CXX_STANDARD 11)

  # Optional libraries that affect compilation
  #
  include (Config)

set_target_properties(libYap
  PROPERTIES VERSION ${YAP_FULL_VERSION}
  SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
  OUTPUT_NAME Yap
  )


set(CMAKE_TOP_BINARY_DIR ${CMAKE_BINARY_DIR})
set(YAP_PL_SRCDIR ${CMAKE_SOURCE_DIR}/pl)

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
set(YAP_INCLUDEDIR "${includedir}")
set(YAP_ROOTDIR "${prefix}")

set(YAP_YAPLIB libYap${CMAKE_SHARED_LIBRARY_SUFFIX})
set(YAP_STARTUP startup.yss)
string(TIMESTAMP YAP_TIMESTAMP)
string( SUBSTRING ${CMAKE_SHARED_LIBRARY_SUFFIX} 1 -1 SO_EXT )
#
include_directories (H include os OPTYap utf8proc JIT/HPP)
include_directories (BEFORE ${CMAKE_BINARY_DIR})

# rpath stuff, hopefully it works

# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

SET(CMAKE_INSTALL_RPATH "${dlls}:${libdir}")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)


# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
ENDIF("${isSystemDir}" STREQUAL "-1")


#
set ( MIN_STACKSPACE 1024*SIZEOF_INT_P )
set ( MIN_HEAPSPACE 2*1024*SIZEOF_INT_P )
set ( MIN_TRAILSPACE 512*SIZEOF_INT_P )
set ( DEF_STACKSPACE 0 )
set ( DEF_HEAPSPACE 0 )
set ( DEF_TRAILSPACE 0 )

# option (RATIONAL_TREES "support infinite rational trees" ON)
# dd_definitions (-D)

## don't touch these opts
  set_property( DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS  DEPTH_LIMIT=1;COROUTINING=1;RATIONAL_TREES=1 )

# inform we are compiling YAP
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  "_YAP_NOT_INSTALLED_=1;HAVE_CONFIG_H=1" )

# Compilation model
#  target_compile_definitions(libYap PUBLIC  _XOPEN_SOURCE=700 )

#add_definitions( -Wall  -Wstrict-prototypes -Wmissing-prototypes)

# Model Specific
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS   $<$<CONFIG:Debug>:DEBUG=1> )

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

# Model Specific
if (HAVE_GCC)
  set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  -Wall )
  set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  $<$<CONFIG:Release>:-O3;-fomit-frame-pointer;-fstrict-aliasing;-freorder-blocks;-fsched-interblock> )
  set_property( DIRECTORY APPEND  PROPERTY  COMPILE_OPTIONS  -fexceptions )
endif()

# set_property( DIRECTORY APPEND_STRING PROPERTY -fsanitize=memory;-fsanitize-memory-track-origins=2)

message(STATUS "Running with CMAKE_C_FLAGS ${CMAKE_C_FLAGS}")

if (HAVE_GCC)
  # replace instructions codes by the address of their code
  option (USE_THREADED_CODE "threaded code" ON)
  if (USE_THREADED_CODE)
    set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  THREADED_CODE=1)
  endif (USE_THREADED_CODE)
endif (HAVE_GCC)

#
set (BUILD_SHARED_LIBS ON)

#option (YAP_SWI_IO ON)



CMAKE_DEPENDENT_OPTION (USE_CALL_TRACER
"support for procedure-call tracing" ON "CMAKE_BUILD_TYPE=DEBUG" OFF )
#TODO:
if (USE_CALL_TRACER)
    set_target_properties(libYap PUBLIC PROPERTIES APPEND COMPILE_DEFINITIONS  $<$<CONFIG:Debug>:LOW_LEVEL_TRACER=1>)
endif (USE_CALL_TRACER)

#set( CMAKE_REQUIRED_LIBRARIES  ${READLINE_LIBS} ${CMAKE_REQUIRED_LIBRARIES} )
#target_link_libraries(libYap ${READLINE_LIBS})

#utf-8 is not an option
# we use the nice UTF-8 package
#available at the Julia project
include_directories ( utf8proc )
set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  UTF8PROC=1)
add_subDIRECTORY ( utf8proc )


option (USE_GMP "Use Multiprecision Library" ON)
if (USE_GMP)
  find_package (GMP)
  macro_log_feature (GMP_FOUND
      "libgmp"
      "GNU big integers and rationals"
      "http://gmplib.org")
  if (GMP_FOUND)
    include_directories (${GMP_INCLUDE_DIR})
    #add_executable(test ${SOURCES})
    target_link_libraries(libYap ${GMP_LIBRARIES})
    #config.h needs this (TODO: change in code latter)
    set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIR} )
    #set( CMAKE_REQUIRED_LIBRARIES  ${GMP_LIBRARIES} ${CMAKE_REQUIRED_LIBRARIES} )
  endif (GMP_FOUND)
endif(USE_GMP)


macro_optional_find_package (Threads OFF)
macro_log_feature (THREADS_FOUND "Threads Support"
  "GNU Threads Library (or similar)"
  "http://www.gnu.org/software/threads")
CMAKE_DEPENDENT_OPTION (USE_THREADS
  "support system threads" OFF "THREADS_FOUND" OFF)
if (USE_THREADS)
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
#    set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} )
    check_function_exists( pthread_mutexattr_setkind_np HAVE_PTHREAD_MUTEXATTR_SETKIND_NP )
    check_function_exists( pthread_mutexattr_settype HAVE_PTHREAD_MUTEXATTR_SETTYPE )
    check_function_exists( pthread_setconcurrency HAVE_PTHREAD_SETCONCURRENCY )
  endif (CMAKE_USE_PTHREADS_INIT)
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  THREADS=1)
  #
  # Please note that the compiler flag can only be used with the imported
  # target. Use of both the imported target as well as this switch is highly
  # recommended for new code.
  else()
endif (USE_THREADS)

CMAKE_DEPENDENT_OPTION (USE_MAX_THREADS 1024
  "maximum number of threads" "USE_THREADS" 1)

CMAKE_DEPENDENT_OPTION (USE_MAX_WORKERS 64
  "maximum number of or-parallel workers" "USE_THREADS" 1)
  
cmake_dependent_option (USE_USE_PTHREAD_LOCKING
  "use pthread locking primitives for internal locking" ON
  "USE_THREADS" OFF)

IF(USE_USE_PTHREAD_LOCKING)
  set_DIRECTORY_properties(PROPERTIES APPEND COMPILE_DEFINITIONS USE_PTHREAD_LOCKING=1)
ENDIF()


#
# include OS and I/o stuff
#
# convenience libraries
# OPTYap exports important flags
#
add_subDIRECTORY (OPTYap)

option (USE_MYDDAS "MYDDAS High-Level Data-base interface" ON)
if (USE_MYDDAS) 
#add_subDIRECTORY (packages/myddas)
endif (USE_MYDDAS) 

add_subDIRECTORY (os)

#bootstrap and saved state
add_subDIRECTORY (pl)

#C++ interface
add_subDIRECTORY (CXX)

ADD_SUBDIRECTORY(library)
ADD_SUBDIRECTORY(swi/library)
# ADD_SUBDIRECTORY(os)
# ADD_SUBDIRECTORY(packages)

option (USE_JIT
"just in Time Clause Compilation" OFF)
if (USE_JIT) 
  add_subDIRECTORY(JIT)
endif (USE_JIT) 

macro_optional_add_subDIRECTORY(library/mpi)

option (USE_GECODE  "Generic Constraint Development Environment (GECODE) library" ON)
if (USE_GECODE) 
add_subDIRECTORY (packages/gecode)
endif (USE_GECODE) 

option (USE_REAL "Prolog->R interface" ON)
if (USE_REAL) 
add_subDIRECTORY (packages/real)
endif (USE_REAL) 

option (USE_PYTHON "Prolog->Python interface" ON)
if (USE_PYTHON) 
add_subDIRECTORY (packages/python)
endif (USE_PYTHON) 

#add_subDIRECTORY (packages/archive)
option (USE_JPL "Prolog<->Python interface" ON)
if (USE_JPL) 
add_subDIRECTORY (packages/jpl)
endif (USE_JPL) 

option (USE_SWIG "ON")
if (USE_SWIG) 
add_subDIRECTORY (packages/swig)
endif (USE_SWIG) 

option (USE_BDD "ON")
if (USE_BDD) 
add_subDIRECTORY (packages/bdd)
endif (USE_BDD) 

option (USE_CLPBN "ON")
if (USE_CLPBN) 
add_subDIRECTORY (packages/CLPBN)
endif (USE_CLPBN) 

option (USE_HORUS "enable HORUS graphical model tookit" "ON")
if (USE_HORUS) 
add_subDIRECTORY (packages/CLPBN/horus)
endif (USE_HORUS) 

option (USE_PROBLOG "ON")
if (USE_PROBLOG) 
add_subDIRECTORY (packages/ProbLog)
endif (USE_PROBLOG) 

option (USE_RAPTOR "ON")
if (USE_RAPTOR) 
add_subDIRECTORY (packages/raptor)
endif (USE_RAPTOR) 

option (USE_CUDA "ON")
if (USE_CUDA) 
# add_subDIRECTORY (packages/cuda)
endif(USE_CUDA)

#add_subDIRECTORY (packages/prosqlite)

#add_subDIRECTORY (packages/zlib)

#todo: use cmake target builds
# option (USE_MAXPERFORMANCE
#   "try using the best flags for specific architecture" OFF)

# option (USE_MAXMEMORY
#   "try using the best flags for using the memory to the most" ON)
#TODO: check MAXMEMORY

#TODO: use cmake target builds
# option (USE_DEBUGYAP
#   "enable C-debugging for YAP" OFF)

#TODO: use cmake arch/compiler
# option (USE_CYGWIN
#   "use cygwin library in WIN32" OFF)

option (USE_PRISM
  "use PRISM system in YAP" ON)
#TODO:

option (USE_YAP_DLL
  "compile YAP as a DLL" ON)
#TODO:

option (USE_YAP_STATIC
  "compile YAP statically" OFF)
#TODO:

# modern systems do this.
set ( MALLOC_T "void *" )
CMAKE_DEPENDENT_OPTION (USE_SYSTEM_MALLOC
  "use malloc to allocate memory" ON  "NOT USE_COPY_OR_PARALELISM" OFF)
CMAKE_DEPENDENT_OPTION (USE_DL_MALLOC
  "use malloc to allocate memory" ON  "NOT USE_SYSTEM_MALLOC" OFF)
CMAKE_DEPENDENT_OPTION (USE_YAP_MALLOC
  "use malloc to allocate memory" ON  "NOT USE_SYSTEM_MALLOC;NOT USE_DL_MALLOC" OFF)


option(USE_YAP_CONDOR
  "allow YAP to be used from condor" OFF)

if (USE_YAP_CONDOR)
  # use default allocator
  set ( YAP_STATIC ON )
  set ( YAP_DLL OFF )
endif()

#TODO: detect arch before allow this option
# OPTION(WIN64
#   "compile YAP for win64" OFF)

# option (APRIL
#   "compile Yap to support April ILP system" OFF)
# option (DLCOMPAT
#   "use dlcompat library for dynamic loading on Mac OS X" OFF)

# SHARED PACKAGES with SWI
# swi packages have both Makefile.in which we will use and
# Makefile.mak, we will use the later to identify this packages
# while we keep both autotools amd cmake working side by side
# Search for available packages which all have a Makefile.mak
#file (GLOB PACKAGES packages/*/Makefile.mak)

# needed by this packages


target_link_libraries(libYap
  utf8proc
  ${GMP_LIBRARIES}
  ${READLINE_LIBS}
  ${CMAKE_DL_LIBS}
  )

add_executable (yap-bin ${CONSOLE_SOURCES})

set_target_properties (yap-bin PROPERTIES OUTPUT_NAME yap)

target_link_libraries(yap-bin libYap )


CMAKE_DEPENDENT_OPTION (USE_SYSTEM_MMAP "Use MMAP for shared memory allocation" ON
  "NOT USE_YAPOR_THOR" OFF)

CMAKE_DEPENDENT_OPTION (USE_SYSTEM_SHM "Use SHM for shared memory allocation" ON
  "NOT USE_YAPOR_THOR; NOT USE_SYSTEM_MMAP" OFF )

if (USE_MPI)

  add_subDIRECTORY(library/lammpi)

  if (MPI_C_FOUND)
  
  CMAKE_DEPENDENT_OPTION( USE_MPI ON "Interface to OpenMPI/MPICH"
    "MPI_C_FOUND" OFF)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${MPI_C_COMPILE_FLAGS} ")
    include_directories(${MPI_C_INCLUDE_PATH})
    target_link_libraries(yap-bin ${MPI_C_LIBRARIES} )
    if(MPI_C_COMPILE_FLAGS)
      set_target_properties(yap-bin PROPERTIES
	COMPILE_FLAGS "${MPI_C_COMPILE_FLAGS}")
    endif(MPI_C_COMPILE_FLAGS)

    if(MPI_C_LINK_FLAGS)
      set_target_properties(yap-bin PROPERTIES
	LINK_FLAGS "${MPI_C_LINK_FLAGS}")
    endif()
  endif (MPI_C_FOUND)
endif (USE_MPI)


add_custom_target (main ALL DEPENDS ${YAP_STARTUP} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )
#
# include subdirectories configuration
## after we have all functionality in
#

configure_file ("${PROJECT_SOURCE_DIR}/config.h.cmake"
  "${PROJECT_BINARY_DIR}/YapConfig.h" )
configure_file ("${PROJECT_SOURCE_DIR}/YapTermConfig.h.cmake"
  "${PROJECT_BINARY_DIR}/YapTermConfig.h" )
configure_file ("${PROJECT_SOURCE_DIR}/config.h.cmake"
  "${PROJECT_BINARY_DIR}/config.h" )
 configure_file("${PROJECT_SOURCE_DIR}/GitSHA1.c.in" "${PROJECT_BINARY_DIR}/GitSHA1.c" @ONLY)

# ADD_SUBDIRECTORY(console/terminal)


install (
    TARGETS yap-bin libYap
    RUNTIME DESTINATION ${bindir}
    ARCHIVE  DESTINATION ${libdir}
    LIBRARY  DESTINATION ${libdir}
  )


macro_display_feature_log()
if(POLICY CMP0058)
cmake_policy(SET CMP0058 NEW)
endif(POLICY CMP0058)

