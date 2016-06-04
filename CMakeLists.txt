# organised as follows:
#  main variables
#  sources
#  system core
#  libraries

project(YAP)

cmake_minimum_required (VERSION 2.8)

set (ENV{ANDROID_NDK} "/Volumes/Transcend/vsc/AndroidSDK/ndk-bundle")

# where we have most scripts
# set path to additional CMake modules
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(disallow)

disallow_intree_builds()

# set(CMAKE_BUILD_TYPE  Debug)

set (MACOSX_RPATH ON)
if(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)
if(POLICY CMP0043)
cmake_policy(SET CMP0043 NEW)
endif(POLICY CMP0043)


set(YAP_FOUND ON)

set(YAP_MAJOR_VERSION 6)
set(YAP_MINOR_VERSION 3)
set(YAP_PATCH_VERSION 4)

set(YAP_SYSTEM_OPTIONS "attributed_variables" )

if (WIN32)
  set (YAP_ARCH $ENV{PROCESSOR_ARCHITECTURE})
  set(YAP_SYSTEM_OPTIONS "windows " ${YAP_SYSTEM_OPTIONS})
endif()
if (UNIX)
  find_program (UNAME uname)
  execute_process (
    COMMAND ${UNAME} -m
    OUTPUT_VARIABLE YAP_ARCH OUTPUT_STRIP_TRAILING_WHITESPACE )
    set(YAP_SYSTEM_OPTIONS "unix " ${YAP_SYSTEM_OPTIONS})
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
include(CheckVariableExists)
include(CheckCXXSourceCompiles)
include(TestBigEndian)
include (CMakeDependentOption)
include (MacroOptionalAddSubdirectory)
include (MacroOptionalFindPackage)
include (MacroLogFeature)
include(GetGitRevisionDescription)

#cross-compilation support
# Search packages for host system instead of packages for target system
# in case of cross compilation these macro should be defined by toolchain file
if(NOT COMMAND find_host_package)
  macro(find_host_package)
    find_package(${ARGN})
  endmacro()
endif()
if(NOT COMMAND find_host_program)
  macro(find_host_program)
    find_program(${ARGN})
  endmacro()
endif()


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


if (ANDROID)
    set (GMP_ROOT "${CMAKE_SOURCE_DIR}/../gmp/${ANDROID_ABI}")
else()
  set(YAP_STARTUP startup.yss)

  option (WITH_CUDD "BDD CUDD package" ON)
if (WITH_CUDD)
#detect cudd setup, as it is shared between different installations.

find_package(CUDD)
#  CUDD_FOUND       - system has CUDD
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#
if (CUDD_FOUND)
    set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${CUDD_INCLUDE_DIR} )

  check_include_files( cudd.h HAVE_CUDD_H )
check_include_files( "stdio.h;cudd/cudd.h" HAVE_CUDD_CUDD_H )
check_include_files( cuddInt.h HAVE_CUDDINT_H )
check_include_files( "stdio.h;cudd/cudd.h;cudd/cuddInt.h" HAVE_CUDD_CUDDINT_H )
endif (CUDD_FOUND)

endif(WITH_CUDD)

option(WITH_JAVA "Try to use Java (currently Java 6,7,8)" ON)

if (WITH_JAVA)
#detect java setup, as it is shared between different installations.

find_package(Java 1.8 COMPONENTS Runtime Development)
# find_package(Java COMPONENTS Development)
# find_package(Java COMPONENTS Runtime)
#find_package(JavaLibs)
macro_log_feature (Java_Development_FOUND "Java"
    "Use Java System"
    "http://www.java.org" FALSE)

if (Java_Development_FOUND)

  find_package(JNI)

  if (JNI_FOUND)

  include(UseJava)

  #
  #   Java_JAVA_EXECUTABLE      = the full path to the Java runtime
  #   Java_JAVAC_EXECUTABLE     = the full path to the Java compiler
  #   Java_JAVAH_EXECUTABLE     = the full path to the Java header generator
  #   Java_JAVADOC_EXECUTABLE   = the full path to the Java documention generator
  #   Java_IDLJ_EXECUTABLE      = the full path to the Java idl compiler
  #   Java_JAR_EXECUTABLE       = the full path to the Java archiver
  #   Java_JARSIGNER_EXECUTABLE = the full path to the Java jar signer
  #   Java_VERSION_STRING       = Version of java found, eg. 1.6.0_12
  #   Java_VERSION_MAJOR        = The major version of the package found.
  #   Java_VERSION_MINOR        = The minor version of the package found.
  #   Java_VERSION_PATCH        = The patch version of the package found.
  #   Java_VERSION_TWEAK        = The tweak version of the package found (after '_')
  #   Java_VERSION              = This is set to: $major.$minor.$patch(.$tweak)
  #
  # The Java_ADDITIONAL_VERSIONS variable can be used to specify a list
  # of version numbers that should be taken into account when searching
  # for Java.  You need to set this variable before calling
          # find_package(JavaLibs).
  #
  #macro_optional_find_package(JNI ON)
  #   JNI_INCLUDE_DIRS      = the include dirs to use
  #   JNI_LIBRARIES         = the libraries to use
  #   JNI_FOUND             = TRUE if JNI headers and libraries were found.
  #   JAVA_AWT_LIBRARY      = the path to the jawt library
  #   JAVA_JVM_LIBRARY      = the path to the jvm library
  #   JAVA_INCLUDE_PATH     = the include path to jni.h
  #   JAVA_INCLUDE_PATH2    = the include path to jni_md.h
  #   JAVA_AWT_INCLUDE_PATH = the include path to jawt.h
endif (JNI_FOUND)


endif (Java_Development_FOUND)
endif(WITH_JAVA)

option(WITH_PYTHON  "Try to use Python (currently Python 3)" ON)

if (WITH_PYTHON)


#BREW install for Python3


if (APPLE)
foreach (i 3.6 3.5 3.4 3.3 3.2 3.1 3.0)
  set (PYTHON_INCLUDE_DIRS /usr/local/Frameworks/Python.framework/Versions/${i}/Headers)
  message("Trying Python ${i}")
  if (EXISTS ${PYTHON_INCLUDE_DIRS})
      set (PYTHON_EXECUTABLE /usr/local/bin/python${i} CACHE FILEPATH  "Path to a program" FORCE )
    set (PYTHON_INCLUDE_DIR /usr/local/Frameworks/Python.framework/Versions/${i}/include/python${i}m
    CACHE PATH "Path to a file." FORCE   )
    set (PYTHON_LIBRARY /usr/local/Frameworks/Python.framework/Versions/${i}/lib/libpython${i}.dylib
    CACHE FILEPATH  "Path to a library" FORCE )
    break()
  endif()
endforeach()
endif()

find_package(PythonInterp)
find_package(PythonLibs)

 macro_log_feature (PYTHONLIBS_FOUND "Python"
   "Use Python System"
   "http://www.python.org" FALSE)
endif()


    endif(ANDROID)


if (
)
  cmake_policy( SET CMP0042 NEW)
  #cmake_policy( NO_POLICY_SCOPE )
endif()

set (BUILD_SHARED_LIBS ON)

## define system

include (Sources)


if (YAP_SINGLE_FILE)
set(YAP_MODULES
    $<TARGET_OBJECTS:Yapsqlite3>
    $<TARGET_OBJECTS:Yap++>
    $<TARGET_OBJECTS:utf8proc>
)
else(YAP_SINGLE_FILE)
  if (WIN32)
  set(YAP_MODULES
    $<TARGET_OBJECTS:Yap++>
    ${YAP_MODULES}
    )
  endif(  )
  set( UTF8PROC_DL utf8proc)
endif(YAP_SINGLE_FILE)


add_library(libYap
  ${ENGINE_SOURCES}
  ${C_INTERFACE_SOURCES}
  ${STATIC_SOURCES}
  ${OPTYAP_SOURCES}
  ${HEADERS}
  ${WINDLLS}
  $<TARGET_OBJECTS:libYAPOs>
  $<TARGET_OBJECTS:libOPTYap>
  $<TARGET_OBJECTS:myddas>
  $<TARGET_OBJECTS:libswi>
  ${YAP_MODULES}
)



set_property(DIRECTORY PROPERTY CXX_STANDARD 11)

  # Optional libraries that affect compilation
  #
  include (Config)

IF (NOT MSVC)
  IF (ANDROID)
  target_link_libraries(libYap m android log )
  ELSE (ANDROID)
  target_link_libraries(libYap m)
  ENDIF (ANDROID)
ENDIF (NOT MSVC)

set_target_properties(libYap
  PROPERTIES VERSION ${YAP_FULL_VERSION}
  SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
  OUTPUT_NAME Yap
  )


set(CMAKE_TOP_BINARY_DIR ${CMAKE_BINARY_DIR})
set(YAP_PL_SRCDIR ${CMAKE_SOURCE_DIR}/pl)

# Compatibility vars with autotols
if (0)
set ( prefix "${CMAKE_SOURCE_DIR}/android/app/build/generated")
set ( exec_prefix "${prefix}")
set ( libdir "${exec_prefix}/jniLibs/${ANDROID_ABI}")
set ( dlls "${libdir}")
set ( includedir "${prefix}/src/include")
set ( datarootdir "${prefix}/share")
set ( libpl  "${datarootdir}/Yap")
set ( datadir "${datarootdir}")
set ( mandir "${datarootdir}/man")
set ( bindir "${exec_prefix}/bin")
set ( docdir "${exec_prefix}/share/doc/Yap")
else()
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
endif()

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
string(TIMESTAMP YAP_TIMESTAMP)
string( SUBSTRING ${CMAKE_SHARED_LIBRARY_SUFFIX} 1 -1 SO_EXT )
#
include_directories (H  H/generated include os OPTYap utf8proc JIT/HPP)
include_directories (BEFORE ${CMAKE_BINARY_DIR})

# rpath stuff, hopefully it works

# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

SET(CMAKE_INSTALL_RPATH "${libdir};${dlls}:")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)


# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${libdir};${dlls}" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${libdir};${dlls}")
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
if ( ${C_COMPILER} MATCHES "GNU")
  set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  $<$<CONFIG:Release>:-O3;-fomit-frame-pointer;-fstrict-aliasing;-freorder-blocks;-fsched-interblock> )
else()
   set_property( DIRECTORY APPEND PROPERTY COMPILE_OPTIONS  $<$<CONFIG:Release>:-O3;-fstrict-aliasing;-freorder-blocks;-fsched-interblock> )
endif()
  set_property( DIRECTORY APPEND  PROPERTY  COMPILE_OPTIONS  -fexceptions )
endif()

# set_property( DIRECTORY APPEND_STRING PROPERTY -fsanitize=memory;-fsanitize-memory-track-origins=2)
if (HAVE_GCC)
  # replace instructions codes by the address of their code
  option (WITH_THREADED_CODE "threaded code" ON)
  if (WITH_THREADED_CODE)
    set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  THREADED_CODE=1;USE_SYSTEM_MALLOC=1)
  endif (WITH_THREADED_CODE)
endif (HAVE_GCC)

#
#option (YAP_SWI_IO ON)

OPTION (WITH_CALL_TRACER
"support for procedure-call tracing" ON)
#TODO:
if (WITH_CALL_TRACER)
    set(YAP_SYSTEM_OPTIONS "call_tracer " ${YAP_SYSTEM_OPTIONS})
set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS $<$<CONFIG:Debug>:LOW_LEVEL_TRACER=1> )
endif (WITH_CALL_TRACER)

#set( CMAKE_REQUIRED_LIBRARIES  ${READLINE_LIBS} ${CMAKE_REQUIRED_LIBRARIES} )
#target_link_libraries(libYap ${READLINE_LIBS})

#utf-8 is not an option
# we use the nice UTF-8 package
#available at the Julia project
include_directories ( utf8proc )
set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  UTF8PROC=1)
ADD_SUBDIRECTORY ( utf8proc )

find_package (GMP)
macro_log_feature (GMP_FOUND
  "GNU libgmp (in some cases MPIR"
  "GNU big integers and rationals"
  "http://gmplib.org")
  set(YAP_SYSTEM_OPTIONS "big_numbers " ${YAP_SYSTEM_OPTIONS})

if (GMP_FOUND)
# GMP_FOUND         - true if GMP/MPIR was found
# GMP_INCLUDE_DIRS  - include search path
# GMP_LIBRARIES      - libraries to link with
# GMP_LIBRARY_DLL    - library DLL to install. Only available on WIN32.
# GMP_LIBRARIES_DIR - the directory the library we link with is found in.
  include_directories (${GMP_INCLUDE_DIRS})
    #add_executable(test ${SOURCES})
    target_link_libraries(libYap ${GMP_LIBRARIES})
    #config.h needs this (TODO: change in code latter)
    set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIRS} )
    #set( CMAKE_REQUIRED_LIBRARIES  ${GMP_LIBRARIES} ${CMAKE_REQUIRED_LIBRARIES} )
IF (MSVC)
file(COPY ${GMP_LIBRARY_DLL} DESTINATION ${CMAKE_BINARY_DIR} )
ENDIF(MSVC)

  endif (GMP_FOUND)

macro_optional_find_package (Threads OFF)
macro_log_feature (THREADS_FOUND "Threads Support"
  "GNU Threads Library (or similar)"
  "http://www.gnu.org/software/threads")
if (WITH_Threads)
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
  set(YAP_SYSTEM_OPTIONS "threads " ${YAP_SYSTEM_OPTIONS})
 set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  THREADS=1)
  #
  # Please note that the compiler flag can only be used with the imported
  # target. Use of both the imported target as well as this switch is highly
  # recommended for new code.
endif (WITH_Threads)

CMAKE_DEPENDENT_OPTION (WITH_MAX_Threads 1024
  "maximum number of threads" "WITH_MAX_Threads" 1)

CMAKE_DEPENDENT_OPTION (WITH_MAX_Workers 64
  "maximum number of or-parallel workers" "WITH_MAX_Workers" 1)

cmake_dependent_option (WITH_Pthread_Locking
  "use pthread locking primitives for internal locking" ON
  "WITH_ThreadsS" OFF)

IF(WITH_Pthread_Lockin)
  set_DIRECTORY_properties(PROPERTIES APPEND COMPILE_DEFINITIONS USE_PTHREAD_LOCKING=1)
ENDIF()

#
# include OS and I/o stuff
#
# convenience libraries
# OPTYap exports important flags
#
add_subDIRECTORY (OPTYap)

add_subDIRECTORY (packages/myddas)


add_subDIRECTORY (os)

#bootstrap and saved state
add_subDIRECTORY (pl)

#C++ interface
add_subDIRECTORY (CXX)

ADD_SUBDIRECTORY(library)
ADD_SUBDIRECTORY(swi/library)
# ADD_SUBDIRECTORY(os)
# ADD_SUBDIRECTORY(packages)


option (WITH_JIT
"just in Time Clause Compilation" OFF)
if (WITH_JIT)
  add_subDIRECTORY(JIT)
endif (WITH_JIT)


add_subDIRECTORY (packages/raptor)

add_subDIRECTORY (packages/xml)


IF (NOT MSVC AND NOT ANDROID)
OPTION (WITH_CLPBN " Enable the CLPBN and PFL probabilistic languages"  ON)

OPTION (WITH_CPLINT " Enable the cplint probabilistic language"   ON)

OPTION (WITH_HORUS " Enable the CLPBN and PFL probabilistic languages"  ON)

IF (WITH_CLPBN)
add_subDIRECTORY (packages/CLPBN)
ENDIF(WITH_CLPBN)

IF (WITH_CPLINT)
add_subDIRECTORY (packages/cplint)
ENDIF(WITH_CPLINT)

ENDIF(NOT MSVC AND NOT ANDROID)

#must be last
add_subDIRECTORY (packages/swig)


# please install doxygen for prolog first
# git clone http://www.github.com/vscosta/doxygen-yap
# cd doxygen-yap
# mkdir -p build
# cd build
# make; sudo make install
option (WITH_DOCS
  "generate YAP docs" OFF)

 # add_subDIRECTORY (docs)

# add_subDIRECTORY (packages/cuda)

add_subDIRECTORY (packages/gecode)

add_subDIRECTORY (packages/real)

add_subDIRECTORY (packages/python)

add_subDIRECTORY (packages/jpl)

add_subDIRECTORY (packages/bdd)

add_subDIRECTORY (packages/ProbLog)

add_subDIRECTORY (packages/swi-minisat2)

#todo: use cmake target builds
# option (USE_MAXPERFORMANCE
#   "try using the best flags for specific architecture" OFF)

# option (USE_MAXMEMORY
#   "try using the best flags for using the memory to the most" ON)
#TODO: check MAXMEMORY

#TODO: use cmake target builds
# option (USE_DEBUGYAP
#   "enable C-debugging for YAP" OFF)e

#TODO: use cmake arch/compiler
# option (USE_CYGWIN
#   "use cygwin library in WIN32" OFF)

option (WITH_PRISM
  "use PRISM system in YAP" ON)
#TODO:


option (WITH_YAP_DLL
  "compile YAP as a DLL" ON)
#TODO:

option (WITH_YAP_STATIC
  "compile YAP statically" OFF)
#TODO:

# modern systems do this.
set ( MALLOC_T "void *" )
CMAKE_DEPENDENT_OPTION (WITH_SYSTEM_MALLOC
  "use malloc to allocate memory" ON  "NOT WITH_COPY_OR_PARALELISM" OFF)
CMAKE_DEPENDENT_OPTION (WITH_DL_MALLOC
  "use malloc to allocate memory" ON  "NOT WITH_SYSTEM_MALLOC" OFF)
CMAKE_DEPENDENT_OPTION (WITH_YAP_MALLOC
  "use malloc to allocate memory" ON  "NOT WITH_SYSTEM_MALLOC;NOT WITH_DL_MALLOC" OFF)

option(WITH_YAP_CONDOR
  "allow YAP to be used from condor" OFF)

if (WITH_YAP_CONDOR)
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
  ${UTF8PROC_DL}
  ${GMP_LIBRARIES}
  ${READLINE_LIBS}
  ${CMAKE_DL_LIBS}
  )

if(WIN32)

  if(MSVC)
      set(MSVC_RUNTIME "dynamic")
  ENDIF(MSVC)

  target_link_libraries(libYap  wsock32 ws2_32 Shlwapi)

endif(WIN32)

add_executable (yap-bin ${CONSOLE_SOURCES})

if (ANDROID)

set_target_properties (yap-bin PROPERTIES OUTPUT_NAME yapi)
else()
set_target_properties (yap-bin PROPERTIES OUTPUT_NAME yap)


endif()

target_link_libraries(yap-bin libYap )


CMAKE_DEPENDENT_OPTION (WITH_SYSTEM_MMAP "Use MMAP for shared memory allocation" ON
  "NOT WITH_YAPOR_THOR" OFF)

CMAKE_DEPENDENT_OPTION (WITH_SYSTEM_SHM "Use SHM for shared memory allocation" ON
  "NOT WITH_YAPOR_THOR; NOT WITH_SYSTEM_MMAP" OFF )

  add_subDIRECTORY(library/lammpi)

  if (MPI_C_FOUND)

  CMAKE_DEPENDENT_OPTION( WITH_MPI ON "Interface to OpenMPI/MPICH"
    "MPI_C_FOUND" OFF)
  macro_optional_add_subDIRECTORY(library/mpi)
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


 ##   add_subDIRECTORY(utils)

if (NOT ANDROID)
  add_custom_target (main ALL DEPENDS ${YAP_STARTUP} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )
endif()
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




if (ANDROID)
INSTALL(FILES ${GMP_LIBRARIES} DESTINATION ${libdir}
        )
endif(ANDROID)

   install(TARGETS  libYap yap-bin
    RUNTIME DESTINATION ${bindir}
    LIBRARY DESTINATION ${libdir}
     ARCHIVE DESTINATION ${libdir}
  )

macro_display_feature_log()
if(POLICY CMP0058)
cmake_policy(SET CMP0058 NEW)
endif(POLICY CMP0058)
