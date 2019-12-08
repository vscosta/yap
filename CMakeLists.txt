
# value of 3.4.0 or lower.
cmake_minimum_required(VERSION 3.4)

# Sets the version of CMake required to build the native
# library. You should either keep the default value or  pass a
# value of 3.4.0 or lower.fg
include(CMakeToolsHelpers OPTIONAL)

include(FeatureSummary)
include(GNUInstallDirs)

project(YAP)

set(YAP_APP_DIR "${CMAKE_SOURCE_DIR}/../yaplib")
cmake_policy(VERSION 3.4)

set( CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/cmake")

set(ANACONDA $ENV{CONDA_BUILD} CACHE BOOL "Anaconda Environment")
#message(STATUS "ANACONDA found:   ${ANACONDA}")

include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(CMakeDependentOption)
include(MacroOptionalAddSubdirectory)
include(MacroOptionalFindPackage)
include(FeatureSummary)
include(FindPackageHandleStandardArgs)

# Optional Components
#
include(CheckIncludeFileCXX)
include(CheckFunctionExists)
include(CheckFunctionExists)
include(CheckPrototypeExists)
include(CheckVariableExists)
include(CheckCXXSourceCompiles)
include(TestBigEndian)
include(GetGitRevisionDescription)

# Creates and names a library, sets it as either STATIC
# or SHARED, and provides the relative paths to its source code.z
# You can define  libraries, and CMake builds it for you.
# Gradle automatically packages shared libraries with your APK.

#cross-compilation support
# Search packages for host system instead of packages for target system
# in case of cross compilation define these macro by toolchain file
if (NOT COMMAND find_host_package)
  macro(find_host_package)
    find_package(${ARGN})
  endmacro()
endif ()
if (NOT COMMAND find_host_program)
  macro(find_host_program)
    find_program(${ARGN})
  endmacro()
endif ()

if (POLICY CMP0003)
  cmake_policy(SET CMP0003 NEW)
endif ()
if (POLICY CMP0066)
  cmake_policy(SET CMP0066 NEW)
endif()
if (POLICY CMP0068)
  cmake_policy(SET CMP0068 NEW)
endif()
if (POLICY CMP0075)
  cmake_policy(SET CMP0075 NEW)
endif ()

## options: compilation flags
option(BUILD_SHARED_LIBS "Build shared library" ON)
CMAKE_DEPENDENT_OPTION(WITH_SYSTEM_MMAP "Use MMAP for shared memory allocation" ON  "NOT WITH_YAPOR_THOR" OFF)
CMAKE_DEPENDENT_OPTION(WITH_SYSTEM_SHM "Use SHM for shared memory allocation" ON "NOT WITH_YAPOR_THOR; NOT WITH_SYSTEM_MMAP" OFF)
OPTION(WITH_CALL_TRACER  "support for procedure-call tracing" ${DEBUG})
option(WITH_YAP_DLL "compile YAP as a DLL" ON)
option(WITH_YAP_STATIC  "compile YAP statically" OFF)
option(WITH_YAP_CONDOR   "allow YAP to be used from condor" OFF)
option(WITH_YAP_DLL  "compile YAP as a DLL" ON)
if (WITH_YAP_CONDOR)
  set(WITH_YAP_STATIC ON)
  set(WITH_YAP_DLL OFF)
endif ()

## options: libraries
option(WITH_THREADED_CODE "threaded code" ON)
if (NOT ANDROID)
  option(WITH_MPI "Interface to OpenMPI/MPICH" ON)
endif()
option(WITH_JIT  "just in Time Clause Compilation" OFF)

if (APPLE)
  set(MACOSX_RPATH ON)
  list(INSERT CMAKE_FRAMEWORK_PATH pre
    /Library/Frameworks
    )


  option(WITH_BREW "brew" ON)
  if (WITH_BREW)
    EXECUTE_PROCESS(COMMAND brew --prefix RESULT_VARIABLE DETECT_BREW OUTPUT_VARIABLE BREW_PREFIX ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

    IF (${DETECT_BREW} EQUAL 0)

      # "/opt/local" is where MacPorts lives, add `/lib` suffix and link
      LINK_DIRECTORIES( ${BREW_PREFIX}/lib)

      list(INSERT CMAKE_PREFIX_PATH pos
	${BREW_PREFIX}/opt/gmp
	${BREW_PREFIX}/opt/libxml2
	${BREW_PREFIX}/opt/openmpi
	${BREW_PREFIX}/opt/openssl
	${BREW_PREFIX}/opt/postgresql
	${BREW_PREFIX}/opt/python3
	${BREW_PREFIX}/opt/readline
	${BREW_PREFIX}/opt/swig
	)
    else()
      option(WITH_MACPORTS "mac-ports" ON)
      if (WITH_MACPORTS)
	# Add MacPorts
	# Detect if the "port" command is valid on this system; if so, return full path
	EXECUTE_PROCESS(COMMAND which port RESULT_VARIABLE DETECT_MACPORTS OUTPUT_VARIABLE MACPORTS_PREFIX ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

	IF (${DETECT_MACPORTS} EQUAL 0)
	  # "/opt/local/bin/port" doesn't have libs, so we get the parent directory
	  GET_FILENAME_COMPONENT(MACPORTS_PREFIX ${MACPORTS_PREFIX} DIRECTORY)

	  # "/opt/local/bin" doesn't have libs, so we get the parent directory
	  GET_FILENAME_COMPONENT(MACPORTS_PREFIX ${MACPORTS_PssREFIX} DIRECTORY)

	  # "/opt/local" is where MacPorts lives, add `/lib` suffix and link
	  LINK_DIRECTORIES(${LINK DIRECTORIES} ${MACPORTS_PREFIX}/lib)
	endif()
      endif()
    endif()
  endif()
endif()

OPTION(WITH_MYDDAS " Enable MYDDAS DBMS interface" ON)

if (ANDROID)
  option (WITH_PACKAGES "packages and libraries  that add value to YAP" OFF)
  OPTION(WITH_SWIG " Enable SWIG interfaces to foreign languages" ON)
  OPTION(WITH_SQLITE3 " Enable MYDDAS SQLITE3 driver" ON)
else()
  option (WITH_PACKAGES "packages and libraries  that add value to YAP" ON)
  OPTION(WITH_SWIG " Enable SWIG interfaces to foreign languages" ${WITH_PACKAGES})
  OPTION(WITH_SQLITE3 " Enable MYDDAS SQLITE3 driver" ${WITH_PACKAGES})
endif()
OPTION(WITH_MYSQL " Enable MYDDAS MYSQL driver" ${WITH_MYDDAS}})
OPTION(WITH_ODBC " Enable MYDDAS ODBC driver" ${WITH_MYDDAS})
OPTION(WITH_POSTGRES " Enable MYDDAS POSTGRES driver" ${WITH_MYDDAS})



IF (WITH_SWIG)
  find_host_package(SWIG)
  # macro_log_feature (SWIG_FOUND "Swig"
  #         "Use SWIG Interface Generator "
  #         "http://www.swig.org" ON)

ENDIF (WITH_SWIG)

OPTION(WITH_RAPTOR " Enable the RAPTOR RDF library" ${WITH_PACKAGES})
OPTION(WITH_XML2 " Enable the RAPTOR XML2 library" ${WITH_PACKAGES})
OPTION(WITH_XML " Enable the Prolog XML library" ${WITH_PACKAGES})
OPTION(WITH_CLPBN " Enable the CLPBN and PFL probabilistic languages" ${WITH_PACKAGES})
OPTION(WITH_HORUS " Enable the HORUS inference libraray for CLPBN and PFL" ${WITH_CLPBN})
option(WITH_PROBLOG "include Problog-I." ${WITH_PACKAGES})
OPTION(WITH_CPLINT " Enable the cplint probabilistic language" ${WITH_PACKAGES})
option(WITH_GECODE "interface gecode constraint solver" ${WITH_PACKAGES})
option(WITH_LBFGS    "interface  with lbfgs" ${WITH_PACKAGES})
option(WITH_PRISM    "use PRISM system in YAP" ${WITH_PACKAGES})
option(WITH_PYTHON  "Allow Python->YAP  and YAP->Python" ${WITH_PACKAGES})
option(WITH_R  "Use R Interface" ${WITH_PACKAGES})
option(WITH_JAVA "Try to use Java (currently Java )" ${WITH_PACKAGES})

set(CMAKE_POSITION_INDEPENDENT_CODE TRUE)
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

#begining stuff for top CMakeLists

include(MacroLogFeature)

if (POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif ()

if (ANDROID)
  function(add_to_dir list output)
  endfunction(add_to_dir list output)

  function(add_to_libgroup el list)
    # add_custom_command(  TARGET ${el} POST_BUILD
    #   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${el}>  ${CMAKE_BINARY_DIR}/packages/python/swig/yap4py
    #   DEPENDS   ${el} )
    #list(APPEND ${list} ${${el}})
    #set(${list} ${${list}} CACHE INTERNAL "prolog dll files")
  endfunction(add_to_libgroup el list)


  function(add_to_corelibgroup el list)
    # add_custom_command(  TARGET ${el} POST_BUILD
    #   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${el}> $<TARGET_SONAME_FILE:${el}>  $<TARGET_LINKER_FILE:${el}>  ${CMAKE_BINARY_DIR}/packages/python/swig/yap4py
    #   DEPENDS   ${el} )
    list(APPEND ${list} ${${el}})
    set(${list} ${${list}} CACHE INTERNAL "prolog dll files")
  endfunction(add_to_corelibgroup el list)

else()
  function(add_to_dir list output)
  endfunction(add_to_dir list output)

endif()

if (ANDROID_OLD)
  macro(MY_add_custom_target)
  endmacro()
else ()
  macro(MY_add_custom_target)
    add_custom_target(${ARGN})
  endmacro()
endif ()

if (WIN32)
  macro(add_component arg1)
    add_library(${arg1} OBJECT ${ARGN})
  endmacro()
else ()
  macro(add_component arg1)
    add_library(${arg1} OBJECT ${ARGN})
  endmacro()
endif ()
macro(add_corelib arg1)
  add_library(${arg1} SHARED ${ARGN})
  add_to_corelibgroup(${arg1} YAP_DLLS)
endmacro()

if (ANDROID_OLD)
  macro(MY_add_dependencies)
  endmacro()
else ()
  macro(MY_add_dependencies)
    add_dependencies(${ARGN})
  endmacro()
endif ()

if (ANDROID_OLD)
  macro(MY_add_library)
  endmacro()
else ()
  macro(MY_add_library)
    add_library(${ARGN})
  endmacro()
endif ()

if (ANDROID)
  macro(MY_add_subdirectory)
  endmacro()
else ()
  macro(MY_add_subdirectory)
    add_subdirectory(${ARGN})
  endmacro()
endif ()

if (ANDROID_OLD)
  macro(MY_include)
  endmacro()
else ()
  macro(MY_include)
    include(${ARGN})
  endmacro()
endif ()

if (ANDROID)
  macro(MY_install)
  endmacro()
else ()
  macro(MY_install)
    install(${ARGN})
  endmacro()
endif ()

if (ANDROID_OLD)
  macro(MY_set_target_properties)
  endmacro()
else ()
  macro(MY_set_target_properties)
    set_target_properties(${ARGN})
  endmacro()
endif ()

if (ANDROID_OLD)
  macro(MY_target_link_libraries)
  endmacro()
else ()
  macro(MY_target_link_libraries)
    target_link_libraries(${ARGN})
  endmacro()
endif ()


# where we have most scripts
# set path to additional CMake modules

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(disallow)

disallow_intree_builds()

# set(CMAKE_BUILD_TYPE  Debug)

if ($ENV{CONDA_BUILD}x STREQUAL "1x" )
  set(CMAKE_LIBRARY_ARCHITECTURE $ENV{PREFIX})
  set(CMAKE_PREFIX_PATH $ENV{PREFIX})
  set( R_COMMAND "$ENV{R}")
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS CONDA_BUILD=1)
  set(YAP_IS_MOVABLE 1)

endif()

ADD_CUSTOM_TARGET(run_install COMMAND ${CMAKE_MAKE_PROGRAM} install)

list (APPEND CMAKE_REQUIRED_INCLUDES
  ${PREFIX}/include
  ${SYS_PREFIX}/include
  )



set(prefix ${CMAKE_INSTALL_PREFIX}) #BINDIR})
set(docdir ${CMAKE_INSTALL_PREFIX}/share/docs) #MANDIR})

set(YAP_ROOTDIR ${prefix})
# erootdir -> rootdir
# libdir defined above
set(YAP_DATADIR  ${CMAKE_INSTALL_FULL_DATADIR})
set(YAP_INCLUDEDIR  ${CMAKE_INSTALL_FULL_INCLUDEDIR}/Yap)
set(YAP_BINDIR  ${CMAKE_INSTALL_FULL_BINDIR})
set(YAP_LIBDIR  ${CMAKE_INSTALL_FULL_LIBDIR})
set(YAP_DLLDIR  ${CMAKE_INSTALL_FULL_LIBDIR}/Yap)
set(YAP_PLDIR  ${CMAKE_INSTALL_FULL_DATADIR}/Yap)

set(YAP_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR}/Yap)
set(YAP_INSTALL_LIBDIR ${CMAKE_INSTALL_LIBDIR}/Yap)
if (ANDROID)
  set(YAP_INSTALL_DATADIR ${CMAKE_SOURCE_DIR}/../yaplib/src/generated/assets/Yap)
else()
  set(YAP_INSTALL_DATADIR ${CMAKE_INSTALL_DATADIR}/Yap)
endif()
#
#
# include( Sources  )
#
# include( Model   )

include(cudd )

set(pl_library "" CACHE INTERNAL "prolog library files")
set(pl_os_library "" CACHE INTERNAL "prolog os files")
set(pl_boot_library "" CACHE INTERNAL "prolog boot files")


find_package(GMP)
# GMP_FOUND         - true if GMP/MPIR was found
# GMP_INCLUDE_DIRS  - include search path
# GMP_LIBRARIES      - libraries to link with
# GMP_LIBRARY_    #add_executable(test ${SOURCES})

list(APPEND YAP_SYSTEM_OPTIONS big_numbers)

if (GMP_INCLUDE_DIRS)
  #config.h needs this (TODO: change in code latter)
  include_directories(${GMP_INCLUDE_DIRS})
endif ()


# - Find the readline library
# This module defines
#  READLINE_INCLUDE_DIR, path to readline/readline.h, etc.
#  READLINE_LIBRARIES, the libraries required to use READLINE.
#  READLINE_FOUND, If false, do not try to use READLINE.
# also defined, but not for general use are
# READLINE_readline_LIBRARY, where to find the READLINE library.
# READLINE_ncurses_LIBRARY, where to find the ncurses library [might not be defined]

if (ANDROID)
  option (WITH_READLINE "use Readline" OFF)
else()

  include(FindReadline)

  option (WITH_READLINE "use Readline" ON)
  # include subdirectories configuration
  ## after we have all functionality in
  #
  # ADD_SUBDIRECTORY(console/terminal)

  if (READLINE_FOUND AND READLINE_INCLUDE_DIR)
    List(APPEND YAP_SYSTEM_OPTIONS readline)
    # required for configure
    include_directories( ${READLINE_INCLUDE_DIR}
      ${READLINE_INCLUDE_DIR}/readline
      )
      else()
      set (READLINE_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
  endif ()
endif()

include_directories(
  ${CMAKE_SOURCE_DIR}/H
  ${CMAKE_SOURCE_DIR}/H/generated
  ${CMAKE_SOURCE_DIR}/include
  ${CMAKE_SOURCE_DIR}/os
  ${CMAKE_SOURCE_DIR}/OPTYap
  ${CMAKE_SOURCE_DIR}/utf8proc
  ${CMAKE_SOURCE_DIR}/JIT/HPP
  ${GMP_INCLUDE_DIRS}
  ${READLINE_INCLUDE_DIR}
  ${CMAKE_BINARY_DIR}
  ) 

add_subdirectory( H )

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
set(YAP_FOUND ON)

set(YAP_MAJOR_VERSION 6)
set(YAP_MINOR_VERSION 5)
set(YAP_PATCH_VERSION 0)

set(YAP_FULL_VERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_FVERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_NUMERIC_VERSION
  ${YAP_MAJOR_VERSION}*10000+${YAP_MINOR_VERSION}*100+${YAP_PATCH_VERSION})
set(MYDDAS_VERSION MYDDAS-0.9.1)

site_name(YAP_SITE)


if (WIN32)
  set(YAP_ARCH $ENV{PROCESSOR_ARCHITECTURE})
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS "MS_WIN64=1")
  set(YAP_SYSTEM_OPTIONS "windows " ${YAP_SYSTEM_OPTIONS})
endif ()
if (UNIX)
  find_program(UNAME uname)
  execute_process(
    COMMAND ${UNAME} -m
    OUTPUT_VARIABLE YAP_ARCH OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(YAP_SYSTEM_OPTIONS "unix " ${YAP_SYSTEM_OPTIONS})
endif ()


#
set(MIN_STACKSPACE 1024*SIZEOF_INT_P)
set(MIN_HEAPSPACE 32*1024*SIZEOF_INT_P)
set(MIN_TRAILSPACE 512*SIZEOF_INT_P)
set(DEF_STACKSPACE 0)
set(DEF_HEAPSPACE 0)
set(DEF_TRAILSPACE 0)

# dd_definitions (-D)

## don't touch these opts
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS DEPTH_LIMIT=1;COROUTINING=1;RATIONAL_TREES=1)

# inform we are compiling YAP
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS "_YAP_NOT_INSTALLED_=1;HAVE_CONFIG_H=1;_GNU_SOURCE=1")

# Compilation model
#  target_compile_definitions(libYap PUBLIC  _XOPEN_SOURCE=700 )

#add_definitions( -Wall  -Wstrict-prototypes -Wmissing-prototypes)

# Model Specific
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS $<$<CONFIG:Debug>:DEBUG=1>)


#ensure cells are properly aligned in code
set(ALIGN_LONGS 1)

#ensure best access to slots in environments
set(MSHIFTOFFS 1)

set(C_COMPILER CMAKE_C_COMPILER_ID)

if (${C_COMPILER} MATCHES "GNU")
  set(HAVE_GCC 1)
endif ()

# compatible compilers
if (${C_COMPILER} MATCHES "Clang")
  set(HAVE_GCC 1)
endif ()

if (${C_COMPILER} MATCHES "Intel")
  set(HAVE_GCC 1)
endif ()

# rpath stuff, hopefully it works
# use, i.e. don't skip the full RPATH for the build tree
#SET(CMAKE_SKIP_BUILD_RPATH  TRUE)


# when building, don't use the install RPATH already
## (but later on when installing)
#SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
#
## SET(CMAKE_INSTALL_FULL_RPATH ${CMAKE_BINARY_DIR})
#
## add the automatically determined parts of the RPATH
## which point to directories outside the build tree to the install RPATH
#SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH FAlSE)
#
#
## the RPATH to be used when installing, but only if it's not a system directory
#LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${YAP_INSTALL_LIBDIR}" isSystemDir)
# IF("${isSystemDir}" STREQUAL "-1")
#    SET(CMAKE_INSTALL_RPATH  ${YAP_INSTALL_LIBDIR})
#ENDIF("${isSystemDir}" STREQUAL "-1")
#
IF(NOT WIN32 AND NOT APPLE)
  LIST(APPEND CMAKE_INSTALL_RPATH \\$ORIGIN/../lib/Yap)
  LIST(APPEND CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_FULL_LIBDIR})
  LIST(APPEND CMAKE_INSTALL_RPATH \\$ORIGIN/../lib)
  LIST(APPEND CMAKE_INSTALL_RPATH \\$ORIGIN/../../../lib)
ELSE()
  LIST(APPEND CMAKE_INSTALL_RPATH @loader_path/../lib/Yap)
  LIST(APPEND CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_FULL_LIBDIR})
  LIST(APPEND CMAKE_INSTALL_RPATH @loader_path/../lib)
  LIST(APPEND CMAKE_INSTALL_RPATH @loader_path/../../../lib)
ENDIF()

set(YAP_STARTUP startup.yss)
set(YAP_SOURCEBOOT boot.yap   )


#endif()

# Model Specific
if (HAVE_GCC)
  set_property(DIRECTORY APPEND PROPERTY COMPILE_OPTIONS -Wall)
  if (${C_COMPILER} MATCHES "GNU")
    set_property(DIRECTORY APPEND PROPERTY COMPILE_OPTIONS $<$<CONFIG:Release>:-O3;-fomit-frame-pointer;-fstrict-aliasing;-freorder-blocks;-fsched-interblock>)
    set_property(DIRECTORY APPEND PROPERTY COMPILE_OPTIONS $<$<CONFIG:Debug>:-g3 -fdebug-cpp>)
  else ()
    set_property(DIRECTORY APPEND PROPERTY COMPILE_OPTIONS $<$<CONFIG:Release>:-O3;-fstrict-aliasing;-freorder-blocks;-fsched-interblock>)
    set_property(DIRECTORY APPEND PROPERTY COMPILE_OPTIONS $<$<CONFIG:Debug>:-glldb -fdebug-macro>)
  endif ()
  set_property(DIRECTORY APPEND PROPERTY COMPILE_OPTIONS -fexceptions)
endif ()

# set_property( DIRECTORY APPEND_STRING PROPERTY -fsanitize=memory;-fsanitize-memory-track-origins=2)
if (HAVE_GCC)
  # replace instructions codes by the address of their code
  if (WITH_THREADED_CODE)
    set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS THREADED_CODE=1)
  endif (WITH_THREADED_CODE)
endif (HAVE_GCC)

#
#option (YAP_SWI_IO ON)

#TODO:
#TODO:
if (WITH_CALL_TRACER)
  list(APPEND YAP_SYSTEM_OPTIONS "call_tracer " ${YAP_SYSTEM_OPTIONS})
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS $<$<CONFIG:Debug>:LOW_LEVEL_TRACER=1>)
endif (WITH_CALL_TRACER)

set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS UTF8PROC=1)

set_property(SOURCE ${LIBYAP_SOURCES} APPEND PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1)


IF (WITH_PYTHON)
  include(python )
ENDIF (WITH_PYTHON)


IF (WITH_R)
  find_host_package(LibR)
  add_subDIRECTORY(packages/real)
ENDIF (WITH_R)


include(Sources)


ADD_SUBDIRECTORY(OPTYap)
ADD_SUBDIRECTORY(os)
ADD_SUBDIRECTORY(library/dialect/swi/fli)
ADD_SUBDIRECTORY(CXX)

ADD_SUBDIRECTORY(pl)

ADD_SUBDIRECTORY(library)

ADD_SUBDIRECTORY(swi/library)

add_subDIRECTORY(utf8proc )



if(ANDROID)

  set(CXX_SWIG_OUTDIR ${CMAKE_BINARY_DIR}/packages/swig/android)
  add_subdirectory(packages/swig/android)
  add_definitions(-DMYDDAS=1 -DEMBEDDED_MYDDAS=1 -DMYDDAS_SQLITE3=1 -DEMBEDDED_SQLITE3=1)

else()
  add_definitions(-DMYDDAS=1 -DEMBEDDED_MYDDAS=1 -DMYDDAS_SQLITE3=1 )

  if (MYSQL_FOUND)
    add_definitions( -DMYDDAS_MYSQL=1)
  endif ()

  if (ODBC_FOUND)
    add_definitions( -DMYDDAS_ODBC=1)
  endif ()

  if (POSTGRES_FOUND)
    add_definitions( -DMYDDAS_POSTGRES=1)
  endif()
  #utf-8 is not aPconn option
  # we use the nice UTF-8 package
  #available at the Julia project

endif()

add_subDIRECTORY( packages/myddas )
add_subDIRECTORY( packages/clpqr )


List(APPEND YLIBS $<TARGET_OBJECTS:libOPTYap>)
List(APPEND YLIBS $<TARGET_OBJECTS:libYAPOs>)
List(APPEND YLIBS $<TARGET_OBJECTS:utf8proc>)
List(APPEND YLIBS $<TARGET_OBJECTS:myddas>)
List(APPEND YLIBS $<TARGET_OBJECTS:libswi>)
if (WIN32 OR ANDROID)
  List(APPEND YLIBS $<TARGET_OBJECTS:YAP++>)
  if (WIN32 AND WITH_PYTHON AND PYTHON_LIBRARIES)
    List(APPEND YLIBS $<TARGET_OBJECTS:Py4YAP>)
    list (APPEND WINDLLS ${PYTHON_LIBRARIES})
  endif ()
  if (ANDROID)
    List(APPEND YLIBS $<TARGET_OBJECTS:YAPsqlite3>)
    List(APPEND YLIBS $<TARGET_OBJECTS:DROID>)
    set(ANDROID_LIBRARIES ${CMAKE_SOURCE_DIR}/../sqlite-android/jni/${ANDROID_ABI}/libsqliteX.so  android log)
    link_directories(${CMAKE_SOURCE_DIR}/../sqlite-android/jni/${ANDROID_ABI})

  endif ()
endif ()


add_library( # Sets the name of the library.
  libYap

  # Sets the library as a shared library.
  SHARED
  ${ENGINE_SOURCES}
  ${C_INTERFACE_SOURCES}
  ${STATIC_SOURCES}
  # cmake object libraries
  ${YLIBS}
  )

target_link_libraries(libYap
  m
  ${GMP_LIBRARIES}
  ${READLINE_LIBRARIES}
  ${ANDROID_LIBRARIES}
  ${WINDLLS}
  )
if (PYTHON_LIBRARIES)
  target_link_libraries(libYap
    ${PYTHON_LIBRARIES}
    )
endif()

set_target_properties(libYap
  PROPERTIES OUTPUT_NAME Yap
  )


## define system

# Optional libraries that affect compilation
#


set(CMAKE_TOP_BINARY_DIR ${CMAKE_BINARY_DIR})
set(YAP_PL_SRCDIR ${CMAKE_SOURCE_DIR}/pl)

set(YAP_YAPLIB libYap${CMAKE_SHARED_LIBRARY_SUFFIX})

string(TIMESTAMP YAP_TIMESTAMP)

string(SUBSTRING ${CMAKE_SHARED_LIBRARY_SUFFIX} 1 -1 SO_EXT)

set_property(DIRECTORY PROPERTY CXX_STANDARD 11)


if (ANDROID)
  include_directories(CXX ${CMAKE_SOURCE_DIR}/../yaplib/generated/src/jni)
endif ()
include(Threads)
#
# include OS and I/o stuff
#
# convenience libraries
# OPTYap exports important flags
#
list(APPEND YAP_SYSTEM_OPTIONS "thread support")

set_target_properties(libYap
  PROPERTIES OUTPUT_NAME Yap
  )
MY_set_target_properties(libYap
  PROPERTIES VERSION ${YAP_FULL_VERSION}
  SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
  )


if (PYTHON_LIBRARIES)
# file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/packages/python/swig/yap4py)
include(FindPythonModule)

find_python_module(wheel)
find_python_module(setuptools)

if (PY_WHEEL AND PY_SETUPTOOLS)
  add_subdirectory(packages/python/swig)

  find_python_module(jupyter)
  if (PY_JUPYTER)
    add_subdirectory(packages/python/yap_kernel)
  else()
    message("will not compile yap_kernel, as jupyter=${PY_JUPYTER};wheel=${PY_WHEEL};setuptools=${PY_SETUPTOOLS}")
  ENDIF ()

endif()
endif()


message(STATUS "Building YAP packages version ${YAP_VERSION}")


if (NOT WIN32)
  set(BUILD_SHARED_LIBS ON)
endif ()


if (WITH_JIT)
  add_subDIRECTORY(JIT)
endif (WITH_JIT)


if (WITH_XML2 OR WITH_RAPTOR)

  add_subDIRECTORY(packages/raptor)

endif()


if (WITH_XML)

  add_subDIRECTORY(packages/xml)

endif ()


IF (WITH_CLPBN)
  add_subDIRECTORY(packages/CLPBN)
ENDIF (WITH_CLPBN)

IF (WITH_CPLINT)
  add_subDIRECTORY(packages/cplint)
ENDIF (WITH_CPLINT)


IF (WITH_DOCS)
  get_target_property(YAP_SOURCES libYap SOURCES)

  add_subDIRECTORY(docs)


  find_host_package(Doxygen)

ENDIF (WITH_DOCS)

# add_subDIRECTORY (packages/cuda)

if (WITH_GECODE)
  add_subDIRECTORY(packages/gecode)
endif ()

if (WITH_LBFGS)
  add_subDIRECTORY(packages/yap-lbfgs)
endif ()




if (WITH_JAVA)
  #detect java setup, as it is shared between different installations.

  find_package(Java COMPONENTS Development Runtime)
  # find_package(Java COMPONENTS Development)
  # find_package(Java COMPONENTS Runtime)
  #find_package(JavaLibs)


  if (Java_Development_FOUND)

    set (STD_CMAKE_FIND_FRAMEWORK ${CMAKE_FIND_FRAMEWORK})
    set (CMAKE_FIND_FRAMEWORK LAST) # CMake will find the Java returned by /usr/libexec/java_home.

    macro_optional_find_package(JNI ON)


    set (CMAKE_FIND_FRAMEWORK ${STD_CMAKE_FIND_FRAMEWORK})

    if (JNI_FOUND)

      get_filename_component(JAVA_HOME ${JAVA_INCLUDE_PATH} DIRECTORY)

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



      get_filename_component ( JAVA_AWT_DIR ${JAVA_AWT_LIBRARY} DIRECTORY)
      get_filename_component ( JAVA_JNI_DIR ${JAVA_JVM_LIBRARY} DIRECTORY)
      list(APPEND CMAKE_INSTALL_RPATH ${JAVA_AWT_DIR};${JAVA_JNI_DIR})
      if (APPLE)
	set(CMAKE_MACOSX_RPATH 1)
	find_library (JLI jli ${JAVA_AWT_DIR}/jli)
	#find_library (JAL JavaApplicationLauncher FRAMEWORK ONLY PATH /System/Library/PrivateFrameworks)
	#find_library (JL JavaLaunching FRAMEWORK ONLY PATH /System/Library/PrivateFrameworks)
	list(APPEND CMAKE_INSTALL_RPATH ${JAVA_AWT_DIR}/jli)
	list(APPEND JNI_LIBRARIES  ${JLI};${JAL};${JL})
      endif()

    endif (JNI_FOUND)


  endif (Java_Development_FOUND)

  add_subDIRECTORY(packages/jpl)

endif(WITH_JAVA)

if (WITH_CUDD)

  add_subDIRECTORY(packages/bdd)

endif()

if (WITH_PROBLOG)

  add_subDIRECTORY(packages/ProbLog)

endif()

if (WITH_MINISAT)

  add_subDIRECTORY(packages/swi-minisat2)

endif()

if (WITH_CLPQR)

  add_subDIRECTORY(packages/clpqr)

endif()


#todo: use cmake target builds
# option (USE_MAXPERFORMANCE
#   "try using the best flags for specific architecture" ON)

# option (USE_MAXMEMORY
#   "try using the best flags for using the memory to the most" ON)
#TODO: check MAXMEMORY

#TODO: use cmake target builds
# option (USE_DEBUGYAP
#   "enable C-debugging for YAP" ON)

#TODO: use cmake arch/compiler
# option (USE_CYGWIN
#   "use cygwin library in WIN32" ON)

#TODO:


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


if (WIN32)

  if (MSVC)
    set(MSVC_RUNTIME "dynamic")
  ENDIF (MSVC)

  target_link_libraries(libYap wsock32 ws2_32 shlwapi)

endif (WIN32)

if (NOT ANDROID)
  add_executable(yap-bin ${CONSOLE_SOURCES})

  set_target_properties(yap-bin PROPERTIES OUTPUT_NAME yap)


  target_link_libraries(yap-bin libYap)

  install(TARGETS libYap yap-bin
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )

endif()

if (WITH_MPI)

  add_subDIRECTORY(library/lammpi)

  if (MPI_C_FOUND)

    macro_optional_add_subDIRECTORY(library/mpi)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${MPI_C_COMPILE_FLAGS} ")
    include_directories(${MPI_C_INCLUDE_PATH})
    target_link_libraries(yap-bin ${MPI_C_LIBRARIES})
    if (MPI_C_COMPILE_FLAGS)
      set_target_properties(yap-bin PROPERTIES
	COMPILE_FLAGS "${MPI_C_COMPILE_FLAGS}")
    endif (MPI_C_COMPILE_FLAGS)

    if (MPI_C_LINK_FLAGS)
      set_target_properties(yap-bin PROPERTIES
	LINK_FLAGS "${MPI_C_LINK_FLAGS}")
    endif ()
  endif (MPI_C_FOUND)

endif(WITH_MPI)

##   add_subDIRECTORY(utils)

#

install(FILES ${INCLUDE_HEADERS} ${CONFIGURATION_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/Yap )

macro_display_feature_log()
if (POLICY CMP0058)
  cmake_policy(SET CMP0058 NEW)
endif (POLICY CMP0058)

include(Config )


feature_summary(DESCRIPTION "Packages found:"
  WHAT  PACKAGES_FOUND
  )
feature_summary(DESCRIPTION "Packages not found:"
  WHAT  PACKAGES_NOT_FOUND
  )
