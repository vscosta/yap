# value of 3.4.0 or lower.
cmake_minimum_required(VERSION 3.4)

  if(COMMAND cmake_policy)
      cmake_policy(SET CMP0003 NEW)
    endif(COMMAND cmake_policy)

# Sets the version of CMake required to build the native
# library. You should either keep the default value or  pass a
# value of 3.4.0 or lower.
include(CMakeToolsHelpers OPTIONAL)

project( YAP )

include(FeatureSummary)

set(YAP_APP_DIR "${CMAKE_SOURCE_DIR}/../..")
   cmake_policy(VERSION 3.4)

set(
  CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}"
  "${CMAKE_SOURCE_DIR}/cmake"
  )

include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(CMakeDependentOption)
include(MacroOptionalAddSubdirectory)
include(MacroOptionalFindPackage)
include(FeatureSummary)
include(FindPackageHandleStandardArgs)
include (GNUInstallDirs)

# Creates and names a library, sets it as either STATIC
# or SHARED, and provides the relative paths to its source code.z
# You can define  libraries, and CMake builds it for you.
# Gradle automatically packages shared libraries with your APK.

#cross-compilation support
# Search packages for host system instead of packages for target system
# in case of cross compilation define these macro by toolchain file
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

option(BUILD_SHARED_LIBS "Build shared library" ON)
set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)
set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

#begining stuff for top CMakeLists

include (MacroLogFeature)

if (POLICY CMP0042)
    cmake_policy( SET CMP0042 NEW)
endif()

function(add_to_group list output)
     set (tmp ${${output}})
     foreach (path  ${${list}})
         get_source_file_property(path   ${path}  LOCATION)
         list( APPEND tmp ${path})
     endforeach ()
    set (${output} ${tmp} CACHE INTERNAL "prolog library files")
endfunction(add_to_group list output )

function(add_to_libgroup el list)
  add_custom_command(  TARGET ${el} POST_BUILD
COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${el}>  ${CMAKE_BINARY_DIR}/packages/python/swig/yap4py
  DEPENDS   ${el} )
    list( APPEND ${list} ${${el}})
    set (${list} ${${list}} CACHE INTERNAL "prolog dll files")
endfunction(add_to_libgroup el list )


function(add_to_corelibgroup el list)
  add_custom_command(  TARGET ${el} POST_BUILD
COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${el}> $<TARGET_SONAME_FILE:${el}>  $<TARGET_LINKER_FILE:${el}>  ${CMAKE_BINARY_DIR}/packages/python/swig/yap4py
  DEPENDS   ${el} )
    list( APPEND ${list} ${${el}})
    set (${list} ${${list}} CACHE INTERNAL "prolog dll files")
endfunction(add_to_corelibgroup el list )


if (ANDROID_OLD)
    macro ( MY_add_custom_target)
    endmacro()
else()
    macro ( MY_add_custom_target )
        add_custom_target (${ARGN})
    endmacro()
endif()

if (WIN32)
  macro ( add_component arg1 )
    add_library ( ${arg1} OBJECT ${ARGN})
  endmacro()
else()
  macro ( add_component arg1 )
    add_library ( ${arg1} OBJECT ${ARGN})
  endmacro()
endif()
macro ( add_lib arg1 )
  add_library ( ${arg1} SHARED ${ARGN})
  add_to_libgroup( ${arg1} YAP_DLLS )
endmacro()
macro ( add_corelib arg1 )
  add_library ( ${arg1} SHARED ${ARGN})
  add_to_corelibgroup( ${arg1} YAP_DLLS )
endmacro()

if (ANDROID_OLD)
    macro ( MY_add_dependencies)
    endmacro()
else()
    macro ( MY_add_dependencies )
        add_dependencies (${ARGN})
    endmacro()
endif()

if (ANDROID_OLD)
    macro ( MY_add_library)
    endmacro()
else()
    macro ( MY_add_library )
        add_library (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_add_subdirectory)
    endmacro()
else()
    macro ( MY_add_subdirectory )
        add_subdirectory (${ARGN})
    endmacro()
endif()

if (ANDROID_OLD)
    macro ( MY_include)
    endmacro()
else()
    macro ( MY_include )
        include (${ARGN})
    endmacro()
endif()

if (ANDROID)
    macro ( MY_install)
    endmacro()
else()
    macro ( MY_install )
        install (${ARGN})
    endmacro()
endif()

if (ANDROID_OLD)
    macro ( MY_set_target_properties)
    endmacro()
else()
    macro ( MY_set_target_properties )
        set_target_properties (${ARGN})
    endmacro()
endif()

if (ANDROID_OLD)
    macro ( MY_target_link_libraries)
    endmacro()
else()
    macro ( MY_target_link_libraries )
        target_link_libraries (${ARGN})
    endmacro()
endif()


# where we have most scripts
# set path to additional CMake modules

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(disallow)

disallow_intree_builds()

# set(CMAKE_BUILD_TYPE  Debug)

if (APPLE)
    set (MACOSX_RPATH ON)
endif (APPLE )
if(POLICY CMP0042)
    cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)
if(POLICY CMP0043)
    cmake_policy(SET CMP0043 NEW)
endif(POLICY CMP0043)


if (ANDROID)
    set ( prefix ${YAP_APP_DIR}/build )
    set ( datarootdir ${prefix}/assets )
else()
set ( prefix "${CMAKE_INSTALL_PREFIX}")
    set ( datarootdir ${prefix}/share )
endif()
    set ( libpl  ${datarootdir}/Yap)
set ( includedir "${prefix}/include")
set ( exec_prefix "${prefix}")
set ( libdir "${exec_prefix}/lib")
set ( datadir "${datarootdir}")
set ( mandir "${datarootdir}/man")
set ( bindir "${exec_prefix}/bin")
set ( docdir "${datarootdir}/doc/Yap")
set ( dlls "${exec_prefix}/lib/Yap")

set(YAP_ROOTDIR ${prefix})

# erootdir -> rootdir
# bindir defined above
# libdir defined above
set(YAP_LIBDIR "${dlls}")
set(YAP_SHAREDIR "${datarootdir}")
set(YAP_BINDIR "${bindir}")
set(YAP_INCLUDEDIR "${includedir}")
set(YAP_ROOTDIR "${prefix}")

#
#
# include( Sources NO_POLICY_SCOPE )
#
# include( Model NO_POLICY_SCOPE  )

include (cudd NO-POLICY-SCOPE)
include (java NO-POLICY-SCOPE)

set (pl_library "" CACHE INTERNAL "prolog library files" )
set (pl_os_library "" CACHE INTERNAL "prolog os files" )
set (pl_boot_library "" CACHE INTERNAL "prolog boot files" )

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


  find_package (GMP)
  # GMP_FOUND         - true if GMP/MPIR was found
  # GMP_INCLUDE_DIRS  - include search path
  # GMP_LIBRARIES      - libraries to link with
  # GMP_LIBRARY_    #add_executable(test ${SOURCES})

    list(APPEND YAP_SYSTEM_OPTIONS big_numbers)

# include subdirectories configuration
## after we have all functionality in
#
# ADD_SUBDIRECTORY(console/terminal)

    include(readline)
    if (READLINE_FOUND  )
        list( APPEND CMAKE_REQUIRED_LIBRARIES ${READLINE_LIBRARIES} )

        list(APPEND YAP_SYSTEM_OPTIONS readline)
    endif()

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
    ${CMAKE_SOURCE_DIR}/H/YapTerm.h
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
    ${CMAKE_SOURCE_DIR}/include/YapDefs.h
    ${CMAKE_SOURCE_DIR}/include/YapError.h
    ${CMAKE_SOURCE_DIR}/include/YapErrors.h
    ${CMAKE_SOURCE_DIR}/include/YapFormat.h
    ${CMAKE_SOURCE_DIR}/include/YapInterface.h
    ${CMAKE_SOURCE_DIR}/include/YapRegs.h
    ${CMAKE_SOURCE_DIR}/include/YapStreams.h
    ${CMAKE_SOURCE_DIR}/include/blobs.h
    ${CMAKE_SOURCE_DIR}/include/c_interface.h
    ${CMAKE_SOURCE_DIR}/include/clause_list.h
    ${CMAKE_SOURCE_DIR}/include/pl-types.h
    ${CMAKE_SOURCE_DIR}/include/udi.h
)

set (CONFIGURATION_HEADERS
${CMAKE_BINARY_DIR}/YapConfig.h
    ${CMAKE_BINARY_DIR}/YapTermConfig.h
    ${CMAKE_BINARY_DIR}/config.h
    ${CMAKE_BINARY_DIR}/cudd_config.h
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
	${CMAKE_SOURCE_DIR}/os/encoding.h
	${CMAKE_SOURCE_DIR}/os/fmemopen.h
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
set(YAP_FOUND ON)

set(YAP_MAJOR_VERSION 6)
set(YAP_MINOR_VERSION 3)
set(YAP_PATCH_VERSION 5)

set(YAP_FULL_VERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_FVERSION
  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
set(YAP_NUMERIC_VERSION
  ${YAP_MAJOR_VERSION}*10000+${YAP_MINOR_VERSION}*100+${YAP_PATCH_VERSION})
set(MYDDAS_VERSION MYDDAS-0.9.1)

site_name( YAP_SITE )


if (WIN32)
  set (YAP_ARCH $ENV{PROCESSOR_ARCHITECTURE})
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  "MS_WIN64=1" )
  set(YAP_SYSTEM_OPTIONS "windows " ${YAP_SYSTEM_OPTIONS})
endif()
if (UNIX)
  find_program (UNAME uname)
  execute_process (
    COMMAND ${UNAME} -m
    OUTPUT_VARIABLE YAP_ARCH OUTPUT_STRIP_TRAILING_WHITESPACE )
    set(YAP_SYSTEM_OPTIONS "unix " ${YAP_SYSTEM_OPTIONS})
endif()


#
set ( MIN_STACKSPACE 1024*SIZEOF_INT_P )
set ( MIN_HEAPSPACE 32*1024*SIZEOF_INT_P )
set ( MIN_TRAILSPACE 512*SIZEOF_INT_P )
set ( DEF_STACKSPACE 0 )
set ( DEF_HEAPSPACE 0 )
set ( DEF_TRAILSPACE 0 )

# option (RATIONAL_TREES "support infinite rational trees" ON)
# dd_definitions (-D)

## don't touch these opts
  set_property( DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS  DEPTH_LIMIT=1;COROUTINING=1;RATIONAL_TREES=1 )

# inform we are compiling YAP
  set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  "_YAP_NOT_INSTALLED_=1;HAVE_CONFIG_H=1;_GNU_SOURCE" )

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
    set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  THREADED_CODE=1)
  endif (WITH_THREADED_CODE)
endif (HAVE_GCC)

#
#option (YAP_SWI_IO ON)

OPTION (WITH_CALL_TRACER
"support for procedure-call tracing" ON)
#TODO:
if (WITH_CALL_TRACER)
    list (APPEND YAP_SYSTEM_OPTIONS "call_tracer " ${YAP_SYSTEM_OPTIONS})
set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS $<$<CONFIG:Debug>:LOW_LEVEL_TRACER=1> )
endif (WITH_CALL_TRACER)

set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  UTF8PROC=1)

include_directories ( utf8proc packages/myddas packages/myddas/sqlite3/src)

if (ANDROID)
    include_directories (
            packages/myddas/sqlite3/src/Android/jni/sqlite
            packages/myddas/sqlite3/src/Android/jni/sqlite/nativehelper
    )
endif (ANDROID)

set_property( SOURCE ${LIBYAP_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1)

add_definitions(-DUSE_MYDDAS=1 -DMYDDAS_SQLITE3=1)

if (MYSQL_FOUND)
    add_definitions(= -DMYDDAS_MYSQL=1)
endif()

if (ODBC_FOUND)
    add_definitions(= -DMYDDAS_ODBC=1)
endif()

if (MYSQL_POSTGRES)
    add_definitions(= -DMYDDAS_POSTGRES=1)
endif()


OPTION (WITH_SWIG " Enable SWIG interfaces to foreign languages"  ON)

IF (WITH_SWIG OR ANDROID)
    find_host_package (SWIG)
    # macro_log_feature (SWIG_FOUND "Swig"
    #         "Use SWIG Interface Generator "
    #         "http://www.swig.org" ON)
ENDIF (WITH_SWIG OR ANDROID)

option (WITH_PYTHON
        "Allow Python->YAP  and YAP->Python" ON)

IF (WITH_PYTHON)
    include(python NO_POLICY_SCOPE)
ENDIF (WITH_PYTHON)



    List (APPEND YLIBS    $<TARGET_OBJECTS:libOPTYap> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:libYAPOs> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:utf8proc> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:myddas> )
    List (APPEND YLIBS    $<TARGET_OBJECTS:libswi> )
    if (WIN32)
        List (APPEND YLIBS    $<TARGET_OBJECTS:Yapsqlite3> )
        List (APPEND YLIBS    $<TARGET_OBJECTS:YAP++> )
        if (WITH_PYTHON )
            List (APPEND YLIBS    $<TARGET_OBJECTS:Py4YAP> )
        endif()
    endif()




add_corelib( # Sets the name of the library.
        libYap

        # Sets the library as a shared library.
        SHARED

        ${ENGINE_SOURCES}
        ${C_INTERFACE_SOURCES}
        //${STATIC_SOURCES}
        # cmake object libraries
        ${YLIBS}
        )

if (WIN32)
    target_link_libraries(libYap ${WINDLLS})
    if (PYTHON_INCLUDE_DIRS AND PYTHON_LIBRARIES )
        target_link_libraries(libYap ${PYTHON_LIBRARIES})
    endif()
endif (WIN32)


  if (GMP_FOUND)
      #config.h needs this (TODO: change in code latter)
      include_directories( ${GMP_INCLUDE_DIRS} )
      target_link_libraries(libYap ${GMP_LIBRARIES} )
  endif (GMP_FOUND)



include (Sources)

set(YAP_STARTUP startup.yss)
## define system

  # Optional libraries that affect compilation
  #


set(CMAKE_TOP_BINARY_DIR ${CMAKE_BINARY_DIR})
set(YAP_PL_SRCDIR ${CMAKE_SOURCE_DIR}/pl)

set(YAP_YAPLIB libYap${CMAKE_SHARED_LIBRARY_SUFFIX})

string(TIMESTAMP YAP_TIMESTAMP)

string( SUBSTRING ${CMAKE_SHARED_LIBRARY_SUFFIX} 1 -1 SO_EXT )

set_property(DIRECTORY PROPERTY CXX_STANDARD 11)

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

set_target_properties(libYap
   PROPERTIES OUTPUT_NAME Yap
   )
     MY_set_target_properties(libYap
  PROPERTIES VERSION ${YAP_FULL_VERSION}
  SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
  )

# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${libdir};${dlls}" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${libdir};${dlls}")
ENDIF("${isSystemDir}" STREQUAL "-1")


include_directories (H
    H/generated
    include os OPTYap utf8proc JIT/HPP)
include_directories (BEFORE ${CMAKE_BINARY_DIR} ${CMAKE_TOP_BINARY_DIR})

if (ANDROID)
include_directories (CXX ${CMAKE_SOURCE_DIR}/../generated/src/jni)
endif()
include( Threads )
#
# include OS and I/o stuff
#
# convenience libraries
# OPTYap exports important flags
#
list(APPEND YAP_SYSTEM_OPTIONS "thread support")

#utf-8 is not an option
# we use the nice UTF-8 package
#available at the Julia project

 ADD_SUBDIRECTORY ( OPTYap )
 ADD_SUBDIRECTORY ( os )
ADD_SUBDIRECTORY ( packages/myddas )
ADD_SUBDIRECTORY ( utf8proc )
ADD_SUBDIRECTORY ( library/dialect/swi/fli )
ADD_SUBDIRECTORY ( CXX )


add_subDIRECTORY ( H )

#bootstrap and saved state
add_subDIRECTORY ( pl  )

ADD_SUBDIRECTORY( library)

ADD_SUBDIRECTORY( swi/library "swiLibrary" )

if (ANDROID)
    target_link_libraries(libYap android log)

endif ()

set_target_properties(libYap
      PROPERTIES OUTPUT_NAME Yap
  )

if (PYTHONLIBS_FOUND AND SWIG_FOUND)
    add_subdirectory(packages/python/swig)

    include(FindPythonModule)

    find_python_module( jupyter )

    if (PY_JUPYTER)
        add_subdirectory(packages/python/yap_kernel)
    ENDIF()
endif()


IF (SWIG_FOUND OR ANDROID)
    add_subDIRECTORY (packages/swig  NO_POLICY_SCOPE)
ENDIF()


message(STATUS "Building YAP packages version ${YAP_VERSION}")


if (NOT WIN32)
  set (BUILD_SHARED_LIBS ON)
endif()



option (WITH_JIT
  "just in Time Clause Compilation" OFF)
if (WITH_JIT)
  add_subDIRECTORY(JIT)
endif (WITH_JIT)

add_subDIRECTORY (packages/raptor)

add_subDIRECTORY (packages/xml)


OPTION (WITH_CLPBN " Enable the CLPBN and PFL probabilistic languages"  ON)

OPTION (WITH_CPLINT " Enable the cplint probabilistic language"   ON)

OPTION (WITH_HORUS " Enable the CLPBN and PFL probabilistic languages"  ON)

OPTION (WITH_HORUS " Enable documentation, requires doxygen-yap"  ON)

IF (WITH_CLPBN)
  add_subDIRECTORY (packages/CLPBN)
ENDIF(WITH_CLPBN)

IF (WITH_CPLINT)
    add_subDIRECTORY (packages/cplint)
  ENDIF(WITH_CPLINT)

  # please install doxygen for prolog first
  # git clone http://www.github.com/vscosta/doxygen-yap
  # cd doxygen-yap
  # mkdir -p build
  # cd build
  # make; sudo make install
  option (WITH_DOCS
    "generate YAP docs" OFF)

     IF (WITH_DOCS)
  add_subDIRECTORY (docs)
 ENDIF (WITH_DOCS)

# add_subDIRECTORY (packages/cuda)

option (WITH_GECODE
"interface gecode constraint solver" ON)
if (WITH_GECODE)
add_subDIRECTORY (packages/gecode)
endif()

add_subDIRECTORY (packages/real)


add_subDIRECTORY (packages/jpl)

add_subDIRECTORY (packages/bdd)

add_subDIRECTORY (packages/ProbLog)

add_subDIRECTORY (packages/swi-minisat2)

add_subDIRECTORY (packages/clpqr)


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

option (WITH_PRISM
  "use PRISM system in YAP" ON)
#TODO:


option (WITH_YAP_DLL
  "compile YAP as a DLL" ON)
#TODO:

option (WITH_YAP_STATIC
  "compile YAP statically" OFF)
#TODO:

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



if(WIN32)

  if(MSVC)
      set(MSVC_RUNTIME "dynamic")
  ENDIF(MSVC)

  target_link_libraries(libYap  wsock32 ws2_32 Shlwapi)

endif(WIN32)

add_executable (yap-bin ${CONSOLE_SOURCES})

set_target_properties (yap-bin PROPERTIES OUTPUT_NAME yap)


target_link_libraries(yap-bin libYap )

        install(TARGETS  libYap yap-bin
         RUNTIME DESTINATION ${bindir}
         LIBRARY DESTINATION ${libdir}
          ARCHIVE DESTINATION ${libdir}
       )


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

#



macro_display_feature_log()
if(POLICY CMP0058)
cmake_policy(SET CMP0058 NEW)
endif(POLICY CMP0058)

include(Config NO_POLICY_SCOPE)



feature_summary(WHAT ENABLED_FEATURES DISABLED_FEATURES
                INCLUDE_QUIET_PACKAGES
)
