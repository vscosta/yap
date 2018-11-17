
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

IF (WITH_CLPBN)
add_subDIRECTORY (packages/CLPBN)
ENDIF(WITH_CLPBN)

IF (WITH_CPLINT)
add_subDIRECTORY (packages/cplint)
ENDIF(WITH_CPLINT)


#must be last
add_subDIRECTORY (packages/python)

OPTION (WITH_SWIG " Enable SWIG interfaces to foreign languages"  ON)
IF (WITH_SWIG)
add_subDIRECTORY (packages/swig)
ENDIF (WITH_SWIG)


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
         LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
          ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
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
# include subdirectories configuration
## after we have all functionality in
#
# ADD_SUBDIRECTORY(console/terminal)





macro_display_feature_log()
if(POLICY CMP0058)
cmake_policy(SET CMP0058 NEW)
endif(POLICY CMP0058)
