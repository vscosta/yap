if (WITH_MPI)

find_package(MPI)

if (NOT MPI_FOUND)

if (NOT MPI_C_COMPILER AND EXISTS /usr/lib64/openmpi/bin/mpicc)
    set(MPI_C_COMPILER /usr/lib64/openmpi/bin/mpicc)
endif()
if (NOT MPI_C_COMPILER AND EXISTS /usr/lib/openmpi/bin/mpicc)
    set(MPI_C_COMPILER /usr/lib/openmpi/bin/mpicc)
endif()
if (NOT MPI_CXX_COMPILER AND EXISTS /usr/lib64/openmpi/bin/mpicxx)
    set(MPI_CXX_COMPILER /usr/lib64/openmpi/bin/mpicxx)
endif()
if (NOT MPI_CXX_COMPILER AND EXISTS /usr/lib/openmpi/bin/mpicxx)
    set(MPI_CXX_COMPILER /usr/lib/openmpi/bin/mpicxx)
endif()

# try again
find_package(MPI)

endif()

  if (MPI_FOUND)
     if (MPI_C_FOUND)
  # === Variables ===
  #
  # This module will set the following variables per language in your
  # project, where <lang> is one of C, CXX, or Fortran:
  #
  # ::
  #
  #    MPI_<lang>_FOUND           TRUE if FindMPI found MPI flags for <lang>
  #    MPI_<lang>_COMPILER        MPI Compiler wrapper for <lang>
  #    MPI_<lang>_COMPILE_FLAGS   Compilation flags for MPI programs
  #    MPI_<lang>_INCLUDE_PATH    Include path(s) for MPI header
  #    MPI_<lang>_LINK_FLAGS      Linking flags for MPI programs
  #    MPI_<lang>_LIBRARIES       All libraries to link MPI programs against
  #
  # Additionally, FindMPI sets the following variables for running MPI
  # programs from the command line:
  #
  # ::
  #
  #    MPIEXEC                    Executable for running MPI programs
  #    MPIEXEC_NUMPROC_FLAG       Flag to pass to MPIEXEC before giving
  #                               it the number of processors to run on
  #    MPIEXEC_PREFLAGS           Flags to pass to MPIEXEC directly
  #                               before the executable to run.
  #    MPIEXEC_POSTFLAGS          Flags to pass to MPIEXEC after other flags
  #
  # === Usage ===
  #
  # To use this module, simply call FindMPI from a CMakeLists.txt file, or
  # run find_package(MPI), then run CMake.  If you are happy with the
  # auto- detected configuration for your language, then you're done.  If
  # not, you have two options:
  #
  # ::
  #
  #    1. Set MPI_<lang>_COMPILER to the MPI wrapper (mpicc, etc.) of your
  #       choice and reconfigure.  FindMPI will attempt to determine all the
  #       necessary variables using THAT compiler's compile and link flags.
  #    2. If this fails, or if your MPI implementation does not come with
  #       a compiler wrapper, then set both MPI_<lang>_LIBRARIES and
  #       MPI_<lang>_INCLUDE_PATH.  You may also set any other variables
  #       listed above, but these two are required.  This will circumvent
  #       autodetection entirely.
  #
  # When configuration is successful, MPI_<lang>_COMPILER will be set to
  # the compiler wrapper for <lang>, if it was found.  MPI_<lang>_FOUND
  # and other variables above will be set if any MPI implementation was
  # found for <lang>, regardless of whether a compiler was found.
  #
  # When using MPIEXEC to execute MPI applications, you should typically
  # use all of the MPIEXEC flags as follows:
  #
  # ::
  #
  #    ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} PROCS
  #      ${MPIEXEC_PREFLAGS} EXECUTABLE ${MPIEXEC_POSTFLAGS} ARGS
  #
  # where PROCS is the number of processors on which to execute the
  # program, EXECUTABLE is the MPI program, and ARGS are the arguments to
  # pass to the MPI program.
  #

set (MPI_YAP_SOURCES
  hash.c prologterms2c.c yap_mpi.c)
add_library(YAPmpi ${MPI_YAP_SOURCES})

 target_link_libraries(YAPmpi libYap ${MPI_C_LIBRARIES})

  include_directories( ${MPI_C_INCLUDE_DIRS} ${CMAKE_BINARY_DIR})

  set_property( TARGET YAPmpi  APPEND PROPERTY LINK_FLAGS  ${MPI_C_LINK_FLAGS})

set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS  HAVE_MPI_H=1;${MPI_C_COMPILE_DEFINITIONS})

add_compile_options( ${MPI_C_COMPILE_OPTIONS})

  install(TARGETS  YAPmpi
    RUNTIME DESTINATION ${CMAKE_INSTALL_DLLDIR}
      LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
    )
  set(HAVE_MPI_H 1)

  endif (MPI_C_FOUND)
endif (MPI_FOUND)

endif(WITH_MPI)
