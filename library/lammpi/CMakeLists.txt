macro_optional_find_package (MPI OFF)
macro_log_feature (MPI_C_FOUND "mpi" 
  "MPI Distributed Parallel Programming" 
  "http://gmplib.org")
if (MPI_C_FOUND)
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
endif (MPI_C_FOUND)

