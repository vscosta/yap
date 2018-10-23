

macro_optional_find_package (Threads OFF)
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


cmake_dependent_option (WITH_Pthread_Locking
  "use pthread locking primitives for internal locking" ON
  "WITH_Threads" OFF)

IF(WITH_Pthread_Lockin)
  set_DIRECTORY_properties(PROPERTIES APPEND COMPILE_DEFINITIONS USE_PTHREAD_LOCKING=1)
ENDIF()

CMAKE_DEPENDENT_OPTION (WITH_MAX_Threads 1024
  "maximum number of threads" "WITH_Threads" 1)

CMAKE_DEPENDENT_OPTION (WITH_MAX_Workers 64
  "maximum number of or-parallel workers" "WITH_MAX_Workers" 1)
