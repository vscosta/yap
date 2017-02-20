
option (WITH_PYTHON
"Allow Python->YAP  and YAP->Python" ON)


#   PYTHONLIBS_FOUND           - have the Python libs been found
#   PYTHON_LIBRARIES           - path to the python library
#   PYTHON_INCLUDE_PATH        - path to where Python.h is found (deprecated)
#   PYTHON_INCLUDE_DIRS        - path to where Python.h is found
#   PYTHON_DEBUG_LIBRARIES     - path to the debug library (deprecated)
#   PYTHONLIBS_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
#
#
IF (WITH_PYTHON)
    set (Python_ADDITIONAL_VERSIONS 3.5 3.6 3.4 3.3)

    find_package(PythonInterp)
   # find_package(PythonLibs)


 execute_process ( COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print( sysconfig.get_path( 'include' ) )"
OUTPUT_VARIABLE _ABS_PYTHON_INCLUDE_PATH
OUTPUT_STRIP_TRAILING_WHITESPACE )
get_filename_component ( ABS_PYTHON_INCLUDE_PATH ${_ABS_PYTHON_INCLUDE_PATH} ABSOLUTE )

    set ( PYTHON_INCLUDE_DIR
            ${ABS_PYTHON_INCLUDE_PATH}
            CACHE "PATH" "Directory with Python.h "
            )

    set ( PYTHON_INCLUDE_DIRS
            ${ABS_PYTHON_INCLUDE_PATH}
            CACHE "PATH" "Python.h Dir  (Deprecated)"
            )

            execute_process ( COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print( sysconfig.get_path( 'stdlib' ) )"
           OUTPUT_VARIABLE _ABS_PYTHON_SYSLIB_PATH
           OUTPUT_STRIP_TRAILING_WHITESPACE )

  set( _ABS_PYTHON_SYSLIB_PATH
  ${_ABS_PYTHON_SYSLIB_PATH}/../${CMAKE_SHARED_LIBRARY_PREFIX}python${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}m${CMAKE_SHARED_LIBRARY_SUFFIX} )
  message("${_ABS_PYTHON_SYSLIB_PATH}")
  get_filename_component ( ABS_PYTHON_SYSLIB_PATH ${_ABS_PYTHON_SYSLIB_PATH} ABSOLUTE )


    set ( PYTHON_LIBRARY
            ${ABS_PYTHON_SYSLIB_PATH}
            CACHE "FILEPATH" "Python Library"
            )
    set ( PYTHON_LIBRARIES
            ${PYTHON_LIBRARY}
            CACHE "FILEPATH" "Python Library (Deprecated)"
            )
        if ( (EXISTS PYTHON_LIBRARY) AND ( EXISTS ${PYTHON_INCLUDE_DIR}) )
    set ( PYTHONLIBS_FOUND ON)
else()
find_package(PythonLibs)
endif()

    macro_log_feature (PYTHONLIBS_FOUND "Python"
    "Use Python System"
  "http://www.python.org" FALSE)


  include_directories( BEFORE ${PYTHON_INCLUDE_DIR} )

  LIST( APPEND
   CMAKE_REQUIRED_INCLUDES ${PYTHON_INCLUDE_DIR}  ${CMAKE_REQUIRED_INCLUDES} )

  check_include_file(Python.h HAVE_PYTHON_H)


endif(WITH_PYTHON)
