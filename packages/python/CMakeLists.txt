#CHECK: PythonLibs

set (PYTHON_SOURCES
  python.c)

  macro_optional_find_package(PythonInterp ON)
  macro_optional_find_package (PythonLibs ON)
  macro_log_feature (PYTHONLIBS_FOUND "Python"
    "Use Python System"
    "http://www.python.org" FALSE)

if (PYTHONLIBS_FOUND)         #   PYTHONLIBS_FOUND           - have the Python libs been found
  #   PYTHON_LIBRARIES           - path to the python library
  #   PYTHON_INCLUDE_PATH        - path to where Python.h is found (deprecated)
  #   PYTHON_INCLUDE_DIRS        - path to where Python.h is found
  #   PYTHON_DEBUG_LIBRARIES     - path to the debug library (deprecated)
  #   PYTHONLIBS_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
  #
  #
  #
  # The Python_ADDITIONAL_VERSIONS variable can be used to specify a list
  # of version numbers that should be taken into account when searching
  # for Python.  You need to set this variable before calling
  # find_package(PythonLibs).
  #
  # If you'd like to specify the installation of Python to use, you should
  # modify the following cache variables:
  #
  # ::
  #
  #   PYTHON_LIBRARY             - path to the python library
  #   PYTHON_INCLUDE_DIR         - path to where Python.h is found
  #
  # If also calling find_package(PythonInterp), call find_package(PythonInterp)
  # first to get the currently active Python version by default with a consistent
  # version of PYTHON_LIBRARIES.

  add_library (libpython SHARED ${PYTHON_SOURCES})

  target_link_libraries(libpython libYap ${PYTHON_LIBRARIES})
  

  set_target_properties (libpython PROPERTIES PREFIX "")

  include_directories (${PYTHON_INCLUDE_DIRS})

  install(TARGETS  libpython
    LIBRARY DESTINATION ${dlls}
    )

  install(FILES python.pl
    DESTINATION ${libpl}
    )


endif (PYTHONLIBS_FOUND)
