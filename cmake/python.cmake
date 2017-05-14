set (Python_ADDITIONAL_VERSIONS 3.7 3.6 3.5 3.6 3.4 )

find_package(PythonInterp)
find_package(PythonLibs)


#  PYTHONLIBS_FOUND           - have the Python libs been found
#   PYTHON_LIBRARIES           - path to the python library
#   PYTHON_INCLUDE_PATH        - path to where Python.h is found (deprecated)
#   PYTHON_INCLUDE_DIRS        - path to where Python.h is found
#   PYTHON_DEBUG_LIBRARIES     - path to the debug library (deprecated)
#   PYTHONLIBS_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
##
#




include_directories( BEFORE ${PYTHON_INCLUDE_DIRS} )

LIST( APPEND
  CMAKE_REQUIRED_INCLUDES ${PYTHON_INCLUDE_DIRS}  ${CMAKE_REQUIRED_INCLUDES})

check_include_file(Python.h HAVE_PYTHON_H)

IF (PYTHONLIBS_FOUND)
  add_subDIRECTORY (packages/python)
ENDIF()

if (PYTHONLIBS_FOUND AND SWIG_FOUND)
  add_subdirectory(packages/python/swig)
  include(FindPythonModule)

  find_python_module( jupyter )

  if (PY_JUPYTER)
    add_subdirectory(packages/python/yap_kernel)
  ENDIF()
endif()
