
#CHECK: PythonLibs

set (PYTHON_SOURCES
  python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c )

set (PYTHON_HEADERS
  python.h)

#try to use Brew first
#set ( PYTHON_LIBRARY /Anaconda/lib/libpython2.7.dylib )
#set ( PYTHON_INCLUDE_DIR /Anaconda/include/python2.7 )
#set(  PYTHON_LIBRARY /usr/local/opt/python/Frameworks/Python.framework/Versions/Current/lib/libpython2.7.dylib) #             - path to the python library
#set(  PYTHON_INCLUDE_DIR /usr/local/opt/python/Frameworks/Python.framework/Versions/Current/include/python2.7 )  #      - path to where Python.h is found


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

 configure_file ("setup.py.cmake"  "setup.py" )

  target_link_libraries(libpython libYap ${PYTHON_LIBRARIES})

set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

add_custom_target ( YAPex ALL
        COMMAND ${PYTHON_EXECUTABLE} setup.py build -f
        DEPENDS yapex.py  )

  set_target_properties (libpython PROPERTIES PREFIX "")

  include_directories (${PYTHON_INCLUDE_DIRS})

  install(TARGETS  libpython
    LIBRARY DESTINATION ${dlls}
     ARCHIVE DESTINATION ${dlls}
   )

  install(FILES python.pl
    DESTINATION ${libpl}
    )

install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} ${SETUP_PY} install -f
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

add_subdirectory(yap_kernel)

endif (PYTHONLIBS_FOUND)
