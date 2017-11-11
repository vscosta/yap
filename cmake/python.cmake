set (Python_ADDITIONAL_VERSIONS 3.7 3.6 ) #3.5 3.6 3.4 )


#  PYTHONLIBS_FOUND           - have the Python libs been found
#   PYTHON_LIBRARIES           - path to the python library
#   PYTHON_INCLUDE_PATH        - path to where Python.h is found (deprecated)
#   PYTHON_INCLUDE_DIRS        - path to where Python.h is found
#   PYTHON_DEBUG_LIBRARIES     - path to the debug library (deprecated)
#   PYTHONLIBS_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
##
#
if (WIN320)
    set (PYTHONLIBS_FOUND YES CACHE BOOL "MINGW/MSYS2" FORCE )
        set (PYTHON_LIBRARY C:/msys64/mingw64/lib/libpython3.6m.dll.a CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHON_LIBRARIES C:/msys64/mingw64/lib/libpython3.5m.dll.a CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHON_INCLUDE_PATH C:/msys64/mingw64/include/python3.5m CACHE PATH "MINGW/MSYS2" FORCE )
    set (PYTHON_INCLUDE_DIRS C:/msys64/mingw64/include/python3.5m CACHE PATH "MINGW/MSYS2" FORCE  )
    set (PYTHON_EXECUTABLE C:/msys64/mingw64/bin/python3.exe CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHONLIBS_VERSION_STRING 3.5 CACHE STRING "MINGW/MSYS2" FORCE )

else()
    find_package(PythonInterp)

    find_package(PythonLibs)


endif()

include_directories( BEFORE ${PYTHON_INCLUDE_DIRS} )

LIST( APPEND
CMAKE_REQUIRED_INCLUDES ${PYTHON_INCLUDE_DIRS}  ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIRS})

check_include_file(Python.h HAVE_PYTHON_H)

IF (PYTHONLIBS_FOUND)
  add_subDIRECTORY (packages/python)
ENDIF()
