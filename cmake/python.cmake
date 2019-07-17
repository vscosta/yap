set (Python_ADDITIONAL_VERSIONS 3.7 3.6 ) #3.5 3.6 3.4 )


#  PYTHONLIBS_FOUND           - have the Python libs been found
#   PYTHON_LIBRARIES           - path to the python library
#   PYTHON_INCLUDE_PATH        - path to where Python.h is found (deprecated)
#   PYTHON_INCLUDE_DIRS        - path to where Python.h is found
#   PYTHON_DEBUG_LIBRARIES     - path to the debug library (deprecated)
#   PYTHONLIBS_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
##
#

 # ensure the script is in your cmake module path
if (WIN32 AND EXISTS(C:/msys64/mingw64))
    set (PYTHONLIBS_FOUND YES CACHE BOOL "MINGW/MSYS2" FORCE )
        set (PYTHON_LIBRARY C:/msys64/mingw64/lib/libpython3.6m.dll.a CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHON_LIBRARIES C:/msys64/mingw64/lib/libpython3.5m.dll.a CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHON_INCLUDE_PATH C:/msys64/mingw64/include/python3.5m CACHE PATH "MINGW/MSYS2" FORCE )
    set (PYTHON_INCLUDE_DIRS C:/msys64/mingw64/include/python3.5m CACHE PATH "MINGW/MSYS2" FORCE  )
    set (PYTHON_EXECUTABLE C:/msys64/mingw64/bin/python3.exe CACHE FILEPATH "MINGW/MSYS2" FORCE )
    set (PYTHONLIBS_VERSION_STRING 3.5 CACHE STRING "MINGW/MSYS2" FORCE )

elseif ($ENV{CONDA_BUILD}x STREQUAL "1x")


    set (PYTHONLIBS_FOUND YES CACHE BOOL "MINGW/MSYS2" FORCE )
set( PYTHON_EXECUTABLE $ENV{PYTHON} CACHE FILEPATH "MINGW/MSYS2" FORCE )
set( PYTHON_LIBRARIES  $ENV{PYTHON_LIBRARIES} CACHE FILEPATH "MINGW/MSYS2" FORCE )
set( PYTHON_INCLUDE_PATH  $ENV{PYTHON_INCLUDE_DIRS} CACHE PATH "MINGW/MSYS2" FORCE  )
set( PYTHON_INCLUDE_DIRS  $ENV{PYTHON_INCLUDE_DIRS} CACHE PATH "MINGW/MSYS2" FORCE   )
    set (PYTHONLIBS_VERSION_STRING $ENV{PY_VER} CACHE STRING "MINGW/MSYS2" FORCE )
 list (APPEND CMAKE_REQUIRED_INCLUDES
        ${PYTHON_INCLUDE_PATH})
else()
    find_package(PythonInterp)

    find_package(PythonLibs)
    execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; sysconfig.get_paths()['include']"
    OUTPUT_VARIABLE PYTHON_INCLUDE_PATH
  OUTPUT_STRIP_TRAILING_WHITESPACE)
 list (APPEND CMAKE_REQUIRED_INCLUDES
        ${PYTHON_INCLUDE_PATH})
endif()


IF (PYTHONLIBS_FOUND)
  add_subDIRECTORY (packages/python)
ENDIF()

IF (CMAKE_INSTALL_PREFIX MATCHES $ENV{HOME}.* )
 set (PYTHON_USER_INSTALL --user)
 ENDIF() 