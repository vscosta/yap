
#CHECK: PythonLibs

if (PYTHONLIBS_FOUND)

  
set (PYTHON_SOURCES
  python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c )

add_library (libpython SHARED ${PYTHON_SOURCES})

set (PYTHON_HEADERS
  python.h)

# configure_file ("setup.py.cmake"  "setup.py" )

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
