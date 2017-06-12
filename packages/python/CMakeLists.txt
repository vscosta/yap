
#CHECK: PythonLibs, changed to work in WIN32

set (PYTHON_SOURCES python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c)

set (PYTHON_HEADERS  python.h)

set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

add_library (YAPPython0 OBJECT ${PYTHON_SOURCES})
if (WIN32)
add_library (YAPPython SHARED pyload.c )
endif()

# arithmetic hassle.
set_property(TARGET YAPPython PROPERTY CXX_STANDARD 11)
set_property(TARGET YAPPython PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(YAPPython libYap ${PYTHON_LIBRARY})

set_property( SOURCE ${PYTHON_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1 _GNU-SOURCE=1)

set (PYTHON_PL python.pl)

install(FILES python.pl DESTINATION ${libpl} )

add_to_group( pl_library PYTHON_PL )

#  configure_file ("setup.py.cmake"  "setup.py" )

# set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

#set_target_properties (YAPPython PROPERTIES PREFIX "")

IF(WIN32)
    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${libdir}
      RUNTIME DESTINATION ${bindir}
      ARCHIVE DESTINATION ${libdir} )
  else()
    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${dlls}
      RUNTIME DESTINATION ${dlls}
      ARCHIVE DESTINATION ${dlls} )
  endif()

