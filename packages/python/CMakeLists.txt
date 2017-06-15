
#CHECK: PythonLibs, changed to work in WIN32

set (PYTHON_SOURCES python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c)

set (PYTHON_HEADERS  py4yap.h)

set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

include_directories( ${CMAKE_BINARY_DIR} ${CMAKE_SOURCE_DIR}/include )

if (WIN32)
add_library (Py4YAP OBJECT ${PYTHON_SOURCES} ${PYTHON_HEADERS})
add_library (YAPPython SHARED pyload.c  ${PYTHON_HEADERS} )
else()
add_library (Py4YAP SHARED ${PYTHON_SOURCES} ${PYTHON_HEADERS})
add_library (YAPPython SHARED pyload.c ${PYTHON_SOURCES} ${PYTHON_HEADERS})

# arithmetic hassle.
set_property(TARGET Py4YAP PROPERTY CXX_STANDARD 11)
set_property(TARGET Py4YAP PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(Py4YAP libYap ${PYTHON_LIBRARIES})
target_link_libraries(YAPPython Py4YAP)

   MY_install(TARGETS  Py4YAP
     LIBRARY DESTINATION ${libdir}
           RUNTIME DESTINATION ${dlls}
           ARCHIVE DESTINATION ${libdir} )
endif()


# arithmetic hassle.
set_property(TARGET YAPPython PROPERTY CXX_STANDARD 11)
set_property(TARGET YAPPython PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(YAPPython libYap ${PYTHON_LIBRARIES})

set_property( SOURCE ${PYTHON_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1 _GNU-SOURCE=1)

set (PYTHON_PL python.pl)

install(FILES python.pl DESTINATION ${libpl} )

add_to_group( pl_library PYTHON_PL )

#  configure_file ("setup.py.cmake"  "setup.py" )

# set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

#set_target_properties (YAPPython PROPERTIES PREFIX "")

    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${dlls}
      RUNTIME DESTINATION ${dlls}
      ARCHIVE DESTINATION ${dlls} )

