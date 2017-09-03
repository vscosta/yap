
#CHECK: PythonLibs, changed to work in WIN32

set (PYTHON_SOURCES python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c)

set (PYTHON_HEADERS  py4yap.h)

set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

include_directories( ${CMAKE_BINARY_DIR} ${CMAKE_SOURCE_DIR}/include )

add_library (YAPPython SHARED pyload.c  ${PYTHON_HEADERS} )

if (WIN32)
add_library (Py4YAP OBJECT ${PYTHON_SOURCES} ${PYTHON_HEADERS})

target_link_libraries(YAPPython libYap ${PYTHON_LIBRARIES})

else()
add_library (Py4YAP SHARED ${PYTHON_SOURCES} ${PYTHON_HEADERS})

# arithmetic hassle.
set_property(TARGET Py4YAP PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(Py4YAP libYap ${PYTHON_LIBRARIES})

   MY_install(TARGETS  Py4YAP
     LIBRARY DESTINATION ${libdir}
           RUNTIME DESTINATION ${dlls}
           ARCHIVE DESTINATION ${libdir} )
endif()


# arithmetic hassle.
set_property(TARGET YAPPython PROPERTY CXX_STANDARD_REQUIRED ON)


set_property( SOURCE ${PYTHON_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1 _GNU_SOURCE=1)

set (PYTHON_PL python.pl)

install(FILES python.pl DESTINATION ${libpl} )

add_to_group( pl_library PYTHON_PL )

set_target_properties (YAPPython PROPERTIES PREFIX "")

    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${dlls}
      RUNTIME DESTINATION ${dlls}
      ARCHIVE DESTINATION ${dlls} )

