
#CHECK: PythonLibs, changed to work in WIN32

set (PYTHON_SOURCES python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c)

set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

message( ${GMP_INCLUDE_DIRS} )
include_directories( ${CMAKE_BINARY_DIR}
${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/os ${GMP_INCLUDE_DIRS} )

#talk to python.pl
add_lib(YAPPython  pyload.c ${PYTHON_HEADERS} )


if (WIN32)
add_library (Py4YAP OBJECT ${PYTHON_SOURCES} ${PYTHON_HEADERS})
else()
add_library(Py4YAP SHARED  ${PYTHON_SOURCES} ${PYTHON_HEADERS})

set_target_properties(
    Py4YAP
PROPERTIES VERSION ${YAP_FULL_VERSION}
SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
)

# arithmetic hassle.
target_link_libraries(Py4YAP YAPPython libYap ${PYTHON_LIBRARIES})


install(TARGETS  Py4YAP 
  RUNTIME DESTINATION ${bindir}
  LIBRARY DESTINATION ${libdir}
  ARCHIVE DESTINATION ${libdir}
  )


 endif()

target_link_libraries(YAPPython libYap)

# arithmetic hassle.
set_property(TARGET YAPPython PROPERTY CXX_STANDARD_REQUIRED ON)


set_property( SOURCE ${PYTHON_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1 _GNU_SOURCE=1)

set (PYTHON_PL python.pl)

install(FILES python.pl DESTINATION ${libpl} )

add_to_group(  PYTHON_PL pl_library )

set_target_properties (YAPPython PROPERTIES PREFIX "")

    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${dlls}
      RUNTIME DESTINATION ${dlls}
      ARCHIVE DESTINATION ${dlls} )
