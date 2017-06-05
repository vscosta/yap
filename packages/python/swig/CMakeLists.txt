
# This is a CMake example for Python

INCLUDE(NewUseSWIG)

include(FindPythonModule)

set (PROLOG_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yapi.yap ${CMAKE_CURRENT_SOURCE_DIR}/jupyter.yap)
set (PYTHON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yapi.py ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py ${CMAKE_CURRENT_SOURCE_DIR}/__main__.py)

file(RELATIVE_PATH RELATIVE_SOURCE ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR})

configure_file("setup.py.in" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/YAP4PY.md" ${CMAKE_CURRENT_BINARY_DIR}/README)

INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/CXX")

SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES CPLUSPLUS ON)
SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
SET_SOURCE_FILES_PROPERTIES(../../swiyap.i PROPERTIES SWIG_MODULE_NAME yap)
#SET_SOURCE_FILES_PROPERTIES(../../swi/yap.i PROPERTIES OUTPUT_NAME yap)

SWIG_ADD_LIBRARY(Py2YAP LANGUAGE python  SHARED SOURCES ../../swig/yap.i )
if (WIN32)
    SWIG_LINK_LIBRARIES(Py2YAP YAPPython libYap ${PYTHON_LIBRARIES} )
else()
    SWIG_LINK_LIBRARIES(  Py2YAP  libYap YAP++ YAPPython ${PYTHON_LIBRARIES} )
  endif()
  set_target_properties ( ${SWIG_MODULE_Py2YAP_REAL_NAME}
    PROPERTIES
    NO_SONAME ON
    OUTPUT_NAME _yap
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS YAPPython YAPPython YAP++
    )
  # inform we are compiling YAP
  # s used in MSYS
  #
  #
  #  INSTALL ( TARGETS  ${SWIG_MODULE_Py2YAP_REAL_NAME}
  #  RUNTIME  DESTINATION ${PYTHON_MODULE_PATH}
  #  	ARCHIVE DESTINATION ${PYTHON_MODULE_PATH}
  #  	LIBRARY DESTINATION ${PYTHON_MODULE_PATH}
  #  	)


  set (dlls $<TARGET_FILE:matrix>
    $<TARGET_FILE:regexp>
    $<TARGET_FILE:yap_rl>
    $<TARGET_FILE:tries>
    $<TARGET_FILE:itries>
    $<TARGET_FILE:sys>
    $<TARGET_FILE:yap_random>)
  if (TARGET real)
    list( APPEND dlls $<TARGET_FILE:real>
 )
endif()

set (PL ${pl_library} ${PROLOG_SOURCES} )

add_custom_target( YAP4PY_SETUP 
  COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py
  COMMAND ${CMAKE_COMMAND} -E  copy ${dlls}  ${CMAKE_BINARY_DIR}/libYap${CMAKE_SHARED_LIBRARY_SUFFIX} ${CMAKE_BINARY_DIR}/${YAP_STARTUP} ${PYTHON_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/yap4py 
  COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog
  COMMAND ${CMAKE_COMMAND} -E  copy ${PL} ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog 
  COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/pl
  COMMAND ${CMAKE_COMMAND} -E  copy ${pl_boot_library} ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/pl 
  COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/os
  COMMAND ${CMAKE_COMMAND} -E   copy ${pl_os_library} ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/os 
DEPENDS  STARTUP ${dlls} ${PYTHON_SOURCES} ${PROLOG_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/setup.py ${SWIG_MODULE_Py2YAP_REAL_NAME}  )

add_custom_target( YAP4PY ALL
  COMMAND  ${PYTHON_EXECUTABLE} setup.py sdist bdist_wheel
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
DEPENDS YAP4PY_SETUP)
  


install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install  --force --no-index -f dist yap4py
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})"
 DEPENDS Py4YAP ${CMAKE_BINARY_DIR}/${YAP_STARTUP} ${dlls} )

install(FILES ${PROLOG_SOURCES} DESTINATION ${libpl})



	  if (WITH_DOCS AND DOXYGEN_FOUND)

        set(CMAKE_SWIG_FLAGS -DDOXYGEN=${DOXYGEN_FOUND})

        # Run doxygen to only generate the xml
        add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
                COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc
                COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/Doxyfile.xml
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
		DEPENDS ${c_headers};${c_sources};${cpp_sources};${cpp_headers}
		)

        # generate .i from doxygen .xml
        add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
                COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/doxy2swig.py -n
                ${CMAKE_BINARY_DIR}/doc/xm11l/ftdi_8c.xml
                ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
                DEPENDS ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
                )
        add_custom_target(doc_i DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i)
        add_dependencies(${SWIG_MODULE_ftdi1_REAL_NAME} doc_i)

    ENDIF ()
