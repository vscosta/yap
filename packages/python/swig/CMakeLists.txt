
# This is a CMake example for Python

INCLUDE(UseSWIG)

include(FindPythonModule)

list (APPEND pl_library ${CMAKE_CURRENT_SOURCE_DIR}/prolog/jupyter.yap ${CMAKE_CURRENT_SOURCE_DIR}/prolog/yapi.yap )
set (PYTHON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/yapi.py ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__init__.py ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__main__.py)


configure_file("setup.py.in" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
configure_file(../../swig/yap.i ${CMAKE_CURRENT_BINARY_DIR}/yap.i)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/YAP4PY.md" ${CMAKE_CURRENT_BINARY_DIR}/README)


INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/packages/python" "${CMAKE_SOURCE_DIR}/CXX")

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


file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/os)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/pl)

add_custom_target( YAP4PY ALL
#     COMMAND ${CMAKE_COMMAND} -E copy ${YAP_DLLS} ${CMAKE_BINARY_DIR}/startup.yss  ${CMAKE_CURRENT_BINARY_DIR}/yap4py
    COMMAND  ${PYTHON_EXECUTABLE} setup.py sdist bdist_wheel
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
DEPENDS  STARTUP ${PYTHON_SOURCES} ${PROLOG_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/setup.py ${SWIG_MODULE_Py2YAP_REAL_NAME} )

function(f_copy filename output )
    get_filename_component(base ${filename} NAME_WE)
    set(outfile ${output}/${base})
    file (COPY ${filename} DESTiNATION ${output})
    add_custom_command(
      OUTPUT ${outfile}
      COMMAND ${CMAKE_COMMAND} -E COPY ${filename} ${outfile}
      DEPENDS ${filename}
      )
    set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)
endfunction()


	  install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install --no-index -f dist yap4py
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})"
 DEPENDS Py4YAP ${CMAKE_BINARY_DIR}/${YAP_STARTUP} ${dlls} )


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
