
# This is a CMake example for Python

INCLUDE(UseSWIG)

include(FindPythonModule)

list (APPEND pl_library ${CMAKE_CURRENT_SOURCE_DIR}/prolog/jupyter.yap ${CMAKE_CURRENT_SOURCE_DIR}/prolog/yapi.yap )
set (PYTHON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/yapi.py ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__init__.py ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__main__.py)

SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES CPLUSPLUS ON)
SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
SET_SOURCE_FILES_PROPERTIES(../../swiyap.i PROPERTIES SWIG_MODULE_NAME yap)
#SET_SOURCE_FILES_PROPERTIES(../../swi/yap.i PROPERTIES OUTPUT_NAME yap)



configure_file("setup.py.in" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/YAP4PY.md" ${CMAKE_CURRENT_BINARY_DIR}/README)

INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH}  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/include

           DEPENDS ${SWIG_SOURCES} YAP++
   	)

 

SWIG_ADD_LIBRARY(Py2YAP LANGUAGE python  SHARED SOURCES ../../swig/yap.i )
if (WIN32)
    SWIG_LINK_LIBRARIES(Py2YAP YAPPython libYap ${PYTHON_LIBRARIES} )
else()
    SWIG_LINK_LIBRARIES(  Py2YAP  libYap YAP++ YAPPython ${PYTHON_LIBRARIES} )
  endif()
  set_target_properties ( ${SWIG_MODULE_Py2YAP_REAL_NAME}
    PROPERTIES
    NO_SONAME ON
    OUTPUT_NAME yap
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS YAPPython

    )


    SET(SWIG_SOURCES
      ${CMAKE_SOURCE_DIR}/packages/swig/yap.i
      )

    # inform we are compiling YAP
  # s used in MSYS


  #  INSTALL ( TARGETS  ${SWIG_MODULE_Py2YAP_REAL_NAME}
  #  RUNTIME  DESTINATION ${PYTHON_MODULE_PATH}
  #  	ARCHIVE DESTINATION ${PYTHON_MODULE_PATH}
  #  	LIBRARY DESTINATION ${PYTHON_MODULE_PATH}
  #  	)


  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py)
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/include)
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog)
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/os)
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/pl)

  add_custom_command( OUTPUT yap.i
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/packages/swig/yap.i  .
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS  ${CMAKE_SOURCE_DIR}/packages/swig/yap.i )

  add_custom_command( OUTPUT yapi.cpp
COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_SOURCE_DIR}/CXX/yapi.cpp  .
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS  ${CMAKE_SOURCE_DIR}/CXX/yapi.cpp )

  add_custom_command( OUTPUT yap4py/prolog/lists.yap
COMMAND ${CMAKE_COMMAND} -E copy ${pl_library}  yap4py/prolog
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS  ${pl_library} )

    add_custom_command( OUTPUT yap4py/prolog/os/yio.yap
    COMMAND ${CMAKE_COMMAND} -E copy ${pl_os_library}  yap4py/prolog/os
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS  ${pl_os_library} )

      add_custom_command( OUTPUT yap4py/prolog/pl/boot.yap
  COMMAND ${CMAKE_COMMAND} -E copy ${pl_boot_library}  yap4py/prolog/pl
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
DEPENDS  ${pl_boot_library} )

  add_custom_command( OUTPUT yap4py/include/YapInterface.h
COMMAND ${CMAKE_COMMAND} -E copy ${c_headers}  yap4py/include
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS  ${c_headers} )

  add_custom_command(  OUTPUT yap4py/include/yapi.hh
    COMMAND ${CMAKE_COMMAND} -E copy ${cxx_headers}  yap4py/include
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS   ${cxx_headers} )

if (CONDA)
    set(bdist bdist_conda)
else()
    set(bdist bdist_wheel)
endif()


  add_custom_target( YAP4PY ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/startup.yss  ${CMAKE_CURRENT_BINARY_DIR}/yap4py
    COMMAND ${CMAKE_COMMAND} -E copy ${CONFIGURATION_HEADERS}  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/include
            COMMAND ${SWIG_EXECUTABLE} -c++  -python -O -py3 -module "yap"  -outcurrentdir -addextern -Iyap4py/include -outdir ${CMAKE_CURRENT_BINARY_DIR} -I${GMP_INCLUDE_DIRS} -DX_API=""  -o yapPyTHON_wrap.cxx ${SWIG_SOURCES}
    COMMAND  ${PYTHON_EXECUTABLE} setup.py sdist ${bdist}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS  ${YAP_DLLS} STARTUP yap4py/prolog/pl/boot.yap yapi.cpp yap4py/include/yapi.hh yap4py/include/YapInterface.h yap4py/prolog/os/yio.yap yap4py/prolog/lists.yap yap.i
)


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

  install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install .
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
