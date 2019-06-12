
# This is a CMake example for Python

INCLUDE(UseSWIG)

include(FindPythonModule)

if (WITH_PYTHON AND PYTHONLIBS_FOUND AND SWIG_FOUND)
  if (CONDA)
    set(bdist bdist_conda)
  else()
    set(bdist bdist_wheel)
  endif()

  set( ENV{PYTHONPATH} ${CMAKE_BINARY_DIR}/packages/python/swig:${CMAKE_BINARY_DIR}/packages/python/yap_kernel:. )
  list (APPEND pl_library ${CMAKE_CURRENT_SOURCE_DIR}/prolog/yapi.yap )
  set (PYTHON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/yapi.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/systuples.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__main__.py)


  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES CPLUSPLUS ON)
  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_MODULE_NAME yap4py.yap)
  #SET_SOURCE_FILES_PROPERTIES(../../swi/yap.i PROPERTIES OUTPUT_NAME yap)

  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py)

  set(YAP4PY_PL prolog/yapi.yap)
  set(YAP4PY_PY  yap4py/__main__.py yap4py/yapi.py)

  configure_file("setup.py.in" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
  configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
  configure_file("YAP4PY.md" README.md)
  configure_file(yap4py/__init__.py.in yap4py/__init__.py)

  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH}  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/include
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

  add_custom_command( OUTPUT yap4py/yapi.cpp
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/CXX/yapi.cpp  ${CMAKE_CURRENT_BINARY_DIR}/yap4py
    DEPENDS ${CMAKE_SOURCE_DIR}/CXX/yapi.cpp
    )

  add_custom_command( OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/yap_wrap.cxx ${CMAKE_CURRENT_BINARY_DIR}/yap4py/yap_wrap.hh 
    COMMAND ${SWIG_EXECUTABLE} -c++  -python  -O -py3 -module "yap"  -addextern -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/H/generated -I${CMAKE_SOURCE_DIR}/include -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/utf8proc -I.././.. -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/packages/python -outdir ${CMAKE_CURRENT_BINARY_DIR}/yap4py -I${GMP_INCLUDE_DIRS} -DX_API=""   -o ${CMAKE_CURRENT_BINARY_DIR}/yap4py/yap_wrap.cxx -oh ${CMAKE_CURRENT_BINARY_DIR}/yap4py/yap_wrap.hh ${SWIG_SOURCES}  
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS  ${SWIG_SOURCES} YAP++
    )

  
  add_custom_target( YAP4PY ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${PYTHON_SOURCES}  ${CMAKE_CURRENT_BINARY_DIR}/yap4py
    COMMAND ${PYTHON_EXECUTABLE} setup.py  ${bdist}
    DEPENDS ${CMAKE_SOURCE_DIR}/CXX/yapi.cpp Py4YAP YAP++ yap4py/yapi.cpp ${YAP4PY_PY} ${CMAKE_CURRENT_BINARY_DIR}/yap4py/yap_wrap.cxx ${CMAKE_CURRENT_BINARY_DIR}/yap4py/yap_wrap.hh ${PYTHON_SOURCES} Py4YAP STARTUP libYap
    )


  install(CODE "execute_process(
   COMMAND ${PYTHON_EXECUTABLE} -m pip install  ${PYTHON_USER_INSTALL}    --ignore-installed .
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")



  install(FILES ${YAP4PY_PL} DESTINATION ${YAP_INSTALL_DATADIR} )
  
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

endif()
