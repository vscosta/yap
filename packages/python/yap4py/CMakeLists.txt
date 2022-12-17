
# This is a CMake example for Python

INCLUDE(UseSWIG)

# include(FindPythonModule)

if ( Python3_Development_FOUND AND SWIG_FOUND)
  if (CONDA)
    set(bdist bdist_conda)
  else()
    set(bdist bdist_wheel)
  endif()

  set( ENV{PYTHONPATH} ${CMAKE_BINARY_DIR}/packages/python/yap4py:${CMAKE_BINARY_DIR}/packages/python/yap_kernel:. )



 set (PYTHON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/yapi.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/systuples.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/predicates.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/queries.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__main__.py)


  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES CPLUSPLUS ON)
  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_MODULE_NAME qyap4py.yap)
  #SET_SOURCE_FILES_PROPERTIES(../../swi/yap.i PROPERTIES OUTPUT_NAME yap)

set (SWIG_PATH Path     )
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py)
  file(TOUCH ${CMAKE_CURRENT_BINARY_DIR}/yap4py/stub.cxx)
  
  set(YAP4PY_PL )
  set(YAP4PY_PY __main__.py yap4py/yapi.py)

#  configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
  configure_file("YAP4PY.md" README.md)
  configure_file(yap4py/__init__.py.in yap4py/__init__.py)
  configure_file(setup.py.in setup.py)

  INCLUDE_DIRECTORIES(${Python3_LIBRARY_DIRS}  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/include ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/packages/python
    )
  
  SET(SWIG_SOURCES     ${CMAKE_SOURCE_DIR}/packages/swig/yap.i)
    

  # inform we are compiling YAP
  # s used in MSYS


    INSTALL ( TARGETS  ${SWIG_MODULE_Py2YAP_REAL_NAME}
    RUNTIME  DESTINATION ${PYTHON_MODULE_PATH}
    	ARCHIVE DESTINATION ${PYTHON_MODULE_PATH}
    	LIBRARY DESTINATION ${PYTHON_MODULE_PATH}
  )

 add_custom_command( OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/_yap.cxx
   COMMAND ${CMAKE_COMMAND} -E copy ${PYTHON_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/yap4py
   COMMAND ${CMAKE_COMMAND} -E copy yap4py/stub.cxx  ${CMAKE_CURRENT_BINARY_DIR}/yap4py
   COMMAND ${SWIG_EXECUTABLE} -c++  -python  -O -py3 -module "yap"  -addextern -doxygen  -I${CMAKE_SOURCE_DIR}/utf8proc -I.././.. -I${CMAKE_CURRENT_BINARY_DIR}/yap4py -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/packages/python -outdir ${CMAKE_CURRENT_BINARY_DIR}/yap4py -I${GMP_INCLUDE_DIRS} -DX_API=""   -o ${CMAKE_CURRENT_BINARY_DIR}/yap4py/_yap.cxx  -oh ${CMAKE_CURRENT_BINARY_DIR}/yap4py/py4yap.h   ${SWIG_SOURCES}
   DEPENDS  ${SWIG_SOURCES} YAP++ Py4YAP ${PYTHON_SOURCES}
     )

#cmake_minimum_required(VERSION 3.11.0)
#find_package(PythonExtensions REQUIRED)

add_library(_yap SHARED  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/_yap.cxx)
target_link_libraries(_yap libYap YAPPython  YAP++ Py4YAP)

set_target_properties(_yap
  PROPERTIES OUTPUT_NAME _yap0
  )

install(TARGETS _yap LIBRARY DESTINATION yap4py._yap)

    INSTALL ( TARGETS _yap
      LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )

#  if (WITH_DOCS AND DOXYGEN_FOUND)
#    set(CMAKE_SWIG_FLAGS -DDOXYGEN=${DOXYGEN_FOUND})
#
#    # Run doxygen to only generate the xml
#    add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
#      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc
#      COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/Doxyfile.xml
#      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
#      DEPENDS ${c_headers};${c_sources};${cpp_sources};${cpp_headers}
#      )
#
#    # generate .i from doxygen .xml
#    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
#      COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/doxy2swig.py -n
#      ${CMAKE_BINARY_DIR}/doc/xm11l/ftdi_8c.xml
#      ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
#      DEPENDS ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
#      )
#    add_custom_target(doc_i DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i)
#    add_dependencies(${SWIG_MODULE_ftdi1_REAL_NAME} doc_i)
#
#  ENDIF ()

endif()
