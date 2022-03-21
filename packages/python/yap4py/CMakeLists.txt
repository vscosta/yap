
# This is a CMake example for Python

INCLUDE(UseSWIG)

# include(FindPythonModule)

if ( Python3_Development_FOUND AND SWIG_FOUND)
  if (CONDA)
    set(bdist bdist_conda)
  else()
    set(bdist bdist_wheel)
  endif()

  set( ENV{PYTHONPATH} ${CMAKE_BINARY_DIR}/packages/python/swig:${CMAKE_BINARY_DIR}/packages/python/yap_kernel:. )

  set (PYTHON_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/yapi.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/systuples.py
    ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__main__.py)


  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES CPLUSPLUS ON)
  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
  SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_MODULE_NAME qyap4py.yap)
  #SET_SOURCE_FILES_PROPERTIES(../../swi/yap.i PROPERTIES OUTPUT_NAME yap)

set (SWIG_PATH Path     )
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4py)

  set(YAP4PY_PL )
  set(YAP4PY_PY __main__.py yap4py/yapi.py)

  configure_file("setup.py.in" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
#  configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
  configure_file("YAP4PY.md" README.md)
  configure_file(yap4py/__init__.py.in yap4py/__init__.py)

  INCLUDE_DIRECTORIES(${Python3_LIBRARY_DIRS}  ${CMAKE_CURRENT_BINARY_DIR}/yap4py/include "${CMAKE_SOURCE_DIR}"
    )
  
  SET(SWIG_SOURCES    ${CMAKE_SOURCE_DIR}/packages/swig/yap.i
    )

  # inform we are compiling YAP
  # s used in MSYS


  #  INSTALL ( TARGETS  ${SWIG_MODULE_Py2YAP_REAL_NAME}
  #  RUNTIME  DESTINATION ${PYTHON_MODULE_PATH}
  #  	ARCHIVE DESTINATION ${PYTHON_MODULE_PATH}
  #  	LIBRARY DESTINATION ${PYTHON_MODULE_PATH}v
  #  	)
  
  add_custom_command( OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/_yap.cpp
    COMMAND ${CMAKE_COMMAND} -E copy ${PYTHON_SOURCES}  yap4py
    COMMAND ${SWIG_EXECUTABLE} -c++  -python  -O -py3 -module "yap"  -addextern -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/H/generated -I${CMAKE_SOURCE_DIR}/include -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/utf8proc -I.././.. -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/packages/python -outdir ${CMAKE_CURRENT_BINARY_DIR}/yap4py -I${GMP_INCLUDE_DIRS} -DX_API=""   -o _yap.cpp ${SWIG_SOURCES}
    DEPENDS  ${SWIG_SOURCES} YAP++ Py4YAP
    )

  add_library(
    YAP4Py ${CMAKE_CURRENT_BINARY_DIR}/_yap.cpp)
  


target_link_libraries(YAP4Py YAPPython Py4YAP ${Python3_LIBRARIES}  YAP++ libYap )


target_include_directories(YAP4Py PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/yap4py ${CMAKE_SOURCE_DIR}/packages/python)

  install(CODE "execute_process(
   COMMAND ${Python3_EXECUTABLE} -m pip install --user  .
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")
    



    install(TARGETS  YAP4Py
      LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR} )

  install(FILES ${YAP4PY_PL} DESTINATION ${YAP_INSTALL_DATADIR} )
  
#  if (WITH_DOCS AND DOXYGEN_FOUND)
#
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
