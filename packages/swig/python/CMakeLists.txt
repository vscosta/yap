
  # This is a CMake example for Python

  INCLUDE( NewUseSWIG     )

  if (PYTHONLIBS_FOUND)

    include(FindPythonModule)

    INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

    INCLUDE_DIRECTORIES( "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/CXX" )


    SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES CPLUSPLUS ON)
    SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
    SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES SWIG_MODULE_NAME yap )
    SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES OUTPUT_NAME yap )

    configure_file ("setup.py.cmake"  ${CMAKE_CURRENT_BINARY_DIR}/setup.py )
    # configure_file ("../yap.i"  "yap.i" )

    SWIG_ADD_LIBRARY(Py2YAP LANGUAGE python  SHARED SOURCES ../yap.i )
      if (WIN32)
        SWIG_LINK_LIBRARIES(Py2YAP YAPPython libYap ${PYTHON_LIBRARIES} )
          else()
          SWIG_LINK_LIBRARIES(  Py2YAP  libYap YAP++ YAPPython ${PYTHON_LIBRARIES} )
        endif()
  set_target_properties ( ${SWIG_MODULE_Py2YAP_REAL_NAME}
    PROPERTIES
   NO_SONAME ON
  OUTPUT_NAME _yap
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"  )
  # inform we are compiling YAP
  # s used in MSYS

  execute_process ( COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print( sysconfig.get_path( 'platlib' ) )"
  OUTPUT_VARIABLE _ABS_PYTHON_MODULE_PATH
  OUTPUT_STRIP_TRAILING_WHITESPACE )
  get_filename_component ( ABS_PYTHON_MODULE_PATH ${_ABS_PYTHON_MODULE_PATH} ABSOLUTE )
  file ( RELATIVE_PATH _REL_PYTHON_MODULE_PATH ${CMAKE_INSTALL_PREFIX} ${_ABS_PYTHON_MODULE_PATH} )

  set ( PYTHON_MODULE_PATH
    ${ABS_PYTHON_MODULE_PATH}
  )


  INSTALL ( FILES ${CMAKE_CURRENT_BINARY_DIR}/yap.py DESTINATION ${PYTHON_MODULE_PATH} )
  INSTALL ( FILES ${CMAKE_SOURCE_DIR}/packages/python/yapex.py DESTINATION ${PYTHON_MODULE_PATH} )


  INSTALL ( TARGETS  ${SWIG_MODULE_Py2YAP_REAL_NAME}
  RUNTIME  DESTINATION ${PYTHON_MODULE_PATH}
  	ARCHIVE DESTINATION ${PYTHON_MODULE_PATH}
  	LIBRARY DESTINATION ${PYTHON_MODULE_PATH}
  	)



  if ( DOCUMENTATION AND DOXYGEN_FOUND )

    set(CMAKE_SWIG_FLAGS -DDOXYGEN=${DOXYGEN_FOUND})

    # Run doxygen to only generate the xml
    add_custom_command ( OUTPUT ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc
      COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/Doxyfile.xml
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      DEPENDS ${c_headers};${c_sources};${cpp_sources};${cpp_headers}
    )

    # generate .i from doxygen .xml
    add_custom_command ( OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
      COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/doxy2swig.py -n
      ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
      ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
      DEPENDS ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
    )
    add_custom_target ( doc_i DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i )
    add_dependencies( ${SWIG_MODULE_ftdi1_REAL_NAME} doc_i )

  ENDIF()
  ENDIF()
