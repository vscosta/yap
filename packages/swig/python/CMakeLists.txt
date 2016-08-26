
# This is a CMake example for Python

INCLUDE(${SWIG_USE_FILE})


if (PYTHONLIBS_FOUND)

  include(FindPythonModule)  
  
  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

  INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/CXX)

  SET(CMAKE_SWIG_FLAGS "-py3")

  SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES CPLUSPLUS ON)
  SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES SWIG_FLAGS "-includeall")
  SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTY SWIG_MODULE_NAME libPy2YAP )

  configure_file ("setup.py.cmake"  "setup.py" )
  configure_file ("../yap.i"  "yap.i" )


  #SET( CMAKE_SWIG_OUTDIR "yap" )


#  SWIG_ADD_MODULE(Py2YAP python ../yap.i )
#  SWIG_LINK_LIBRARIES(Py2YAP ${PYTHON_LIBRARIES} Yap++ libYap YAPPython)
#  set_target_properties ( ${SWIG_MODULE_Py2YAP_REAL_NAME} PROPERTIES
#  NO_SONAME ON )


  # execute_process ( COMMAND ${PYTHON_EXECUTABLE} -c "from distutils import sysconfig; print( sysconfig.get_python_lib( plat_specific=True, prefix='${CMAKE_INSTALL_PREFIX}' ) )"
  #                   OUTPUT_VARIABLE _ABS_PYTHON_MODULE_PATH
  #                   OUTPUT_STRIP_TRAILING_WHITESPACE )
  # get_filename_component ( _ABS_PYTHON_MODULE_PATH ${_ABS_PYTHON_MODULE_PATH} ABSOLUTE )
  # file ( RELATIVE_PATH _REL_PYTHON_MOULE_PATH ${CMAKE_INSTALL_PREFIX} ${_ABS_PYTHON_MODULE_PATH} )

#    set ( PYTHON_MODULE_PATH
#      ${_REL_PYTHON_MODULE_PATH}
#      )

    set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

    add_custom_target ( py2yap ALL
      COMMAND ${PYTHON_EXECUTABLE} setup.py bdist
      DEPENDS ../yap.i YAPPython  )

    
    install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} ${SETUP_PY} install 
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

#     INSTALL ( FILES ${CMAKE_CURRENT_BINARY_DIR}/yap/_yap.so DESTINATION ${PYTHON_MODULE_PATH} )
#       INSTALL ( FILES ${CMAKE_CURRENT_BINARY_DIR}/yap/yap.py DESTINATION ${PYTHON_MODULE_PATH} )
#       INSTALL ( FILES ${CMAKE_CURRENT_SOURCE_DIR}/yap/yapex.py DESTINATION ${PYTHON_MODULE_PATH} )


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

    endif ()


  install(TARGETS ${SWIG_MODULE_Py2YAP_REAL_NAME} 
    LIBRARY DESTINATION ${dlls}
     ARCHIVE DESTINATION ${dlls}
   )

  #   add_subdirectory ( examples )
else ()
  message(STATUS "Not building python bindings")
endif(PYTHONLIBS_FOUND)
