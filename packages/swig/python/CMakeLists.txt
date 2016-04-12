
# This is a CMake example for Python

INCLUDE(${SWIG_USE_FILE})

if (PYTHONLIBS_FOUND)
INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/CXX)

SET(CMAKE_SWIG_FLAGS "")

SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES CPLUSPLUS ON)
SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES SWIG_FLAGS "-includeall")
SWIG_ADD_MODULE(yap python ../yap.i)
SWIG_LINK_LIBRARIES(yap ${PYTHON_LIBRARIES} Yap++ libYap)
set_target_properties ( ${SWIG_MODULE_yap_REAL_NAME} PROPERTIES NO_SONAME ON )

   execute_process ( COMMAND ${PYTHON_EXECUTABLE} -c "from distutils import sysconfig; print( sysconfig.get_python_lib( plat_specific=True, prefix='${CMAKE_INSTALL_PREFIX}' ) )"
                     OUTPUT_VARIABLE _ABS_PYTHON_MODULE_PATH
                     OUTPUT_STRIP_TRAILING_WHITESPACE )

   get_filename_component ( _ABS_PYTHON_MODULE_PATH ${_ABS_PYTHON_MODULE_PATH} ABSOLUTE )
   file ( RELATIVE_PATH _REL_PYTHON_MODULE_PATH ${CMAKE_INSTALL_PREFIX} ${_ABS_PYTHON_MODULE_PATH} )

   set ( PYTHON_MODULE_PATH
         ${_REL_PYTHON_MODULE_PATH}
       )

   INSTALL ( FILES ${CMAKE_CURRENT_BINARY_DIR}/_yap.so DESTINATION ${PYTHON_MODULE_PATH} )
   INSTALL ( FILES ${CMAKE_CURRENT_BINARY_DIR}/yap.py DESTINATION ${PYTHON_MODULE_PATH} )
   INSTALL ( FILES ${CMAKE_CURRENT_SOURCE_DIR}/yapex.py DESTINATION ${PYTHON_MODULE_PATH} )

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


#   add_subdirectory ( examples )
 else ()
   message(STATUS "Not building python bindings")
endif(PYTHONLIBS_FOUND)
