    #get package NAME

 get_filename_component(    TMPD ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY   )

get_filename_component(  CMAKE_PROJECT_NAME ${TMPD} NAME   )
        set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

set( DOC_INPUT_FILES ${TMPD} )
LIST (APPEND DOC_TAGS ${CMAKE_PROJECT_NAME}=${CMAKE_CURRENT_BINARY_DIR}/html/${CMAKE_PROJECT_NAME} PARENT_SCOPE)
LIST (APPEND DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/html/${CMAKE_PROJECT_NAME} PARENT_SCOPE)

 add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/${CMAKE_PROJECT_NAME}
     COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}     VERBATIM
     DEPENDS ${doxyfile} ${DOCS}
 )

   configure_file(${doxyfile_in} ${doxyfile} @ONLY)

   configure_file(${CMAKE_SOURCE_DIR}/docs/source/conf.py.in source/conf.py)
   configure_file(${CMAKE_SOURCE_DIR}/docs/source/index.rst source/index.rst)
