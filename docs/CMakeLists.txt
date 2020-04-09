
if (WITH_DOCS)




    set (TOP_DIR YES)

set(DOCS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/docs)

SET (CMAKE_HTML_EXTRA_
  ${DOCS_SOURCE_DIR}/custom/application.js
  ${DOCS_SOURCE_DIR}/custom/assets
    ${DOCS_SOURCE_DIR}/custom/bootstrap.min.css
    ${DOCS_SOURCE_DIR}/custom/bootstrap.min.js
    ${DOCS_SOURCE_DIR}/custom/customdoxygen.css
    ${DOCS_SOURCE_DIR}/custom/default
    ${DOCS_SOURCE_DIR}/custom/doxy-boot.js
    ${DOCS_SOURCE_DIR}/custom/font-awesome.min.css
    ${DOCS_SOURCE_DIR}/custom/footer.html
    ${DOCS_SOURCE_DIR}/custom/header.html
    ${DOCS_SOURCE_DIR}/custom/jquery-3.2.1.min.js
    ${DOCS_SOURCE_DIR}/custom/offcanvas.css
    ${DOCS_SOURCE_DIR}/custom/offcanvas.js
    ${DOCS_SOURCE_DIR}/custom/solarized-light.css
    ${DOCS_SOURCE_DIR}/custom/theme.css
    ${DOCS_SOURCE_DIR}/custom/yap.css
    )


    foreach(i ${CMAKE_HTML_EXTRA_})
      string(APPEND CMAKE_HTML_EXTRA ${i} " ")
    endforeach(i ${CMAKE_HTML_EXTRA_})

set(DOCS_EXCLUDE_
${CMAKE_SOURCE_DIR}/packages/jpl
${CMAKE_SOURCE_DIR}/packages/swig
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/src
${CMAKE_SOURCE_DIR}/packages/gecode/4.4.0
${CMAKE_SOURCE_DIR}/packages/gecode/4.2.1
${CMAKE_SOURCE_DIR}/packages/gecode/4.2.0
${CMAKE_SOURCE_DIR}/packages/gecode/4.0.0
${CMAKE_SOURCE_DIR}/packages/gecode/3.7.3
${CMAKE_SOURCE_DIR}/packages/gecode/3.7.2
${CMAKE_SOURCE_DIR}/packages/gecode/3.7.1
${CMAKE_SOURCE_DIR}/packages/gecode/3.7.0
${CMAKE_SOURCE_DIR}/packages/gecode/3.6.0
${CMAKE_SOURCE_DIR}/packages/gecode/dev
${CMAKE_SOURCE_DIR}/C/traced_absmi_insts.h
${CMAKE_SOURCE_DIR}/H/locals.h
${CMAKE_SOURCE_DIR}/H/globals.h
${CMAKE_SOURCE_DIR}/packages/cplint
${CMAKE_SOURCE_DIR}/packages/CLPBN/examples
${CMAKE_SOURCE_DIR}/packages/CLPBN/horus
${CMAKE_SOURCE_DIR}/packages/prosqlite
${CMAKE_SOURCE_DIR}/packages/pyswip
${CMAKE_SOURCE_DIR}/packages/yap-lbfgs/liblbfgs-1.10
${CMAKE_SOURCE_DIR}/library/dialect/swi/os
${CMAKE_SOURCE_DIR}/packages/gecode/gecode3_yap.cc
            ${CMAKE_SOURCE_DIR}/packages/gecode/gecode4_yap.cc
            ${CMAKE_SOURCE_DIR}/packages/gecode/gecode3.yap
            ${CMAKE_SOURCE_DIR}/packages/gecode/gecode4.yap
            ${CMAKE_SOURCE_DIR}/packages/gecode/gecode3_yap_hand_written.yap
           ${CMAKE_SOURCE_DIR}/packages/gecode/gecode4_yap_hand_written.yap
            */CMakeFiles/* *~ */#*

)


foreach(i ${DOCS_EXCLUDE_})
  string(APPEND DOCS_EXCLUDE ${i} " ")
endforeach(i ${DOCS_EXCLUDE_})


# add a target to generate API documentation with Doxygen


    set(doxyfile_in ${CMAKE_SOURCE_DIR}/docs/Doxyfile.in)





   add_subdirectory(../packages/raptor/doc ${CMAKE_BINARY_DIR}/packages/raptor/doc)




SET(DOC_INPUT_FILES_ 	${CMAKE_SOURCE_DIR}/INSTALL.md
		       ${CMAKE_SOURCE_DIR}/pl
                       ${CMAKE_SOURCE_DIR}/CXX
                       ${CMAKE_SOURCE_DIR}/OPTYap
                       ${CMAKE_SOURCE_DIR}/C
                       ${CMAKE_SOURCE_DIR}/H
                       ${CMAKE_SOURCE_DIR}/include
                       ${CMAKE_SOURCE_DIR}/os
                       ${CMAKE_SOURCE_DIR}/library
                       ${CMAKE_SOURCE_DIR}/swi/library
                       ${CMAKE_SOURCE_DIR}/docs/md
)

foreach(i ${DOC_INPUT_FILES_})

  string(APPEND DOC_INPUT_FILES ${i} " ")
endforeach(i ${DOC_INPUT_FILES_})

    set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
   configure_file(${doxyfile_in} ${doxyfile} @ONLY)

   configure_file(${CMAKE_SOURCE_DIR}/docs/source/conf.py.in source/conf.py)
   configure_file(${CMAKE_SOURCE_DIR}/docs/source/index.rst source/index.rst)

add_custom_target(docs
  COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
  COMMAND ${CMAKE_COMMAND} -E COPY ${CMAKE_SOURCE_DIR}/docs/custom/assets html/assets/
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
     COMMENT "Generating API documentation with Doxygen"
     VERBATIM
     DEPENDS ${DEPENDS})



   if (EXISTS ${CMAKE_CURRENT_BINARY_DIR}/html)
   install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html DESTINATION ${docdir})

   install(FILES  ${CODES} DESTINATION ${docdir})
   endif()


endif()
