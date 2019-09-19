if (WITH_DOCS)


  set (TOP_DIR YES)

  set(DOCS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/docs)

  set(CMAKE_HTML_EXTRA_
${DOCS_SOURCE_DIR}/custom/jquery-3.4.1.min.js
    ${DOCS_SOURCE_DIR}/custom/doxy-boot.js
          ${DOCS_SOURCE_DIR}/custom/application.js
          ${DOCS_SOURCE_DIR}/custom/search.js
          ${DOCS_SOURCE_DIR}/custom/all.js
          ${DOCS_SOURCE_DIR}/custom/lunr.js
${DOCS_SOURCE_DIR}/custom/bootstrap.min.css
${DOCS_SOURCE_DIR}/custom/bootstrap.min.js
${DOCS_SOURCE_DIR}/custom/popper.min.js
${DOCS_SOURCE_DIR}/custom/customdoxygen.css
${DOCS_SOURCE_DIR}/custom/font-awesome.min.css
${DOCS_SOURCE_DIR}/custom/offcanvas.css
${DOCS_SOURCE_DIR}/custom/theme.css
${DOCS_SOURCE_DIR}/custom/yap.css
          ${DOCS_SOURCE_DIR}/custom/solarized-light.css
          ${DOCS_SOURCE_DIR}/custom/all.js
          ${DOCS_SOURCE_DIR}/custom/search.js
${DOCS_SOURCE_DIR}/custom/assets
${DOCS_SOURCE_DIR}/icons/yap_32x32x32.png
           )
  
foreach(i ${CMAKE_HTML_EXTRA_})
    string(APPEND CMAKE_HTML_EXTRA ${i} " ")
  endforeach(i ${CMAKE_HTML_EXTRA_})
  

  set(DOCS_DIR_EXCLUDE_
    ${CMAKE_SOURCE_DIR}/build
    ${CMAKE_SOURCE_DIR}/Debug
    ${CMAKE_SOURCE_DIR}/packages/jpl
    ${CMAKE_SOURCE_DIR}/swi
    ${CMAKE_SOURCE_DIR}/packages/Problog/problog_examples
    ${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/src
    ${CMAKE_SOURCE_DIR}/packages/gecode/6.0.1
    ${CMAKE_SOURCE_DIR}/packages/gecode/6.0.0
    ${CMAKE_SOURCE_DIR}/packages/gecode/5.1.0
    ${CMAKE_SOURCE_DIR}/packages/gecode/5.0.0
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
    ${CMAKE_SOURCE_DIR}/packages/cplint
xb    ${CMAKE_SOURCE_DIR}/packages/CLPBN/examples
    ${CMAKE_SOURCE_DIR}/packages/CLPBN/horus
    ${CMAKE_SOURCE_DIR}/packages/cuda
    ${CMAKE_SOURCE_DIR}/packages/prosqlite
    ${CMAKE_SOURCE_DIR}/packages/pyswip
    ${CMAKE_SOURCE_DIR}/packages/python/yap_kernel
    ${CMAKE_SOURCE_DIR}/packages/python/swig
    ${CMAKE_SOURCE_DIR}/packages/yap-lbfgs/liblbfgs-1.10
    ${CMAKE_SOURCE_DIR}/packages/swi-minisat2
    ${CMAKE_SOURCE_DIR}/library/dialect/swi/os
    )


  set(DOCS_EXCLUDE_
          ${CMAKE_SOURCE_DIR}/C/traced_absmi_insts.h
          ${CMAKE_SOURCE_DIR}/C/*.i
          ${CMAKE_SOURCE_DIR}/H/globals.h
    ${CMAKE_SOURCE_DIR}/OPTYap/locks_alpha_funcs.h
    ${CMAKE_SOURCE_DIR}/OPTYap/locks_mips_funcs.h
    ${CMAKE_SOURCE_DIR}/OPTYap/locks_sparc.h
    ${CMAKE_SOURCE_DIR}/*/bprolog/* 
    ${CMAKE_SOURCE_DIR}/*/prism/* 
    ${CMAKE_SOURCE_DIR}/*/gecode/[345]*
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode3_yap.cc 
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode4_yap.cc 
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode5_yap.cc 
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode3.yap 
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode4.yap 
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode5.yap 
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode3_yap_hand_written.yap  
    ${CMAKE_SOURCE_DIR}/packages/gecode/gecode4_yap_hand_written.yap
          ${CMAKE_SOURCE_DIR}/packages/gecode/gecode5_yap_hand_written.yap
          ${CMAKE_SOURCE_DIR}/traced_absmi_insts.h
          */CMakeFiles/*
    )



  foreach(i ${DOCS_EXCLUDE_})
    string(APPEND DOCS_EXCLUDE ${i} " ")
  endforeach(i ${DOCS_EXCLUDE_})


  foreach(i ${DOCS_DIR_EXCLUDE_})
    string(APPEND DOCS_DIR_EXCLUDE ${i} " ")
  endforeach(i ${DOCS_DIR_EXCLUDE_})


  # add a target to generate API documentation with Doxygen


  set(doxyfile_in ${CMAKE_SOURCE_DIR}/docs/Doxyfile.in)

  set(doxyfile_inxml ${CMAKE_SOURCE_DIR}/docs/source/Doxyfile.in)


  add_subdirectory(../packages/raptor/doc ${CMAKE_BINARY_DIR}/packages/raptor/doc)

  SET(DOC_INPUT_FILES_
    ${CMAKE_SOURCE_DIR}/docs/md
    ${CMAKE_SOURCE_DIR}/pl
    ${CMAKE_SOURCE_DIR}/CXX
    ${CMAKE_SOURCE_DIR}/OPTYap
    ${CMAKE_SOURCE_DIR}/C
    ${CMAKE_SOURCE_DIR}/H
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/os
    ${CMAKE_SOURCE_DIR}/library
    ${CMAKE_SOURCE_DIR}/swi/library
#    ${CMAKE_SOURCE_DIR}/packages
    ${CMAKE_SOURCE_DIR}/BEAM
    )

  SET( HTML_INPUT_FILES_
          ${DOC_INPUT_FILES_}
          ${CMAKE_SOURCE_DIR}/docs/dox.md
          ${CMAKE_SOURCE_DIR}/packages
          ${CMAKE_SOURCE_DIR}/BEAM
          )

  foreach(i ${HTML_INPUT_FILES_})
    string(APPEND HTML_INPUT_FILES ${i} " ")
  endforeach(i ${HTML_INPUT_FILES_})

    SET( XML_INPUT_FILES_
            ${DOC_INPUT_FILES_}
          ${CMAKE_SOURCE_DIR}/docs/dox.md
              ${CMAKE_SOURCE_DIR}/BEAM
            )

    foreach(i ${XML_INPUT_FILES_})

      string(APPEND XML_INPUT_FILES ${i} " ")
  endforeach(i ${XML_INPUT_FILES_})

  set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
  set(doxyfilexml ${CMAKE_CURRENT_BINARY_DIR}/source/Doxyfile)
  configure_file(${doxyfile_in} ${doxyfile} @ONLY)
  configure_file(${doxyfile_inxml} ${doxyfilexml} @ONLY)

  configure_file(${CMAKE_SOURCE_DIR}/docs/source/conf.py.in source/conf.py)
  configure_file(${CMAKE_SOURCE_DIR}/docs/source/index.rst source/index.rst)

add_custom_target(docs
COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
)

add_custom_target(sphinx
COMMAND ${DOXYGEN_EXECUTABLE}  ${doxyfilexml}

)


if (EXISTS ${CMAKE_CURRENT_BINARY_DIR}/html)
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html DESTINATION ${docdir})

    install(FILES  ${CODES} DESTINATION ${docdir})
  endif()


endif()
