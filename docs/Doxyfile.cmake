
  get_target_property(YAP_SOURCES libYap SOURCES)


  find_host_package(Doxygen
    OPTIONAL_COMPONENTS dot dia)

if (DOXYGEN_FOUND)
  set(DOXYGEN_PROJECT_NUMBER  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
  set(DOXYGEN_BRIEF  "The YAP Prolog Compiler Manual")
  set( DOXYGEN_PROJECT_LOGO ${PROJECT_SOURCE_DIR}/docs/icons/yap_96x96x32.png)
#  set( DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/docs)
  set( DOXYGEN_CREATE_SUBDIRS NO)
  set( DOXYGEN_ALLOW_UNICODE_NAMES NO)
  set( DOXYGEN_OUTPUT_LANGUAGE English)
  set( DOXYGEN_ALWAYS_DETAILED_SEC NO)
  set( DOXYGEN_JAVADOC_AUTOBRIEF      YES)

  
#  set (Doxygen::doxygen doxygen-yap)
set( DOXYGEN_EXCLUDE
    CMakeLists.txt
    CMakeCache.txt
     ${PROJECT_SOURCE_DIR}/pl/boot2.yap
    ${PROJECT_SOURCE_DIR}/library/apply.yap
    ${PROJECT_SOURCE_DIR}/library/dialect/bprolog
    ${PROJECT_SOURCE_DIR}/library/clp
    ${PROJECT_SOURCE_DIR}/swi/library/clp
    ${PROJECT_SOURCE_DIR}/swi/console
    ${PROJECT_SOURCE_DIR}/include/cudd
    ${PROJECT_SOURCE_DIR}/C/traced_absmi_insts.h
    ${PROJECT_SOURCE_DIR}/C/absmi_insts.i)

  set( DOXYGEN_EXCLUDE_PATTERNS
      */.git/*
    */.svn/*
    */.hg/*
    */CMakeFiles/*
    */_CPack_Packages/*)

  set( DOXYGEN_REPEAT_BRIEF NO)
  set( DOXYGEN_ENABLE_PREPROCESSING  YES)
  set( DOXYGEN_MACRO_EXPANSION YES)
  set(DOXYGEN_EXPAND_ONLY_PREDEF YES)
  set(DOXYGEN_PREDEFINED DOXYGEN=1)
  set(DOXYGEN_EXPAND_AS_DEFINED YAP_FLAG )
 set(DOXYGEN_HIDE_SCOPE_NAMES YES)
  set(DOXYGEN_HIDE_COMPOUND_REFERENCE YES)
  set (DOXYGEN_HTML_EXTRA_STYLESHEET ${PROJECT_SOURCE_DIR}/docs/assets/css/solarized-light.css)
  set(DOXYGEN_HIDE_UNDOC_MEMBERS     YES)
  set(DOXYGEN_GENERATE_HTML YES)
  set(DOXYGEN_GENERATE_XML YES)
  set(DOXYGEN_GENERATE_MAN NO)
  set(DOXYGEN_SHOW_FILES YES)
  set(DOXYGEN_OPTIMIZE_OUTPUT_FOR_C NO)
  set(DOXYGEN_OPTIMIZE_OUTPUT_FOR_PROLOG YES)
  set(DOXYGEN_SHOW_NAMESPACES NO)
  set(DOXYGEN_CREATE_SUBDIRS NO)
  set(DOXYGEN_INLINE_GROUPED_CLASSES YES)
    set(DOXYGEN_HAVE_DOT NO)
    set(DOXYGEN_GENERATE_TREEVIEW YES)
set(DOXYGEN_LAYOUT_FILE ${PROJECT_SOURCE_DIR}/docs/assets/DoxygenLayout.xml)
set(DOXYGEN_FILE_PATTERNS *.pl *.yap *.c *.cc *.cxx *.cpp *.c++ *.java *.ii *.ixx *.ipp *.i++ *.inl *.idl *.ddl *.odl *.h *.hh *.hxx *.hpp *.h++ *.cs *.d *.php *.php4 *.php5 *.phtml *.inc *.m *.markdown *.md *.mm *.dox *.py *.pyw *.f90 *.f95 *.f03 *.f08 *.f *.for *.tcl *.vhd *.vhdl *.ucf *.qsf *.ice)
set(DOXYGEN_INLINE_GROUPED_CLASSES  YES)
set(DOXYGEN_INCLUDE_PATH ${INCLUDE_DIRECTORIES}  ${PROJECT_SOURCE_DIR}/H/generated  ${PROJECT_SOURCE_DIR}/H  ${PROJECT_SOURCE_DIR}/include   ${PROJECT_SOURCE_DIR}/os   ${PROJECT_SOURCE_DIR}/OPTYap   ${PROJECT_SOURCE_DIR}/CXX) 
set(DOXYGEN_SOURCE_BROWSER YES)
#set(DOXYGEN_VERBATIM_HEADERS NO)

                     
configure_file(yap.md.in ${CMAKE_BINARY_DIR}/README.md)
configure_file(INSTALL.md.in ${CMAKE_BINARY_DIR}/INSTALL.md)


doxygen_add_docs(
  dox
    ${CMAKE_BINARY_DIR}/README.md
    ${CMAKE_BINARY_DIR}/INSTALL.md
    ${PROJECT_SOURCE_DIR}/docs/md
    ${PROJECT_SOURCE_DIR}/C
    ${PROJECT_SOURCE_DIR}/H
    ${PROJECT_SOURCE_DIR}/H/generated
    ${PROJECT_SOURCE_DIR}/CXX
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/pl
    ${PROJECT_SOURCE_DIR}/library
    ${PROJECT_SOURCE_DIR}/os
    ${PROJECT_SOURCE_DIR}/OPTYap
    ${PROJECT_SOURCE_DIR}/library/dialect/swi/os
    COMMENT "Generate man pages"
)

add_custom_target (doxybook  
      COMMAND ${CMAKE_COMMAND} -E make_directory mkdocs
      COMMAND ${CMAKE_COMMAND} -E make_directory mkdocs/docs
      COMMAND ${CMAKE_COMMAND} -E make_directory mkdocs/docs/images
      COMMAND ${CMAKE_COMMAND} -E make_directory mkdocs/docs/images/images
      COMMAND ${CMAKE_COMMAND} -E make_directory mkdocs/docs/javascripts
      COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/mkdocs/mkdocs.yml mkdocs
#      COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/assets/js/highlight.min.js mkdocs/docs/javascripts
      COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/images/images/yap_256x256x32.png mkdocs/docs/images
      COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/images/favicon_32x32.ico mkdocs/docs/images/favicon.ico
      COMMAND doxybook2 -i xml -o mkdocs/docs  -c ${PROJECT_SOURCE_DIR}/docs/mkdocs/config.json ${PROJECT_SOURCE_DIR}/docs/mkdocs/config.json
      DEPENDS dox 
      )

    
    add_custom_target (mkdocs
      COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/mkdocs/mkdocs.yml .
      COMMAND mkdocs build
      WORKING_DIRECTORY mkdocs
      DEPENDS doxybook ${PROJECT_SOURCE_DIR}/docs/mkdocs/mkdocs.yml
     )
  
endif()
