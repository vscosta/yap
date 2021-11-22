
  get_target_property(YAP_SOURCES libYap SOURCES)


  find_host_package(Doxygen
    OPTIONAL_COMPONENTS dot dia)


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
    ${PROJECT_SOURCE_DIR}/library/dialect/swi/os
    ${PROJECT_SOURCE_DIR}/library/apply.yap
    ${PROJECT_SOURCE_DIR}/library/dialect
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

  set( DOXYGEN_ENABLE_PREPROCESSING  YES)
  set( DOXYGEN_MACRO_EXPANSION YES)
  set(DOXYGEN_EXPAND_ONLY_PREDEF YES)
  set(DOXYGEN_SKIP_FUNCTION_MACROS NO)
  set( DOXYGEN_PREDEFINED
    "YAP_FLAG(ITEM,NAME,WRITABLE,DEF,INIT,HELPER)=NAME"
    "START_GLOBAL_FLAGS:= enum GlobalFlags {"
    "END_GLOBAL_FLAGS:= }; "
    "START_LOCAL_FLAGS:= enum LocalFlags {"
    "END_LOCAL_FLAGS:= } ;"
    )
    set(DOXYGEN_HIDE_SCOPE_NAMES YES)
  set(DOXYGEN_HIDE_COMPOUND_REFERENCE YES)
  set (DOXYGEN_HTML_EXTRA_STYLESHEET ${PROJECT_SOURCE_DIR}/docs/custom/solarized-light.css)
  set(DOXYGEN_GENERATE_HTML YES)
  set(DOXYGEN_GENERATE_XML YES)
  set(DOXYGEN_GENERATE_MAN NO)
  set(DOXYGEN_SHOW_FILES YES)
  set(DOXYGEN_OPTIMIZE_OUTPUT_FOR_C NO)
  set(DOXYGEN_OPTIMIZE_OUTPUT_FOR_PROLOG YES)
  set(DOXYGEN_INLINE_GROUPED_CLASSES YES)
  set(DOXYGEN_ALIASES "tbd=@todo"
    Term="_?"
    Term_predicate=@brief
    license="@par License\n" 
   "tbd=@par TBD\n" 
 "compat=@par Compatibility\n" 
        "error=@par Error:\n")
    set(DOXYGEN_SHOW_NAMESPACES YES)
    set(DOXYGEN_HAVE_DOT NO)
    set(DOXYGEN_GENERATE_TREEVIEW YES)
set(DOXYGEN_LAYOUT_FILE ${PROJECT_SOURCE_DIR}/docs/custom/DoxygenLayout.xml)
set(DOXYGEN_FILE_PATTERNS *.pl *.yap *.c *.cc *.cxx *.cpp *.c++ *.java *.ii *.ixx *.ipp *.i++ *.inl *.idl *.ddl *.odl *.h *.hh *.hxx *.hpp *.h++ *.cs *.d *.php *.php4 *.php5 *.phtml *.inc *.m *.markdown *.md *.mm *.dox *.py *.pyw *.f90 *.f95 *.f03 *.f08 *.f *.for *.tcl *.vhd *.vhdl *.ucf *.qsf *.ice)
set(DOXYGEN_INLINE_GROUPED_CLASSES  YES)
set(DOXYGEN_INCLUDE_PATH ${INCLUDE_DIRECTORIES}  ${PROJECT_SOURCE_DIR}/H/generated)
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
    COMMENT "Generate man pages"
)
