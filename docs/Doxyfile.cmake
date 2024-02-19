  get_target_property(YAP_SOURCES libYap SOURCES)


set(DOX_MD_FILES
docs/md/AttributedVariables.md
docs/md/fli_c_xx.md
docs/md/load_files.md
docs/md/modules.md
docs/md/packages.md
docs/md/programming.md
docs/md/run.md
docs/md/swi.md
docs/md/syntax.md
docs/md/YapExtensions.md
)

file( MAKE_DIRECTORY mkdocs )
file( MAKE_DIRECTORY mkdocs/docs)
file( MAKE_DIRECTORY mkdocs/docs/images)
file( MAKE_DIRECTORY mkdocs/docs/images/images)
file( MAKE_DIRECTORY mkdocs/docs/javascripts)
file( COPY ${CMAKE_SOURCE_DIR}/docs/mkdocs/mkdocs.yml DESTINATION mkdocs)
file( COPY ${CMAKE_SOURCE_DIR}/docs/assets/js/highlight.min.js DESTINATION mkdocs/docs/javascripts)
file( COPY ${CMAKE_SOURCE_DIR}/docs/images/yap_256x256x32.png DESTINATION  mkdocs/docs/images)
file( COPY ${CMAKE_SOURCE_DIR}/docs/images/favicon_32x32.ico DESTINATION mkdocs/docs/images/favicon.ico)

file( MAKE_DIRECTORY sphinx )
file( MAKE_DIRECTORY sphinx/source)
file( MAKE_DIRECTORY sphinx/source/images)
file( COPY ${CMAKE_SOURCE_DIR}/docs/sphinx/Makefile DESTINATION sphinx)
file( COPY ${DOX_MD_FILES} DESTINATION sphinx/source)
file( COPY ${CMAKE_SOURCE_DIR}/docs/sphinx/source/conf.py DESTINATION sphinx/source)
file( COPY ${CMAKE_SOURCE_DIR}/docs/sphinx/source/index.rst DESTINATION sphinx/source)
file( COPY ${CMAKE_SOURCE_DIR}/docs/images/yap_256x256x32.png DESTINATION  sphinx/source/images)
file( COPY ${CMAKE_SOURCE_DIR}/docs/images/favicon_32x32.ico DESTINATION sphinx/source/images/favicon.ico)



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
     ${PROJECT_SOURCE_DIR}/C/absmi.c
     ${PROJECT_SOURCE_DIR}/include/SWI-Prolog.h
     ${PROJECT_SOURCE_DIR}/C/absmi_insts.i)

  set( DOXYGEN_EXCLUDE_PATTERNS
      */.git/*
    */.svn/*
    */.hg/*
    */H/*
    */include/*
    */CMakeFiles/*
     ${PROJECT_SOURCE_DIR}/H/Tags_24*
     ${PROJECT_SOURCE_DIR}/C/Tags_32*
    */_CPack_Packages/*
    )

  set( DOXYGEN_REPEAT_BRIEF NO)
  set( DOXYGEN_ENABLE_PREPROCESSING  YES)
  set( DOXYGEN_MACRO_EXPANSION YES)
  set(DOXYGEN_EXPAND_ONLY_PREDEF YES)
  set(DOXYGEN_PREDEFINED DOXYGEN=1)
  set(DOXYGEN_EXPAND_AS_DEFINED YAP_FLAG )
 set(DOXYGEN_HIDE_SCOPE_NAMES YES)
  set(DOXYGEN_HIDE_COMPOUND_REFERENCE YES)
set (DOXYGEN_REFERENCES_LINK_SOURCE NO)
set (DOXYGEN_HTML_EXTRA_STYLESHEET ${PROJECT_SOURCE_DIR}/docs/assets/css/solarized-light.css)
  set(DOXYGEN_HIDE_UNDOC_MEMBERS     YES)
  set(DOXYGEN_GENERATE_HTML NO)
  set(DOXYGEN_GENERATE_XML YES)
  set(DOXYGEN_XML_PROGRAMLISTING NO)
  set(DOXYGEN_GENERATE_MAN NO)
  set(DOXYGEN_SHOW_FILES NO)
  set(DOXYGEN_OPTIMIZE_OUTPUT_FOR_C NO)
  set(DOXYGEN_OPTIMIZE_OUTPUT_FOR_PROLOG YES)
  set(DOXYGEN_SHOW_NAMESPACES NO)
  set(DOXYGEN_CREATE_SUBDIRS NO)
  set(DOXYGEN_INLINE_GROUPED_CLASSES YES)
    set(DOXYGEN_HAVE_DOT NO)
    set(DOXYGEN_GENERATE_TREEVIEW NO)
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
    ${PROJECT_SOURCE_DIR}/docs/md1  
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

add_custom_target (mkdocs
  COMMAND mkdocs serve
  DEPENDS moxygen
  WORKING_DIRECTORY mkdocs
      )


    add_custom_target(moxygen
      COMMAND $ENV{HOME}/github/moxygen/bin/moxygen.js ../../xml -g %.md -p -H
      WORKING_DIRECTORY mkdocs/docs
      DEPENDS dox ${PROJECT_SOURCE_DIR}/docs/mkdocs/mkdocs.yml
     )

    add_custom_target(sphinx
      COMMAND breathe-apidoc -f -o source/dox -p YAP -g class,group ../xml
      COMMAND make html
      WORKING_DIRECTORY sphinx
      DEPENDS dox
     )

endif()
