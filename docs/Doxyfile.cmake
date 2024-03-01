
get_target_property(YAP_SOURCES libYap SOURCES)


set(DOX_MD_FILES
${CMAKE_SOURCE_DIR}/docs/md/AttributedVariables.md
${CMAKE_SOURCE_DIR}/docs/md/fli_c_xx.md
${CMAKE_SOURCE_DIR}/docs/md/load_files.md
${CMAKE_SOURCE_DIR}/docs/md/packages.md
${CMAKE_SOURCE_DIR}/docs/md/run.md
${CMAKE_SOURCE_DIR}/docs/md/swi.md
${CMAKE_SOURCE_DIR}/docs/md/syntax.md
)

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
  set(DOXYGEN_CMAKE_NUMBER  ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}.${YAP_PATCH_VERSION})
  set(DOXYGEN_BRIEF  "The YAP Prolog Compiler Manual")
  set( DOXYGEN_CMAKE_LOGO ${CMAKE_SOURCE_DIR}/docs/icons/yap_96x96x32.png)
#  set( DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/docs)
  set( DOXYGEN_CREATE_SUBDIRS NO)
  set( DOXYGEN_ALLOW_UNICODE_NAMES NO)
  set( DOXYGEN_OUTPUT_LANGUAGE English)
  set( DOXYGEN_ALWAYS_DETAILED_SEC NO)
  set( DOXYGEN_JAVADOC_AUTOBRIEF      YES)

  list (APPEND PREDEFINED "YAP_FLAG(A,B,C,D,E,F)=B"  )
#  set (Doxygen::doxygen doxygen-yap)
set( DOXYGEN_EXCLUDE
    CMakeLists.txt
    CMakeCache.txt
     ${CMAKE_SOURCE_DIR}/pl/boot2.yap
    ${CMAKE_SOURCE_DIR}/library/apply.yap
    ${CMAKE_SOURCE_DIR}/library/dialect/bprolog
    ${CMAKE_SOURCE_DIR}/library/clp
    ${CMAKE_SOURCE_DIR}/swi/library/clp
    ${CMAKE_SOURCE_DIR}/swi/console
    ${CMAKE_SOURCE_DIR}/include/cudd
     ${CMAKE_SOURCE_DIR}/C/absmi.c
     ${CMAKE_SOURCE_DIR}/include/SWI-Prolog.h
     ${CMAKE_SOURCE_DIR}/C/absmi_insts.i)

  set( DOXYGEN_EXCLUDE_PATTERNS
      */.git/*
    */.svn/*
    */.hg/*
    */CMakeFiles/*
     ${CMAKE_SOURCE_DIR}/H/Tags_24*
     ${CMAKE_SOURCE_DIR}/C/Tags_32*
    */_CPack_Packages/*
    )

  set( DOXYGEN_REPEAT_BRIEF NO)
  set( DOXYGEN_ENABLE_PREPROCESSING  YES)
  set( DOXYGEN_MACRO_EXPANSION YES)
  set(DOXYGEN_EXPAND_ONLY_PREDEF YES)
  set(DOXYGEN_PREDEFINED ${PREDEFINED} )
 set(DOXYGEN_HIDE_SCOPE_NAMES YES)
  set(DOXYGEN_HIDE_COMPOUND_REFERENCE YES)
set (DOXYGEN_REFERENCES_LINK_SOURCE NO)
set (DOXYGEN_HTML_EXTRA_STYLESHEET ${CMAKE_SOURCE_DIR}/docs/assets/css/solarized-light.css)
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
  set(DOXYGEN_MARKDOWN_SUPPORT YES)
  set(DOXYGEN_TOC_INCLUDE_HEADINGS 5  )
  set(DOXYGEN_AUTOLINK_SUPPORT YES  )
  set(DOXYGEN_ID_STYLE YES)
    set(DOXYGEN_HAVE_DOT NO)
    set(DOXYGEN_GENERATE_TREEVIEW NO)
set(DOXYGEN_LAYOUT_FILE ${CMAKE_SOURCE_DIR}/docs/assets/DoxygenLayout.xml)
set(DOXYGEN_FILE_PATTERNS *.pl *.yap *.c *.cc *.cxx *.cpp *.c++ *.java *.ii *.ixx *.ipp *.i++ *.inl *.idl *.ddl *.odl *.h *.hh *.hxx *.hpp *.h++ *.cs *.d *.php *.php4 *.php5 *.phtml *.inc *.m *.markdown *.md *.mm *.dox *.py *.pyw *.f90 *.f95 *.f03 *.f08 *.f *.for *.tcl *.vhd *.vhdl *.ucf *.qsf *.ice)
set(DOXYGEN_INLINE_GROUPED_CLASSES  YES)
set(DOXYGEN_INCLUDE_PATH ${INCLUDE_DIRECTORIES}  ${CMAKE_SOURCE_DIR}/H/generated  ${CMAKE_SOURCE_DIR}/H  ${CMAKE_SOURCE_DIR}/include   ${CMAKE_SOURCE_DIR}/os   ${CMAKE_SOURCE_DIR}/OPTYap   ${CMAKE_SOURCE_DIR}/CXX)
set(DOXYGEN_SOURCE_BROWSER NO)
#set(DOXYGEN_VERBATIM_HEADERS NO)


file( MAKE_DIRECTORY mkdocs )
file( MAKE_DIRECTORY mkdocs/docs)
file( MAKE_DIRECTORY mkdocs/docs/images)
configure_file(yap.md.in ${CMAKE_BINARY_DIR}/README.md)
configure_file(INSTALL.md.in ${CMAKE_BINARY_DIR}/INSTALL.md)
			     
doxygen_add_docs(
  dox
    ${CMAKE_BINARY_DIR}/INSTALL.md
    ${CMAKE_BINARY_DIR}/README.md
    ${CMAKE_SOURCE_DIR}/docs/md
    ${CMAKE_SOURCE_DIR}/docs/extra
    ${CMAKE_SOURCE_DIR}/C
    ${CMAKE_SOURCE_DIR}/H
    ${CMAKE_SOURCE_DIR}/H/generated
    ${CMAKE_SOURCE_DIR}/CXX
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/pl
    ${CMAKE_SOURCE_DIR}/library
    ${CMAKE_SOURCE_DIR}/os
    ${CMAKE_SOURCE_DIR}/OPTYap
    ${CMAKE_SOURCE_DIR}/library/dialect/swi/os
    COMMENT "Generated Xmls"
)


add_custom_target (mkdocs
  COMMAND ${CMAKE_COMMAND} -E make_directory docs/images
  COMMAND ${CMAKE_COMMAND} -E make_directory  docs/images/images
  COMMAND ${CMAKE_COMMAND} -E make_directory docs/javascripts
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/mkdocs/mkdocs.yml  mkdocs.yml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/assets/js/highlight.min.js  docs/javascripts/highlight.min.js
COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/images/yap_256x256x32.png   docs/images/yap_256x256x32.png
COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/images/favicon_32x32.ico  docs/images/favicon.ico
  COMMAND yap -l ${CMAKE_SOURCE_DIR}/docs/dox2md -z main
  COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_BINARY_DIR}/INSTALL.md docs
  COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_BINARY_DIR}/README.md docs/index.md
  COMMAND ${CMAKE_COMMAND} -E copy ${DOX_MD_FILES} docs
  COMMAND  mkdocs build
  DEPENDS dox docs/mkdocs/mkdocs.yml docs/dox2md.yap ${MD_TARGETS}
  WORKING_DIRECTORY mkdocs
      )

    add_custom_target(moxygen
      COMMAND $ENV{HOME}/github/moxygen/bin/moxygen.js ../../xml -g %.md -p -H
      WORKING_DIRECTORY mkdocs/docs
      DEPENDS dox ${CMAKE_SOURCE_DIR}/docs/mkdocs/mkdocs.yml  ${CMAKE_BINARY_DIR}/INSTALL.md 
     )

    add_custom_target(sphinx
      COMMAND breathe-apidoc -f -o source/dox -p YAP -g class,group ../xml
      COMMAND make html
      WORKING_DIRECTORY sphinx
      DEPENDS dox
     )

endif()
