

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

#  set (Doxygen::doxygen doxygen-yap)
set( DOXYGEN_EXCLUDE
    CMakeLists.txt
    CMakeCache.txt
     ${CMAKE_SOURCE_DIR}/pl
    ${CMAKE_SOURCE_DIR}/library
)

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
  list (APPEND PREDEFINED "YAP_FLAG(A,B,C,D,E,F)=B"  )
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
set(DOXYGEN_FILE_PATTERNS *.pl *.yap *.c *.cc *.cxx *.cpp *.c++ *.java *.ii *.ixx *.ipp *.i++ *.inl *.idl *.ddl *.odl *.h *.hh *.hxx *.hpp *.h++ *.cs *.d *.php *.php4 *.php5 *.phtml *.inc *.m *.markdown *.md *.mm *.dox *.py *.pyw *.f90 *.f95 *.f03 *.f08 *.f *.for *.tcl *.vhd *.vhdl *.ucf *.qsf *.ice)
set(DOXYGEN_INLINE_GROUPED_CLASSES  YES)
set(DOXYGEN_INCLUDE_PATH ${INCLUDE_DIRECTORIES}  ${CMAKE_SOURCE_DIR}/H/generated  ${CMAKE_SOURCE_DIR}/H  ${CMAKE_SOURCE_DIR}/include   ${CMAKE_SOURCE_DIR}/os   ${CMAKE_SOURCE_DIR}/OPTYap   ${CMAKE_SOURCE_DIR}/CXX)
set(DOXYGEN_SOURCE_BROWSER NO)
#set(DOXYGEN_VERBATIM_HEADERS NO)

file( MAKE_DIRECTORY  ${CMAKE_BINARY_DIR}/mkdocs/docs/fli )

doxygen_add_docs(
  dox++
     ${CMAKE_SOURCE_DIR}/CXX
     ${CMAKE_SOURCE_DIR}/include
${CMAKE_SOURCE_DIR}/library/dialect/swi/fli
COMMENT "Generating Xmls"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/CXX
)

add_custom_target (mkdocsFLI
#  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/CXX/index.md fli
  COMMAND   moxygen ${CMAKE_BINARY_DIR}/CXX/xml -g -o fli/group__%s.md -H
DEPENDS  dox++
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/mkdocs/docs
    )

endif()
