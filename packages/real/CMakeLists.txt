
if (WITH_R)

  find_package(LibR)
  add_feature_info(YAPR "LIBR_FOUND" "The R Project for Statistical Compputing, at ${LIBR_HOME}. with libraries ${LIBR_LIBRARIES}" )

  if (LIBR_FOUND)

    # PROJECT ( YAP_REAL C )

    set(YAP4R_SOURCES
      yap4r/R/zzz.R
      yap4r/NAMESPACE
      yap4r/DESCRIPTION
      yap4r/src/yap4r.cpp
    )

    file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R)


    set(REAL_SOURCES real.c)

    # LIBR_FOUND
    # LIBR_HOME
    # LIBR_INCLUDE_DIRS
    # LIBR_DOC_DIR
    # LIBR_LIBRARIES

    set_package_properties(R PROPERTIES
      DESCRIPTION "The R Project for Statistical Computing."
      URL "https://www.r-project.org/")




    include_directories (
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_BINARY_DIR}
      ${GMP_INCLUDE_DIRS}
      ${CMAKE_SOURCE_DIR}/include
      ${CMAKE_CURRENT_SOURCE_DIR}
      ${LIBR_INCLUDE_DIRS}
    ) 
    list (APPEND CMAKE_REQUIRED_INCLUDES
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_SOURCE_DIR}/include
      ${LIBR_INCLUDE_DIRS}
      ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src
    )	
    check_include_files( "stdio.h;R.h" HAVE_R_H )
    check_include_files( "R.h;Rinterface.h" HAVE_R_INTERFACE_H )
    check_include_files( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
    check_include_files( "R.h;Rinternals.h" HAVE_RINTERNALS_H )

    configure_file( ${CMAKE_CURRENT_LIST_DIR}/r4yapconfig.h.cmake ${CMAKE_BINARY_DIR}/R4YAPconfig.h)
    configure_file( ${CMAKE_CURRENT_LIST_DIR}/yap4r/src/Makevars.in ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/Makevars)
    add_library(YAPR  ${REAL_SOURCES})
    target_link_libraries (YAPR ${LIBR_LIBRARIES}   libYap)

    install(TARGETS  YAPR
      LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}

    )

    INSTALL(FILES r.yap  real.pl DESTINATION ${YAP_INSTALL_DATADIR})

    if (WITH_YAP4R)
      
      set_package_properties(YAP4R PROPERTIES
	DESCRIPTION "The YAP4R Extension Computing."
	URL "https://www.r-project.org/")

      set(LIBR_INCLUDE_DIRS "${LIBR_HOME}/include" CACHE PATH "R include directory")
      check_include_file(  "Rcpp" HAVE_RCPP ${Rcpp_INCLUDE_DIRS})

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r
	COMMAND ${CMAKE_COMMAND} -E create_symlink  ${CMAKE_CURRENT_LIST_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r
	COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_LIST_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R/zzz.R
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R
	COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_LIST_DIR}/yap4r/R/zzz.R       ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R/zzz.R)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man
	COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_LIST_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src
	COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_LIST_DIR}/yap4r/src/yap4r.cpp       ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp)

      #################################################################################################################### #
      # execute_process(												     #
      #     COMMAND ${LIBR_EXECUTABLE} "--slave" "-e" "stopifnot(require('Rcpp'));cat(Rcpp:::Rcpp.system.file('include'))" #
      #     OUTPUT_VARIABLE LIBRCPP_INCLUDE_DIRS									     #
      #     ) 													     #
      # ################################################################################################################### #







      
      set (YAP4R_PACKAGE ${CMAKE_BINARY_DIR}/yap4r_1.0.tar.gz)

      include_directories(BEFORE ${LIBR_INCLUDE_DIRS})
      include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})
      include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src)
      
      add_custom_command(OUTPUT ${YAP4R_PACKAGE}
	COMMAND ${LIBR_EXECUTABLE} "--slave" "-e" "\"library(Rcpp);compileAttributes('yap4r');compileAttributes('yap4r')\""
	COMMAND ${LIBR_EXECUTABLE} CMD build yap4r
	COMMAND ${CMAKE_COMMAND} -E rename ${CMAKE_CURRENT_BINARY_DIR}/yap4r_1.0.tar.gz ${CMAKE_BINARY_DIR}/yap4r_1.0.tar.gz
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp YAPR YAP++)


      add_custom_target(YAP4R ALL
	DEPENDS ${YAP4R_PACKAGE})


      INSTALL(FILES ${YAP4R_PACKAGE} DESTINATION ${YAP_INSTALL_DATADIR} )
    endif()
  endif()
endif()
