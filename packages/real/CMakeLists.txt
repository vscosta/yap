if (WITH_R)

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
  # LIBLIBR_INCLUDE_DIRS
  # LIBR_DOC_DIR
  # LIBR_LIBRARIES

  list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
  
  find_package(LibR)



  if (LIBR_FOUND)
    set_package_properties(R PROPERTIES
      DESCRIPTION "The R Project for Statistical Computing."
      URL "https://www.r-project.org/")

    add_feature_info(YAPR "LIBR_FOUND" "The R Project for Statistical Compputing, at ${LIBR_HOME}. with libraries ${LIBR_LIBRARIES}" )



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
    )	
    check_include_files( "stdio.h;R.h" HAVE_R_H )
    check_include_files( "R.h;Rinterface.h" HAVE_R_INTERFACE_H )
    check_include_files( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
    check_include_files( "R.h;Rinternals.h" HAVE_RINTERNALS_H )

    configure_file( ${CMAKE_CURRENT_LIST_DIR}/r4yapconfig.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/R4YAPconfig.h)
    configure_file( ${CMAKE_CURRENT_LIST_DIR}/yap4r/src/Makevars.in ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/Makevars)
    add_library(YAPR  ${REAL_SOURCES})
    target_link_libraries (YAPR ${LIBR_LIBRARIES}   libYap)

    install(TARGETS  YAPR
      LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}

    )

    INSTALL(FILES real.pl DESTINATION ${YAP_INSTALL_DATADIR})
    # FindRcpp.cmake
    # - Try to find Rcpp
    #
    # The following variables are optionally searched for defaults
    #  Rcpp_ROOT_DIR:  Base directory where all Rcpp components are found
    #
    # Once done this will define
    #  Rcpp_FOUND - System has Rcpp
    #  Rcpp_INCLUDE_DIRS - The Rcpp include directories
    #  Rcpp_LIBRARIES - The libraries needed to use Rcpp

    find_package(Rcpp)



    if (Rcpp_FOUND)

      add_feature_info(YAP4R "Rcpp_FOUND" "The YAP4R interface allows YAP to be called from R. It requires the Rcpp package, installed at ${Rcpp_ROOT_DIR}" )
      check_include_file(  "Rcpp" HAVE_RCPP ${Rcpp_INCLUDE_DIRS})

      #    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src )

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION
	COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_LIST_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_LIST_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R/zzz.R
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_LIST_DIR}/yap4r/R/zzz.R       ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_LIST_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man)

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_LIST_DIR}/yap4r/src/yap4r.cpp       ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src)

      #################################################################################################################### #
      # execute_process(												     #
      #     COMMAND ${LIBR_EXECUTABLE} "--slave" "-e" "stopifnot(require('Rcpp'));cat(Rcpp:::Rcpp.system.file('include'))" #
      #     OUTPUT_VARIABLE LIBRCPP_INCLUDE_DIRS									     #
      #     ) 													     #
      # ################################################################################################################### #

      set (YAP4R_PACKAGE ${CMAKE_CURRENT_BINARY_DIR}/yap4r_1.0.tar.gz)

      include_directories(BEFORE ${LIBR_INCLUDE_DIRS})
      message(STATUS ${LIBR_INCLUDE_DIRS})
      include_directories(BEFORE ${LIBRCPP_INCLUDE_DIRS})
      message(STATUS ${LIBRCPP_INCLUDE_DIRS})
      add_custom_command(OUTPUT ${YAP4R_PACKAGE}
	COMMAND ${LIBR_EXECUTABLE} "--slave" "-e" "\"library(Rcpp);compileAttributes('yap4r');compileAttributes('yap4r')\""
	COMMAND ${LIBR_EXECUTABLE} CMD build yap4r
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp YAPR YAP++)


      add_custom_target(YAP4R ALL
	DEPENDS ${YAP4R_PACKAGE})


      INSTALL(FILES ${YAP4R_PACKAGE} DESTINATION ${YAP_INSTALL_DATADIR} )
    endif()
  endif()
endif()
