
set ( REGEX_INCLUDES
    cclass.h
    cname.h
    collate.h
    utils.h
    yapregex.h
    engine.c
    )

set ( REGEX_SOURCES
    regcomp.c
    regerror.c
    regexec.c
    regfree.c
    )



add_library(regexp regexp.c  ${REGEX_SOURCES})

target_link_libraries(regexp libYap)

set_target_properties (regexp PROPERTIES PREFIX "")
TARGET_INCLUDE_DIRECTORIES (regexp PUBLIC BEFORE
  ${CMAKE_SOURCE_DIR}/include;${CMAKE_CURRENT_BINARY_DIR};${CMAKE_CURRENT_SOURCE_DIR} )

install(TARGETS  regexp
        LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )
