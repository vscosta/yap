
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

  INCLUDE_DIRECTORIES(BEFORE
.
)
add_library (regexp SHARED regexp.c  ${REGEX_SOURCES})

target_link_libraries(regexp libYap)

set_target_properties (regexp PROPERTIES PREFIX "")

install(TARGETS  regexp
        LIBRARY DESTINATION ${libdir}
        RUNTIME DESTINATION ${dlls}
  ARCHIVE DESTINATION ${dlls} )

