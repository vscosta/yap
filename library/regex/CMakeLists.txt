
  set ( REGEX_SOURCES
    cclass.h
    cname.h
    collate.h
    utils.h
    yapregex.h
    engine.c
    regcomp.c
    regerror.c
    regexec.c
    regfree.c
    )

add_library (regexp SHARED regexp.c  ${REGEX_SOURCES})

target_link_libraries(regexp libYap)

set_target_properties (regexp PROPERTIES PREFIX "")

install(TARGETS  regexp
  LIBRARY DESTINATION ${dlls}
  ARCHIVE DESTINATION ${dlls} )

