
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
set_target_properties (regexp PROPERTIES INCLUDE_DIRECTORIES ".")
set_target_properties (regexp PROPERTIES INCLUDE_DIRECTORIES "${CURRENT_BINARY}")

install(TARGETS  regexp
  LIBRARY DESTINATION ${dlls}
  ARCHIVE DESTINATION ${dlls} )

