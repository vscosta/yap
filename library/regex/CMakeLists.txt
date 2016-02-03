
add_library (regexp SHARED regexp.c)

if( NOT HAVE_REGEX_H )
  set ( REGEX_SOURCES
    cclass.h
    cname.h
    collate.h
    utils.h
    yapregex.h
    engine.c
    regcomp.c
    )
endif( NOT HAVE_REGEX_H )

target_link_libraries(regexp libYap ${REGEX_SOURCES})

set_target_properties (regexp PROPERTIES PREFIX "")

install(TARGETS  regexp
  LIBRARY DESTINATION ${dlls}
  ARCHIVE DESTINATION ${dlls} )

