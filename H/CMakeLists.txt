

file( STRINGS locals.h tmp )
if (WITH_THREADS)
    Foreach(i ${tmp})
        string(REGEX REPLACE "^LOCAL[^(]*[(][^,]+,[^_a-zA-Z0-9]*([_a-zA-Z0-9]+)[^_a-zA-Z0-9,]*,[^_a-zA-Z0-9]*([_a-zA-Z0-9]+)[^)]*.*$"   "#define LOCAL_\\0 (Yap_regs.worker_local->\\1)\\n#define REMOTE_\\1(wid) (REMOTE(wid)->\\1)\\n" i2 ${i})
        list( APPEND tmp2 ${i2} "\n")
    endforeach()
else()
Foreach(i ${tmp})
string(REGEX REPLACE  "^LOCAL[^(]*[(][ \t]*([^,]+)[ \t]*,[ \t]*([^),]+).*"  "#define LOCAL_\\2 (Yap_local.\\2)\\n#define REMOTE_\\2(wid) (REMOTE(wid)->\\2)\\n" i2 ${i})
list( APPEND tmp2 ${i2} "\n")
endforeach()
endif()
file( WRITE ${CMAKE_BINARY_DIR}/dlocals.h ${tmp2})

add_custom_command( OUTPUT ${CMAKE_BINARY_DIR}/dlocals.h
  COMMAND ${CMAKE_COMMAND} -E COPY ${CMAKE_BINARY_DIR}/deflocals.h ${CMAKE_BINARY_DIR}/dlocals.h
  DEPENDS locals.h )



install(FILES ${INCLUDE_HEADERS} ${CORE_HEADERS} ${OPTYap_HEADERS} ${YAPOS_HEADERS} ${CONFIGURATION_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/Yap )
