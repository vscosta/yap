

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
       file( WRITE ${CMAKE_TOP_BINARY_DIR}/dlocals.h ${tmp2})


install (FILES ${PL_SOURCES}
  DESTINATION ${libpl}/pl
  )
