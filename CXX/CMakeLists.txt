
# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)


add_library (Yap++ SHARED
    yapa.hh
    yapdb.hh
    yapi.cpp
    yapi.hh
    yapie.hh
    yapq.hh
    yapt.hh
    )

include_directories (H include ${CMAKE_BINARY_DIR} ${GMP_INCLUDE_DIR})

target_link_libraries(Yap++ ${CMAKE_DL_LIBS})

install(TARGETS  Yap++
  LIBRARY DESTINATION ${libdir}
  ARCHIVE DESTINATION ${libdir}
  )

set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIR} )
#set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GMP_LIBRARIES} )


