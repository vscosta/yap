
# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

set (CXX_SOURCES
    yapa.hh
    yapdb.hh
    yapi.cpp
    yapi.hh
    yapie.hh
    yapq.hh
    yapt.hh
)


if (YAP_SINGLE_FILE)
add_library (Yap++ OBJECT ${CXX_SOURCES} )
else(YAP_SINGLE_FILE)
add_library (Yap++ SHARED ${CXX_SOURCES} )

target_link_libraries(Yap++ ${CMAKE_DL_LIBS} libYap)

install(TARGETS  Yap++
  LIBRARY DESTINATION ${libdir}
  ARCHIVE DESTINATION ${libdir}
  )
endif(YAP_SINGLE_FILE)

include_directories (H include ${CMAKE_BINARY_DIR} ${GMP_INCLUDE_DIR})

set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIR} )
#set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GMP_LIBRARIES} )
