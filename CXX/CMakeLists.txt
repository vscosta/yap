
add_library (Yap++ SHARED yapi.cpp)

include_directories (H include os ${CMAKE_BINARY_DIR} ${GMP_INCLUDE_DIR})

target_link_libraries(Yap++ libYap)

install(TARGETS  Yap++
  LIBRARY DESTINATION ${libdir} )

set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIR} )
#set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GMP_LIBRARIES} )


