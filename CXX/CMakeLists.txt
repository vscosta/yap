
add_library (Yap++ SHARED yapi.cpp)

target_link_libraries(Yap++ libYap)

install(TARGETS  Yap++
  LIBRARY DESTINATION ${libdir} )

include_directories (${GMP_INCLUDE_DIR})



