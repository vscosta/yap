
# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

set (CXX_SOURCES
    yapi.cpp
)


list(APPEND LIBYAP_SOURCES ${CXX_SOURCES} PARENT_SCOPE)

if (ANDROID OR WIN32)
   add_component (Yap++ ${CXX_SOURCES} )
else()
   add_external (Yap++ ${CXX_SOURCES} )
   MY_target_link_libraries(Yap++ ${CMAKE_DL_LIBS} libYap)

   MY_install(TARGETS  Yap++
     LIBRARY DESTINATION ${libdir}
     ARCHIVE DESTINATION ${libdir}
   )
endif()


include_directories ( . ${CMAKE_BINARY_DIR} ${GMP_INCLUDE_DIRS} ${PYTHON_INCLUDE_DIRS})

set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GMP_INCLUDE_DIRS} )



#set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GMP_LIBRARIES} )
