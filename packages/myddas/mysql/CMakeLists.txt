
  set(MYSQL_SOURCES
          myddas_mysql.c
          myddas_util.c
          myddas_util.c
          myddas_wkb2prolog.c
          )

  set(SO_MAJOR 1)
  set(SO_MINOR 0)
  set(SO_PATCH 0)

  macro_optional_find_package(MySQL ON)

  if (MYSQL_FOUND)
      #  MYSQL_INCLUDE_DIR - where to find mysql.h, etc.
      #  MYSQL_LIBRARIES   - List of libraries when using MySQL.
      #  MYSQL_FOUND       - True if MySQL found.

      set_property(GLOBAL
              APPEND PROPERTY COMPILE_DEFINITIONS -DMYDDAS_MYSQL=1)
	    if (WIN32)
	      	    add_library(Yapmysql OBJECT ${MYSQL_SOURCES})
	      else()
	    add_library(Yapmysql SHARED ${MYSQL_SOURCES})
	    target_link_libraries(Yapmysql ${MYSQL_LIBRARIES} libYap)
     install(TARGETS Yapmysql
              RUNTIME DESTINATION ${dlls}
             ARCHIVE DESTINATION ${dlls}
             LIBRARY DESTINATION ${dlls}
              )
	    endif()
      include_directories(${MYSQL_INCLUDE_DIR} ..)
      set_target_properties(Yapmysql PROPERTIES
              POSITION_INDEPENDENT_CODE ON
                  PREFIX ""
              )

      list (APPEND MYDDAS_FLAGS -DMYDDAS_MYSQL=1)
      set (MYDDAS_FLAGS ${MYDDAS_FLAGS} ON PARENT_SCOPE)
      set_property(GLOBAL APPEND PROPERTY COMPILE_DEFINITIONS
              -DMYDDAS_MYSQL=1)


 
  else ()
      add_definitions(-DMYDDAS_MYSQL=0)
  endif (MYSQL_FOUND)

  cmake_dependent_option(USE_MYDDAS_top_level
          "enable the MYDDAS top-level (REPL) support for MySQL" OFF
          'USE_MYDDAS AND MYSQL_FOUND' OFF)
  #TODO:
