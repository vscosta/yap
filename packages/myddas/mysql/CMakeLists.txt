
if (WITH_MYSQL)
    add_feature_info(  YAPodbc "MARIADB_FOUND	" " MariaDB at PkgConfig::mariadb,  ${ODBC_INCLUDE_DIRS}" )

  set(MYSQL_SOURCES
          myddas_mysql.c
          myddas_util.c
          myddas_util.c
          myddas_wkb2prolog.c
          )

  set(SO_MAJOR 1)
  set(SO_MINOR 0)
  set(SO_PATCH 0)

  if (MYSQL_FOUND)
      #  MYSQL_INCLUDE_DIR - where to find mysql.h, etc.
      #  MYSQL_LIBRARIES   - List of libraries when using MySQL.
      #  MYSQL_FOUND       - True if MySQL found.

        include_directories(${MYSQL_INCLUDE_DIR}  ${CMAKE_CURRENT_SOURCE_DIR}/..)
	    if (WIN32)
	      	    add_library(YAPmysql OBJECT ${MYSQL_SOURCES})
	      else()
	    add_library(YAPmysql  ${MYSQL_SOURCES})
	    target_link_libraries(YAPmysql PUBLIC PkgConfig::mariadb} libYap)
     install(TARGETS YAPmysql
              RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
             ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
             LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
              )
	    endif()
      set_target_properties(YAPmysql PROPERTIES
              POSITION_INDEPENDENT_CODE ON
                  PREFIX ""
              )

   
 
  else ()
   
  endif (MYSQL_FOUND)

endif()
