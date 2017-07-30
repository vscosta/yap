
set( YAPPOSTGRES_SOURCES
  myddas_postgres.c
)

set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

macro_optional_find_package(PostgreSQL ON)

if (PostgreSQL_FOUND)
#  PostgreSQL_FOUND         - Set to true when PostgreSQL is found.
#  PostgreSQL_INCLUDE_DIRS  - Include directories for PostgreSQL
#  PostgreSQL_LIBRARY_DIRS  - Link directories for PostgreSQL libraries
#  PostgreSQL_LIBRARIES     - The PostgreSQL libraries.
  add_library (Yappostgres SHARED ${YAPPOSTGRES_SOURCES})
  target_link_libraries(Yappostgres libYap   ${PostgreSQL_LIBRARIES})
  include_directories (${PostgreSQL_INCLUDE_DIRS} ..)
 set_target_properties (Yappostgres PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    PREFIX ""
  )

list (APPEND MYDDAS_FLAGS -DMYDDAS_POSTGRES=1)
set (MYDDAS_FLAGS ${MYDDAS_FLAGS} ON PARENT_SCOPE)
set_property(GLOBAL APPEND PROPERTY COMPILE_DEFINITIONS
        -DMYDDAS_POSTGRES=1)

install(TARGETS  Yappostgres
    LIBRARY DESTINATION ${dlls}
    RUNTIME DESTINATION ${dlls}
    ARCHIVE DESTINATION ${dlls}
    )
endif (PostgreSQL_FOUND)
