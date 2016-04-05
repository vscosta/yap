
set( YAPPOSTGRES_SOURCES
  myddas_postgres.c
)

set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

macro_optional_find_package(PostgreSQL ON)

macro_log_feature (PostgreSQL_FOUND "postgres"
  "PostgreSQL Driver for MYDDAS Data-Base Interface "
  "http://www.postgres.org" FALSE)

if (PostgreSQL_FOUND)
#  PostgreSQL_FOUND         - Set to true when PostgreSQL is found.
#  PostgreSQL_INCLUDE_DIRS  - Include directories for PostgreSQL
#  PostgreSQL_LIBRARY_DIRS  - Link directories for PostgreSQL libraries
#  PostgreSQL_LIBRARIES     - The PostgreSQL libraries.
  add_library (Yappostgres SHARED ${YAPPOSTGRES_SOURCES})
  set (MYDDAS_FLAGS ${MYDDAS_FLAGS} -DMYDDAS_POSTGRES=1 PARENT_SCOPE)
    set_property( DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  MYDDAS_POSTGRES=1 )

  target_link_libraries(Yappostgres libYap   ${PostgreSQL_LIBRARIES})
  include_directories (${PostgreSQL_INCLUDE_DIRS} ..)
  set_target_properties (Yappostgres PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
    SOVERSION ${SO_MAJOR}
  )

   install(TARGETS  Yappostgres
    LIBRARY DESTINATION ${libdir}
    ARCHIVE DESTINATION ${libdir}
    )
else()
  add_definitions (-DMYDDAS_PostgreSQL=0)
endif (PostgreSQL_FOUND)
