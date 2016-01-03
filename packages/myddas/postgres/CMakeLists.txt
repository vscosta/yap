
set( YAPPOSTGRES_SOURCES
  myddas_postgres.c
)

set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

macro_optional_find_package(POSTGRESQL ON)

macro_log_feature (POSTGRES_FOUND "postgres"
  "POSTGRES Driver for MYDDAS Data-Base Interface "
  "http://www.postgres.org" FALSE)

if (POSTGRES_FOUND)
  # POSTGRES_INCLUDE_DIRECTORIES, where to find sql.h
  # POSTGRES_LIBRARIES, the libraries to link against to use POSTGRES
  # POSTGRES_FOUND.  If false, you cannot build anything that requires Postgres.
  add_library (Yappostgres SHARED ${YAPPOSTGRES_SOURCES})
  add_definitions (-DMYDDAS_POSTGRES=1)
  target_link_libraries(Yappostgres libYap   ${POSTGRES_LIBRARIES})
  include_directories (${POSTGRES_INCLUDE_DIRECTORIES} ..)
  set_target_properties (Yappostgres PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
    SOVERSION ${SO_MAJOR}
  )

   install(TARGETS  Yappostgres
    LIBRARY DESTINATION ${libdir}
    )
else()
  add_definitions (-DMYDDAS_POSTGRES=0)
endif (POSTGRES_FOUND)

