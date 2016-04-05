
set( YAPODBC_SOURCES
  myddas_odbc.c
)

set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

macro_optional_find_package(ODBC ON)

macro_log_feature (ODBC_FOUND "Odbc"
  "ODBC Driver for MYDDAS Data-Base Interface "
  "http://www.odbc.org" FALSE)

if (ODBC_FOUND)
  # ODBC_INCLUDE_DIRECTORIES, where to find sql.h
  # ODBC_LIBRARIES, the libraries to link against to use ODBC
  # ODBC_FOUND.  If false, you cannot build anything that requires Odbc.
  set (MYDDAS_FLAGS ${MYDDAS_FLAGS} -DMYDDAS_ODBC=1 PARENT_SCOPE)
    set_property( DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  MYDDAS_ODBC=1 )
add_library (Yapodbc SHARED ${YAPODBC_SOURCES})
target_link_libraries(Yapodbc libYap ${ODBC_LIBRARIES})
include_directories (${ODBC_INCLUDE_DIRECTORIES} ..)

set_target_properties (Yapodbc PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
  SOVERSION ${SO_MAJOR}
  )

   install(TARGETS  Yapodbc
    LIBRARY DESTINATION ${libdir}
    ARCHIVE DESTINATION ${libdir}
    )

else()
  add_definitions (-DMYDDAS_ODBC=0)
  endif (ODBC_FOUND)
