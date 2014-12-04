macro_optional_find_package(ODBC ON)
if (ODBC_FOUND)
  macro_log_feature (ODBC_FOUND "ODBC" 
    "Use ODBC Data-Base Interface " 
    "http://www.odbc.org" FALSE)
  add_definitions (-DMYDDAS_ODBC=1)
  set (EXTRALIBS ${EXTRALIBS} ${ODBC_LIBRARY})
  if (APPLE)
    set (EXTRALIBS ${EXTRALIBS} "-framework CoreFoundation")
  endif (APPLE)
endif (ODBC_FOUND)

macro_optional_find_package(MySQL ON)
if (MYSQL_FOUND)
  macro_log_feature (MYSQL_FOUND "MySQL" 
    "Use MYSQL Data-Base Interface " 
    "http://www.odbc.org" FALSE)
  add_definitions (-DMYDDAS_MYSQL=1)
endif (MYSQL_FOUND)

option (WITH_MYDDAS
  "enable the MYDDAS library" ON)
if (WITH_MYDDAS)
  if (MYSQL_FOUND)
    add_definitions (-DMYDDAS_MYSQL=1)
  endif (MYSQL_FOUND)

  if (MYSQL_FOUND OR ODBC_FOUND)
    set (EXTRALIBS ${EXTRALIBS} myddas)
  else (MYSQL_FOUND OR ODBC_FOUND)
    message(SEND_ERROR "MYDDAS needs either MYSQL or ODBC none where found")
  endif (MYSQL_FOUND OR ODBC_FOUND)
endif (WITH_MYDDAS)

cmake_dependent_option (WITH_MYDDAS_Stats
  "enable the MYDDAS library statistics support" OFF
  WITH_MYDDAS OFF)
#TODO:

cmake_dependent_option (WITH_MYDDAS_top_level
  "enable the MYDDAS top-level support to MySQL" OFF
  'WITH_MYDDAS AND MYSQL_FOUND' OFF)
#TODO:

