macro_optional_find_package (Sqlite3 ON)
macro_log_feature (SQLITE3_FOUND "sqlite3" 
  "SQLITE3 Light Weight Database Manager" 
  "http://sqlite3.com")
if (SQLITE3_FOUND)
# SQLITE3_INCLUDE_DIR - Where to find Sqlite 3 header files (directory)
# SQLITE3_LIBRARIES - Sqlite 3 libraries
# SQLITE3_LIBRARY_RELEASE - Where the release library is
# SQLITE3_LIBRARY_DEBUG - Where the debug library is
# SQLITE3_FOUND - Set to TRUE if we found everything (library, includes and executable)
endif (SQLITE3_FOUND)

