
set( YAPSQLITE3_SOURCES
  myddas_sqlite3.c
  sqlite3.c
  sqlite3.h
 )

if (ANDROID)
set( YAPSQLITE3_ANDROID_SOURCES
Android/jni/sqlite/JNIHelp.cpp
Android/jni/sqlite/JniConstants.cpp
Android/jni/sqlite/android_database_SQLiteCommon.cpp
Android/jni/sqlite/android_database_SQLiteCommon.h
Android/jni/sqlite/android_database_SQLiteConnection.cpp
Android/jni/sqlite/android_database_SQLiteDebug.cpp
Android/jni/sqlite/android_database_SQLiteGlobal.cpp
)
set( YAPSQLITE3_ANDROID_DIRS
  Android/jni/sqlite
  Android/jni/sqlite/nativehelper
  )
endif (ANDROID)

#sqlite3 is now in the system

set (SQLITE3_FOUND ON CACHE PRIVATE "")
  message (
      "   * Sqlite3 Data-Base (http://www.sqlite3ql.org) is distributed with
      MYDDAS" )

 list (APPEND MYDDAS_FLAGS -DMYDDAS_SQLITE3=1)
set (MYDDAS_FLAGS ${MYDDAS_FLAGS} ON PARENT_SCOPE)
set_property(GLOBAL APPEND PROPERTY COMPILE_DEFINITIONS
        -DMYDDAS_SQLITE3=1)

       include_directories (  ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR} ${YAPSQLITE3_ANDROID_DIRS})


  add_component (Yapsqlite3   ${YAPSQLITE3_SOURCES}  ${YAPSQLITE3_ANDROID_SOURCES})
  if (NOT ANDROID)
  #target_link_libraries(Yapsqlite3 libYap)
  set_target_properties (Yapsqlite3 PROPERTIES
  PREFIX ""
  )

   # install(TARGETS  Yapsqlite3
   #  LIBRARY DESTINATION ${dlls}
   #  ARCHIVE DESTINATION ${dlls}
   #  )
 endif()

