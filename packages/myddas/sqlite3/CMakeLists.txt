
set( YAPSQLITE3_SOURCES
  myddas_sqlite3.c
  sqlite3.c
  sqlite3.h
 )

set( YAPSQLITE3_ANDROID_SOURCES
Android/jni/sqlite/ALog-priv.h
Android/jni/sqlite/JNIHelp.cpp
Android/jni/sqlite/JniConstants.cpp
Android/jni/sqlite/android_database_SQLiteCommon.cpp
Android/jni/sqlite/android_database_SQLiteCommon.h
Android/jni/sqlite/android_database_SQLiteConnection.cpp
Android/jni/sqlite/android_database_SQLiteDebug.cpp
Android/jni/sqlite/android_database_SQLiteGlobal.cpp
Android/jni/sqlite/nativehelper/JNIHelp.h
Android/jni/sqlite/nativehelper/JniConstants.h
Android/jni/sqlite/nativehelper/ScopedLocalRef.h
Android/jni/sqlite/nativehelper/jni.h
 )

#sqlite3 is now in the system

set (SQLITE3_FOUND ON PARENT_SCOPE)
  macro_log_feature (SQLITE3_FOUND "Sqlite3"
      "Sqlite3 Data-Base "
  "http://www.sqlite3ql.org" FALSE)


  include_directories (${SQLITE3_INCLUDE_DIRECTORIES}  .. . Android/jni/sqlite Android/jni/sqlite/nativehelper)

  set (MYDDAS_FLAGS ${MYDDAS_FLAGS} -DMYDDAS_SQLITE3=1 PARENT_SCOPE)
  set_property( DIRECTORY .. APPEND PROPERTY COMPILE_DEFINITIONS  MYDDAS_SQLITE3=1 )

  if (ANDROID)
  add_library (Yapsqlite3 OBJECT   ${YAPSQLITE3_SOURCES}  ${YAPSQLITE3_ANDROID_SOURCES})
  else()
  add_library (Yapsqlite3 SHARED ${YAPSQLITE3_SOURCES})
  target_link_libraries(Yapsqlite3 libYap)
  set_target_properties (Yapsqlite3 PROPERTIES
  VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
  SOVERSION ${SO_MAJOR}
  )

   install(TARGETS  Yapsqlite3
    LIBRARY DESTINATION ${libdir}
     ARCHIVE DESTINATION ${libdir}
  )
endif()
