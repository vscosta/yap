message( "   * Sqlite3 Data-Base (http://www.sqlite3ql.org), distributed with MYDDAS" )

set( YAPSQLITE3_SOURCES
  myddas_sqlite3.c
  src/sqlite3.c
  src/sqlite3.h
  src/sqlite3ext.h
 )

#sqlite3 is now in the system

set (SQLITE3_FOUND ON CACHE PRIVATE "")

include_directories ( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/..)

add_definitions(-DSQLITE_ENABLE_COLUMN_METADATA=1 )

add_definitions(-DSQLITE_ENABLE_JSON1=1 )

add_definitions(-DSQLITE_ENABLE_RBU=1 )

add_definitions(-DSQLITE_ENABLE_RTREE=1 )

add_definitions(-DSQLITE_ENABLE_FTS5=1 )


if (ANDROID)

       include_directories( .. src/Android/jni/sqlite src/Android/jni/sqlite/nativehelper )


set(YAPSQLITE3_SOURCES
${YAPSQLITE3_SOURCES}
src/Android/jni/sqlite/JNIHelp.cpp
src/Android/jni/sqlite/JniConstants.cpp
src/Android/jni/sqlite/android_database_SQLiteCommon.cpp
src/Android/jni/sqlite/android_database_SQLiteCommon.h
src/Android/jni/sqlite/android_database_SQLiteConnection.cpp
src/Android/jni/sqlite/android_database_SQLiteDebug.cpp
src/Android/jni/sqlite/android_database_SQLiteGlobal.cpp
src/Android/jni/sqlite/nativehelper/jni.h
src/Android/jni/sqlite/nativehelper/jniConstants.h
src/Android/jni/sqlite/nativehelper/JNIHelp.h
src/Android/jni/sqlite/nativehelper/ScopedLocalRef.h
)

endif (ANDROID)

add_component(  Yapsqlite3                                           
    ${YAPSQLITE3_SOURCES} )

