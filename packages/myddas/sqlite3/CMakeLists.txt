message( "   * Sqlite3 Data-Base (http://www.sqlite3ql.org), distributed with MYDDAS" )

set( YAPSQLITE3_SOURCES
  myddas_sqlite3.c
  sqlite3.c
  sqlite3.h
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

       include_directories(  ${CMAKE_CURRENT_SOURCE_DIR}/.. Android/jni/sqlite Android/jni/sqlite/nativehelper )


set(YAPSQLITE3_SOURCES
 ${YAPSQLITE3_SOURCES}
Android/jni/sqlite/JNIHelp.cpp
Android/jni/sqlite/JniConstants.cpp
Android/jni/sqlite/android_database_SQLiteCommon.cpp
Android/jni/sqlite/android_database_SQLiteCommon.h
Android/jni/sqlite/android_database_SQLiteConnection.cpp
Android/jni/sqlite/android_database_SQLiteDebug.cpp
Android/jni/sqlite/android_database_SQLiteGlobal.cpp
Android/jni/sqlite/nativehelper/jni.h
Android/jni/sqlite/nativehelper/jniConstants.h
Android/jni/sqlite/nativehelper/JNIHelp.h
Android/jni/sqlite/nativehelper/ScopedLocalRef.h
)

endif (ANDROID)

add_component(  Yapsqlite3
    ${YAPSQLITE3_SOURCES} )

    MY_set_target_properties(Yapsqlite3
      PROPERTIES
      #  RPATH ${libdir} VERSION ${LIBYAPTAI_FULL_VERSION}
      #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
      POSITION_INDEPENDENT_CODE TRUE
      )
