
set (ANDROID_SOURCES
    JavaYap.java
   	main.xml
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/DatabaseErrorHandler.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/SQLException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/DefaultDatabaseErrorHandler.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/package.html
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/ExtraUtils.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/CloseGuard.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/DatabaseObjectNotClosedException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteAbortException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteAccessPermException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteBindOrColumnIndexOutOfRangeException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteBlobTooBigException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteCantOpenDatabaseException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteClosable.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteConnection.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteConnectionPool.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteConstraintException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteCursor.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteCursorDriver.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteCustomFunction.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDatabase.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDatabaseConfiguration.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDatabaseCorruptException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDatabaseLockedException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDatatypeMismatchException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDebug.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDirectCursorDriver.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDiskIOException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteDoneException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteFullException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteGlobal.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteMisuseException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteOpenHelper.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteOutOfMemoryException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteProgram.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteQuery.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteReadOnlyDatabaseException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteSession.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteStatement.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteStatementInfo.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteTableLockedException.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SQLiteTransactionListener.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/SqliteWrapper.java
${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database/sqlite/package.html
)

set (target Yap)

add_custom_target (${target} ALL
       SOURCES ${ANDROID_SOURCES}
       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

# set the output directory for the .so files to point to the  project's 'lib/[cpuarch] directory
set(ANDROID_SO_OUTDIR ${CMAKE_CURRENT_BINARY_DIR}/libs/${ANDROID_ABI})
set_target_properties(${target} PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${ANDROID_SO_OUTDIR})
set_target_properties(${target} PROPERTIES LIBRARY_OUTPUT_DIRECTORY_RELEASE ${ANDROID_SO_OUTDIR})
set_target_properties(${target} PROPERTIES LIBRARY_OUTPUT_DIRECTORY_DEBUG ${ANDROID_SO_OUTDIR})

INCLUDE(${SWIG_USE_FILE})

  #  SET(CMAKE_SWIG_FLAGS -package YAP)

    set (CMAKE_SWIG_OUTDIR  ${CMAKE_CURRENT_BINARY_DIR}/src/pt/up/fc/dcc/yap )

    SET(SWIG_SOURCES
      ../yap.i
      )

   #set (CMAKE_JAVA_CLASS_OUTPUT_PATH java)

    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

  include_directories (
    ${CMAKE_SOURCE_DIR}/CXX
    ${JAVA_INCLUDE_DIRS}
    ${JNI_INCLUDE_DIRS}
      )

    if ( ${C_COMPILER} MATCHES "Clang")
      set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-missing-prototypes")
    endif()
    if ( ${C_COMPILER} MATCHES "GNU")
      set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-missing-declarations")
    endif()
    #SET(CMAKE_SWIG_FLAGS "${CMAKE_SWIG_FLAGS} ")

    set (CMAKE_SHARED_LIBRARY_SONAME_CXX_FLAG "")

    SWIG_ADD_MODULE(YapDroid java ${SWIG_SOURCES}
	   )
     set_target_properties(YapDroid PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${ANDROID_SO_OUTDIR})

    SWIG_LINK_LIBRARIES (YapDroid Yap++  )

#The output directory for the shared library linker step is redirected to the ‘libs’ subdirectory of this skeleton project:


execute_process(
  COMMAND  ${ANDROID_SDK_TOOL} create project
                  --path ${CMAKE_CURRENT_BINARY_DIR}
                  --target $ENV{ANDROID_TARGET}
                  --name ${target}
                  --package pt.up.fc.dcc.${target}
                  --activity JavaYap
  )

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/assets/share/Yap)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/assets/lib/Yap)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/assets/lib/Yap)
file(COPY JavaYap.java DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src/pt/up/fc/dcc/app)
file(COPY main.xml DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/res/layout)
file(COPY package.html DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src/pt/up/fc/dcc/Yap)
file(COPY ${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src/org/sqlite)
file(COPY ${GMP_LIBRARIES} DESTINATION ${ANDROID_SO_OUTDIR})

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
    set(ANT_BUILD_TYPE "debug")
else()
    set(ANT_BUILD_TYPE "release")
endif()

add_custom_command(TARGET ${target} POST_BUILD
  COMMAND  ${ANDROID_SDK_TOOL} update project
                  --path ${CMAKE_CURRENT_BINARY_DIR}
  )

INSTALL( CODE " execute_process(COMMAND ${ANDROID_ANT} ${ANT_BUILD_TYPE}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})" )

#set( ANDROID_ASSETS_DIRECTORIES ${CMAKE_SOURCE_DIR}/pl;${CMAKE_SOURCE_DIR}/library} )
#set( ANDROID_JAVA_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR} )
#set( ANDROID_NATIVE_LIB_DEPENDENCIES YapDroid )
#set( ANDROID_NATIVE_LIB_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR} )
#set (ANDROID_GUI ON)


#add_executable (yapdroid JavaYap.java )

#set_target_properties (yapdroid PROPERTIES OUTPUT_NAME yap)

#target_link_libraries(yapdroid ${SWIG_MODULE_${YapDroid≈}_REAL_NAME}. )
