
  # This is a CMake example for Python and Java

  INCLUDE(UseSWIG)
  INCLUDE(UseJava)

    SET(CMAKE_SWIG_FLAGS -package pt.up.yap.lib)

    SET(SWIG_SOURCES
      ../yap.i
      )

  #  set(ANDROID_SO_OUTDIR  ${libdir})
  #  set(CMAKE_SWIG_OUTDIR  ${CMAKE_SOURCE_DIR}/android/yaplib/build/generated/source/pt/up/yap/lib)
  #set(CMAKE_SWIG_OUTDIR  ${CMAKE_CURRENT_BINARY_DIR}/derived)

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
    set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-missing-declarations -Wno-strict-aliasing")
  endif()

set (CMAKE_SHARED_LIBRARY_SONAME_CXX_FLAG "")

    if (ANDROID)
        SWIG_ADD_MODULE(YAPDroid java ${SWIG_SOURCES} )

            ADD_LIBRARY( YAPAndroid OBJECT ${swig_generated_file_fullname})
    else()
    SWIG_LINK_LIBRARIES (YAPDroid Yap++  )
    set (LIBDIR ${CMAKE_SOURCE_DIR}/YAPDroid/app/libs/${ANDROID_ABI})


    add_custom_command (TARGET YAPDroid
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory ${LIBDIR}
                COMMAND ${CMAKE_COMMAND} -E copy  $<TARGET_FILE:YAPDroid> ${LIBDIR}
        COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:libYap> ${LIBDIR}
        COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:Yap++>  ${LIBDIR}
        COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:Yapsqlite3>  ${LIBDIR}
        COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:utf8proc>  ${LIBDIR}
        COMMAND ${CMAKE_COMMAND} -E copy   ${GMP_LIBRARIES}  ${LIBDIR}
        DEPENDS YAPDroid
      )
  endif()

# set( LIBDIR ${CMAKE_SOURCE_DIR}/YAPDroid/app/src/main/jniLibs/${ANDROID_ABI})

#     add_custom_command(TARGET YAPDroid
#       COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_SOURCE_DIR}/YAPDroid/app/src/main/java/pt/up/yap/lib
# COMMAND ${CMAKE_COMMAND} -E copy_directory  ${CMAKE_SWIG_OUTDIR} ${CMAKE_SOURCE_DIR}/YAPDroid/app/src/main/java/pt/up/yap/lib
#  COMMAND ${CMAKE_COMMAND} -E make_directory ${LIBDIR}
#  COMMAND ${CMAKE_COMMAND} -E copy  $<TARGET_FILE:YAPDroid> ${LIBDIR}
#  COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:libYap> ${LIBDIR}
#  COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:Yap++>  ${LIBDIR}
#  COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:Yapsqlite3>  ${LIBDIR}
#  COMMAND ${CMAKE_COMMAND} -E copy   $<TARGET_FILE:utf8proc>  ${LIBDIR}
#  COMMAND ${CMAKE_COMMAND} -E make_directory  ${CMAKE_SOURCE_DIR}/YAPDroid/app/src/main/java/org/sqlite/database
#  COMMAND ${CMAKE_COMMAND} -E copy_directory  ${CMAKE_SOURCE_DIR}/packages/myddas/sqlite3/Android/src/org/sqlite/database
# ${CMAKE_SOURCE_DIR}/android/app/src/main/java/org/sqlite/database
# POST_BUILD
# )

  install(TARGETS  YAPDroid
          LIBRARY DESTINATION ${dlls}
          ARCHIVE DESTINATION ${dlls}
          )
