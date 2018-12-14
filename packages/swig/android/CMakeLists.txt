
      # This is a CMake file for SWIG and Android

      set(JAVA_SWIG_OUTDIR ${CMAKE_SOURCE_DIR}/../yaplib/src/generated/java/pt/up/yap/lib)
      set(SWIG_CXX_DIR ${CMAKE_BINARY_DIR}/src/generated/jni)
      FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/src/generated/java/pt/up/yap/lib)
      FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/src/generated/assets/Yap/pl)
      FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/src/generated/assets/so)
      FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/src/generated/jni)
      set(YAP_ASSETS ${CMAKE_SOURCE_DIR}/../yaplib/src/generated/assets/Yap)

      set(SWIG_SOURCES ${CMAKE_SOURCE_DIR}/packages/swig/yap.i)
      SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

      include_directories(
              ${CMAKE_SOURCE_DIR}/CXX
      )
      set(GMP_ROOT ${CMAKE_SOURCE_DIR}/../gmp/${ANDROID_ABI})
      set(GMP_INCLUDE_DIRS ${GMP_ROOT})
      set(GMP_LIBRARIES ${GMP_ROOT}/libgmp.so)


      file(INSTALL ${pl_library} DESTINATION ${YAP_ASSETS})
      file(INSTALL ${CMAKE_CURRENT_SOURCE_DIR}/android.yap DESTINATION ${YAP_ASSETS})

      file(INSTALL ${pl_boot_library} DESTINATION ${YAP_ASSETS}/pl)
      file(INSTALL ${pl_os_library} DESTINATION ${YAP_ASSETS}/os)


      execute_process(COMMAND swig -c++ -java -package pt.up.yap.lib -O -outdir ${JAVA_SWIG_OUTDIR} -addextern -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/include -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_BINARY_DIR} -I${GMP_INCLUDE_DIRS} -DX_API -o ${SWIG_CXX_DIR}/yapi_swig.cxx ${SWIG_SOURCES}
              )

      execute_process(COMMAND ${SWIG_EXECUTABLE} -c++ -java -package pt.up.yap.lib -O -outdir ${JAVA_SWIG_OUTDIR} -addextern -I${CMAKE_CURRENT_SOURCE_DIR} -o ${SWIG_CXX_DIR}/streamer_swig.cxx streamer.i
              )


      add_library(YAPJava OBJECT
              ${CMAKE_BINARY_DIR}/src/generated/jni/streamer_swig.cxx
              ${CMAKE_BINARY_DIR}/src/generated/jni/yapi_swig.cxx
              streamer.cpp
              )

      set_target_properties(YAPJava PROPERTIES LINKER_LANGUAGE CXX)

      # GMP_FOUND         - true if GMP/MPIR was found
      # GMP_INCLUDE_DIRS  - include search path
      # GMP_LIBRARIES      - libraries to link with
      #config.h needs this (TODO: change in code latter)
      include_directories(.;${GMP_INCLUDE_DIRS};${CMAKE_SOURCE_DIR}/include;${CMAKE_SOURCE_DIR}/H;${CMAKE_SOURCE_DIR}/H/generated;${CMAKE_SOURCE_DIR}/os;${CMAKE_SOURCE_DIR}/OPTYap;${CMAKE_BINARY_DIR};${CMAKE_CURRENT_SOURCE_DIR})


      if (FALSE)

          set(SWIG_ADD_MODULE YAPJava SHARED CPLUSPLUS ${SWIG_SOURCES})
          # Define swig module with given name and specified language


          set(SWIG_LINK_LIBRARIES YAPJava YAP++ libYAP)
          #- Link libraries to swig module


          add_library(YAPJavaTop SHARED
                  main.cpp main.h
                  )

          target_link_libraries(YAPJavaTop ${SWIG_MODULE_${YAPJava}_REAL_NAME} YAP++ libYap android)

      endif ()
