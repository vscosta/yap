
  # This is a CMake file for SWIG and Android

FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/java/pt/up/yap/lib )
FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/assets)
  set(CMAKE_SWIG_OUTDIR    ${YAP_APP_DIR}/src/generated/java/pt/up/yap/lib )
  set( SWIG_MODULE_NAME pt.up.yap.lib )
  set ( pllib ${YAP_APP_DIR}/src/generated/assets/Yap )

    set ( SWIG_SOURCES ${CMAKE_SOURCE_DIR}/packages/swig/yap.i )
    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

  include_directories (
    ${CMAKE_SOURCE_DIR}/CXX
       )
 set( GMP_ROOT ${CMAKE_SOURCE_DIR}/../gmp/${ANDROID_ABI} )
   set (GMP_INCLUDE_DIRS ${GMP_ROOT})
   set (GMP_LIBRARIES ${GMP_ROOT}/libgmp.so)


      add_custom_command (OUTPUT  yap_swig.cpp
        	COMMAND ${CMAKE_COMMAND} -E  make_directory  ${pllib}
        	COMMAND ${CMAKE_COMMAND} -E  make_directory ${pllib}/pl
        	COMMAND ${CMAKE_COMMAND} -E  make_directory ${pllib}/os

        COMMAND ${CMAKE_COMMAND} -E  copy ${pl_library} ${pllib}
        COMMAND ${CMAKE_COMMAND} -E  copy ${pl_boot_library} ${pllib}/pl
        COMMAND ${CMAKE_COMMAND} -E   copy ${pl_os_library} ${pllib}/os
           COMMAND ${SWIG_EXECUTABLE} -c++  -java -package ${SWIG_MODULE_NAME} -outdir ${CMAKE_SWIG_OUTDIR} -outcurrentdir -addextern -I${CMAKE_SOURCE_DIR}/CXX  -I${CMAKE_SOURCE_DIR}/include  -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_BINARY_DIR}  -I${GMP_INCLUDE_DIRS} -DX_API=""  -o yap_swig.cpp ${SWIG_SOURCES}
           DEPENDS ${SWIG_SOURCES} YAP++
   	)

   add_custom_command (OUTPUT  swig_streamer.cpp
        COMMAND ${SWIG_EXECUTABLE} -c++  -java -package ${SWIG_MODULE_NAME} -outdir ${CMAKE_SWIG_OUTDIR} -outcurrentdir -addextern  -I${CMAKE_CURRENT_SOURCE_DIR} -o swig_streamer.cpp streamer.i
        DEPENDS streamer.i
	)


      # GMP_FOUND         - true if GMP/MPIR was found
      # GMP_INCLUDE_DIRS  - include search path
      # GMP_LIBRARIES      - libraries to link with
      #config.h needs this (TODO: change in code latter)
      include_directories(  .;${GMP_INCLUDE_DIRS};${CMAKE_SOURCE_DIR}/include;${CMAKE_SOURCE_DIR}/H;${CMAKE_SOURCE_DIR}/H/generated;${CMAKE_SOURCE_DIR}/os;${CMAKE_SOURCE_DIR}/OPTYap;${CMAKE_BINARY_DIR};${CMAKE_CURRENT_SOURCE_DIR} )




      add_lib(YAPJava 
	yap_swig.cpp swig_streamer.cpp streamer.cpp streamer.h
	)
	target_link_libraries(YAPJava ${GMP_LIBRARIES} )


	target_link_libraries( YAPJava YAP++ libYap android log)

      if (FALSE)

	set (SWIG_ADD_MODULE YAPJava SHARED  CPLUPLUS ${SWIG_SOURCES} )
	# Define swig module with given name and specified language


	set (SWIG_LINK_LIBRARIES YAPJava YAP++ libYAP )
	#- Link libraries to swig module


	add_library (YAPJavaTop SHARED
	  main.cpp main.h
	  )

	target_link_libraries( YAPJavaTop ${SWIG_MODULE_${YAPJava}_REAL_NAME} YAP++ libYap android)

      endif()
