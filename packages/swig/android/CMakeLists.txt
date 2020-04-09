
  # This is a CMake file for SWIG and Android

FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/java/pt/up/yap/lib )
FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/assets)
FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/jni)
  set ( pllib ${YAP_APP_DIR}/src/generated/assets/Yap )

    set ( SWIG_SOURCES ${CMAKE_SOURCE_DIR}/packages/swig/yap.i )
    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

  include_directories (
    ${CMAKE_SOURCE_DIR}/CXX
       )
 set( GMP_ROOT ${CMAKE_SOURCE_DIR}/../gmp/${ANDROID_ABI} )
   set (GMP_INCLUDE_DIRS ${GMP_ROOT})
   set (GMP_LIBRARIES ${GMP_ROOT}/libgmp.so)


      add_custom_target (pllib
         	COMMAND ${CMAKE_COMMAND} -E  make_directory  ${pllib}
         COMMAND ${CMAKE_COMMAND} -E  copy ${pl_library} ${pllib}
 DEPENDS  ${pl_library}
)

 	      add_custom_target (pllibpl
         	COMMAND ${CMAKE_COMMAND} -E  make_directory  ${pllib}/pl
         COMMAND ${CMAKE_COMMAND} -E  copy ${pl_boot_library} ${pllib}/pl
DEPENDS pllib ${pl_boot_library}
 	)
 	      add_custom_target (pllibos ALL
         	COMMAND ${CMAKE_COMMAND} -E  make_directory  ${pllib}/os
         COMMAND ${CMAKE_COMMAND} -E   copy ${pl_os_library} ${pllib}/os
DEPENDS pllibpl ${pl_os_library}
    	)

    add_custom_command (OUTPUT  ${CMAKE_SWIG_OUTPUT}/swig_streamer.cpp
         COMMAND ${SWIG_EXECUTABLE} -c++  -java -package ${SWIG_MODULE_NAME} -outdir ${CMAKE_SWIG_OUTDIR}  -addextern  -I${CMAKE_CURRENT_SOURCE_DIR} -o ${CMAKE_SWIG_OUTPUT}/swig_streamer.cpp -oh ${CMAKE_SWIG_OUTPUT}/swig_streamer.hh streamer.i
         DEPENDS streamer.i
 	)

 	     add_custom_command (OUTPUT  ${CMAKE_SWIG_OUTPUT}/yap_swig.cpp
           COMMAND ${SWIG_EXECUTABLE} -c++  -java -package ${SWIG_MODULE_NAME} -outdir ${CMAKE_SWIG_OUTDIR}  -addextern -I${CMAKE_SOURCE_DIR}/CXX  -I${CMAKE_SOURCE_DIR}/include  -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_BINARY_DIR}  -I${GMP_INCLUDE_DIRS} -DX_API=""  -o ${CMAKE_SWIG_OUTPUT}/yap_swig.cpp -oh ${CMAKE_SWIG_OUTPUT}/yap_swig.hh ${SWIG_SOURCES}
	   DEPENDS pllibos  ${SWIG_SOURCES} YAP++)



         add_library(YAPJava OBJECT
    	${CMAKE_SWIG_OUTPUT}/swig_streamer.cpp
    	${CMAKE_SWIG_OUTPUT}/yap_swig.cpp
    	streamer.cpp
    	)


      # GMP_FOUND         - true if GMP/MPIR was found
      # GMP_INCLUDE_DIRS  - include search path
      # GMP_LIBRARIES      - libraries to link with
      #config.h needs this (TODO: change in code latter)
      include_directories(  .;${GMP_INCLUDE_DIRS};${CMAKE_SOURCE_DIR}/include;${CMAKE_SOURCE_DIR}/H;${CMAKE_SOURCE_DIR}/H/generated;${CMAKE_SOURCE_DIR}/os;${CMAKE_SOURCE_DIR}/OPTYap;${CMAKE_BINARY_DIR};${CMAKE_CURRENT_SOURCE_DIR} )





      if (FALSE)

	set (SWIG_ADD_MODULE YAPJava SHARED  CPLUSPLUS ${SWIG_SOURCES} )
	# Define swig module with given name and specified language


	set (SWIG_LINK_LIBRARIES YAPJava YAP++ libYAP )
	#- Link libraries to swig module


	add_library (YAPJavaTop SHARED
	  main.cpp main.h
	  )

	target_link_libraries( YAPJavaTop ${SWIG_MODULE_${YAPJava}_REAL_NAME} YAP++ libYap android)

      endif()
