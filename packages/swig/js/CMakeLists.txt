

macro_optional_find_package(Java ON)

 find_package(Java COMPONENTS Runtime Development)
 #find_package(Java COMPONENTS Runtime)

macro_optional_find_package(JNI ON)


if (Java_Development_FOUND)

    include ( UseJava  )
    include ( UseSWIG  )


  #  SET(CMAKE_SWIG_FLAGS -package YAP)

    set (CMAKE_SWIG_OUTDIR  ${CMAKE_CURRENT_BINARY_DIR} )

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

    SWIG_ADD_MODULE(Native java ${SWIG_SOURCES}
	   )

	add_dependencies(Native Yap++ )
	 
    SWIG_LINK_LIBRARIES (Native Yap++ )
      

   set(CMAKE_JNI_TARGET 1)
    ADD_JAR( NativeJar
      SOURCES "../../../misc/icons/yap_16x16x32.png"
      DEPENDS Native
      PROPERTIES OUTPUT_NAME Native
      )
    install_jar(NativeJar ${libpl})
    	add_dependencies(NativeJar Native )
    #install_jni_symlink(NativeJar  .)


    ADD_CUSTOM_COMMAND(TARGET NativeJar
      POST_BUILD
     COMMAND cmake -E echo "Compiling Java files..."
     COMMAND ${Java_JAVAC_EXECUTABLE} *.java -cp .
     COMMAND cmake -E echo "Creating jar file..."
     COMMAND ${Java_JAR_EXECUTABLE} -ufM Native.jar *.class
     )

   ADD_JAR( JavaYAP
     SOURCES "JavaYAP.java"
     ENTRY_POINT JavaYAP
     INCLUDE_JARS NativeJar )
   SET(CMAKE_JAVA_COMPILE_FLAGS "-source" "1.8" "-target" "1.8")
install(    TARGETS Native  
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE  DESTINATION ${libdir}
    LIBRARY  DESTINATION ${libdir}
)

install_jar(JavaYAP ${libpl})
#install_jni_symlink(JavaYAP ${libpl})



 Endif (Java_Development_FOUND)
