

macro_optional_find_package(Java ON)

 find_package(Java COMPONENTS Development)
 #find_package(Java COMPONENTS Runtime)
 
macro_log_feature (Java_Development_FOUND "Java"
    "Use Java System"
    "http://www.java.org" FALSE)
macro_optional_find_package(JNI ON)
    macro_log_feature (JNI_FOUND "JNI"
    "Use Java Native Interface"
    "http://www.java.org" FALSE)


if (Java_Development_FOUND)

    include ( UseJava  )

    
  #  SET(CMAKE_SWIG_FLAGS -package YAP)

    SET(SWIG_SOURCES 
      ../yap.i
      )

    set( CMAKE_CXX_FAGS "${CMAKE_CXX_FLAGS} -Wno-missing-declarations")

   include_directories (
ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ       ${CMAKE_CURRENT_SOURCE_DIR}
      ${JAVA_INCLUDE_DIRS}
      ${JNI_INCLUDE_DIRS}
      )
    set (CMAKE_SWIG_OUTDIR  ${CMAKE_CURRENT_BINARY_DIR} )

    #set (CMAKE_JAVA_CLASS_OUTPUT_PATH java)

    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)
    
  #  SWIG_ADD_MODULE(jSWIG java  ${SWIG_SOURCES} )

  #  SWIG_LINK_LIBRARIES(jSWIG ${JAVA_LIBRARIES} ${JNI_LIBRARIES} Yap++)

    set_source_files_properties(../yap.i PROPERTY CPLUSPLUS ON)

   add_jar(JavaYAP
     SOURCES JavaYAP.java
     ENTRY_POINT JavaYAP
     )
   install_jar(JavaYAP ${libpl}/JavaYAP)

   set(CMAKE_JNI_TARGET TRUE)
   ADD_CUSTOM_COMMAND(TARGET JavaYAP
     POST_BUILD
     COMMAND cmake -E echo "Compiling Java files..."
     COMMAND ${Java_JAVAC_EXECUTABLE} *.java
     COMMAND cmake -E echo "Creating jar file..."
     COMMAND ${Java_JAR_EXECUTABLE} uvf JavaYAP.jar *.class
     )
#    install_jni_symlink(shibboleet ${JAVA_LIB_INSTALL_DIR})

#      SET_TARGET_PROPERTIES(jSWIG PROPERTIES OUTPUT_NAME "JavaYAP")
  

#   add_dependencies(jSWIG JavaYAP)

 Endif (Java_Development_FOUND)

  
