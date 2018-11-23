# CMake module to search for YAP library
#
# If YAP_INCLUDE_DIR and YAP_PL_LIBRARY_DIR sets YAP_FOUND
# Also checks for YAP_LIBRARY 

if (YAP_ROOT)
  set( YAP_INCLUDE_DIR ../../include )  
  set( YAP_PL_LIBRARY_DIR ${YAP_INSTALL_DATADIR} )  
  set( YAP_LIBRARY libYap )  
  set( YAP_DLLS ${YAP_INSTALL_LIBDIR} )

else()

  FIND_PATH(YAP_INCLUDE_DIR YapInterface.h
  /usr/local/include/Yap 
  /usr/include/Yap
  #MSVC
  "$ENV{LIB_DIR}/include/Yap"
  #mingw
  c:/msys/local/include/Yap
  c:/Yap/include/Yap    
  c:/Yap64/include/Yap  
  "c:/Program Files/Yap/include/Yap"
  "c:/Program Files (x86)/Yap/include/Yap"
  )

FIND_PATH(YAP_PL_LIBRARY_DIR terms.yap
  /usr/local/share/Yap
  /usr/share/Yap
  #MSVC
  "$ENV{LIB_DIR}/share/Yap"
  #mingw
  c:/msys/local/share/Yap
  c:/Yap/share/Yap    
  c:/Yap64/share/Yap  
  "c:/Program Files/Yap/share/Yap"
  "c:/Program Files (x86)/Yap/share/Yap"
)

FIND_LIBRARY(YAP_LIBRARY NAMES libYap.a libYap.so libYap.dylib PATHS
  /usr/local/lib
  /usr/lib 
  #MSVC
  "$ENV{LIB_DIR}/lib"
  #mingw
  c:/msys/local/lib
  c:/msys/local/lib
  c:/Yap/lib    
  c:/Yap64/lib  
  "c:/Program Files/Yap/lib"
  "c:/Program Files (x86)/Yap/lib"
  )

If (YAP_INCLUDE_DIR AND YAP_PL_LIBRARY_DIR)
   SET(YAP_FOUND TRUE)
ENDIF (YAP_INCLUDE_DIR AND YAP_PL_LIBRARY_DIR)

IF (YAP_FOUND)

   IF (NOT YAP_FIND_QUIETLY)
      MESSAGE(STATUS "Found YAP: ${YAP_LIBRARY}")
   ENDIF (NOT YAP_FIND_QUIETLY)

   get_filename_component( YAP_DLLS ${YAP_LIBRARY} PATH )
   set( dlls  ${YAP_DLLS}/Yap  )    

ELSE (YAP_FOUND)

   IF (YAP_FIND_REQUIRED)
      MESSAGE(SYSTEM_ERROR_FATAL "Could not find YAP")
   ENDIF (YAP_FIND_REQUIRED)

 ENDIF (YAP_FOUND)

endif()

