# CMake module to search for YAP library
#
# If YAP_INCLUDE_DIR and YAP_PL_LIBRARY_DIR sets YAP_FOUND
# Also checks for YAP_LIBRARY 

FIND_PATH(YAP_INCLUDE_DIR YapInterface.h
  /usr/local/include/Yap 
  /usr/include/Yap
  #MSVC
  "$ENV{LIB_DIR}/include/Yap"
  #mingw
  c:/msys/local/include/Yap
  )

FIND_PATH(YAP_PL_LIBRARY_DIR terms.yap
  /usr/local/share/Yap
  /usr/share/Yap
  #MSVC
  "$ENV{LIB_DIR}/share/Yap"
  #mingw
  c:/msys/local/share/Yap
  )

FIND_LIBRARY(YAP_LIBRARY NAMES libyap.a PATHS 
  /usr/local/lib
  /usr/lib 
  #MSVC
  "$ENV{LIB_DIR}/lib"
  #mingw
  c:/msys/local/lib
  )

IF (YAP_INCLUDE_DIR AND YAP_PL_LIBRARY_DIR)
   SET(YAP_FOUND TRUE)
ENDIF (YAP_INCLUDE_DIR AND YAP_PL_LIBRARY_DIR)

IF (YAP_FOUND)

   IF (NOT YAP_FIND_QUIETLY)
      MESSAGE(STATUS "Found YAP: ${YAP_LIBRARY}")
   ENDIF (NOT YAP_FIND_QUIETLY)

ELSE (YAP_FOUND)

   IF (YAP_FIND_REQUIRED)
      MESSAGE(SYSTEM_ERROR_FATAL "Could not find YAP")
   ENDIF (YAP_FIND_REQUIRED)

ENDIF (YAP_FOUND)