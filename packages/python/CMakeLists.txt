find_package (Python3 COMPONENTS Interpreter Development NumPy)

if(WITH_Python3 AND Python3_FOUND AND Python3_Development_FOUND)
set (PYTHON_SOURCES python.c pl2py.c pybips.c py2pl.c pl2pl.c  pypreds.c pyio.c
)
set (PYTHON_HEADERS pyya4p.h)
set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

include_directories( BEFORE ${Python3_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}
  ${GMP_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/os ${CMAKE_SOURCE_DIR}/H ${CMAKE_SOURCE_DIR}/OPTYap  ${CMAKE_SOURCE_DIR}/utf8proc)

  list(APPEND CMAKE_REQUIRED_INCLUDES ${Python3_INCLUDE_DIRS} )
if (Python3_NumPy_FOUND)
include_directories( BEFORE 
  ${Python3_NumPy_INCLUDE_DIRS})
  list(APPEND CMAKE_REQUIRED_INCLUDES ${Python3_NumPy_INCLUDE_DIRS} )
endif()

  list(APPEND CMAKE_REQUIRED_LIBRARIES ${Python3_LIBRARY})

check_include_file(Python.h HAVE_PYTHON_H)

#talk to python.pl
add_library(YAPPython  pyload.c)

if (WIN32)
add_library (Py4YAP OBJECT ${PYTHON_SOURCES})
else()
add_library(Py4YAP SHARED  ${PYTHON_SOURCES})
endif()

set_target_properties(
    Py4YAP
PROPERTIES VERSION ${YAP_FULL_VERSION}
SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
)

# arithmetic hassle.      
target_link_libraries(Py4YAP  libYap  ${Python3_LIBRARIES})


target_link_libraries(YAPPython Py4YAP  YAP++ libYap)

get_target_property(LIBPy4YAP  Py4YAP  <TARGET_FILE:Py4YAP>)



# arithmetic hassle.
set_property(TARGET YAPPython PROPERTY CXX_STANDARD_REQUIRED ON)


set_property( SOURCE ${PYTHON_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1 _GNU_SOURCE=1)

set (PYTHON_PL python.pl completions.yap jupyter.yap lsp.yap yapi.yap toks_lsp.yap verify.yap)


install(FILES ${PYTHON_PL} DESTINATION ${YAP_INSTALL_DATADIR} )

    install(TARGETS  YAPPython Py4YAP
      LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )

add_feature_info(YAPPython "Python3_Development_FOUND" "Python ${Python3_VERSION}, at ${Python3_LIBRARY_DIRS}, ${Python3_INCLUDE_DIRS}, and ${Python3_RUNTIME_LIBRARY_DIRS}")
if (Python3_NumPy_FOUND)
add_feature_info(YAPNumPy "Python3_NumPy_FOUND" "NumPy ${Python3_NumPy_VERSION} at ${Python3_NumPy_INCLUDE_DIRS}")
endif()
    



add_subdirectory (yap4py)
 # add_subdirectory(packages/python/yapkernel)
    
endif()
