
#CHECK: PythonLibs

if (PYTHONLIBS_FOUND)

include(FindPythonModule)  


 include("${CMAKE_SOURCE_DIR}/packages/python/Sources.cmake")

add_library (YAPPython SHARED ${PYTHON_SOURCES})


  target_link_libraries(YAPPython libYap ${PYTHON_LIBRARIES})

  set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

add_custom_target ( YAPex ALL
        COMMAND ${PYTHON_EXECUTABLE} setup.py build -f
        DEPENDS yapex.py  )

#  set_target_properties (YAPPython PROPERTIES PREFIX "")

  include_directories (${PYTHON_INCLUDE_DIRS})

  install(TARGETS  YAPPython
    LIBRARY DESTINATION ${dlls}
     ARCHIVE DESTINATION ${dlls}
   )

  install(FILES python.pl
    DESTINATION ${libpl}
    )

configure_file ("setup.py.cmake"  "setup.py" )

install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} ${SETUP_PY} install -f
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

find_python_module( jupyter )

if (PY_JUPYTER)

add_subdirectory(yap_kernel)

endif()

endif (PYTHONLIBS_FOUND)
