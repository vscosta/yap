
#CHECK: PythonLibs, changed to work in WIN32

if (PYTHONLIBS_FOUND)

  include(FindPythonModule)

  set (PYTHON_SOURCES python.c pl2py.c pybips.c py2pl.c pl2pl.c pypreds.c)

  set (PYTHON_HEADERS  python.h)

  set (CMAKE_POSITION_INDEPENDENT_CODE TRUE)

  add_library (YAPPython SHARED ${PYTHON_SOURCES})

  target_link_libraries(YAPPython libYap ${PYTHON_LIBRARY}  ${WINDLLS}  ${GMP_LIBRARIES})

  set_property( SOURCE ${PYTHON_SOURCES}  APPEND PROPERTY COMPILE_DEFINITIONS   YAP_KERNEL=1)

  set (PYTHON_PL python.pl)

  install(FILES python.pl DESTINATION ${libpl} )

  add_to_group( pl_library PYTHON_PL )
  
#  configure_file ("setup.py.cmake"  "setup.py" )

  set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

  #set_target_properties (YAPPython PROPERTIES PREFIX "")

  find_python_module( jupyter )

  if (PY_JUPYTER)

    add_subdirectory(yap_kernel)

  ENDIF()

  IF(WIN32)
    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${libdir}
      RUNTIME DESTINATION ${bindir}
      ARCHIVE DESTINATION ${libdir} )
  else()
    install(TARGETS  YAPPython
      LIBRARY DESTINATION ${libdir}
      RUNTIME DESTINATION ${libdir}
      ARCHIVE DESTINATION ${libdir} )
endif()

endif (PYTHONLIBS_FOUND)
