  FIND_PACKAGE(PythonLibs)
  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

  INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/CXX)

  set_source_files_properties( yap.i
                               PROPERTIES CPLUSPLUS ON)
