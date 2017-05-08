

set (PYTHON_SOURCES
    __main__.py
    __init__.py
    setup.py
    _version.py
    interactiveshell.py
    kernelapp.py
kernelspec.py
yapkernel.py )


configure_file("setup.py" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
foreach(i ${PYTHON_SOURCES})
  configure_file(${i} ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/${i})
endforeach()
configure_file("_version.py" ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/_version.py)
configure_file("YAP_KERNEL.md" ${CMAKE_CURRENT_BINARY_DIR}/README)
configure_file("${CMAKE_SOURCE_DIR}/docs/icons/yap_32x32x32.png" ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-32x32.png)
configure_file("${CMAKE_SOURCE_DIR}/docs/icons/yap_64x64x32.png" ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-64x64.png)
configure_file("${CMAKE_SOURCE_DIR}/misc/editors/prolog.js" ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/prolog.js)


set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
set(   PYTHON_INSTALL sdist bdist_wheel)

add_custom_target( YAPKernel ALL
  )

add_custom_command (TARGET YAPKernel
  COMMAND ${PYTHON_EXECUTABLE}  setup.py clean sdist bdist_wheel
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS libYap ${SWIG_MODULE_Py2YAP_REAL_NAME}

  )

install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install --no-index -f dist yap_kernel")

install(FILES jupyter.yap
  DESTINATION ${libpl}
  )

