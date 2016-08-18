configure_file ("setup.py.cmake"  "setup.py" )


set(SETUP_PY "${CMAKE_CURRENT_BINARY_DIR}/setup.py")

add_custom_target ( YAPKernel ALL
        COMMAND ${PYTHON_EXECUTABLE} setup.py build -f
        DEPENDS yap_kernel.py
       )


install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} ${SETUP_PY} install -f
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

   install(FILES jupyter.yap
     DESTINATION ${libpl}
     )
