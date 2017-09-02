
# This is a CMake example for Python

#INCLUDE(NewUseSWIG)

include(FindPythonModule)

set (PROLOG_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/yapi.yap
        ${CMAKE_CURRENT_SOURCE_DIR}/jupyter.yap)
set (PYTHON_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/yapi.py
        ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__init__.py
        ${CMAKE_CURRENT_SOURCE_DIR}/yap4py/__main__.py
        )


set (PL ${pl_library} ${PROLOG_SOURCES} )


set (python_dlls
        $<TARGET_FILE:libYap>
        $<TARGET_FILE:YAPPython>
        $<TARGET_FILE:matrix>
        $<TARGET_FILE:regexp>
        $<TARGET_FILE:yap_rl>
        $<TARGET_FILE:tries>
        $<TARGET_FILE:itries>
        $<TARGET_FILE:sys>
        $<TARGET_FILE:yap_random>
        )
if (TARGET real)
    list(APPEND python_dlls $<TARGET_FILE:real>
            )
endif()
if (NOT WIN32)
    list(APPEND python_dlls     $<TARGET_FILE:YAP++>
            )
endif()



file(RELATIVE_PATH RELATIVE_SOURCE ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR})

configure_file("setup.py.in" ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
configure_file("MANIFEST.in" ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.in)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/YAP4PY.md" ${CMAKE_CURRENT_BINARY_DIR}/README)

INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/CXX")

SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES CPLUSPLUS ON)
SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_FLAGS "-O;-py3")
SET_SOURCE_FILES_PROPERTIES(../../swig/yap.i PROPERTIES SWIG_MODULE_NAME yap)

if (WIN32)

#set (SYS_DLLS ${GMP_LIBRARIES}  c:/msys64/mingw64/bin/libgmp-10.dll)

endif()

  # inform we are compiling YAP
  # s used in MSYS
  #
  #
  #  INSTALL ( TARGETS  ${SWIG_MODULE_Py2YAP_REAL_NAME}
  #              RUNTIME  DESTINATION ${PYTHON_MODULE_PATH}
  #  	         ARCHIVE DESTINATION ${PYTHON_MODULE_PATH}
  #  	         LIBRARY DESTINATION ${PYTHON_MODULE_PATH}
  #  	)


add_custom_target( YAP4PY_SETUP_DIRS
        COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py
        COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog
        COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/pl
        COMMAND ${CMAKE_COMMAND} -E  make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4py/prolog/os
        )


add_custom_COMMAND( OUTPUT yap4py/prolog/lists,yap
        COMMAND ${CMAKE_COMMAND} -E  copy ${PL} yap4py/prolog
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS  YAP4PY_SETUP_DIRS  ${PL}  )

add_custom_COMMAND( OUTPUT  yap4py/prolog/pl/boot.yap
        COMMAND ${CMAKE_COMMAND} -E  copy ${pl_boot_library} yap4py/prolog/pl
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS  YAP4PY_SETUP_DIRS  ${pl_boot_library}  )

add_custom_COMMAND( OUTPUT yap4py/prolog/os/yio.yap
        COMMAND ${CMAKE_COMMAND} -E   copy ${pl_os_library} yap4py/prolog/os
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS  YAP4PY_SETUP_DIRS  ${pl_os_library}  )

add_custom_COMMAND( OUTPUT  yap4py/startup.yss
        COMMAND ${CMAKE_COMMAND} -E  copy ${PYTHON_SOURCES} ${python_dlls} ${CMAKE_BINARY_DIR}/startup.yss yap4py
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS  YAP4PY_SETUP_DIRS STARTUP  )

      add_custom_target( YAP4PY ALL
        COMMAND  ${SWIG_EXECUTABLE}  -python -modern -c++ -py3 -DX_API -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/include -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/H/generated -I${CMAKE_SOURCE_DIR}/OPTYap -I../../.. -o yap_wrap.cpp yap.i
        COMMAND  ${PYTHON_EXECUTABLE} setup.py sdist bdist_conda
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
DEPENDS yap4py/yapi.py yap4py/prolog/lists,yap  yap4py/prolog/pl/boot.yap yap4py/startup.yss  yap4py/prolog/os/yio.yap )


install(CODE "execute_process"
        COMMAND ${PYTHON_EXECUTABLE} -m pip install  --force --no-index -f packages/python/swig/dist YAP4PY
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
DEPENDS YAP4PY ${CMAKE_BINARY_DIR}/${YAP_STARTUP} ${dlls} )

install(FILES ${PROLOG_SOURCES} DESTINATION ${libpl})


if (WITH_DOCS AND DOXYGEN_FOUND)

        set(CMAKE_SWIG_FLAGS -DDOXYGEN=${DOXYGEN_FOUND})

        # Run doxygen to only generate the xml
        add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
                COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc
                COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/Doxyfile.xml
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        DEPENDS ${c_headers};${c_sources};${cpp_sources};${cpp_headers}
        )

        # generate .i from doxygen .xml
        add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
                COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/doxy2swig.py -n
                ${CMAKE_BINARY_DIR}/doc/xm11l/ftdi_8c.xml
                ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i
                DEPENDS ${CMAKE_BINARY_DIR}/doc/xml/ftdi_8c.xml
                )
        add_custom_target(doc_i DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ftdi1_doc.i)
        add_dependencies(${SWIG_MODULE_ftdi1_REAL_NAME} doc_i)

    ENDIF ()
