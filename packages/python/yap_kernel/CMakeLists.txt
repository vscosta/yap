
    set (PYTHON_SOURCES
            yap_ipython/core/getipython.py
            yap_ipython/core/__init__.py
            yap_ipython/core/interactiveshell.py
            # yap_ipython/core/modulefind.py
            yap_ipython/core/oinspect.py
            yap_ipython/core/release.py
            yap_ipython/core/shellapp.py
            yap_ipython/display.py
            yap_ipython/__init__.py
            yap_ipython/shellapp.py
            yap_kernel_launcher.py
            yap_kernel/__init__.py
            yap_kernel/__main__.py
            yap_kernel/_version.py
            yap_kernel/codeutil.py
            yap_kernel/connect.py
            yap_kernel/datapub.py
            yap_kernel/displayhook.py
            yap_kernel/embed.py
            yap_kernel/eventloops.py
            yap_kernel/heartbeat.py
            yap_kernel/interactiveshell.py
            yap_kernel/iostream.py
            yap_kernel/jsonutil.py
            yap_kernel/kernelapp.py
            yap_kernel/kernelbase.py
            yap_kernel/kernelspec.py
            yap_kernel/log.py
            yap_kernel/parentpoller.py
            yap_kernel/pickleutil.py
            yap_kernel/serialize.py
            yap_kernel/yapkernel.py
            yap_kernel/zmqshell.py
            yap_kernel/comm/__init__.py
            yap_kernel/comm/comm.py
            yap_kernel/comm/manager.py
            yap_kernel/gui/__init__.py
            yap_kernel/gui/gtk3embed.py
            yap_kernel/gui/gtkembed.py
            yap_kernel/inprocess/__init__.py
            yap_kernel/inprocess/blocking.py
            yap_kernel/inprocess/channels.py
            yap_kernel/inprocess/client.py
            yap_kernel/inprocess/ipkernel.py
            yap_kernel/inprocess/manager.py
            yap_kernel/inprocess/socket.py
            yap_kernel/pylab/__init__.py
            yap_kernel/pylab/backend_inline.py
            yap_kernel/pylab/config.py
            )

            set (EXTRAS
                    MANIFEST.in
                    YAP_KERNEL.md
                    setup.py
                    setup.cfg
                    README.md
                )

                set (RESOURCES
                    resources/custom.js
                    resources/prolog.js
                    #resources/logo-32x32.png
                    #resources/logo-64x64.png
            )
            set (RENAMED_RESOURCES
                resources/logo-32x32.png
                resources/logo-64x64.png
    # resources/codemirror/mode/prolog/prolog.js
    )

            set (PL_SOURCES
                    yap_ipython/prolog/jupyter.yap
                    )

set(FILES ${PYTHON_SOURCES} ${PL_SOURCES} ${EXTRAS} ${RESOURCES})

    set(SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)

    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${i}
      COMMAND ${CMAKE_COMMAND} -E tar cvf ${CMAKE_CURRENT_BINARY_DIR}/yap.tgz  ${FILESi}


      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)


add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/resources/logo-32x32.png
            COMMAND ${CMAKE_COMMAND} -E tar xvf yap.tgz 
            COMMAND ${CMAKE_COMMAND} -E copy_if_changed ${CMAKE_SOURCE_DIR}/docs/icons/yap_32z32x32.png  ${CMAKE_CURRENT_BINARY_DIR}/resources/logo-32x32.png
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/docs/icons/yap_32x32x32.png
        )


        add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/resources/logo-64x64.png
                    COMMAND ${CMAKE_COMMAND} -E copy_if_changed ${CMAKE_SOURCE_DIR}/docs/icons/yap_64x64x32.png  ${CMAKE_CURRENT_BINARY_DIR}/resources/logo-64x64.png
                )

	      add_custom_target(YAP_KERNEL
		COMMAND ${CMAKE_COMMAND} -E tar xvf yap.tgz
        COMMAND ${PYTHON_EXECUTABLE}  ${SETUP_PY} build sdist bdist
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	    )

	  
    install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install     --ignore-installed --no-deps  .
        COMMAND ${PYTHON_EXECUTABLE} -m yap_kernel.kernelspec
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

      install(FILES ${PL_SOURCES} DESTINATION ${libpl} )
