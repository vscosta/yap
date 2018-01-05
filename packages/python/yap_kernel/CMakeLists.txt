    set (EXTRAS
            MANIFEST.in
            YAP_KERNEL.md
            )

    set (PL_SOURCES
            yap_ipython/prolog/jupyter.yap
            )

    set (PYTHON_SOURCES
            yap_ipython/core/getipython.py
            yap_ipython/core/__init__.py
            yap_ipython/core/interactiveshell.py
            yap_ipython/core/modulefind.py
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

            configure_file(setup.py.in ${CMAKE_CURRENT_BINARY_DIR}/setup.py)
            configure_file(${CMAKE_SOURCE_DIR}/misc/editors/prolog.js.in ${CMAKE_CURRENT_BINARY_DIR}/prolog.js )


    set(SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)

    add_custom_target( YAPKernel ALL
        COMMAND ${PYTHON_EXECUTABLE}  ${SETUP_PY} build sdist bdist
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            DEPENDS YAP4PY
            )

    install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install     --ignore-installed --no-deps  .
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

      install(FILES ${PL_SOURCES} DESTINATION ${libpl} )
