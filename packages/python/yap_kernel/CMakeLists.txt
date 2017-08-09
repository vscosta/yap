
    set (EXTRAS
            MANIFEST.in
            YAP_KERNEL.md
            )

    set (PYTHON_SOURCES
            yap_kernel_launcher.py
            data_kernelspec/kernel.json
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

    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources)
    file( GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-32x32.png INPUT ${CMAKE_SOURCE_DIR}/docs/icons/yap_32x32x32.png )
    file( GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-64x64.png INPUT   ${CMAKE_SOURCE_DIR}/docs/icons/yap_64x64x32.png )
    file( COPY ${CMAKE_SOURCE_DIR}/misc/editors/prolog.js DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/)

    set(SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)

    add_custom_target( YAPKernel ALL
            COMMAND ${PYTHON_EXECUTABLE}  ${SETUP_PY} build sdist bdist
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            )

    install(CODE "execute_process(COMMAND ${PYTHON_EXECUTABLE} -m pip install     --ignore-installed --no-deps  .
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

    #    install(FILES jupyter.yap
    #    DESTINATION ${libpl} )
