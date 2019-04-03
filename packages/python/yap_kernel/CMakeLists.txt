set (PYTHON_SOURCES 
#core/yap_kernel/__init__.py
#core/yap_kernel/getipython.py
#core/__init__.py
_version.py
yap_kernel/datapub.py
yap_kernel/serialize.py
yap_kernel/embed.py
yap_kernel/_version.py
yap_kernel/connect.py
yap_kernel/iostream.py
yap_kernel/log.py
yap_kernel/parentpoller.py
yap_kernel/jsonutil.py
yap_kernel/ipkernel.py
yap_kernel/kernelspec.py
yap_kernel/eventloops.py
yap_kernel/_eventloop_macos.py
yap_kernel/inprocess/ipkernel.py
yap_kernel/inprocess/client.py
yap_kernel/inprocess/constants.py
yap_kernel/inprocess/tests/test_kernelmanager.py
yap_kernel/inprocess/tests/__init__.py
yap_kernel/inprocess/tests/test_kernel.py
yap_kernel/inprocess/__init__.py
yap_kernel/inprocess/blocking.py
yap_kernel/inprocess/channels.py
yap_kernel/inprocess/socket.py
yap_kernel/inprocess/manager.py
yap_kernel/tests/test_jsonutil.py
yap_kernel/tests/test_zmq_shell.py
yap_kernel/tests/test_pickleutil.py
yap_kernel/tests/test_embed_kernel.py
yap_kernel/tests/test_connect.py
yap_kernel/tests/test_start_kernel.py
yap_kernel/tests/_asyncio.py
yap_kernel/tests/__init__.py
yap_kernel/tests/test_io.py
yap_kernel/tests/test_kernelspec.py
yap_kernel/tests/test_message_spec.py
yap_kernel/tests/utils.py
yap_kernel/tests/test_kernel.py
yap_kernel/tests/test_serialize.py
yap_kernel/tests/test_eventloop.py
yap_kernel/__init__.py
yap_kernel/comm/comm.py
yap_kernel/comm/__init__.py
yap_kernel/comm/manager.py
yap_kernel/zmqshell.py
yap_kernel/gui/gtk3embed.py
yap_kernel/gui/__init__.py
yap_kernel/gui/gtkembed.py
yap_kernel/codeutil.py
yap_kernel/heartbeat.py
yap_kernel/kernelapp.py
yap_kernel/displayhook.py
yap_kernel/pickleutil.py
yap_kernel/kernelbase.py
yap_kernel/pylab/backend_inline.py
yap_kernel/pylab/config.py
yap_kernel/pylab/__init__.py
yap_kernel/__main__.py
yap_kernel.py
kernelspec.py
#__init__.py
yap_kernel_launcher.py
docs/conf.py
setup.py
interactiveshell.py
examples/embedding/internal_ipkernel.py
examples/embedding/ipkernel_qtapp.py
examples/embedding/inprocess_terminal.py
examples/embedding/ipkernel_wxapp.py
examples/embedding/inprocess_qtconsole.py
kernelapp.py
yap_ipython/config.py
yap_ipython/core/prefilter.py
yap_ipython/core/magic.py
yap_ipython/core/historyapp.py
yap_ipython/core/hooks.py
yap_ipython/core/completerlib.py
yap_ipython/core/alias.py
yap_ipython/core/release.py
yap_ipython/core/display_trap.py
yap_ipython/core/profiledir.py
yap_ipython/core/error.py
yap_ipython/core/formatters.py
yap_ipython/core/events.py
yap_ipython/core/tests/print_argv.py
yap_ipython/core/tests/test_extension.py
yap_ipython/core/tests/test_shellapp.py
yap_ipython/core/tests/test_compilerop.py
yap_ipython/core/tests/test_handlers.py
yap_ipython/core/tests/nonascii.py
yap_ipython/core/tests/simpleerr.py
yap_ipython/core/tests/refbug.py
yap_ipython/core/tests/tclass.py
yap_ipython/core/tests/test_pylabtools.py
yap_ipython/core/tests/test_magic_terminal.py
yap_ipython/core/tests/test_run.py
yap_ipython/core/tests/test_imports.py
yap_ipython/core/tests/test_prompts.py
yap_ipython/core/tests/test_display.py
yap_ipython/core/tests/bad_all.py
yap_ipython/core/tests/test_page.py
yap_ipython/core/tests/test_interactiveshell.py
yap_ipython/core/tests/test_ultratb.py
yap_ipython/core/tests/__init__.py
yap_ipython/core/tests/daft_extension/daft_extension.py
yap_ipython/core/tests/test_profile.py
yap_ipython/core/tests/test_iplib.py
yap_ipython/core/tests/test_magic_arguments.py
yap_ipython/core/tests/test_displayhook.py
yap_ipython/core/tests/test_magic.py
yap_ipython/core/tests/test_hooks.py
yap_ipython/core/tests/test_inputsplitter.py
yap_ipython/core/tests/test_alias.py
yap_ipython/core/tests/test_inputtransformer.py
yap_ipython/core/tests/test_prefilter.py
yap_ipython/core/tests/test_paths.py
yap_ipython/core/tests/test_splitinput.py
yap_ipython/core/tests/test_completerlib.py
yap_ipython/core/tests/test_completer.py
yap_ipython/core/tests/test_application.py
yap_ipython/core/tests/test_debugger.py
yap_ipython/core/tests/test_events.py
yap_ipython/core/tests/test_autocall.py
yap_ipython/core/tests/test_history.py
yap_ipython/core/tests/test_oinspect.py
yap_ipython/core/tests/nonascii2.py
yap_ipython/core/tests/test_formatters.py
yap_ipython/core/tests/test_logger.py
yap_ipython/core/magics/logging.py
yap_ipython/core/magics/execution.py
yap_ipython/core/magics/config.py
yap_ipython/core/magics/pylab.py
yap_ipython/core/magics/osm.py
yap_ipython/core/magics/code.py
yap_ipython/core/magics/__init__.py
yap_ipython/core/magics/display.py
yap_ipython/core/magics/basic.py
yap_ipython/core/magics/extension.py
yap_ipython/core/magics/namespace.py
yap_ipython/core/magics/script.py
yap_ipython/core/magics/auto.py
yap_ipython/core/magics/history.py
yap_ipython/core/inputtransformer.py
yap_ipython/core/splitinput.py
yap_ipython/core/__init__.py
yap_ipython/core/page.py
yap_ipython/core/shellapp.py
yap_ipython/core/logger.py
yap_ipython/core/excolors.py
yap_ipython/core/completer.py
yap_ipython/core/ultratb.py
yap_ipython/core/display.py
yap_ipython/core/prompts.py
yap_ipython/core/debugger.py
yap_ipython/core/payload.py
yap_ipython/core/application.py
yap_ipython/core/extensions.py
yap_ipython/core/builtin_trap.py
yap_ipython/core/displaypub.py
yap_ipython/core/pylabtools.py
yap_ipython/core/interactiveshell.py
yap_ipython/core/autocall.py
yap_ipython/core/getipython.py
yap_ipython/core/inputsplitter.py
yap_ipython/core/oinspect.py
yap_ipython/core/latex_symbols.py
yap_ipython/core/profileapp.py
yap_ipython/core/payloadpage.py
yap_ipython/core/displayhook.py
yap_ipython/core/magic_arguments.py
yap_ipython/core/usage.py
yap_ipython/core/macro.py
yap_ipython/core/crashhandler.py
yap_ipython/core/compilerop.py
yap_ipython/core/history.py
yap_ipython/sphinxext/__init__.py
yap_ipython/sphinxext/custom_doctests.py
yap_ipython/sphinxext/ipython_console_highlighting.py
yap_ipython/sphinxext/ipython_directive.py
yap_ipython/nbformat.py
yap_ipython/paths.py
yap_ipython/nbconvert.py
yap_ipython/qt.py
yap_ipython/html.py
yap_ipython/frontend.py
yap_ipython/__init__.py
yap_ipython/terminal/pt_inputhooks/glut.py
yap_ipython/terminal/pt_inputhooks/gtk.py
yap_ipython/terminal/pt_inputhooks/gtk3.py
yap_ipython/terminal/pt_inputhooks/qt.py
yap_ipython/terminal/pt_inputhooks/__init__.py
yap_ipython/terminal/pt_inputhooks/tk.py
yap_ipython/terminal/pt_inputhooks/pyglet.py
yap_ipython/terminal/pt_inputhooks/osx.py
yap_ipython/terminal/pt_inputhooks/wx.py
yap_ipython/terminal/ptutils.py
yap_ipython/terminal/console.py
yap_ipython/terminal/embed.py
yap_ipython/terminal/shortcuts.py
yap_ipython/terminal/tests/__init__.py
yap_ipython/terminal/tests/test_embed.py
yap_ipython/terminal/tests/test_interactivshell.py
yap_ipython/terminal/tests/test_help.py
yap_ipython/terminal/__init__.py
yap_ipython/terminal/ipapp.py
yap_ipython/terminal/prompts.py
yap_ipython/terminal/debugger.py
yap_ipython/terminal/interactiveshell.py
yap_ipython/terminal/magics.py
yap_ipython/terminal/ptshell.py
yap_ipython/utils/shimmodule.py
yap_ipython/utils/colorable.py
yap_ipython/utils/tempdir.py
yap_ipython/utils/_process_win32_controller.py
yap_ipython/utils/module_paths.py
yap_ipython/utils/py3compat.py
yap_ipython/utils/tokenutil.py
yap_ipython/utils/version.py
yap_ipython/utils/encoding.py
yap_ipython/utils/openpy.py
yap_ipython/utils/_process_cli.py
yap_ipython/utils/tz.py
yap_ipython/utils/terminal.py
yap_ipython/utils/log.py
yap_ipython/utils/dir2.py
yap_ipython/utils/jsonutil.py
yap_ipython/utils/coloransi.py
yap_ipython/utils/daemonize.py
yap_ipython/utils/io.py
yap_ipython/utils/_process_posix.py
yap_ipython/utils/tests/test_pycolorize.py
yap_ipython/utils/tests/test_decorators.py
yap_ipython/utils/tests/test_tempdir.py
yap_ipython/utils/tests/test_importstring.py
yap_ipython/utils/tests/test_imports.py
yap_ipython/utils/tests/__init__.py
yap_ipython/utils/tests/test_dir2.py
yap_ipython/utils/tests/test_io.py
yap_ipython/utils/tests/test_process.py
yap_ipython/utils/tests/test_sysinfo.py
yap_ipython/utils/tests/test_text.py
yap_ipython/utils/tests/test_tokenutil.py
yap_ipython/utils/tests/test_openpy.py
yap_ipython/utils/tests/test_capture.py
yap_ipython/utils/tests/test_module_paths.py
yap_ipython/utils/tests/test_shimmodule.py
yap_ipython/utils/tests/test_path.py
yap_ipython/utils/tests/test_wildcard.py
yap_ipython/utils/__init__.py
yap_ipython/utils/traitlets.py
yap_ipython/utils/ipstruct.py
yap_ipython/utils/strdispatch.py
yap_ipython/utils/wildcard.py
yap_ipython/utils/capture.py
yap_ipython/utils/localinterfaces.py
yap_ipython/utils/timing.py
yap_ipython/utils/signatures.py
yap_ipython/utils/frame.py
yap_ipython/utils/text.py
yap_ipython/utils/_sysinfo.py
yap_ipython/utils/eventful.py
yap_ipython/utils/sysinfo.py
yap_ipython/utils/process.py
yap_ipython/utils/PyColorize.py
yap_ipython/utils/_process_common.py
yap_ipython/utils/contexts.py
yap_ipython/utils/pickleutil.py
yap_ipython/utils/syspathcontext.py
yap_ipython/utils/path.py
yap_ipython/utils/importstring.py
yap_ipython/utils/_process_win32.py
yap_ipython/utils/generics.py
yap_ipython/utils/sentinel.py
yap_ipython/utils/tokenize2.py
yap_ipython/utils/ulinecache.py
yap_ipython/utils/data.py
yap_ipython/utils/decorators.py
yap_ipython/display.py
yap_ipython/yapi.py
yap_ipython/extensions/rmagic.py
yap_ipython/extensions/cythonmagic.py
yap_ipython/extensions/tests/test_autoreload.py
yap_ipython/extensions/tests/__init__.py
yap_ipython/extensions/tests/test_storemagic.py
yap_ipython/extensions/__init__.py
yap_ipython/extensions/storemagic.py
yap_ipython/extensions/sympyprinting.py
yap_ipython/extensions/autoreload.py
yap_ipython/testing/skipdoctest.py
yap_ipython/testing/iptestcontroller.py
yap_ipython/testing/tools.py
yap_ipython/testing/tests/test_ipunittest.py
yap_ipython/testing/tests/test_decorators.py
yap_ipython/testing/tests/__init__.py
yap_ipython/testing/tests/test_tools.py
yap_ipython/testing/plugin/test_ipdoctest.py
yap_ipython/testing/plugin/dtexample.py
yap_ipython/testing/plugin/show_refs.py
yap_ipython/testing/plugin/__init__.py
yap_ipython/testing/plugin/iptest.py
yap_ipython/testing/plugin/test_refs.py
yap_ipython/testing/plugin/setup.py
yap_ipython/testing/plugin/ipdoctest.py
yap_ipython/testing/plugin/simplevars.py
yap_ipython/testing/plugin/simple.py
yap_ipython/testing/__init__.py
yap_ipython/testing/globalipapp.py
yap_ipython/testing/iptest.py
yap_ipython/testing/ipunittest.py
yap_ipython/testing/__main__.py
yap_ipython/testing/decorators.py
yap_ipython/lib/inputhookpyglet.py
yap_ipython/lib/inputhookgtk.py
yap_ipython/lib/inputhookglut.py
yap_ipython/lib/guisupport.py
yap_ipython/lib/kernel.py
yap_ipython/lib/latextools.py
yap_ipython/lib/inputhookwx.py
yap_ipython/lib/inputhookgtk3.py
yap_ipython/lib/security.py
yap_ipython/lib/tests/test_pretty.py
yap_ipython/lib/tests/test_security.py
yap_ipython/lib/tests/test_backgroundjobs.py
yap_ipython/lib/tests/test_deepreload.py
yap_ipython/lib/tests/test_imports.py
yap_ipython/lib/tests/test_display.py
yap_ipython/lib/tests/test_clipboard.py
yap_ipython/lib/tests/__init__.py
yap_ipython/lib/tests/test_lexers.py
yap_ipython/lib/tests/test_latextools.py
yap_ipython/lib/tests/test_editorhooks.py
yap_ipython/lib/__init__.py
yap_ipython/lib/display.py
yap_ipython/lib/inputhookqt4.py
yap_ipython/lib/pretty.py
yap_ipython/lib/deepreload.py
yap_ipython/lib/inputhook.py
yap_ipython/lib/clipboard.py
yap_ipython/lib/demo.py
yap_ipython/lib/editorhooks.py
yap_ipython/lib/backgroundjobs.py
yap_ipython/lib/lexers.py
yap_ipython/consoleapp.py
yap_ipython/external/mathjax.py
yap_ipython/external/decorators/__init__.py
yap_ipython/external/decorators/_decorators.py
yap_ipython/external/decorators/_numpy_testing_noseclasses.py
yap_ipython/external/__init__.py
yap_ipython/external/qt_loaders.py
yap_ipython/external/qt_for_kernel.py
yap_ipython/parallel.py
yap_ipython/__main__.py
yap_ipython/kernel/clientabc.py
yap_ipython/kernel/threaded.py
yap_ipython/kernel/multikernelmanager.py
yap_ipython/kernel/connect.py
yap_ipython/kernel/adapter.py
yap_ipython/kernel/client.py
yap_ipython/kernel/kernelspec.py
yap_ipython/kernel/__init__.py
yap_ipython/kernel/managerabc.py
yap_ipython/kernel/kernelspecapp.py
yap_ipython/kernel/channelsabc.py
yap_ipython/kernel/launcher.py
yap_ipython/kernel/channels.py
yap_ipython/kernel/restarter.py
yap_ipython/kernel/__main__.py
yap_ipython/kernel/manager.py
__main__.py )

set (EXTRAS MANIFEST.in YAP_KERNEL.md setup.py setup.cfg README.md )

set (RESOURCES
   #yap_kernel/resources/logo-32x32.png
   #yap_kernel/resourcess/logo-64x64.png
   )

set (RENAMED_RESOURCES
  yap_kernel/resources/logo-32x32.png
  yap_kernel/resources/logo-64x64.png
  # yap_kernel/resources/codemirror/mode/prolog/prolog.js
)

set (PL_SOURCES yap_ipython/prolog/jupyter.yap yap_ipython/prolog/complete.yap
  yap_ipython/prolog/verify.yap
  )

set(FILES ${PYTHON_SOURCES} ${PL_SOURCES} ${EXTRAS} ${RESOURCES})

set(SETUP_PY ${CMAKE_CURRENT_BINARY_DIR}/setup.py)


add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-32x32.png
  COMMAND ${CMAKE_COMMAND} -E make_directory yap_kernel/resources
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/icons/yap_32x32x32.png  ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-32x32.png
  DEPENDS ${CMAKE_SOURCE_DIR}/docs/icons/yap_32x32x32.png
  )

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-64x64.png
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/docs/icons/yap_64x64x32.png  ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-64x64.png
  DEPENDS ${CMAKE_SOURCE_DIR}/docs/icons/yap_64x64x32.png
  )

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/kernel.js
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/kernel.js  ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/kernel.js
  )

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/prolog.js
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/misc/editors/codemirror/prolog.js  ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/prolog.js
  DEPENDS ${CMAKE_SOURCE_DIR}/misc/editors/codemirror/prolog.js
  )

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/meta.js
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/misc/editors/codemirror/meta.js  ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/meta.js
  DEPENDS ${CMAKE_SOURCE_DIR}/misc/editors/codemirror/meta.js
  )


foreach(f ${FILES})
  add_custom_command(OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/${f}
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${f} ${CMAKE_CURRENT_BINARY_DIR}/${f}
    DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/${f}
    )
  list(APPEND OUTS ${CMAKE_CURRENT_BINARY_DIR}/${f} )
endforeach()



add_custom_target(YAP_KERNEL
  ALL
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
 DEPENDS  ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-32x32.png ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/logo-64x64.png ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/kernel.js ${CMAKE_CURRENT_BINARY_DIR}/yap_kernel/resources/prolog.js  ${OUTS} YAP4PY
   COMMAND ${PYTHON_EXECUTABLE}  ${SETUP_PY} sdist ${bdist}
)

   install(CODE "execute_process(
         COMMAND ${PYTHON_EXECUTABLE} -m pip install ${PYTHON_USER_INSTALL}    --ignore-installed   .
        COMMAND ${PYTHON_EXECUTABLE} -m yap_kernel.kernelspec
        ERROR_VARIABLE setupErr
        OUTPUT_VARIABLE setupOut
         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

    install(FILES ${PL_SOURCES} DESTINATION ${YAP_INSTALL_DATADIR} )
