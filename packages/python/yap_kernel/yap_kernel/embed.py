"""Simple function for embedding an yap_ipython kernel
"""
#-----------------------------------------------------------------------------
# Imports
#-----------------------------------------------------------------------------

import sys

from yap_ipython.utils.frame import extract_module_locals

from .kernelapp import YAPKernelApp

#-----------------------------------------------------------------------------
# Code
#-----------------------------------------------------------------------------

def embed_kernel(module=None, local_ns=None, **kwargs):
    """Embed and start an yap_ipython kernel in a given scope.

    Parameters
    ----------
    module : ModuleType, optional
        The module to load into yap_ipython globals (default: caller)
    local_ns : dict, optional
        The namespace to load into yap_ipython user namespace (default: caller)

    kwargs : various, optional
        Further keyword args are relayed to the YAPKernelApp constructor,
        allowing configuration of the Kernel.  Will only have an effect
        on the first embed_kernel call for a given process.

    """
    # get the app if it exists, or set it up if it doesn't
    if YAPKernelApp.initialized():
        app = YAPKernelApp.instance()
    else:
        app = YAPKernelApp.instance(**kwargs)
        app.initialize([])
        # Undo unnecessary sys module mangling from init_sys_modules.
        # This would not be necessary if we could prevent it
        # in the first place by using a different InteractiveShell
        # subclass, as in the regular embed case.
        main = app.kernel.shell._orig_sys_modules_main_mod
        if main is not None:
            sys.modules[app.kernel.shell._orig_sys_modules_main_name] = main

    # load the calling scope if not given
    (caller_module, caller_locals) = extract_module_locals(1)
    if module is None:
        module = caller_module
    if local_ns is None:
        local_ns = caller_locals

    app.kernel.user_module = module
    app.kernel.user_ns = local_ns
    app.shell.set_completer_frame()
    app.start()
