#-----------------------------------------------------------------------------
# Imports
#-----------------------------------------------------------------------------

import sys

from IPython.lib.kernel import connect_qtconsole
from IPython.kernel.zmq.kernelapp import YAP_KernelApp

#-----------------------------------------------------------------------------
# Functions and classes
#-----------------------------------------------------------------------------
def mpl_kernel(gui):
    """Launch and return an IPython kernel with matplotlib support for the desired gui
    """
    kernel = YAP_KernelApp.instance()
    kernel.initialize(['python', '--matplotlib=%s' % gui,
                       #'--log-level=10'
                       ])
    return kernel


class InternalYAPKernel(object):

    def init_yapkernel(self, backend):
        # Start IPython kernel with GUI event loop and mpl support
        self.yapkernel = mpl_kernel(backend)
        # To create and track active qt consoles
        self.consoles = []
        
        # This application will also act on the shell user namespace
        self.namespace = self.yapkernel.shell.user_ns

        # Example: a variable that will be seen by the user in the shell, and
        # that the GUI modifies (the 'Counter++' button increments it):
        self.namespace['app_counter'] = 0
        #self.namespace['yapkernel'] = self.yapkernel  # dbg

    def print_namespace(self, evt=None):
        print("\n***Variables in User namespace***")
        for k, v in self.namespace.items():
            if not k.startswith('_'):
                print('%s -> %r' % (k, v))
        sys.stdout.flush()

    def new_qt_console(self, evt=None):
        """start a new qtconsole connected to our kernel"""
        return connect_qtconsole(self.yapkernel.abs_connection_file, profile=self.yapkernel.profile)

    def count(self, evt=None):
        self.namespace['app_counter'] += 1

    def cleanup_consoles(self, evt=None):
        for c in self.consoles:
            c.kill()
