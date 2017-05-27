
"""An in-process kernel"""

# Copyright (c) IPython Development Team.
# Distributed under the terms of the Modified BSD License.

from contextlib import contextmanager
import logging
import sys

from IPython.core.interactiveshell import InteractiveShellABC
from yap_kernel.jsonutil import json_clean
from traitlets import Any, Enum, Instance, List, Type, default
from yap_kernel.yapkernel import YAPKernel
from yap_kernel.zmqshell import ZMQInteractiveShell

from .constants import INPROCESS_KEY
from .socket import DummySocket
from ..iostream import OutStream, BackgroundSocket, IOPubThread

#-----------------------------------------------------------------------------
# Main kernel class
#-----------------------------------------------------------------------------

class InProcessKernel(YAPKernel):

    #-------------------------------------------------------------------------
    # InProcessKernel interface
    #-------------------------------------------------------------------------

    # The frontends connected to this kernel.
    frontends = List(
        Instance('yap_kernel.inprocess.client.InProcessKernelClient',
                 allow_none=True)
    )

    # The GUI environment that the kernel is running under. This need not be
    # specified for the normal operation for the kernel, but is required for
    # IPython's GUI support (including pylab). The default is 'inline' because
    # it is safe under all GUI toolkits.
    gui = Enum(('tk', 'gtk', 'wx', 'qt', 'qt4', 'inline'),
               default_value='inline')

    raw_input_str = Any()
    stdout = Any()
    stderr = Any()

    #-------------------------------------------------------------------------
    # Kernel interface
    #-------------------------------------------------------------------------

    shell_class = Type(allow_none=True)
    shell_streams = List()
    control_stream = Any()
    _underlying_iopub_socket = Instance(DummySocket, ())
    iopub_thread = Instance(IOPubThread)

    @default('iopub_thread')
    def _default_iopub_thread(self):
        thread = IOPubThread(self._underlying_iopub_socket)
        thread.start()
        return thread

    iopub_socket = Instance(BackgroundSocket)

    @default('iopub_socket')
    def _default_iopub_socket(self):
        return self.iopub_thread.background_socket

    stdin_socket = Instance(DummySocket, ())

    def __init__(self, **traits):
        super(InProcessKernel, self).__init__(**traits)

        self._underlying_iopub_socket.observe(self._io_dispatch, names=['message_sent'])
        pjoin = os.path.join
        here = os.path.abspath(os.path.dirname(__file__))
        yap_lib_path = pjoin(here, "../yap4py/prolog" )
        yap_dll_path = pjoin(here, "../yap4py" )
        args = yap.YAPEngineArgs()
        args.setYapLibDir(yap_dll_path)
        args.setYapShareDir(yap_lib_path)
        #args.setYapPrologBootFile(os.path.join(yap_lib_path."startup.yss"))
        self.yapeng = yap.YAPEngine( args )
        self.q = None
        self.yapeng.goal( use_module( library('yapi') ) )
        self.shell.run_cell = self.run_cell
        self.shell.kernel = self

    def execute_request(self, stream, ident, parent):
        """ Override for temporary IO redirection. """
        with self._redirected_io():
            super(InProcessKernel, self).execute_request(stream, ident, parent)

    def start(self):
        """ Override registration of dispatchers for streams. """
        self.shell.exit_now = False

    def _abort_queue(self, stream):
        """ The in-process kernel doesn't abort requests. """
        pass

    def _input_request(self, prompt, ident, parent, password=False):
        # Flush output before making the request.
        self.raw_input_str = None
        sys.stderr.flush()
        sys.stdout.flush()

        # Send the input request.
        content = json_clean(dict(prompt=prompt, password=password))
        msg = self.session.msg(u'input_request', content, parent)
        for frontend in self.frontends:
            if frontend.session.session == parent['header']['session']:
                frontend.stdin_channel.call_handlers(msg)
                break
        else:
            logging.error('No frontend found for raw_input request')
            return str()

        # Await a response.
        while self.raw_input_str is None:
            frontend.stdin_channel.process_events()
        return self.raw_input_str

    #-------------------------------------------------------------------------
    # Protected interface
    #-------------------------------------------------------------------------

    @contextmanager
    def _redirected_io(self):
        """ Temporarily redirect IO to the kernel.
        """
        sys_stdout, sys_stderr = sys.stdout, sys.stderr
        sys.stdout, sys.stderr = self.stdout, self.stderr
        yield
        sys.stdout, sys.stderr = sys_stdout, sys_stderr

    #------ Trait change handlers --------------------------------------------

    def _io_dispatch(self, change):
        """ Called when a message is sent to the IO socket.
        """
        ident, msg = self.session.recv(self.iopub_socket, copy=False)
        for frontend in self.frontends:
            frontend.iopub_channel.call_handlers(msg)

    #------ Trait initializers -----------------------------------------------

    @default('log')
    def _default_log(self):
        return logging.getLogger(__name__)

    @default('session')
    def _default_session(self):
        from jupyter_client.session import Session
        return Session(parent=self, key=INPROCESS_KEY)

    @default('shell_class')
    def _default_shell_class(self):
        return InProcessInteractiveShell

    @default('stdout')
    def _default_stdout(self):
        return OutStream(self.session, self.iopub_thread, u'stdout')

    @default('stderr')
    def _default_stderr(self):
        return OutStream(self.session, self.iopub_thread, u'stderr')

#-----------------------------------------------------------------------------
# Interactive shell subclass
#-----------------------------------------------------------------------------

class InProcessInteractiveShell(ZMQInteractiveShell):

    kernel = Instance('yap_kernel.inprocess.yapkernel.InProcessKernel',
                      allow_none=True)

    #-------------------------------------------------------------------------
    # InteractiveShell interface
    #-------------------------------------------------------------------------

    def enable_gui(self, gui=None):
        """Enable GUI integration for the kernel."""
        from yap_kernel.eventloops import enable_gui
        if not gui:
            gui = self.kernel.gui
        enable_gui(gui, kernel=self.kernel)
        self.active_eventloop = gui


    def enable_matplotlib(self, gui=None):
        """Enable matplotlib integration for the kernel."""
        if not gui:
            gui = self.kernel.gui
        return super(InProcessInteractiveShell, self).enable_matplotlib(gui)

    def enable_pylab(self, gui=None, import_all=True, welcome_message=False):
        """Activate pylab support at runtime."""
        if not gui:
            gui = self.kernel.gui
        return super(InProcessInteractiveShell, self).enable_pylab(gui, import_all,
                                                            welcome_message)


    def closeq(self):
        if self.q:
            self.q.close()
            self.q = None

    def run_cell(self, s, store_history=True, silent=False, shell_futures=True):

        """Run a complete IPython cell.

        Parameters
                   ----------
                   raw_cell : str
                   The code (including IPython code such as %magic functions) to run.
                   store_history : bool
          If True, the raw and translated cell will be stored in IPython's
                   history. For user code calling back into IPython's machinery, this
                   should be set to False.
                   silent : bool
          If True, avoid side-effects, such as implicit displayhooks and
                   and logging.  silent=True forces store_history=False.
                   shell_futures : bool
          If True, the code will share future statements with the interactive
                   shell. It will both be affected by previous __future__ imports, and
                   any __future__ imports in the code will affect the shell. If False,
                   __future__ imports are not shared in either direction.

        Returns
                   -------
                   result : :class:`ExecutionResult`
                   """

        def numbervars(self, l):
            return self.yapeng.fun(bindvars(l))

        result = ExecutionResult()

        if (not s) or s.isspace():
            self.shell.last_execution_succeeded = True
            return result

        if store_history:
            result.execution_count = self.shell.execution_count

        def error_before_exec(value):
            result.error_before_exec = value
            self.shell.last_execution_succeeded = False
            return result


        if not self.q:
            try:
                self.q = self.yapeng.query(s)
            except SyntaxError:
                return error_before_exec( sys.exc_info()[1])

        cell = s  # cell has to exist so it can be stored/logged

        # Store raw and processed history
        # if not silent:
        #    self.shell..logger.log(cell, s)

        has_raised = False
        try:
            #f = io.StringIO()
            # with redirect_stdout(f):
            run = self.q.next()
            # print('{0}'.format(f.getvalue()))
            # Execute the user code
            if run:
                myvs = self.numbervars(self.q.namedVars())
                if myvs:
                    for eq in myvs:
                        name = eq[0]
                        binding = eq[1]
                        if name != binding:
                            print(name + " = " + str(binding))
                else:
                    print("yes")
                if self.q.deterministic():
                    self.closeq()
            else:
                print("No (more) answers")
                self.closeq()
        except:
            result.error_in_exec = sys.exc_info()[1]
            # self.showtraceback()
            has_raised = True
            self.closeq()


        self.shell.last_execution_succeeded = not has_raised
        result.result = self.shell.last_execution_succeeded
        print( self.q )
        # Reset this so later displayed values do not modify the
        # ExecutionResult
        # self.displayhook.exec_result = None

        #self.events.trigger('post_execute')
        #if not silent:
        #    self.events.trigger('post_run_cell')

        if store_history:
            # Write output to the database. Does nothing unless
            # history output logging is enabled.
            # self.history_manager.store_output(self.execution_count)
            # Each cell is a *single* input, regardless of how many lines it has
            self.shell.execution_count += 1

        return result

InteractiveShellABC.register(InProcessInteractiveShell)
