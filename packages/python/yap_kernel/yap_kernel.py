from __future__ import print_function

from metakernel import MetaKernel

import signal
import yap

class MetaKernelyap(MetaKernel):
    implementation = 'MetaKernel YAP'
    implementation_version = '1.0'
    language = 'text'
    language_version = '0.1'
    banner = "MetaKernel YAP"
    language_info = {
        'mimetype': 'text/plain',
        'name': 'text',
        # ------ If different from 'language':
         'codemirror_mode': {
            "version": 2,
            "name": "prolog"
         },
         'pygments_lexer': 'prolog',
         'version'       : "0.0.1",
        'file_extension': '.yap',
        'help_links': MetaKernel.help_links,
    }

    def __init__(self, **kwargs):

        MetaKernel.__init__(self, **kwargs)
        self._start_yap()

    def _start_yap(self):
        # Signal handlers are inherited by forked processes, and we can't easily
        # reset it from the subprocess. Since kernelapp ignores SIGINT except in
        # message handlers, we need to temporarily reset the SIGINT handler here
        # so that yap and its children are interruptible.
        #sig = signal.signal(signal.SIGINT, signal.SIG_DFL)
        #try:
            self.engine = yap.YAPEngine()
            self.q = None
            #engine.query("load_files(library(python), [])").command()
            banner = "YAP {0} Kernel".format(self.engine.version())

        #finally:
        #     signal.signal(signal.SIGINT, sig)

        # Register Yap function to write image data to temporary file
        #self.yapwrapper.run_command(image_setup_cmd)

    def get_usage(self):
        return "This is the YAP kernel."

    def do_execute_direct(self, code):
        if not code.strip():
            return ""

        interrupted = False
        try:
            if self.q is None:
                self.q = self.engine.query(code.rstrip())
            if self.q.next():
                vs = self.q.namedVars()
                if vs:
                    l = {}
                    for eq in vs:
                        l[eq.getArg(1)] = eq.getArg(2)
                    return l
                else:
                    return 'yes'
            else:
                return 'no'
        except KeyboardInterrupt:
            return 'stopped by user'



    def repr(self, data):
        return repr(data)

if __name__ == '__main__':
    try:
        from ipykernel.kernelapp import IPKernelApp
    except ImportError:
        from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=MetaKernelyap)
