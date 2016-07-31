from __future__ import print_function

from metakernel import MetaKernel

import sys
import signal
import yap
import yapex

import ipywidgets as widgets


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


class MetaKernelyap(MetaKernel):
    implementation = 'MetaKernel YAP'
    implementation_version = '1.0'
    language = 'text'
    language_version = '0.1'
    banner = "MetaKernel YAP"
    language_info = {
        'mimetype': 'text/prolog',
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
        self._start_yap(**kwargs)
        self.olines = ""
        self.ask = True

    def _start_yap(self, **kwargs):
        # Signal handlers are inherited by forked processes, and we can't easily
        # reset it from the subprocess. Since kernelapp ignores SIGINT except in
        # message handlers, we need to temporarily reset the SIGINT handler here
        # so that yap and its children are interruptible.
        sig = signal.signal(signal.SIGINT, signal.SIG_DFL)
        try:
            self.engine = yap.YAPEngine()
            self.q = None
            self.engine.query("load_files(library(python), [])").command()
            banner = "YAP {0} Kernel".format(self.engine.version())
            self.olines = banner
        finally:
             signal.signal(signal.SIGINT, sig)

        # Register Yap function to write image data to temporary file
        #self.yapwrapper.run_command(image_setup_cmd)

    def get_usage(self):
        return "This is the YAP kernel."

    def query_prolog(self, s):

        if not self.q:
            self.q = self.engine.query(s)
        if self.q.next():
            vs = self.q.namedVarsCopy()
            wrote = False
            if vs:
                i = 0
                for eq in vs:
                    name = eq[0]
                    bind = eq[1]
                    if bind.isVar():
                        var = yap.YAPAtom('$VAR')
                        f = yap.YAPFunctor(var, 1)
                        bind.unify(yap.YAPApplTerm(f, (name)))
                    else:
                        i = bind.numberVars(i, True)
                        print(name.text() + " = " + bind.text())
                        wrote = True
            print("yes")
            if self.q.deterministic():
                self.closeq()
            return
        print("No (more) answers")
        self.closeq()
        return

    def closeq( self):
        if self.q:
            self.q.close()
            self.q = None

    def do_execute_direct(self, code):
        if not code.strip():
            return ""
        lines = code.split("\n")
        interrupted = False
        self.doReset = True
        nlines = ""
        try:
            for line in lines:
                line = line.strip()
                if line.startswith('#'):
                    # wait
                    print( "comment")
                elif line.startswith('%'):
                    # wait
                    call_magic( line )
                elif line.endswith(';'):
                    nlines += line.rstrip(';').rstrip()
                    self.doReset = False
                    break
                elif line.endswith('!'):
                    nlines += line.rstrip('!').rstrip()
                    self.ask = False
                    self.doReset = False
                    break
                else: 
                    line = line.rstrip()
                    if line:
                        nlines += line + "\n"
            if nlines != self.olines:
                self.closeq( )
                self.olines = nlines
            elif self.doReset:
                opt = widgets.ToggleButtons(
                    description='Query Solutions:',
                    options=['First', 'Next', 'All'],
                )
                print( opt )
                if opt == 'First':
                    self.closeq( )
                elif  opt == 'Next':
                    self.doReset = False
                else:
                    self.ask = False
                    self.doReset = False  
            self.query_prolog( nlines )
            while not self.ask and self.q:
                self.query_prolog( nlines )

        except SyntaxError as err:
            print("Syntax Error error: {0}".format(err))
        except EOFError:
            return
        except RuntimeError as err:
            print("YAP Execution Error: {0}".format(err))
        except ValueError:
            print("Could not convert data to an integer.")
        except KeyboardInterrupt:
            return 'stopped by user'
        except:
            print("Unexpected error:", sys.exc_info()[0])
            raise

    def do_complete(self, code, cursor_pos):
        eprint( code, " -- ", str(cursor_pos) )
        # code = code[:cursor_pos]
        # default = {'matches': [], 'cursor_start': 0,
        #            'cursor_end': cursor_pos, 'metadata': dict(),
        #            'status': 'ok'}

        # if not code or code[-1] == ' ':
        #     return default

        # tokens = code.replace(';', ' ').split()
        # if not tokens:
        #     return default

        # matches = []
        # token = tokens[-1]
        # start = cursor_pos - len(token)

        # if token[0] == '$':
        #     # complete variables
        #     cmd = 'compgen -A arrayvar -A export -A variable %s' % token[1:] # strip leading $
        #     output = self.bashwrapper.run_command(cmd).rstrip()
        #     completions = set(output.split())
        #     # append matches including leading $
        #     matches.extend(['$'+c for c in completions])
        # else:
        #     # complete functions and builtins
        #     cmd = 'compgen -cdfa %s' % token
        #     output = self.bashwrapper.run_command(cmd).rstrip()
        #     matches.extend(output.split())
            
        # if not matches:
        #     return default
        # matches = [m for m in matches if m.startswith(token)]

        # return {'matches': sorted(matches), 'cursor_start': start,
        #         'cursor_end': cursor_pos, 'metadata': dict(),
        #         'status': 'ok'}




    def repr(self, data):
        return repr(data)

if __name__ == '__main__':
    try:
        from ipykernel.kernelapp import IPKernelApp
    except ImportError:
        from jupyter_client.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=MetaKernelyap)
