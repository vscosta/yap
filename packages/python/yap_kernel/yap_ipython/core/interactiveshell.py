import os
import sys
import abc

import yap4py.yapi
from IPython.core import interactiveshell
from IPython.core.completer import IPCompleter
from IPython.core.interactiveshell import ExecutionResult, InteractiveShell
from IPython.utils.strdispatch import StrDispatch
# import yap_ipython.core
from traitlets import Instance

from pygments import highlight
from pygments.lexers.prolog import PrologLexer
from pygments.formatters import HtmlFormatter


from collections import namedtuple

use_module = namedtuple('use_module', 'file')
bindvars = namedtuple('bindvars', 'list')
library = namedtuple('library', 'list')
v = namedtuple('_', 'slot')
load_files = namedtuple('load_files', 'file ofile args')
python_query= namedtuple('python_query', 'query_mgr string')
jupyter_query = namedtuple('jupyter_query', 'self string')
enter_cell = namedtuple('enter_cell', 'self' )
exit_cell = namedtuple('exit_cell', 'self' )
completions = namedtuple('completions', 'txt self' )

class YAPCompleter:

    def __init__(self, engine):
        self.yapeng = engine
        self.completions = None

    def complete(self, text, line=None, cursor_pos=None):
        """Return the completed text and a list of completions.

        Parameters
        ----------

           text : string
             A string of text to be completed on.  It can be given as empty and
             instead a line/position pair are given.  In this case, the
             completer itself will split the line like readline does.

           line : string, optional
             The complete line that text is part of.

           cursor_pos : int, optional
             The position of the cursor on the input line.

        Returns
        -------
          text : string
            The actual text that was completed.

          matches : list
            A sorted list with all possible completions.

        The optional arguments allow the completion to take more context into
        account, and are part of the low-level completion API.

        This is a wrapper around the completion mechanism, similar to what
        readline does at the command line when the TAB key is hit.  By
        exposing it as a method, it can be used by other non-readline
        environments (such as GUIs) for text completion.

        Simple usage example:

        In [1]: x = 'hello'

        In [2]: _ip.complete('x.l')
        Out[2]: ('x.l', ['x.ljust', 'x.lower', 'x.lstrip'])
        """

        if not  text:
            text = line[:cursor_pos]
        self.yapeng.goal(completions(text, self))
        return text, self.completions

    #    def _init__(self, **kwargs) -> None:
        # PyCompleter.__init__(**kwargs__)



class YAPInteractive(InteractiveShell):
    """An enhanced, interactive shell for YAP."""
    
    def init_yap_completer(self):
        """Initialize the completion machinery.

        This creates completion machinery that can be used by client code,
        either interactively in-process (typically triggered by the readline
        library), programmatically (such as in test suites) or out-of-process
        (typically over the network by remote frontends).
        """
        print(self)

        # Add custom completers to the basic ones built into IPCompleter
        self.Completer = YAPCompleter(self.yapeng)
        self.configurables.append(self.Completer)

    def __init__(self, **kwargs):
        super(YAPInteractive, self).__init__(**kwargs)
        # type: (object, object) -> object
        pjoin = os.path.join
        here = os.path.dirname(__file__)
        self.yapeng = yap4py.yapi.Engine()
        self.yapeng.goal(use_module(pjoin(here, '../prolog/jupyter.yap')))
        self.q = None
        self.run = False
        self.os = ""
        self.port = None
        self.init_yap_completer()

    def init_syntax_highlighting(self, changes=None):
        # Python source parser/formatter for syntax highlighting
        # pyformat = PyColorize.Parser(style=self.colors, parent=self).format
        # self.pycolorize = lambda src: pyformat(src,'str')
        self.pycolorize = lambda code : highlight(code, PrologLexer(), HtmlFormatter())




    def complete(self, text, line=None, cursor_pos=None):
        """Return the completed text and a list of completions.

        Parameters
        ----------

           text : string
             A string of text to be completed on.  It can be given as empty and
             instead a line/position pair are given.  In this case, the
             completer itself will split the line like readline does.

           line : string, optional
             The complete line that text is part of.

           cursor_pos : int, optional
             The position of the cursor on the input line.

        Returns
        -------
          text : string
            The actual text that was completed.

          matches : list
            A sorted list with all possible completions.

        The optional arguments allow the completion to take more context into
        account, and are part of the low-level completion API.

        This is a wrapper around the completion mechanism, similar to what
        readline does at the command line when the TAB key is hit.  By
        exposing it as a method, it can be used by other non-readline
        environments (such as GUIs) for text completion.

        Simple usage example:

        In [1]: x = 'hello'

        In [2]: _ip.complete('x.l')
        Out[2]: ('x.l', ['x.ljust', 'x.lower', 'x.lstrip'])
        """

        # Inject names into __builtin__ so we can complete on the added names.
        return self.Completer.complete(text, line, cursor_pos)



        
    def run_cell(self, raw_cell, store_history=True, silent=False,
                 shell_futures=True):
        """Run a complete IPython cell.

        Parameters
                   ----------
                   raw_cell : str
                   The code (including IPython code such as
                   %magic functions) to run.
                   store_history : bool
          If True, the raw and translated cell will be stored in IPython's
                   history. For user code calling back into
                   IPython's machinery, this
                   should be set to False.
                   silent : bool
          If True, avoid side-effects, such as implicit displayhooks and
                   and logging.  silent=True forces store_history=False.
                   shell_futures : bool
          If True, the code will share future statements with the interactive
                   shell. It will both be affected by previous
                    __future__ imports, and any __future__ imports in the code
                     will affect the shell. If False,
                   __future__ imports are not shared in either direction.

        Returns

                   -------

`result : :class:`ExecutionResult`
                   """

        # construct a query from a one-line string
        # q is opaque to Python
        # vs is the list of variables
        # you can print it out, the left-side is the variable name,
        # the right side wraps a handle to a variable
        # pdb.set_trace()
        #     #pdb.set_trace()
        # atom match either symbols, or if no symbol exists, strings, In this case
        # variable names should match strings
        # ask = True
        # launch the query
        result = ExecutionResult()

        if (not raw_cell) or raw_cell.isspace():
            self.last_execution_succeeded = True
            return result

        if silent:
            store_history = False

        if store_history:
            result.execution_count = self.execution_count

        def error_before_exec(value):
            result.error_before_exec = value
            self.last_execution_succeeded = False
            return result

        self.events.trigger('pre_execute')
        if not silent:
            self.events.trigger('pre_run_cell')

        # If any of our input transformation (input_transformer_manager or
        # prefilter_manager) raises an exception, we store it in this variable
        # so that we can display the error after logging the input and storing
        # it in the history.
        # preprocessing_exc_tuple = None
        # try:
        #     # Static input transformations
        #     cell = raw_cell #self.input_transformer_manager.transform_cell(raw_cell)
        # except SyntaxError:
        #     preprocessing_exc_tuple = sys.exc_info()
        cell = raw_cell  # cell has to exist so it can be stored/logged
        # else:
        #     # import pdb; pdb.set_trace()
        #     if False and len(cell.splitlines()) == 1:
        #         # Dynamic transformations - only applied for single line commands
        #         with self.builtin_trap:
        #             try:
        #                 # use prefilter_lines to handle trailing newlines
        #                 # restore trailing newline for ast.parse
        #                 cell = self.prefilter_manager.prefilter_lines(cell) + '\n'
        #             except Exception:
        #                 # don't allow prefilter errors to crash IPython
        #                 preprocessing_exc_tuple = sys.exc_info()


        # Store raw and processed history
        if store_history:
            self.history_manager.store_inputs(self.execution_count,
                                              cell, raw_cell)
        if not silent:
            self.logger.log(cell, raw_cell)

        # # Display the exception if input processing failed.
        # if preprocessing_exc_tuple is not None:
        #     self.showtraceback(preprocessing_exc_tuple)
        #     if store_history:
        #         self.execution_count += 1
        #     return error_before_exec(preprocessing_exc_tuple[2])

        # Our own compiler remembers the __future__ environment. If we want to
        # run code with a separate __future__ environment, use the default
        # compiler
        # compiler = self.compile if shell_futures else CachingCompiler()

        cell_name = str( self.execution_count)

        if cell[0] == '%':
            if cell[1] == '%':
                linec = False
                mcell = cell.lstrip('%%')
            else:
                linec = True
                mcell = cell.lstrip('%')
            txt0 = mcell.split(maxsplit = 2, sep = '\n')
            txt = txt0[0].split(maxsplit = 2)
            magic = txt[0]
            if len(txt) == 2:
                line = txt[1]
            else:
                line = ""
            if linec:
                self.run_line_magic(magic, line)
                if len(txt0) == 2:
                    cell = txt0[1]
                else:
                    cellArea = ""
            else:
                self.run_cell_magic(magic, line, cell)
                return
        # Give the displayhook a reference to our ExecutionResult so it
        # can fill in the output value.
        self.displayhook.exec_result = result
        has_raised = False
        try:
            self.bindings = dict = {}
            state = self.jupyter_query(cell)
            if state:
                self.last_execution_succeeded = True
                result.result = (True, dict)
            else:
                self.last_execution_succeeded = True
                result.result = (True, {})
        except Exception as e:
            print(e)
            has_raised = True
            result.result = False

        self.last_execution_succeeded = not has_raised

        # Reset this so later displayed values do not modify the
        # ExecutionResult
        self.displayhook.exec_result = None
        print(self.complete)
        self.events.trigger('post_execute')
        if not silent:
            self.events.trigger('post_run_cell')

        if store_history:
            # Write output to the database. Does nothing unless
            # history output logging is enabled.
            self.history_manager.store_output(self.execution_count)
            # Each cell is a *single* input, regardless of how many lines it has
            self.execution_count += 1

        return result

    def jupyter_query(self, s):
        # import pdb; pdb.set_trace()
        #
        # construct a self.query from a one-line string
        # self.q is opaque to Python
        self.bindings = {}
        self.port = "call"
        if self.q and s != self.os:
            self.q.close()
            self.q = None
        if not self.q:
            self.q = self.yapeng.query(jupyter_query(self, s))
            self.os = s
        # vs is the list of variables
        # you can print it out, the left-side is the variable name,
        # the right side wraps a handle to a variable
        # pdb.set_trace()
        #     #pdb.set_trace()
        # atom match either symbols, or if no symbol exists, sttrings, In this case
        # variable names should match strings
        #for eq in vs:
        #    if not isinstance(eq[0],str):
        #        print( "Error: Variable Name matches a Python Symbol")
        #        return
        # ask = True
        # launch the query
        # run the new command using the given tracer
        rc = self.answer(self.q)
        if rc:
            # deterministic = one solution
            if self.port == "exit":
                # done
                self.q.close()
                self.q = None
                self.os = ""
            print("yes")
            return True, self.bindings
        print("No (more) answers")
        self.q.close()
        self.q = None
        return True, None

    def answer(self, q):
        try:
            return q.next()
        except Exception as e:
            print(e.args[1])
            self.yapeng.goal(exit_cell(self))
            return False, None


class YAPInteractiveABC(metaclass=abc.ABCMeta):
    """An abstract base class for YAPInteractive."""

YAPInteractiveABC.register(YAPInteractive)
