import os
import sys
import abc
import math

try:
    import yap4py.yapi
except:
    print("Could not load _yap dll.")
from yap_ipython.core import interactiveshell
from yap_ipython.core.completer import IPCompleter
from yap_ipython.utils.strdispatch import StrDispatch
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
JupyterQuery = namedtuple('jupyter_query', 'self text query')
enter_cell = namedtuple('enter_cell', 'self' )
exit_cell = namedtuple('exit_cell', 'self' )
completions = namedtuple('completions', 'txt self' )

class YAPCompleter:

    def __init__(self,
                 shell=None, namespace=None, global_namespace=None,
                 parent=None,
                 ):
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



class YAPLineProcessor:

    def __init__(self,
                 shell
                 ):
        self.engine = shell.engine

    def validQuery(self, text, line=None, cursor_pos=None):
        """Return whether a legal query
        """
        if not  line:
            (_,line,_) = self.prolog_cell(text)
        line = line.strip().rstrip()
        if not line:
            return False
        self.yapeng.goal(errors(text, line))
        return not self.errors

    #    def _init__(self, **kwargs) -> None:
        # PyCompleter.__init__(**kwargs__)



class YAPRun():
    """An enhanced, interactive shell for YAP."""

    def init(self):
        self.yapeng = yap4py.yapi.Engine()
        self.yapeng.goal(use_module(library("jupyter")))
        self.q = None
        self.run = False
        self.port = None

    def jupyter_query(self, s):
        # import pdb; pdb.set_trace()
        #
        # construct a self.query from a one-line string
        # self.q is opaque to Python
        self.bindings = {}
        if self.q and s != self.os:
            self.q.close()
            self.q = None
        if not self.q:
            #import pdb; pdb.set_trace()
            self.port = "call"
            program,query,iterations = YAPRun.prolog_cell(self,s)
            self.q = self.yapeng.query(JupyterQuery(self, program, query))
        self.Solutions = []
        if not self.q:
            return True, []
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
        while True:
            iterations = iterations - 1
            rc = YAPRun.answer(self, self.q)
            if rc:
                # deterministic = one solution
                #Dict = {}
                #engine.goal(show_answer( q.namedVars(), Dict))
                self.Solutions += [self.bindings]
                if self.port == "exit":
                    # done
                    self.q.close()
                    self.q = None
                    self.os = ""
                    return True, self.Solutions
                if iterations == 0:
                    return True, self.Solutions
            else:
                print("No (more) answers")
                self.q.close()
                self.q = None
                self.os = ''
                return True, self.Solutions

    def answer(self, q):
        try:
            return q.next()
        except Exception as e:
            print(e.args[1])
            self.yapeng.goal(exit_cell(self))
            return False, None


    def _yrun_cell(self, raw_cell, store_history=True, silent=False,
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

        info = interactiveshell.ExecutionInfo(
            raw_cell, store_history, silent, shell_futures)

        result = interactiveshell.ExecutionResult(info)

        if (raw_cell == "") or raw_cell.isspace():
            self.last_execution_succeeded = True
            return result

        if silent:
            store_history = False

        if store_history:
            result.execution_count = self.execution_count+1

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
        preprocessing_exc_tuple = None
        try:
            # Static input transformations
            cell = self.input_transformer_manager.transform_cell(raw_cell)
        except SyntaxError:
            preprocessing_exc_tuple = self.syntax_error() # sys.exc_info()
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
            state = YAPRun.jupyter_query(self, cell)
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

    def    prolog_cell(self,s):
        """"
        Trasform a text into program+query. A query is the
        last line if the last line is non-empty and does not terminate
        on a dot. You can also finish with

            - `*`: you request all solutions
            - '^': you want to check if there is an answer
            - '?'[N]: you want an answer; optionally you want N answers

            If the line terminates on a `*/` or starts on a `%` we assume the line
        is a comment.
        """
        s = s.rstrip()
        take = 0
        its = 0
        s0 = ''
        for c in s:
            if c == '\n' or c.isblank():
                s0 += [c]
        sf = ''
        for c in reversed(s):
            if c == '\n' or c.isblank():
                sf += [c]+sf
        [program,x,query] = s.rpartition('\n')
        if query == '':
            query = program
        while take < len(query):
            take += 1
            ch = query[-take]
            if ch.isdigit():
                its = its*10 + ord(ch) - ord('0')
            elif ch == '*' and take == 1:
                return program, query[:-take], -1
            elif ch == '.' and take == 1:
                return s, '', 1
            elif ch == '/' and query[-2] == '*' and take == 1:
                return program, query[:-take], 1
            elif ch == '^' and take == 1:
                return program, query[:-take], 1
            elif ch == '?':
                return program, query[:-take], its+1
            else:
                return program, query, 1
        return s, '', 1
