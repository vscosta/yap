import os
import sys
import abc
import math
import itertools


from typing import Iterator, List, Tuple, Iterable, Union
from traitlets import Bool, Enum, observe, Int

try:
    from yap4py.yapi import Engine
except:
    print("Could not load _yap dll.")
from yap_ipython.core import interactiveshell
from yap_ipython.core.completer import Completer, Completion
from yap_ipython.utils.strdispatch import StrDispatch
# import yap_ipython.core
from traitlets import Instance
from yap_ipython.core.inputsplitter import *
from yap_ipython.core.inputtransformer import *
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
jupyter_query = namedtuple('jupyter_query', 'self text query')
enter_cell = namedtuple('enter_cell', 'self' )
exit_cell = namedtuple('exit_cell', 'self' )
completions = namedtuple('completions', 'txt self' )
errors = namedtuple('errors', 'self text' )


class YAPInputSplitter(InputSplitter):
    """An input splitter that recognizes all of iyap's special syntax."""

    # String with raw, untransformed input.
    source_raw = ''

    # Flag to track when a transformer has stored input that it hasn't given
    # back yet.
    transformer_accumulating = False

    # Flag to track when assemble_yap_lines has stored input that it hasn't
    # given back yet.
    within_yap_line = False

    # Private attributes

    # List with lines of raw input accumulated so far.
    _buffer_raw = None

    def __init__(self, line_input_checker=True, physical_line_transforms=None,
                    logical_line_transforms=None):
        self._buffer_raw = []
        self._validate = True
        self.yapeng = None

        if physical_line_transforms is not None:
            self.physical_line_transforms = physical_line_transforms
        else:
            self.physical_line_transforms = [
                                             leading_indent(),
                                             classic_prompt(),
                                             ipy_prompt(),
                                             cellmagic(end_on_blank_line=line_input_checker),
                                            ]

        self.assemble_logical_lines = assemble_logical_lines()
        if logical_line_transforms is not None:
            self.logical_line_transforms = logical_line_transforms
        else:
            self.logical_line_transforms = [
                                            help_end(),
                                            escaped_commands(),
                                            assign_from_magic(),
                                            assign_from_system(),
                                           ]


    @property
    def transforms(self):
        "Quick access to all transformers."
        return self.physical_line_transforms + \
            [self.assemble_logical_lines] + self.logical_line_transforms


    @property
    def transforms_in_use(self):
        """Transformers, excluding logical line transformers if we're in a
        Python line."""
        t = self.physical_line_transforms + \
        [self.assemble_logical_lines] + self.logical_line_transforms

    def engine(self, engine):
        self.yapeng = engine

    def validQuery(self, text, line=None):
        """Return whether a legal query
        """
        print("valid")
        if not  line:
            (_,line,_) = self.shell.prolog_cell(text)
        line = line.strip().rstrip()
        if not line:
            return False
        self.errors = []
        self.yapeng.mgoal(errors(self, line),"user")
        print(self.errors)
        return self.errors != []


    def reset(self):
        """Reset the input buffer and associated state."""
        #super(YAPInputSplitter, self).reset()
        self._buffer_raw[:] = []
        self.source_raw = ''
        self.transformer_accumulating = False

        for t in self.transforms:
            try:
                t.reset()
            except SyntaxError:
                # Nothing that calls reset() expects to handle transformer
                # errors
                pass

    def flush_transformers(self):
        def _flush(transform, outs):
            """yield transformed lines

            always strings, never None

            transform: the current transform
            outs: an iterable of previously transformed inputs.
                 Each may be multiline, which will be passed
                 one line at a time to transform.
            """
            for out in outs:
                for line in out.splitlines():
                    # push one line at a time
                    tmp = transform.push(line)
                    if tmp is not None:
                        yield tmp

            # reset the transform
            tmp = transform.reset()
            if tmp is not None:
                yield tmp

        out = []

        for t in self.transforms:
            out = _flush(t, out)

        out = list(out)
        if out:
            self._store('\n'.join(out))

    def raw_reset(self):
        """Return raw input only and perform a full reset.
        """
        out = self.source_raw
        self.reset()
        return out

    def source_reset(self):
        try:
            self.flush_transformers()
            return self.source
        finally:
            self.reset()

    def push_accepts_more(self):
        if self.transformer_accumulating:
            return True
        else:
            return self,validQuery(self.source)

    def transform_cell(self, cell):
        """Process and translate a cell of input.
        """
        self.reset()
        try:
            self.push(cell)
            self.flush_transformers()
            return self.source
        finally:
            self.reset()

    def push(self, lines):
        """Push one or more lines of yap_ipython input.

        This stores the given lines and returns a status code indicating
        whether the code forms a complete Python block or not, after processing
        all input lines for special yap_ipython syntax.

        Any exceptions generated in compilation are swallowed, but if an
        exception was produced, the method returns True.

        Parameters
        ----------
        lines : string
          One or more lines of Python input.

        Returns
        -------
        is_complete : boolean
          True if the current input source (the result of the current input
          plus prior inputs) forms a complete Python execution block.  Note that
          this value is also stored as a private attribute (_is_complete), so it
          can be queried at any time.
        """

        # We must ensure all input is pure unicode
        lines = cast_unicode(lines, self.encoding)
        # ''.splitlines() --> [], but we need to push the empty line to transformers
        lines_list = lines.splitlines()
        if not lines_list:
            lines_list = ['']

        # Store raw source before applying any transformations to it.  Note
        # that this must be done *after* the reset() call that would otherwise
        # flush the buffer.
        self._store(lines, self._buffer_raw, 'source_raw')

        transformed_lines_list = []
        for line in lines_list:
            transformed = self._transform_line(line)
            if transformed is not None:
                transformed_lines_list.append(transformed)
        if transformed_lines_list:
            transformed_lines = '\n'.join(transformed_lines_list)
        else:
            # Got nothing back from transformers - they must be waiting for
            # more input.
            return False

    def _transform_line(self, line):
        """Push a line of input code through the various transformers.

        Returns any output from the transformers, or None if a transformer
        is accumulating lines.

        Sets self.transformer_accumulating as a side effect.
        """
        def _accumulating(dbg):
            #print(dbg)
            self.transformer_accumulating = True
            return None

        for transformer in self.physical_line_transforms:
            line = transformer.push(line)
            if line is None:
                return _accumulating(transformer)

        for transformer in self.logical_line_transforms:
            line = transformer.push(line)
            if line is None:
                return _accumulating(transformer)


        #print("transformers clear") #debug
        self.transformer_accumulating = False
        return line


class YAPCompleter(Completer):

    greedy = Bool(False,
                  help="""Activate greedy completion
        PENDING DEPRECTION. this is now mostly taken care of with Jedi.

        This will enable completion on elements of lists, results of function calls, etc.,
        but can be unsafe because the code is actually evaluated on TAB.
        """
                  ).tag(config=True)

    debug = Bool(default_value=False,
                 help='Enable debug for the Completer. Mostly print extra '
                      'information for experimental jedi integration.') \
        .tag(config=True)

    backslash_combining_completions = Bool(True,
                                           help="Enable unicode completions, e.g. \\alpha<tab> . "
                                                "Includes completion of latex commands, unicode names, and expanding "
                                                "unicode characters back to latex commands.").tag(config=True)



    def __init__(self, namespace=None, global_namespace=None, shell=None, **kwargs):
        """Create a new completer for the command line.

        Completer(namespace=ns, global_namespace=ns2) -> completer instance.

         """

        self.shell = shell
        self.magic_escape = ESC_MAGIC
        super(Completer, self).__init__(**kwargs)

    def complete(self, text, line=None, cursor_pos=None):
        """Return the completed text and a list of completions.

        Parameters
        ----------

           text : string
             A string of text to be completed on.  It can be given as empty and
             instead a line/position pair are given.  In this case, the
             completer itself will split the line like readline does.

       This is called successively with state == 0, 1, 2, ... until it
        returns None.  The completion should begin with 'text'.

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
        return self.completions(text, cursor_pos)


    def magic_matches(self, text):
        """Match magics"""
        # Get all shell magics now rather than statically, so magics loaded at
        # runtime show up too.
        lsm = self.shell.magics_manager.lsmagic()
        line_magics = lsm['line']
        cell_magics = lsm['cell']
        pre = self.magic_escape
        pre2 = pre+pre

        explicit_magic = text.startswith(pre)

        # Completion logic:
        # - user gives %%: only do cell magics
        # - user gives %: do both line and cell magics
        # - no prefix: do both
        # In other words, line magics are skipped if the user gives %% explicitly
        #
        # We also exclude magics that match any currently visible names:
        # https://github.com/ipython/ipython/issues/4877, unless the user has
        # typed a %:
        # https://github.com/ipython/ipython/issues/10754
        bare_text = text.lstrip(pre)
        global_matches = []
        if not explicit_magic:
            def matches(magic):
                """
                Filter magics, in particular remove magics that match
                a name present in global namespace.
                """
                return ( magic.startswith(bare_text) and
                         magic not in global_matches )
        else:
            def matches(magic):
                return magic.startswith(bare_text)

        comp = [ pre2+m for m in cell_magics if matches(m)]
        if not text.startswith(pre2):
            comp += [ pre+m for m in line_magics if matches(m)]

        return comp

    def magic_config_matches(self, text:str) -> List[str]:
        """ Match class names and attributes for %config magic """
        texts = text.strip().split()

        if len(texts) > 0 and (texts[0] == 'config' or texts[0] == '%config'):
            # get all configuration classes
            classes = sorted(set([ c for c in self.shell.configurables
                                   if c.__class__.class_traits(config=True)
                                   ]), key=lambda x: x.__class__.__name__)
            classnames = [ c.__class__.__name__ for c in classes ]

            # return all classnames if config or %config is given
            if len(texts) == 1:
                return classnames

            # match classname
            classname_texts = texts[1].split('.')
            classname = classname_texts[0]
            classname_matches = [ c for c in classnames
                                  if c.startswith(classname) ]

            # return matched classes or the matched class with attributes
            if texts[1].find('.') < 0:
                return classname_matches
            elif len(classname_matches) == 1 and \
                    classname_matches[0] == classname:
                cls = classes[classnames.index(classname)].__class__
                help = cls.class_get_help()
                # strip leading '--' from cl-args:
                help = re.sub(re.compile(r'^--', re.MULTILINE), '', help)
                return [ attr.split('=')[0]
                         for attr in help.strip().splitlines()
                         if attr.startswith(texts[1]) ]
        return []


    def magic_color_matches(self, text:str) -> List[str] :
        """ Match color schemes for %colors magic"""
        texts = text.split()
        if text.endswith(' '):
            # .split() strips off the trailing whitespace. Add '' back
            # so that: '%colors ' -> ['%colors', '']
            texts.append('')

        if len(texts) == 2 and (texts[0] == 'colors' or texts[0] == '%colors'):
            prefix = texts[1]
            return [ color for color in InspectColors.keys()
                     if color.startswith(prefix) ]
        return []




    def completions(self, text, offset):
        """
        Returns an iterator over the possible completions

        .. warning:: Unstable

            This function is unstable, API may change without warning.
            It will also raise unless use in proper context manager.

        Parameters
        ----------

        text:str
            Full text of the current input, multi line string.
        offset:int
            Integer representing the position of the cursor in ``text``. Offset
            is 0-based indexed.

        Yields
        ------
            :any:`Completion` object


        The cursor on a text can either be seen as being "in between"
        characters or "On" a character depending on the interface visible to
        the user. For consistency the cursor being on "in between" characters X
        and Y is equivalent to the cursor being "on" character Y, that is to say
        the character the cursor is on is considered as being after the cursor.

        Combining characters may span more that one position in the
        text.


        .. note::

            If ``IPCompleter.debug`` is :any:`True` will yield a ``--jedi/ipython--``
            fake Completion token to distinguish completion returned by Jedi
            and usual yap_ipython completion.

        .. note::

            Completions are not completely deduplicated yet. If identical
            completions are coming from different sources this function does not
            ensure that each completion object will only be present once.
        """
        self.matches = []
        prolog_res = self.shell.yapeng.mgoal(completions(text, self), "user")
        magic_res = self.magic_matches(text)

        return text, self.matches+magic_res



class YAPRun:
    """An enhanced, interactive shell for YAP."""

    def __init__(self, shell):
        self.shell = shell
        self.yapeng = Engine()
        self.yapeng.goal(use_module(library("jupyter")))
        self.q = None
        self.run = False
        self.shell.port = None
        self.shell.yapeng = self.yapeng
        self._get_exc_info = shell._get_exc_info

    def syntaxErrors(self, text):
        """Return whether a legal query
        """
        if not  text:
            return []
        self.errors=[]
        self.yapeng.mgoal(errors(self,text),"user")
        return self.errors

    def jupyter_query(self, s, mx):
        #
        # construct a self.query from a one-line string
        # self.q is opaque to Python
        iterations = 0
        bindings = []
        program,query,_ = self.prolog_cell(s)
        if query == self.shell.os:
            q = self.shell.q
            self.shell.os = None
        else:
            q = Goal(jupyter_query(self, query), self.yapeng, module="user",program=program)
        for q in q:
            bindings += [q.bindings()]
            iterations += 1
            if mx == iterations:
                break
        if q:
            self.shell.os = query
        self.shell.q = q
        return bindings


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
            self.shell.last_execution_succeeded = True
            return result

        if silent:
            store_history = False

        if store_history:
            result.execution_count = self.shell.execution_count+1

        def error_before_exec(value):
            result.error_before_exec = value
            self.shell.last_execution_succeeded = False
            return result

        self.shell.events.trigger('pre_execute')
        if not silent:
            self.shell.events.trigger('pre_run_cell')
        # If any of our input transformation (input_transformer_manager or
        # prefilter_manager) raises an exception, we store it in this variable
        # so that we can display the error after logging the input and storing
        # it in the history.
        preprocessing_exc_tuple = None
        # try:
        #     # Static input transformations
        #     cell = self.shell.input_transformer_manager.transform_cell(raw_cell)
        # except SyntaxError:
        #     preprocessing_exc_tuple = self.shell.syntax_error() # sys.exc_info()
        cell = raw_cell  # cell has to exist so it can be stored/logged
        # else:
        #     if False and len(cell.splitlines()) == 1:
        #         # Dynamic transformations - only applied for single line commands
        #         with self.shell.builtin_trap:
        #             try:
        #                 # use prefilter_lines to handle trailing newlines
        #                 # restore trailing newline for ast.parse
        #                 cell = self.shell.prefilter_manager.prefilter_lines(cell) + '\n'
        #             except Exception:
        #                 # don't allow prefilter errors to crash IPython
        #                 preprocessing_exc_tuple = sys.exc_info()


        for i in self.syntaxErrors(raw_cell):
            try:
                (what,lin,_,text) = i
                e = SyntaxError(what, ("<string>", lin, 1, text))
                raise e
            except SyntaxError:
                self.shell.showsyntaxerror(  )
                preprocessing_exc_tuple = sys.exc_info()

        # Store raw and processed history
        if store_history:
            self.shell.history_manager.store_inputs(self.shell.execution_count,
                                              cell, raw_cell)
        silent = False
        if not silent:
            self.shell.logger.log(cell, raw_cell)
        # # Display the exception if input processing failed.
        # if preprocessing_exc_tuple is not None:
        #     self.showtraceback(preprocessing_exc_tuple)
        #     if store_history:
        #         self.shell.execution_count += 1
        #     return error_before_exec(preprocessing_exc_tuple[2])

        # Our own compiler remembers the __future__ environment. If we want to
        # run code with a separate __future__ environment, use the default
        # compiler
        # compiler = self.shell.compile if shell_futures else CachingCompiler()
        cell_name = str( self.shell.execution_count)

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
                self.shell.run_line_magic(magic, line)
                if len(txt0) == 2:
                    cell = txt0[1]
                else:
                    cellArea = ""
            else:
                self.shell.run_cell_magic(magic, line, cell)
                return
        # Give the displayhook a reference to our ExecutionResult so it
        # can fill in the output value.
        self.shell.displayhook.exec_result = result
        has_raised = False
        try:
            self.shell.bindings = dict = {}
            state = self.jupyter_query( cell)
            if state:
                self.shell.last_execution_succeeded = True
                result.result = (True, dict)
            else:
                self.shell.last_execution_succeeded = True
                result.result = (True, {})
        except Exception as e:
            print(e)
            has_raised = True
            result.result = False

        self.shell.last_execution_succeeded = not has_raised

        # Reset this so later displayed values do not modify the
        # ExecutionResult
        self.shell.displayhook.exec_result = None
        self.shell.events.trigger('post_execute')
        if not silent:
            self.shell.events.trigger('post_run_cell')

        if store_history:
            # Write output to the database. Does nothing unless
            # history output logging is enabled.
            self.shell.history_manager.store_output(self.shell.execution_count)
            # Each cell is a *single* input, regardless of how many lines it has
            self.shell.execution_count += 1

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
            if c == '\n' or c.isspace():
                s0 += c
            break
        sf = ''
        for c in reversed(s):
            if c == '\n' or c.isspace():
                sf += c
            break
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
