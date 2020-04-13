import sys

from typing import  List
from traitlets import Bool


from yap4py.systuples import *
from yap4py.yapi import *
from IPython.core.completer import Completer
# import IPython.core
from IPython.core.inputsplitter import *
from IPython.core.inputtransformer import *
from IPython.core.interactiveshell import *
from ipython_genutils.py3compat import builtin_mod

import copy
import json

from yap_kernel.displayhook import ZMQShellDisplayHook

import traceback

global  engine

def tracefunc(frame, event, arg, indent=[0]):
    if event == "call":
        indent[0] += 2
        print( "-" * indent[0] + "> call function", frame.f_code.co_name )
    elif event == "return":
        print( "<" + "-" * indent[0], "exit function", frame.f_code.co_name )
        indent[0] -= 2
    return tracefunc


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

    def __init__(self, engine=None, shell=None, line_input_checker=True, physical_line_transforms=None,
                    logical_line_transforms=None):
        self._buffer_raw = []
        self._validate = True
        self.engine = engine
        self.shell = shell

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
        return t

    def validQuery(self, text, engine, shell, line=None):
        """Return whether a legal query
        """
        if shell and text == shell.os:
            return True
        if not  line:
            line = text.rstrip()
        self.errors = []
        engine.mgoal(errors(self, line),"user",True)
        return self.errors == []


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
            return self.validQuery(self.source, engine, self.shell)

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
        """Push one or more lines of IPython input.

        This stores the given lines and returns a status code indicating
        whether the code forms a complete Python block or not, after processing
        all input lines for special IPython syntax.

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

    def magic_config_matches(self, text:str) -> List:
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


    def magic_color_matches(self, text:str) -> List:
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
            and usual IPython completion.

        .. note::

            Completions are not completely deduplicated yet. If identical
            completions are coming from different sources this function does not
            ensure that each completion object will only be present once.
        """
        if text[0] == '%' and text.find('\n') > offset:
            magic_res = self.magic_matches(text)
            return text,  magic_res
        self.matches = []
        prolog_res = self.shell.engine.mgoal(completions(text, self), "user",True)
        return text, self.matches





class YAPRun(InteractiveShell):
    """An enhanced, interactive shell for YAP."""

    def __init__(self, shell):
        self.shell = shell
        self.engine = JupyterEngine()
        global engine
        engine = self.engine
        self.errors = []
        self.q = None
        self.os = None
        self.it = None
        self.port = "None"
        self.answers = None
        self.bindings = dicts = []
        self.shell.engine = self.engine
        self._get_exc_info = shell._get_exc_info
        self.iterations = 0


    def showtraceback(self, exc_info):
        try:
            (etype, value, tb) = e
            traceback.print_exception(etype, value, tb)
        except:
            print(e)
            pass


    def syntaxErrors(self, text):
        """Return whether a legal query
        """
        if not  text:
            return []
        if text == self.os:
            return self.errors
        self.errors=[]
        self.engine.mgoal(errors(self,text),"user",True)
        return self.errors

    def prolog(self, ccell, result):

        #
        # construct a self.query from a one-line string
        # self.q is opaque to Python
        try:
            # sys.settrace(tracefunc)
            (program, squery, command, howmany) = ccell
            if not self.q or self.os != (program,squery):
                if self.q:
                    self.q.close()
                    self.q = None
                self.answers = []
                result.result = []
                self.os = (program,squery)
                self.engine.reSet()
                pg = jupyter_query(self,program,squery)
                self.q = Query(self.engine, pg)
            for v in self.q:
                self.iterations += 1
                howmany -= 1
                # o = '[ '
                # o += str(self.iterations )
                # o += '    '
                # o += json.dumps(self.q.answer)
                # o += ' ]\n\n'
                self.answers += [self.q.answer]
                if howmany == 0 :
                    return self.answers
            if self.answers:
               return self.answers
            else:
                print("No\n")
                return None
            
     
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in query '+ str(self.q)+
                             '\n  Answers'+ json.dumps( self.answers)+ '\n')

            has_raised = True
            return result.result


    def _yrun_cell(self, raw_cell, result, store_history=True, silent=False,
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
        #import pdb; pdb.set_trace()
        #     #pdb.set_trace()
        # atom match either symbols, or if no symbol exists, strings, In this case        # If any of our input transformation (input_transformer_manager or
        # prefilter_manager) raises an exception, we store it in this variable
        # so that we can display the error after logging the input and storing
        # it in the history.
        preprocessing_exc_tuple = None
        # try:
        #     # Static input transformations
        #     cell = self.shell.input_transformer_manager.transform_cell(raw_cell)
        # except SyntaxError:
        #     preprocessing_exc_tuple = self.shell.syntax_error() # sys.exc_info()
        cell = raw_cell
        for i in self.errors:
            try:
                (_,lin,pos,text) = i
                e = self.SyntaxError( (self.cell_name, lin, pos, text+'\n'))
                raise e
            except SyntaxError:
                self.shell.showsyntaxerror(  )
                preprocessing_exc_tuple = sys.exc_info()
        # Store raw and processed history
        if store_history:
            self.shell.history_manager.store_inputs(self.shell.execution_count,
                                              cell, raw_cell)
        if not silent:
            self.shell.logger.log(cell, raw_cell)
        # # Display the exception if input processing failed.
        if preprocessing_exc_tuple is not None:
            self.showtraceback(preprocessing_exc_tuple)
            if store_history:
                self.shell.execution_count += 1
            return self.error_before_exec(preprocessing_exc_tuple[2])

        # Our own compiler remembers the __future__ environment. If we want to
        # run code with a separate __future__ environment, use the default
        # compiler
        # compiler = self.shell.compile if shell_futures else CachingCompiler()
        self.cell_name = str( self.shell.execution_count)
        self.shell.displayhook.exec_result= result
        cell = raw_cell.strip()
        while cell[0] == '%':
            if cell[1] == '%':
                ## cell magic
                txt0 = cell[2:].split(maxsplit = 1, sep = '\n')
                try:
                    body = txt0[1]
                    magic = txt0[0].strip()    
                except:
                    magic = cell[2:].strip()
                    body = ""
                linec = False
                try:
                    [magic,line] = magic.split(maxsplit=1)
                except:
                    line = ""
                self.shell.last_execution_succeeded = True
                self.shell.execution_count += 1
                if magic == "python3":
                    result.result = eval(body)
                    return
                result.result = self.shell.run_cell_magic(magic, line, body)
                return
            else:
                linec = True
                rcell = cell[1:].strip()
                try:
                    [magic,cell] = rcell.split(maxsplit = 1, sep = '\n')
                except:
                    magic = rcell.strip()
                    cell = ""
                try:
                    [magic,line] = magic.split(maxsplit=1)
                except:
                    line = ""
                self.shell.last_execution_succeeded = True
                self.shell.run_line_magic(magic, line)
                # go execute the body
        # Give the displayhook a reference to our ExecutionResult so it
        # can fill in the output value.
        self.shell.displayhook.exec_result = result
        #import pdb; pdb.set_trace()
        ccell = self.prolog_cell(cell)
        #print(ccell)
        (  program,squery,_ ,howmany) = ccell
        if howmany == 0 and not program:
            return result
        if self.syntaxErrors(program+squery+".\n") :
            result.result = []
            return result
        has_raised = False
        try:
            self.shell.execution_count += 1
            builtin_mod.input = input
            self.shell.input = input
            #self.engine.mgoal(streams(True),"user", True)
            #create a Trace object, telling it what to ignore, and whether to
            # do tracing or line-counting or both.
            # tracer = trace.Trace(
            #     ignoredirs=[sys.prefix, sys.exec_prefix],
            #     trace=1,
            #     count=0)
            #

            # def f(self, cell, state):
            #     state = self.jupyter_query( cell )

            # run the new command using the given tracer
            #
            # tracer.runfunc(f,self,cell,state)
            answers = self.prolog( ccell, result )
            # state = tracer.runfunc(hist
            # er_query( self, cell ) )
        except Exception as e:
            has_raised = True
            try:
                (etype, value, tb) = e
                traceback.print_exception(etype, value, tb)
            except:
                print(e)

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

        #self.engine.mgoal(streams(False),"user", True)
        return



    def prolog_cell(self, s):
        return cell_structure(s)

def cell_structure(s):
    """
    Trasform a text into program+query. A query is the
    last line if the last line is non-empty and does not terminate
    on a dot. You can also finish with

        - `*`:a you request all solutions
        - ';'[N]: you want an answer; optionally you want N answers

        If the line terminates on a `*/` or starts on a `%` we assume the line
    is a comment.
    """
    try:
        i=0
        while s[i] == '%':
            while s[i] and s[i] != '\n':
                i += 1;
        l = len(s)
        p = l
        repeats = 0
        while p > i and s[p-1].isspace():
            p-=1
        d = ''
        e=1
        while p > i and s[p-1].isdigit():
            repeats += e*int(s[p-1])
            p-=1
            e *= 10
        while p > i and s[p-1].isspace():
            p-=1
        if p == i:
            return (s, '', '', 0)
        p0=p-1
        c = s[p-1]
        if c== '*' and repeats == 0:
            sep = '*'
            reps = -1
        elif c== '?':
            sep = '?'
            reps = max(1, repeats)
        elif c== '.':
            return (s, '', '', 0)
        else:
            sep = ""
            reps = 1
        while p > i and s[p-1].isspace():
            p-=1
        p0 = p
        while p > i and s[p-1] != '\n':
            p -= 1
        if sep == "":
            return (s[:p], s[p:], "", 1)
        return (s[:p], s[p:p0-1], sep, reps)
    except Exception as e:
        try:
            (etype, value, tb) = e
            traceback.print_exception(etype, value, tb)
        except:
            print(e)
