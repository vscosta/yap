import abc
import json
import pprint
import sys
from collections import namedtuple
from traitlets.config.configurable import SingletonConfigurable
from traitlets.utils.importstring import import_item
from IPython.core import oinspect
from traitlets import ( Any )
from yap4py.queries import Query, TopQuery
from yap4py.yapi import Engine, EngineArgs
import IPython.core.getipython
from IPython.core.async_helpers import _pseudo_sync_runner
from IPython.core.interactiveshell import InteractiveShell, ExecutionInfo, ExecutionResult
from IPython.core.inputtransformer2 import TransformerManager
from typing import  Optional
from IPython.core.display import DisplayObject, display
#from IPython.core.async_helpers import (_asyncio_runner,  _asyncify, _pseudo_sync_runner)
from IPython.core.async_helpers import _curio_runner, _trio_runner, _should_be_async
import traceback

def showtraceback( exc_info):
    print( exc_info )


gate = None
    
class JupyterEngine( Engine ):
    def __init__(self, args=None,self_contained=False,**kwargs):
        # type: (object) -> object
        if not args:
            args = EngineArgs(**kwargs)
        args.jupyter = True
        args.setPrologGoal("load_files(library(jupyter),[source_module(user)])")
        Engine.__init__(self, args)
        
        self.errors = []
        self.warnings = []
        self.shell = None
        self.q = None
        self.os = None
        self.port="call"
        self.iterations = 0
        try:
            self.set_prolog_flag("verbose_load",False)
            self.load_library('jupyter')
            self.load_library('completer')
            self.load_library('verify')
            self.set_prolog_flag("verbose_load",True)
        except Exception as e:
            print( e )



    def run_prolog_cell(self, result, all):
        """
        Reconsult a Prolog program  and execute/reexecute a Prolog query. It<< receives as input:
        - self, the Prolog engine;\:
            self.q contains the Prolog query, incluindo the current answers (self.answers) and the last tahg
        - result, that stores execution results;
the number of solutions to return,
        """

        def set_gate(self,gate):
            self.gate = gate
        

        # import pdb;pdb.set_trace()
        try:
            for _ in self.q:
                if not all:
                    result.result = self.answers
                    return False
        except StopIteration as e:
            if self.answers:
                result.result = self.answers
            self.q = None
            return False
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in squery '+ str(self.q)+'\n')
            result.error_in_exec=e
            return True

    def jupyter_consult(self, text, m=None, release=False):
        jupyter_consult = namedtuple('jupyter_consult', 'program server')
        self.run(jupyter_consult(text, self), m, release)

 
    async def jupyter_cell(self, result, cell, raw_cell, shell, store_history=False, cell_id=None):
        try:
            all = False
            self.shell = shell
            query = cell
            # sys.settrace(tracefunc)
            query = query.strip()

            if not query:
                return False
            if not self.q or not self.os or self.os != cell:
                if self.q:
                    self.q.close()
                    self.q = None
                self.os = cell
                self.warnings = []
                cell += "\n"
                self.errors = []
                self.errors = Jupyter4YAP.syntaxErrors( self, cell )
                if self.errors:
                    return True
                self.warnings = []
                if query[-1] == '.':
                    self.reSet()
                    self.jupyter_consult(cell)
                    result.result = [[]]
                    return False
                if query[-1] == '*':
                    query = query[:-1].strip()
                    all = True
                query+=".\n"
                self.reSet()
                self.q = TopQuery(engine,query)
                self.gate = self.q.gate
                self.answers = []
            self.run_prolog_cell(result, all)
            self.iterations = 0
            self.errors = []
            return False

        except Exception as e:
            showtraceback(e)
            return True
        #pp = pprint.PrettyPrinter(indent=4)
        #sys.stdout.write(self.q.gate+': ')
        #pp.pprint(result.result)



engine =  JupyterEngine()
def get_ipython():
    return engine.shell #IPython.core.getipython.get_ipython

def get_engine():
    return engine

class Jupyter4YAP( InteractiveShell ):
    """An enhanced, interactive shell for YAP."""


    def __init__(self, ipython_dir=None, profile_dir=None,
                 user_module=None, user_ns=None,
                 custom_exceptions=((), None), **kwargs):
        # This is where traits with a config_key argument are updated
        # from the values on config.
        super(Jupyter4YAP, self).__init__(**kwargs)

    def run_cell(
        self,
        raw_cell,
        store_history=False,
        silent=False,
        shell_futures=True,
        cell_id=None,
    ):
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
        result = None
        
        try:
            result = self._run_cell(
                raw_cell, store_history, silent, shell_futures, cell_id
            )
        finally:
            self.events.trigger('post_execute')
            if not silent:
                self.events.trigger('post_run_cell', result)
        return result

    def _run_cell(
        self,
        raw_cell: str,
        store_history: bool,
        silent: bool,
        shell_futures: bool,
        cell_id: str,
    ) -> ExecutionResult:
        """Internal method to run a complete IPython cell."""

        # we need to avoid calling self.transform_cell multiple time on the same thing
        # so we need to store some results:
        preprocessing_exc_tuple = None
        try:
            transformed_cell = self.transform_cell(raw_cell)
        except Exception:
            transformed_cell = raw_cell
            preprocessing_exc_tuple = sys.exc_info()

        assert transformed_cell is not None
        coro = self.run_cell_async(
            raw_cell,
            store_history=store_history,
            silent=silent,
            shell_futures=shell_futures,
            transformed_cell=transformed_cell,
            preprocessing_exc_tuple=preprocessing_exc_tuple,
            cell_id=cell_id,
        )

        # run_cell_async is async, but may not actually need an eventloop.
        # when this is the case, we want to run it using the pseudo_sync_runner
        # so that code can invoke eventloops (for example via the %run , and
        # `%paste` magic.
        if self.trio_runner:
            runner = self.trio_runner
        elif self.should_run_async(
            raw_cell,
            transformed_cell=transformed_cell,
            preprocessing_exc_tuple=preprocessing_exc_tuple,
        ):
            runner = self.loop_runner
        else:
            runner = _pseudo_sync_runner

        try:
            result = runner(coro)
        except BaseException as e:
            info = ExecutionInfo(
                raw_cell, store_history, silent, shell_futures, cell_id
            )
            result = ExecutionResult(info)
            result.error_in_exec = e
            self.showtraceback(running_compiled_code=True)
        finally:
            return result


    def syntaxErrors(self, text):
        """
        Returns an exception if the query has syntax errors
        """
        self.errors=[]
        self.warnings=[]
        if text and text.isspace():
            file ="scratch"
            try:
                text = i["parserTextA"]
            except:
                text ="scratch"
            try:
                e= SyntaxError(i["label"],(file,i["parserLine"],i["parserPos"],text))
                raise
            except x as Exception:
                self.showsyntaxerror()
        return self.errors


                    

    async def run_cell_async(
        self,
        raw_cell: str,
        store_history=False,
        silent=False,
        shell_futures=True,
        *,
        transformed_cell: Optional[str] = [],
            cell_id = None,
        preprocessing_exc_tuple: Optional[Any] = []
    ) -> ExecutionResult:
        """Run a complete IPython cell asynchronously.

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
        transformed_cell: str
          cell that was passed through transformers
        preprocessing_exc_tuple:
          trace if the transformation failed.

        Returns
        -------
        result : :class:`ExecutionResult`

        .. versionadded: 7.0
        """
        info = ExecutionInfo(
            raw_cell, store_history, silent, shell_futures, cell_id)
        result = ExecutionResult(info)

        if (not raw_cell) or raw_cell.isspace():
            self.last_execution_succeeded = True
            self.last_execution_result = result
            return result

        python = False
        if (not transformed_cell) or transformed_cell.isspace():
            self.last_execution_succeeded = True
            self.last_execution_result = result
            return result

        if silent:
            store_history = False

        if store_history:
            result.execution_count = self.execution_count

        self.events.trigger('pre_execute')
        if not silent:
            self.events.trigger('pre_run_cell', info)

        if transformed_cell is None:
            warnings.warn(
                "`run_cell_async` will not call `transform_cell`"
                " automatically in the future. Please pass the result to"
                " `transformed_cell` argument and any exception that happen"
                " during the ffsform in `preprocessing_exc_tuple` in"
                " IPython 7.17 and above.",
                DeprecationWarning,
                stacklevel=2,
            )
            engine.shell = self
            # If any of our input transformation (input_transformer_manager or
            # prefilter_manager) raises an exception, we store it in this variable
            # so that we can display the error after logging the input and storing
            # it in the history.
            try:
                cell = self.transform_cell(raw_cell)
            except IndentationError as e:
                preprocessing_exc_tuple = None
                cell = raw_cell  # cell has to exist so it can be stored/logged
            except Exception as e:
                preprocessing_exc_tuple = sys.exc_info()
                cell = raw_cell  # cell has to exist so it can be stored/logged
            else:
                preprocessing_exc_tuple = None
        else:
            if preprocessing_exc_tuple is None:
                cell = transformed_cell
            else:
                cell = raw_cell
        _run_async = False
        # Store raw and processed history
        if store_history:
            self.history_manager.store_inputs(self.execution_count,
                                              cell, raw_cell)
        if not silent:
            self.logger.log(cell, raw_cell)

        def error_before_exec(value):
            if store_history:
                self.execution_count += 1
            result.error_before_exec = value
            self.last_execution_succeeded = False
            self.last_execution_result = result
            return result

        # Display the exception if input processing failed.
        if preprocessing_exc_tuple is not None:
            showtraceback(preprocessing_exc_tuple)
            if store_history:
                self.execution_count += 1
            return error_before_exec(preprocessing_exc_tuple[1])

        # Our own compiler remembers the __future__ environment. If we want to
        # run code with a separate __future__ environment, use the default
        # compiler
        #compiler = self.compile if shell_futures else self.compiler_class()
        has_raised = False
        plcell=cell
        if raw_cell.startswith("#!python"):
            python = True
            cell = raw_cell[raw_cell.find("\n")+1:]
            plcell = ""
        elif raw_cell.startswith("%"):
            python=True
            if raw_cell.startswith("%%"):
                plcell = ""
            else:
                subcells = cell.partition("\n")
                cell = subcells[0]
                plcell = subcells[2]
        if python:
            # Our own compiler remembers the __future__ environment. If we want to
            # run code with a separate __future__ environment, use the default
            # compiler
            compiler = self.compile if shell_futures else self.compiler_class()

            _run_async = False

            with self.builtin_trap:
                cell_name = compiler.cache(cell, self.execution_count, raw_code=raw_cell)

                with self.display_trap:
                    # Compile to bytecode
                    try:
                        if sys.version_info < (3,8) and self.autoawait:
                            if _should_be_async(cell):
                                # the code AST below will not be user code: we wrap it
                                # in an `async def`. This will likely make some AST
                                # transformer below miss some transform opporunity and
                                # introduce a small coupling to run_code (in which we
                                # bake some assumptions of what _ast_asyncify returns.
                                # they are ways around (like grafting part                                                                                                                           of the ast
                                # later:
                                #    - Here, return code_ast.body[0].body[1:-1], as well
                                #    as last expression in  return statement which is
                                #    the user code part.
                                #    - Let it go through the AST transformers, and graft
                                #    - it back after the AST transform
                                # But that seem unreasonable, at least while we
                                # do not need it.
                                code_ast = _ast_asyncify(cell, 'async-def-wrapper')
                                _run_async = True
                            else:
                                code_ast = compiler.ast_parse(cell, filename=cell_name)
                        else:
                            code_ast = compiler.ast_parse(cell, filename=cell_name)
                    except self.custom_exceptions as e:
                        etype, value, tb = sys.exc_info()
                        self.CustomTB(etype, value, tb)
                        return error_before_exec(e)
                    except IndentationError as e:
                        return error_before_exec(e)
                    except (OverflowError, SyntaxError, ValueError, TypeError,
                            MemoryError) as e:
                        self.showsyntaxerror()
                        return error_before_exec(e)

                    # Apply AST transformations
                    try:
                        code_ast = self.transform_ast(code_ast)
                    except InputRejected as e:
                        self.showtraceback()
                        return error_before_exec(e)

                    # Give the displayhook a reference to our ExecutionResult so it
                    # can fill in the output value.
                    self.displayhook.exec_result = result

                    # Execute the user code
                    interactivity = "none" if silent else self.ast_node_interactivity
                    if _run_async:
                        interactivity = 'async'

                has_raised =  await self.run_ast_nodes(code_ast.body, cell_name,
                       interactivity=interactivity, compiler=compiler, result=result)
        if plcell:
            cell = plcell
            if cell.isspace():
                return result

            self.engine = engine

                # Execute the user code
            interactivity = "none" if silent else 'all'
            if _run_async:
                interactivity = 'async'
            has_raised = await engine.jupyter_cell(result,cell,raw_cell,self,store_history=store_history)

        self.last_execution_succeeded = not has_raised
        self.last_execution_result = result

        # Reset this so later displayed values do not modify the
        # ExecutionResult
        self.displayhook.exec_result = None

        if store_history:
    # Write output to the database. Does nothing unless
            # history output logging is enabled.
            self.history_manager.store_output(self.execution_count)
            # Each cell is a *single* input, rgeardless of how many lines it has
            self.execution_count += 1

        return result

    def transform_cell(self, cell: str) -> str:

        """Transforms a cell of input code"""
        if cell.startswith("%%"):
            return super(Jupyter4YAP, self).transform_cell(cell)
        if cell.startswith("%"):
            (line,_,rcell) = cell.partition("\n")
            return super(Jupyter4YAP, self).transform_cell( line+"\n")+"\n"+rcell
        return cell

    def check_complete(self, cell):
        self._is_complete = True
        return 'complete', ''

    
    def complete(self, text, line=None, cursor_pos=None):
        """Compute matches when text is a simple name.

        Return a list o          f all keywords, built-in functions and names currently
        defined in self.namespace or self.global_namespace that match.

        """
        try:
            self.matches = []
            engine.mgoal(completions(text, line, cursor_pos, self),"completer",True)
            with self.builtin_trap:
                _,ipy = self.Completer.complete(text, line, cursor_pos)
            l = len(text)
            return text,[i[l:] for i in self.matches+ipy]
        except:
            return text,[]



class Jupyter4YAPABC(metaclass=abc.ABCMeta):
    """An abstract base class for Jupyter4YAP."""

Jupyter4YAPABC.register(Jupyter4YAP)
