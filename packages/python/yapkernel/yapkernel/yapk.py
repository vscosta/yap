import abc
import json
import pprint

from traitlets.config.configurable import SingletonConfigurable
from traitlets.utils.importstring import import_item
from IPython.core import oinspect
from traitlets import ( Any )
from yap4py.systuples import *
from yap4py.yapi import *
import IPython.core.getipython
from IPython.core.interactiveshell import InteractiveShell, ExecutionInfo, ExecutionResult
from IPython.core.inputtransformer2 import TransformerManager
from typing import  Optional
from IPython.core.display import DisplayObject, display
from IPython.core.async_helpers import (_asyncio_runner,  _asyncify, _pseudo_sync_runner)
from IPython.core.async_helpers import _curio_runner, _trio_runner, _should_be_async

class JupyterEngine( Engine ):

    def __init__(self, args=None,self_contained=False,**kwargs):
        # type: (object) -> object
        if not args:
            args = EngineArgs(**kwargs)
        args.jupyter = True
        Engine.__init__(self, args)
        self.errors = []
        self.warnings = []
        self.shell = None
        try:
            set_prolog_flag("verbose_load",False)
            self.run(compile(library('jupyter')),m="user",release=True)
            self.run(compile(library('completer')),m="user",release=True)
            self.run(compile(library('verify')),m="user",release=True)
            self.run(set_prolog_flag("verbose_load",True))
        except Exception as e:
            print( e )


engine =  JupyterEngine()

def get_ipython():
    return engine.shell

IPython.core.getipython.get_ipython = get_ipython

def get_engine():
    return engine

def showtraceback(self, exc_info):
    try:
        (etype, value, tb) = e
        traceback.print_exception(etype, value, tb)
    except:
        print(e)
        pass

class Jupyter4YAP:
    """An enhanced, interactive shell for YAP."""

    def syntaxErrors(self, text):
        """
        Returns an exception if the query has syntax errors
        """
        self.errors=[]
        self.warnings=[]
        if text and text.isspace():
            return False
        text = text.rstrip()
        while len(text) > 0 and text[-1] == '\n':
            text = text[:-1].rstrip()
            if text[-1] == '*':
                text = text[:-1]
            elif text[-1].isdigit():
                t = text
                while len(t) > 0 and  t[-1].isdigit():
                    t = t[:-1]
                t = t.rstrip()
                if t[-1] == '?':
                    text = t[:-1].rstrip()
        engine.mgoal(errors(text,self),"verify",True)
        return self.errors



    def syntax_error_handler(self,errors,cell):
        for i in self.syntaxErrors(cell):
            try:
                file = i["parserFile"]
            except:
                file ="scratch"
            try:
                text = i["parserTextA"]
            except:
                text ="scratch"
            e= SyntaxError(i["label"],(file,i["parserLine"],i["parserPos"],text))
            try:
                self.handle_syntax_errors(e , text)
            except x as Exception:
                self.showtraceback(x)


    def prolog_call(self, result, cell, all=False):
        """
        Reconsult a Prolog program  and execute/reexecute a Prolog query. It receives as input:
        - self, the Python shell:
            engine.q contains the Prolog query, incluindo the current answers (self.answers) and the last tahg
        - result, that stores execution results;
        - ccell, that contains the program (or not), a query (or not), and the number of solutions to return,
        """        
        if not cell:
            return result
        engine.iterations = 0
        try:
            engine.q.answer = Answer()
            for v in engine.q:
                answer = v.answer
                #print(answer, answer.port)
                
                if answer.port == "fail":
                    engine.close()
                    engine.q = []
                    engine.os = []
                    result.result = engine.answers
                    #print( engine.q.port+": "+str(engine.answer) )
                    return result
                elif answer.port == "exit":
                    #engine.answers += [answer[1]]
                    engine.q.close()
                    engine.q = []
                    engine.os = []
                    engine.iterations += 1
                    result.result = engine.answers
                    #print( engine.q.port +": "+str(engine.answer) )
                    return result
                elif answer.port == "!":
                    engine.q.close()
                    engine.q = []
                    engine.os = []
                    result.result = engine.answers
                    #print( engine.q.port+": "+str(engine.answer) )
                    return result
                elif answer.port == "answer":
                    # print( engine.answer )
                    #engine.answers += [answer[1]]
                    engine.iterations += 1
                if not all:
                     result.result = engine.answers
                     return result
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in squery '+ str(engine.q)+'\n')
            result.error_in_exec=e
            return  result

        try:
            if engine.answers:
                result.result = engine.answers
            return result
        except Exception as e:
            #sys.stderr.write('Exception '+str(e)+' in query '+ str(engine.q)+
            #                 '\n  Answers'+ json.dumps( engine.answers)+ '\n')
            result.error_in_exec=e
            return  result



    async def prolog(self, result, cell, all=False, store_history=False):
        try:
            # sys.settrace(tracefunc)
            ccell = cell.strip()
            posnl = ccell.find('\n')
            if ccell  and ccell[-1] != '.':
                query = cell
                if engine.q != [] and engine.os and engine.os == query:
                    answer.port = "retry"
                    result =  self.prolog_call(result, query, all = True)
                    return result
                elif not query.isspace():
                    if ccell[-1] == '*':
                        query = ccell[:-1].strip()
                        all = True
                    engine.iterations = 0
                    engine.reSet()
                    pg = python_query(self, query)
                    engine.q = Query(engine,pg)
                    engine.answers = []
                    self.displayhook.exec_result = result
                    result = self.prolog_call(result, query)
                return False
            elif cell and not cell.isspace():
                try:
                    errors =  self.syntaxErrors( cell )
                    if errors:
                        self.display(errors, text)
                        return error_before_exec(e)
                    self.displayhook.exec_result = result
                    answer =  ["call",[]]
                    pc = jupyter_consult(cell, answer)
                    engine.mgoal(pc,"user",True)
                    return False
                except Exception as e:
                    self.showtraceback(e)
                    return True
        except Exception as e:
            self.showtraceback(e)
            return True
        #pp = pprint.PrettyPrinter(indent=4)
        #sys.stdout.write(engine.q.port+': ')
        #pp.pprint(result.result)

                    

    async def run_cell_async(
        self,
        raw_cell: str,
        store_history=False,
        silent=False,
        shell_futures=True,
        *,
        transformed_cell: Optional[str] = [],
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
            raw_cell, store_history, silent, shell_futures)
        result = ExecutionResult(info)

        if (not raw_cell) or raw_cell.isspace():
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
            # If any of our input transformation (input_transformer_manager or
            # prefilter_manager) raises an exception, we store it in this variable
            # so that we can display the error after logging the input and storing
            # it in the history.
            try:
                cell = self.magics(raw_cell)
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
            self.showtraceback(preprocessing_exc_tuple)
            if store_history:
                self.execution_count += 1
            return error_before_exec(preprocessing_exc_tuple[1])

        # Our own compiler remembers the __future__ environment. If we want to
        # run code with a separate __future__ environment, use the default
        # compiler
        #compiler = self.compile if shell_futures else self.compiler_class()
        has_raised = False
        if raw_cell.find("#!python") == 0 or raw_cell.startswith("%%"):
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
                                # transformer below miss some transform opportunity and
                                # introduce a small coupling to run_code (in which we
                                # bake some assumptions of what _ast_asyncify returns.
                                # they are ways around (like grafting part of the ast
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
        else:
            if raw_cell.isspace():
                return result



                # Execute the user code
            interactivity = "none" if silent else 'all'
            if _run_async:
                interactivity = 'async'
            has_raised = await self.prolog(result,raw_cell,store_history=store_history)

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
        if cell.startswith( "%%"):
            return self.ipy_transform_cell(str)
        if cell.startswith( "%"):
            return self.ipy_transform_cell(str.split()[0])+str.split()[2]
        if cell.startswith( "#!python"):
            return str.split()[0]+"\n"+self.ipy_transform_cell(str.split()[2])
        return cell

    def __init__(self, shell):
        # This is where tra its with a config_key argument are updated
        # from the values on config.
        engine.q = []
        engine.errors = []
        engine.warnings = []
        engine.os = []        
        engine.bindings = dicts = []
        engine.iterations = 0 
        try:
            engine.reSet()
            shell.input_splitter.ipy_check_complete = IPyCompleter.check_complete
            #shell.shell.input_splitter.check_complete = YAPCompleter.check_complete
            #InteractiveShell.IPyCompleter = shell.shell.Completer
            #shell.shell.Completer = YAPCompleter(shell.shell)
            #InteractiveShell.YAPCompleter = shell.shell.Completer
        except:
            print("******************************",   file=sys.stderr)
        



class YAPCompleter():

    def __init__(self, shell):
        self.engine = engine
        self.shell = shell

    def check_complete(self, cell):
        self._is_complete = True
        return 'complete', ''

    def complete(self, text, line=None, cursor_pos=None):
        """Compute matches when text is a simple name.

        Return a list of all keywords, built-in functions and names currently
        defined in self.namespace or self.global_namespace that match.

        """
        try:
            self.matches = []
            print("Input",text, line, cursor_pos, self, file=sys.stderr)
            engine.mgoal(completions(text, line, cursor_pos, self),"completer",True)
            text, pymatches = self.IPyCompleter.complete(text, line, cursor_pos)
            print(  self.matches , (text,pymatches) , file=sys.stderr)
            matches = self.matches + pymatches
            return text,matches
        except:
            return text,[]


        
