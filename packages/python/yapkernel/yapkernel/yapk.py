
import json
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
            self.run(set_prolog_flag("verbose_load",False))
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


#class InteractiveShell(SingletonConfigurable):
class YAPRun(InteractiveShell):
    """An enhanced, interactive shell for YAP."""

    def init(self, shell):
        #super(InteractiveShell,self).__init__()
        self.shell = shell
        self.engine = engine
        self.d = []
        engine.shell = shell
        self.errors = []
        self.warnings = []
        self.q = None
        self.os = None
        self.it = None
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
        self.engine.mgoal(errors(text,self),"verify",True)
            

    async def prolog_call(self, result, ccell):
        """
        Reconsult a Prolog program  and execute/reexecute a Prolog query. It receives as input:
        - self, the Python shell:
            self.q contains the Prolog query, incluindo the current answers (self.q.answers) and the last taken execution port 
        - result, that stores execution results;
        - ccell, that contains the program (or not), a query (or not), and the number of solutions to return,
        """ 
        if not ccell:
            return result
        (program,squery,_,iterations) = ccell
        howmany = iterations
        self.iterations = 0
        try:
            for v in self.q:
                if self.port == "fail":
                    self.q.close()
                    self.q = None
                    self.os = None
                    result.result = self.answers
                    return result
                if self.port == "exit":
                    self.answers += [self.answer]
                    self.q.close()
                    self.q = None
                    self.os = None
                    self.iterations += 1
                    result.result = self.answers
                    return result
                elif self.q.port == "answer":

                    self.answers += [self.answer]
                    self.os = (program,squery)
                    self.iterations += 1
                if howmany == self.iterations:
                    result.result = self.answers
                    return result
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in squery '+ str(self.q)+
                             '\n  Answers'+ json.dumps( self.answers)+ '\n')
            result.error_in_exec=e
            return  result

        try:
            if self.iterations:
                result.result = self.answers
            return result
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in query '+ str(self.q)+
                             '\n  Answers'+ json.dumps( self.answers)+ '\n')
            result.error_in_exec=e
            return  result


    async def prolog(self, result, cell, ccell):
        #
        # Actually execute, or restart, a Prolog query.
        (program,query,_,iterations) = ccell
        try:
            # sys.settrace(tracefunc)
            if self.q != None and self.os == (program,query):
                result = await self.prolog_call(result, ccell)
                return result
            if program and not program.isspace():
                pc = jupyter_consult(program+"\n")
                self.engine.mgoal(pc,"jupyter",True)
            if not query.isspace():
                self.answers = []
                self.iterations = 0
                self.engine.reSet()
                pg = jupyter_query(query,self)
                print(self)
                self.q = Query(engine,pg)
                self.port = "call"
                self.answer = None
                result = await self.prolog_call(result, ccell)
            else:
                result = await self.prolog_call(result, None)
        except Exception as e:
            print(e)
            result = await self.prolog_call(result, None)
        return result


    async def run_cell_async(
        self,
        raw_cell: str,
        store_history=False,
        silent=False,
        shell_futures=True,
        *,
        transformed_cell: Optional[str] = None,
        preprocessing_exc_tuple: Optional[Any] = None
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

        def error_before_exec(value):
            if store_history:
                self.execution_count += 1
            result.error_before_exec = value
            self.last_execution_succeeded = False
            self.last_execution_result = result
            return result

        self.events.trigger('pre_execute')
        if not silent:
            self.events.trigger('pre_run_cell', info)

        if transformed_cell is None:
            warnings.warn(
                "`run_cell_async` will not call `transform_cell`"
                " automatically in the future. Please pass the result to"
                " `transformed_cell` argument and any exception that happen"
                " during the"
                "transform in `preprocessing_exc_tuple` in"
                " IPython 7.17 and above.",
                DeprecationWarning,
                stacklevel=2,
            )
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
        if raw_cell.find("#!python") == 0 or raw_cell.startswith("%"):
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

                has_raised = await self.run_ast_nodes(code_ast.body, cell_name,
                       interactivity=interactivity, compiler=compiler, result=result)
        else:

            if raw_cell.isspace():
                return result

            ccell = self.split_cell(raw_cell)

            if self.q and self.os and ccell and (ccell[0], ccell[1]) == (self.os[0],self.os[1]):
                return await self.prolog(result, raw_cell, ccell)
            self.errors=[]
            self.warnings = []
            self.os = None

            try:
                self.syntaxErrors( ccell[0])
            except Exception as e:
                return error_before_exec(e)
            errors = self.errors
            for i in errors:
                try:
                    file = i["parserFile"]
                except:
                    file ="scratch"
                try:
                    text = i["parserTextA"]
                except:
                    text ="scratch"
                try:
                    e = SyntaxError(i["label"],(file,i["parserLine"],i["parserPos"],text))
                    print(e)
                    raise  e
                except (OverflowError, SyntaxError, ValueError, TypeError,
                    MemoryError) as e:
                    self.showsyntaxerror()
                    return error_before_exec(e)

                except IndentationError as e:
                    pass
                except self.custom_exceptions as e:
                    etype, value, tb = sys.exc_info()
                    self.CustomTB(etype, value, tb)

                    return error_before_exec(e)
            for w in self.warnings:
                # # Compile to bytecode
                e =  SyntaxWarning
                warnings.warn(e, source=w["parserTextA"])
                # Give the displayhook a reference to our ExecutionResult so it
                # can fill in the output value.
                self.displayhook.exec_result = result



                # Execute the user code
            interactivity = "none" if silent else 'all'
            if _run_async:
                interactivity = 'async'
            has_raised = await self.prolog(result,raw_cell, ccell)

            self.last_execution_succeeded = not has_raised
            self.last_execution_result = result

        # Reset this so later displayed values do not modify the
        # ExecutionResult
        self.displayhook.exec_result = None

        if store_history:
    # Write output to the database. Does nothing unless
            # history output logging is enabled.
            self.history_manager.store_output(self.execution_count)
            # Each cell is a *single* input, regardless of how many lines it has
            self.execution_count += 1

        return result

    def split_cell(self,s):
        """
        Trasform a text into program+query. A query is the
        last line if the last line is non-empty and does not terminate
        on a dot. You can also finish with

            - `*`: you request all solutions
            - ';'[N]: you want an answer; optionally you want N answers

            NOT IMPLEMENTED YET: If the line terminates on a

ent.
        """
        if len(s) < 2:
            return  '','',False,0
        if len(s) > 2 and s[0] == '%' and s[1] == '%':
            return '','',False,0
        else:
            program = ''
            while s[0] == '%':
                line = s.find('\n')
                if line < 0:
                    break
                program += s[:line+1]
                s = s[line+1:]
        sp = (s+' ').rfind('. ')
        tb = s.rfind('.\t')
        nl = s.rfind('.\n')
        k = max(sp,tb,nl)
        n = len(s)
        if k>0:
            program+=s[:k+2]
            query=s[k+2:]
            cmpp   = program.strip(' \n\j\t')
            if len(cmpp)==0:
                query=s[k+2:]
        else:
            query=s
        qp = query.strip(' \n\j\t')
        if len(qp)==0:
            return program,'',False,0
        if qp[-1] == '*':
            return program, qp[:-1],True,1000000
        i = len(qp)  -1
        n=0
        d = 1
        while qp[i].isdecimal(  ):
            n=n+d*int(qp[i])
            d *= 10
            i -= 1
        if n >0 and i >=0  and qp[i] == ';':
            return program, qp[:i-1],False,n
        return program, query,False,1


    def transform_cell(self, cell: str) -> str:

        """Transforms a cell of input code"""
        if cell.startswith("%") or cell.startswith("#!python"):
            return TransformerManager.python_transform_cell(self,cell)
        return cell



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
            text, pymatches = self.shell.IPyCompleter.complete(text, line, cursor_pos)
            print(  self.matches , (text,pymatches) , file=sys.stderr)
            matches = self.matches + pymatches
            return text,matches
        except:
            return text,[]
