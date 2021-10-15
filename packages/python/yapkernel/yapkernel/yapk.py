
import abc
import json
import math
import re
from traitlets.config.configurable import SingletonConfigurable
from traitlets.utils.importstring import import_item
from traitlets import (
Integer, Bool, CaselessStrEnum, Enum, List, Dict, Unicode, Instance, Type,
observe, default, validate, Any
)
from yap4py.systuples import *
from yap4py.yapi import *
from yapkernel.share import YAPShare
from IPython.core.completer import Completer
from IPython.core.interactiveshell import InteractiveShell, ExecutionInfo, ExecutionResult
from typing import List as ListType, Tuple, Optional
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
        try:
            self.run(set_prolog_flag("verbose_load",False))
            self.run(compile(library('jupyter')),m="user",release=True)
            self.run(compile(library('completer')),m="user",release=True)
            self.run(compile(library('verify')),m="user",release=True)
            self.run(set_prolog_flag("verbose_load",True))
        except Exception as e:
            print( e )


engine =  JupyterEngine()

#class InteractiveShell(SingletonConfigurable):
class YAPRun(InteractiveShell):
    """An enhanced, interactive shell for YAP."""

    def init(self, shell):
        self.shell = shell
        self.engine = engine
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
        """Return whether a legal query
        """
        try:
            if text and text.isspace():
                return []
            self.errors=[]
            self.warnings=[]
            self.engine.mgoal(errors(text,self),"verify",True)
            return (self.errors,self.warnings)
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in query\n')

    def prolog_call(self,howmany, ccell, result):
         # new cell
        if self.q and self.os == (ccell[0],ccell[1]):
            has_raised = self.prolog_call(ccell[3], ccell[1], result)
        else:
            self.os = None

        (program,squery,_,iterations) = ccell
        self.iterations = 0
        self.answers = []
        try:
            for v in self.q:
                if self.q.port == "fail":
                    self.q.close()
                    self.q = None
                    self.os = None
                    break
                self.answers += [self.q.answer]
                if self.q.port == "exit":
                    self.q.close()
                    self.q = None
                    self.os = None
                    break
                else:
                    self.os = (program,squery)
                self.iterations += 1
                if howmany == self.iterations:
                    break



        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in squery '+ str(self.q)+
                             '\n  Answers'+ json.dumps( self.answers)+ '\n')
            result.result = None
            return  True

        try:
            if self.iterations:
                result.result = self.answers
            else:
                result.result = []
                print("No\n")
            return False
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in query '+ str(self.q)+
                             '\n  Answers'+ json.dumps( self.answers)+ '\n')
            result.result = None
            return  True



    async def prolog(self, cell, result):
        #
        # construct a self.query from a one-line str
        ccell = self.split_cell(cell)
        (program,query,_,iterations) = ccell 
        try:
            sys.stdout.flush()
            sys.stderr.flush()
            # sys.settrace(tracefunc)
            #if not self.q or self.os != (program,squery):
            if program and not program.isspace():
                pc = jupyter_consult(program+"\n")
                self.engine.mgoal(pc,"jupyter",True)
            if not query.isspace():
                self.iterations = 0
                self.engine.reSet()
                pg = jupyter_query(query,self)
                self.q = Query(engine,pg)
                self.q.port = "call"
                self.q.answer = None
                exceptions = self.prolog_call(iterations, ccell, result)
                return exceptions
            else:
                result.result = []
                return False
        except Exception as e:
            print(e)
            return True

    def run_cell(self, raw_cell, store_history=False, silent=False, shell_futures=True):
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
        from IPython.core.inputtransformer2 import TransformerManager
        from IPython.core.completer import IPCompleter
        if raw_cell.find(":-python") ==0:
            self.complete = InteractiveShell.complete 
            self.input_transformer_manager.check_complete = TransformerManager.check_complete 
            try:
                result = InteractiveShell._run_cell(self,
                                                    raw_cell[raw_cell.find("\n"):], store_history, silent, shell_futures)
            finally:
                self.events.trigger('post_execute')
                if not silent:
                    self.events.trigger('post_run_cell', result)
            self.complete = YAPRun.complete 
            self.input_transformer_manager.check_complete = YAPRun.check_complete 
            return result

        try:
            result = YAPRun._run_cell(self,
                                      raw_cell, store_history, silent, shell_futures)
        finally:
            self.events.trigger('post_execute')
            if not silent:
                self.events.trigger('post_run_cell', result)
        return result

        
    def _run_cell(self, raw_cell:str, store_history:bool, silent:bool, shell_futures:bool) -> ExecutionResult:
        """Internal method to run a complete IPython cell."""

        # we need to avoid calling self.transform_cell multiple time on the same thing
        # so we need to store some results:
        preprocessing_exc_tuple = None
        try:
            transformed_cell = YAPRun.yap_transform_cell(self,raw_cell)
        except Exception:
            transformed_cell = raw_cell
            preprocessing_exc_tuple = sys.exc_info()

        assert transformed_cell is not None
        coro = YAPRun.run_cell_async(self,
            raw_cell,
            store_history=store_history,
            silent=silent,
            shell_futures=shell_futures,
            transformed_cell=transformed_cell,
            preprocessing_exc_tuple=preprocessing_exc_tuple,
        )

        # run_cell_async is async, but may not actually need an eventloop.
        # when this is the case, we want to run it using the pseudo_sync_runner
        # so that code can invoke eventloops (for example via the %run , and
        # `%paste` magic.
        if self.trio_runner:
            runner = self.trio_runner
        elif YAPRun.yap_should_run_async(
                self,
            raw_cell,
            transformed_cell=transformed_cell,
            preprocessing_exc_tuple=preprocessing_exc_tuple,
        ):
            runner = self.loop_runner
        else:
            runner = _pseudo_sync_runner

        try:
            return runner(coro)
        except BaseException as e:
            info = ExecutionInfo(raw_cell, store_history, silent, shell_futures)
            result = ExecutionResult(info)
            result.error_in_exec = e
            self.showtraceback(running_compiled_code=True)
            return result

    def yap_should_run_async(
        self, raw_cell: str, *, transformed_cell=None, preprocessing_exc_tuple=None
    ) -> bool:
        """Return whether a cell should be run asynchronously via a coroutine runner

        Parameters
        ----------
        raw_cell: str
            The code to be executed

        Returns
        -------
        result: bool
            Whether the code needs to be run with a coroutine runner or not

        .. versionadded: 7.0
        """
        if not self.autoawait:
            return False
        if preprocessing_exc_tuple is not None:
            return False
        assert preprocessing_exc_tuple is None
        if transformed_cell is None:
            warnings.warn(
                "`should_run_async` will not call `transform_cell`"
                " automatically in the future. Please pass the result to"
                " `transformed_cell` argument and any exception that happen"
                " during the"
                "transform in `preprocessing_exc_tuple` in"
                " IPython 7.17 and above.",
                DeprecationWarning,
                stacklevel=2,
            )
            try:
                cell = YAPRun.yap_transform_cell(self,raw_cell)
            except Exception:
                # any exception during transform will be raised
                # prior to execution
                return False
        else:
            cell = transformed_cell
        return False # _should_be_async(cell)

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
            except Exception:
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

        if not raw_cell.isspace():
            errors, warnings = self.syntaxErrors( raw_cell)
            for i in errors:
                # # Compile to bytecode
                try:
                    try:
                        file = i["parserFile"]
                    except:
                        file ="scratch"
                    try:
                        text = i["parserTextA"]
                    except:
                        text ="scratch"
                    e =  SyntaxError(i["label"],(file,i["parserLine"],i["parserPos"],text))
                    raise e
                    _run_async = True
                except self.custom_exceptions as e:

                    etype, value, tb = sys.exc_info()
                    self.CustomTB(etype, value, tb)
                    return self.error_before_exec(e)
                except (OverflowError, SyntaxError, ValueError, TypeError,
                        MemoryError) as e:
                    self.showsyntaxerror()
                    return self.error_before_exec(e)
                except self.custom_exceptions as e:


                    etype, value, tb = sys.exc_info()
                    self.CustomTB(etype, value, tb)
                    return self.error_before_exec(e)
            for w in warnings:
                # # Compile to bytecode
                e =  SyntaxWarning
                warnings.warn(e, source=w["parserTextA"])


        #
        # Give the displayhook a reference to our ExecutionResult so it
        # can fill in the output value.
        self.displayhook.exec_result = result

        # Execute the user code
        interactivity = "none" if silent else 'all'
        if _run_async:
            interactivity = 'async'
        has_raised = await self.prolog(cell ,result)
            
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

    def yap_transform_cell(self, cell: str) -> str:
        """Transforms a cell of input code"""
        if not cell.endswith('\n'):
            cell += '\n'  # Ensure the cell has a trailing newline
        lines = cell.splitlines(keepends=True)
        if lines[0].startswith('%%'):
            if not re.match(r'%%\w+\?', lines[0]):
                # This case will be handled by help_end
                
                magic_name, _, first_line = lines[0][2:].rstrip().partition(' ')
                body = ''.join(lines[1:])
                lines = self.run_cell_magic(magic_name, first_line, body)
                return ""
            elif lines[0].get(magic_name):
                line[0] = self.run_line_magic(magic_name,line[0][line[0].find(" "):])
            # Python specific
            #??token_transforms = [self.do_token_transforms[0],self.do_token_transforms[3]]
            # lines = self.do_token_transforms(lines)
        return ''.join(lines)

class YAPCompleter():

    def check_complete(self, cell):
        return 'complete',None

    def complete(self, text, line, cursor_pos):
        """Compute matches when text is a simple name.

        Return a list of all keywords, built-in functions and names currently
        defined in self.namespace or self.global_namespace that match.

        """
        self.shell.engine.mgoal(completions(line[:cursor_pos], self), "completer",True) + self.Completer.complete(text, line, cursor_pos)
        matches = []
        n = len(text)
        for word in self.matches:
            matches.append(word[cursor_pos:])
        return text,matches


