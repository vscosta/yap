
import abc
import json
import math
from traitlets.config.configurable import SingletonConfigurable
from traitlets.utils.importstring import import_item
from traitlets import (
Integer, Bool, CaselessStrEnum, Enum, List, Dict, Unicode, Instance, Type,
observe, default, validate, Any
)
from yap4py.systuples import *
from yap4py.yapi import *
from IPython.core.completer import Completer
from IPython.core.interactiveshell import InteractiveShell, ExecutionInfo, ExecutionResult
from typing import List as ListType, Tuple, Optional

class JupyterEngine( Engine ):

    def __init__(self, args=None,self_contained=False,**kwargs):
        # type: (object) -> object
        if not args:
            args = EngineArgs(**kwargs)
        args.jupyter = True
        Engine.__init__(self, args)
        self.errors = []
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
<<<<<<< HEAD
            self.engine.mgoal(errors(text,self),"user",True)
            print(self.errors)
=======
            self.engine.mgoal(errors(text,self),"verify",True)
>>>>>>> 495eba1bd79ee73f40cb4862d2d00ad1b67a6a26
            return self.errors
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in query\n')

    def prolog_call(self,howmany, ccell, result):

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
            return  result

        try:
            if self.iterations:
                result.result = self.answers
            else:
                result.result = []
                print("No\n")
            return result
        except Exception as e:
            sys.stderr.write('Exception '+str(e)+' in query '+ str(self.q)+
                             '\n  Answers'+ json.dumps( self.answers)+ '\n')
            result.result = None
            return  result



    def prolog(self, ccell, result):
        #
        # construct a self.query from a one-line str
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
                return self.prolog_call(iterations, ccell, result)
            else:
                result.result = []
                return result
        except Exception as e:
            print(e)
            return None
            return None

    def run_cell(self, raw_cell:str, store_history:bool, silent:bool) -> ExecutionResult:
        """Internal method to run a complete IPython cell."""
        # we need to avoid calling self.transform_cell multiple time on the same thing
        # so we need to store some results:
        preprocessing_exc_tuple = None
        try:
            transformed_cell = self.transform_cell(raw_cell)
        except Exception:
            transformed_cell = raw_cell
            preprocessing_exc_tuple = sys.exc_info()

# run the new command using the given tracer
        coro = self.yrun_cell(
            raw_cell,
            store_history=store_history,
            silent=silent,
            shell_futures=True,
            transformed_cell=transformed_cell,
            preprocessing_exc_tuple=preprocessing_exc_tuple,
        )

        return coro

    def error_before_exec(self,value):
        if store_history:
            self.execution_count += 1
        self.result.error_before_exec = value
        self.last_execution_succeeded = False
        self.last_execution_result = result
        return self.result

    def yrun_cell(
        self,
        raw_cell: str,
        store_history=True,
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

        #import pdb; pdb.set_trace  ()
        if (not raw_cell) or raw_cell.isspace():
            self.last_execution_succeeded = True
            self.last_execution_result = result
            return result

        if silent:
            store_history = False

        if store_history:
            result.execution_count = self.execution_count


        ccell=self.transform_cell(raw_cell)
        (program,squery,_,iterations) = ccell
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

        # Store raw and processed history
        if store_history:
             self.history_manager.store_inputs(self.execution_count,
                                              raw_cell, raw_cell)
        if not silent:
            self.logger.log(cell, raw_cell)

        # Display the exception if input processing failed.
        # if preprocessing_exc_tuple is not None:
        #     self.showtraceback(preprocessing_exc_tuple)
        #     if store_history:
        #         self.execution_count += 1
        #     return error_before_exec(preprocessing_exc_tuple[1])

        # Our own compiler remembers the __future__ environment. If we want to
        # run code with a separate __future__ environment, use the default
        # compiler
        #compiler = self.compile if shell_futures else self.compiler_class()

        if not program.isspace():
<<<<<<< HEAD
            errors = self.syntaxErrors( program, self)
            print(errors)
            for i in errors:
                # # Compile to bytecode
                try:
                    print(i)
                    e =  SyntaxError(i["culprit"],lineno=i["parserLine"]+1,offset=i["parserCount"],text=i["ParserTextA"])
                    print(e)
=======
            errors = self.syntaxErrors( program)
            for i in errors:
                # # Compile to bytecode
                try:
                    e =  SyntaxError("",(i["parserFile"],i["parserLine"],i["parserPos"],i["parserTextA"]))
>>>>>>> 495eba1bd79ee73f40cb4862d2d00ad1b67a6a26
                    raise e
                #     if sys.version_info < (3,8) and self.autoawait:
                #         if _should_be_async(cell):
                #             # the code AST below will not be user code: we wrap it
                #             # in an `async def`. This will likely make somez AST
                #             # transformer below miss some transform opportunity and
                #             # introduce a small coupling to run_code (in which we
                #             # bake some assumptions of what _ast_asyncify returns.
                #             # they are ways around (like grafting part of the ast
                #             # later:
                #             #    - Here, return code_ast.body[0].body[1:-1], as well
                #             #    as last expression in  return statement which is
                #             #    the user code part.
                #             #    - Let it go through the AST transformers, and graft
                #             #    - it back after the AST transform
                #             # But that seem unreasonable, at least while we
                #             # do not need it.
                #             code_ast = _ast_asyncify(cell, 'async-def-wrapper')
                #             _run_async = True
                #         else:
                #     else:
                #         code_ast = compiler.ast_parse(cell, filename=cell_name)
                except self.custom_exceptions as e:

                    etype, value, tb = sys.exc_info()
                    self.CustomTB(etype, value, tb)
                    return error_before_exec(e)
                except (OverflowError, SyntaxError, ValueError, TypeError,
                        MemoryError) as e:
                    self.showsyntaxerror()
                    return error_before_exec(e)


        if self.q and self.os == (program,squery):
            return self.prolog_call(iterations, ccell, result)
        # new cell
        self.os = None
        _run_async = False

        #
        # Give the displayhook a reference to our ExecutionResult so it
        # can fill in the output value.
        self.displayhook.exec_result = result

        # Execute the user code
        interactivity = "none" if silent else 'all'
        if _run_async:
            interactivity = 'async'

        has_raised =  self.prolog(ccell ,result)

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


    def transform_cell(self,s):
        """
        Trasform a text into program+query. A query is the
        last line if the last line is non-empty and does not terminate
        on a dot. You can also finish with

            - `*`: you request all solutions
            - ';'[N]: you want an answer; optionally you want N answers

            NOT IMPLEMENTED YET: If the line terminates on a

ent.
        """

        sp = (s+' ').rfind('. ')
        tb = s.rfind('.\t')
        nl = s.rfind('.\n')
        k = max(sp,tb,nl)
        n = len(s)
        if k>0:
            program=s[:k+1]
            query=s[k+2:]
            cmpp   = program.strip(' \n\j\t')
            if len(cmpp)==0:
                program = ''
                query=s[k+2:]
        else:
            program = ''
            query=s
        qp = query.strip(' \n\j\t')
        if len(qp)==0:
            return program,'',False,0
        if qp[-1] == '*':
            return program, qp[:-1]+'.\n',True,1000000
        i = -1
        n=0
        d = 1
        while qp[i].isdecimal(  ):
            n=n+d*int(qp[i])
            d *= 10
            i -= 1
        if n-i >0 and qp[i] == ';':
            return program, qp[:i]+'.\n',False,n
        return program, qp+'.\n',False,1

    def should_run_async(
            code,
            transformed_cell="",
            preprocessing_exc_tuple=None):
        return False

class YAPCompleter():

    def check_complete(self, cell):
        return 'complete',None

    def complete(self, text, line, cursor_pos):
        """Compute matches when text is a simple name.

        Return a list of all keywords, built-in functions and names currently
        defined in self.namespace or self.global_namespace that match.

        """
        self.shell.engine.mgoal(completions(line[:cursor_pos], self), "completer",True) #+ self.Completer.complete(text, line, cursor_pos)
        matches = []
        n = len(text)
        for word in self.matches:
            matches.append(word[cursor_pos:])
        return text,matches


