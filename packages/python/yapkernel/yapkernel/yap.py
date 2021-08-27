
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
            #if not self.q or self.os != (program,squery):
            if self.q:
                self.q.close()
                self.q = None
            self.answers = []
            result.result = []
            self.os = (program,squery)
            self.engine.reSet()
            pg = jupyter_cell(program,squery,self)
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
        #self.engine.mgoal(streams(True),"jupyter", True)
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

    #self.engine.mgoal(streams(False),"jupyter", True)
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
