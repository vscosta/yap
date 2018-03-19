import readline
from yap4py.yap import *
from os.path import join, dirname
from collections import namedtuple
import sys

yap_lib_path = dirname(__file__)

compile = namedtuple('compile', 'file')
bindvars = namedtuple('bindvars', 'list')
library = namedtuple('library', 'list')
v0 = namedtuple('v', 'slot')
yap_query = namedtuple('yap_query', 'query owner')
jupyter_query = namedtuple('jupyter_query', 'vars dict')
python_query = namedtuple('python_query', 'vars dict')
yapi_query = namedtuple('yapi_query', 'vars dict')
show_answer = namedtuple('show_answer', 'vars dict')
set_prolog_flag = namedtuple('set_prolog_flag', 'flag new_value')


class Engine( YAPEngine ):

    def __init__(self, args=None,self_contained=False,**kwargs):
        # type: (object) -> object
        if not args:
            args = EngineArgs(**kwargs)
        if self_contained:
            yap_lib_path = dirname(__file__)
            args.setYapShareDir(join(yap_lib_path, "prolog"))
            args.setYapPLDIR(yap_lib_path)
            args.setSavedState(join(yap_lib_path, "startup.yss"))
        YAPEngine.__init__(self, args)
        self.goal(set_prolog_flag('verbose', 'silent'))
        self.goal(compile(library('yapi')))
        self.goal(set_prolog_flag('verbose', 'normal'))

    def run(self, g, m=None):
        if m:
            self.mgoal(g, m)
        else:
            self.goal(g)


class EngineArgs( YAPEngineArgs ):
    """ Interface to Engine Options class"""
    def __init__(self, args=None,**kwargs):
        super().__init__()


class Predicate( YAPPredicate ):
    """ Interface to Generic Predicate"""

    def __init__(self, t, module=None):
        super().__init__(t)

class Query:
    """Goal is a predicate instantiated under a specific environment """
    def __init__(self, engine, g):
        self.q = engine.query(g)
        if self.q:
            self.port = "call"
            self.bindings = None
            self.engine = engine
            self.answer = {}

    def __iter__(self):
        return self

    def __next__(self):
        print(self)
        if not self.q:
            raise StopIteration()
        if self.q.next():
            rc = self.answer
            if self.port == "exit":
                self.close()
            return rc
        else:
            if self:
                self.close()
            raise StopIteration()

    def close( self ):
        self.q.close()
        self.q = None


def name( name, arity):
    try:
        if  arity > 0 and name.isidentifier(): # and not keyword.iskeyword(name):
            s = []
            for i in range(arity):
                s += ["A" + str(i)]
            return namedtuple(name, s)
    except:
        return None

class PrologPredicate( YAPPrologPredicate ):
    """ Interface to Prolog  Predicate"""

class v(YAPVarTerm,v0):
    def __init__(self):
        YAPVarTerm.__init__()

    def binding(self):
        return self.term()


class YAPShell:

    def numbervars( self ):
        Dict = {}
        self.engine.goal(show_answer( self, Dict))
        return Dict
        # rc = self.q.namedVarsVector()
        # self.q.r = self.q.goal().numbervars()
        # o = []
        # for i  in rc:
        #     if len(i) == 2:
        #         do = str(i[0]) + " = " + str( i[1] ) + "\n"
        #         o += do
        #     else:
        #         do = str(i[0]) + " = " + str( i[1] ) + "\n"
        #         o += do
        # return o


    def query_prolog(self, query):
        #import pdb; pdb.set_trace()
        #
        # construct a query from a one-line string
        # q is opaque to Python
        #
        #q = engine.query(python_query(self, s))
        #
        #        # vs is the list of variables
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
        self.do_ask = True
        engine = self.engine
        bindings = []
        g = python_query(self, query)
        if not self.q:
            self.q = Query( engine, g )
        for bind in self.q:
            bindings += [bind]
            if self.do_ask:
                print(bindings)
                bindings = []
                s = input("more(;),  all(*), no(\\n), python(#) ?").lstrip()
            else:
                s = ";"
            if s.startswith(';') or s.startswith('y'):
                continue
            elif s.startswith('#'):
                try:
                    exec(s.lstrip('#'))
                except:
                    raise
            elif s.startswith('*') or s.startswith('a'):
                self.do_ask = False
                continue
            else:
                break
        if self.q:
            self.os = query
        if bindings:
            return True,bindings
        print("No (more) answers")
        return False, None


    def live(self, engine, **kwargs):
        loop = True
        self.q = None
        while loop:
            try:
                s = input("?- ")
                if not s:
                    loop = False
                else:
                    self.query_prolog(s)
            except SyntaxError as err:
                print("Syntax Error error: {0}".format(err))
            except EOFError:
                return
            except RuntimeError as err:
                print("YAP Execution Error: {0}".format(err))
            except ValueError:
                print("Could not convert data to an integer.")
            except:
                print("Unexpected error:", sys.exc_info()[0])
                raise
        engine.close()
    #
    # initialize engine
    # engine = yap.YAPEngine();
    # engine = yap.YAPEngine(yap.YAPParams());
    #
    def __init__(self, engine, **kwargs):
        self.engine = engine
        self.live(engine)



def main():
    engine = Engine()
    YAPShell(engine)

if __name__ == "__main__":
    main()
