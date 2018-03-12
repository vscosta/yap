
import os.path
import sys
import keyword
# debugging support.
# import pdb
from collections import namedtuple
import readline
from .yap import *


class Engine( YAPEngine ):

    def __init__(self, args=None,self_contained=False,**kwargs):
        # type: (object) -> object
        if not args:
            args = EngineArgs(**kwargs)
        if self_contained:
            yap_lib_path = os.path.dirname(__file__)
            args.setYapShareDir(os.path.join(yap_lib_path, "prolog"))
            args.setYapPLDIR(yap_lib_path)
            args.setSavedState(os.path.join(yap_lib_path, "startup.yss"))
        YAPEngine.__init__(self, args)
        self.goal(set_prolog_flag('verbose', 'silent'))
        self.goal(compile(library('yapi')))
        self.goal(set_prolog_flag('verbose', 'normal'))

    def run(self, g, m=None):
        if m:
            self.mgoal(g, m)
        else:
            self.goal(g)

    def f(self, g):
        self.E.fun(g)


class EngineArgs( YAPEngineArgs ):
    """ Interface to Engine Options class"""
    def __init__(self, args=None,**kwargs):
        super().__init__()


class Predicate( YAPPredicate ):
    """ Interface to Generic Predicate"""

    def __init__(self, t, module=None):
        super().__init__(t)

class IQuery(YAPQuery):
    """Goal is a predicate instantiated under a specific environment """
    def __init__(self, engine, g):
        self = engine.query(g)
        self.port = "call"
        self.bindings = None

    def __iter__(self):
        return PrologTableIter( self )

class PrologTableIter:

    def __init__(self, q):
        try:
            self.q = q
        except:
            print('Error')

    def __iter__(self):
        # Iterators are iterables too.
        # -        # Adding this functions to make them so.
        return self

    def __next__(self):
        if not self.q:
            raise StopIteration()
        if self.q.next():
            rc = self.q.bindings
            if self.q.port == "exit":
                self.q.close()
            return rc
        else:
            if self.q:
                self.close()
            raise StopIteration()

    def close(self):
        self.q.close()
        self.q = None

f2p = {"fails":{}}
for i in range(16):
    f2p[i] ={}




global engine, handler

yap_lib_path = os.path.dirname(__file__)

compile = namedtuple('compile', 'file')
bindvars = namedtuple('bindvars', 'list')
library = namedtuple('library', 'list')
v = namedtuple( 'v', 'slot')
yap_query = namedtuple( 'yap_query', 'query owner')
jupyter_query = namedtuple( 'jupyter_query', 'vars dict')
python_query = namedtuple( 'python_query', 'vars dict')
yapi_query = namedtuple( 'yapi_query', 'vars dict')
show_answer = namedtuple( 'show_answer', 'vars dict')
set_prolog_flag = namedtuple('set_prolog_flag', 'flag new_value')


def named( name, arity):
    try:
        if  arity > 0 and name.isidentifier() and not keyword.iskeyword(name):
            s = []
            for i in range(arity):
                s += ["A" + str(i)]
            f2p[arity][name] = namedtuple(name, s)
    except:
        f2p[fails][name] = True

class PrologPredicate( YAPPrologPredicate ):
    """ Interface to Prolog  Predicate"""

class v(YAPVarTerm):
    def __init__(self):
        super().__init__()

    def binding(self):
        return self.term()

def numbervars(  q ):
    Dict = {}
    if True:
        engine.goal(show_answer( q.namedVars(), Dict))
        return Dict
    rc = q.namedVarsVector()
    q.r = q.goal().numbervars()
    o = []
    for i  in rc:
        if len(i) == 2:
            do = str(i[0]) + " = " + str( i[1] ) + "\n"
            o += do
        else:
            do = str(i[0]) + " = " + str( i[1] ) + "\n"
            o += do
    return o

class YAPShell:




    def query_prolog(self, engine, query):
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
        do_ask = True
        self.e = engine
        bindings = []
        g = python_query(self, query)
        if not self.q:
            self.it = IQuery( engine, g )
        for bind in self.it:
            bindings += [bind]
            if do_ask:
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
                do_ask = False
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
                    self.query_prolog(engine, s)
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
       self.live(engine)



def main():
    engine = Engine()
    handler = numbervars
    YAPShell(engine)

if __name__ == "__main__":
    main()
