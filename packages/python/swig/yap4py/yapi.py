
import os.path
import sys
import keyword
# debugging support.
# import pdb
from collections import namedtuple

from .yap import *


class Engine( YAPEngine ):
    def __init__(self, args=None,**kwargs):
        # type: (object) -> object
        self.contained = False
        if not args:
            args = EngineArgs(**kwargs)
        if self.contained:
            yap_lib_path = os.path.dirname(__file__)
            args.setYapShareDir(os.path.join(yap_lib_path,"prolog"))
            args.setYapLibDir(yap_lib_path)
            args.setSavedState(os.path.join(yap_lib_path,"startup.yss"))
        YAPEngine.__init__(self,args)
        self.goal( set_prolog_flag('verbose', 'silent' ) )
        self.goal( use_module(library('yapi') ) )
        self.goal( set_prolog_flag('verbose', 'normal' ) )


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

class Predicate:
    """Goal is a predicate instantiated under a specific environment """
    def __init__( self, name, args, module=None, engine = None):
        self = namedtuple( name, args )
        if module:
            self.p = YAPPredicate( name, len(self), module )
        else:
            self.p = YAPPredicate( name, len(self) )
        self.e = engine

    def goals( self, engine):
        self.e = engine

    def __iter__(self):
        return PrologTableIter(self.e, self.p)

    def holds(self):
        return self.e.run(self._make_())

class PrologTableIter:

    def __init__(self, e, goal):
        try:
            self.e = e
            self.q = e.YAPQuery(goal)
        except:
            print('Error')

    def __iter__(self):
        # Iterators are iterables too.
        # Adding this functions to make them so.
        return self

    def next(self):
        if self.q.next():
            return goal
        else:
            self.q.close()
            self.q = None
            raise StopIteration()

f2p = {"fails":{}}
for i in range(16):
    f2p[i] ={}




global engine, handler

yap_lib_path = os.path.dirname(__file__)

use_module = namedtuple('use_module', 'file')
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


    def answer(self, q):
        try:
            self.bindings = {}
            v = q.next()
            if v:
                print( self.bindings )
            return v
        except Exception as e:
            print(e.args[1])
            return False

    def query_prolog(self, engine, s):
        # import pdb; pdb.set_trace()
        #
        # construct a query from a one-line string
        # q is opaque to Python
        #
        q = engine.query(python_query(self, s))
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
        self.port = "call"
        # launch the query
        while self.answer(q):
            if self.port == "exit":
                # done
                q.close()
                return True, True
            if do_ask:
                s = input("more(;),  all(*), no(\\n), python(#) ?").lstrip()
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
        print("No (more) answers")
        q.close()
        return

    def live(self, engine, **kwargs):
        loop = True
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
