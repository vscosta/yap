
import os.path
import sys
# debugging support.
# import pdb
from collections import namedtuple

from yap import *

class Engine( YAPEngine ):
    def __init__(self, args=None):
        # type: (object) -> object
        if not args:
            args = YAPEngineArgs()
        yap_lib_path = os.path.dirname(__file__)
        args.setYapShareDir(os.path.join(yap_lib_path,"prolog"))
        args.setYapLibDir(yap_lib_path)
        args.setSavedState(os.path.join(yap_lib_path,"startup.yss"))
        YAPEngine.__init__(self,args)
        self.goal( set_prolog_flag('verbose', 'silent' ) )
        self.goal( use_module(library('yapi') ) )

    def run(self, g, m=None):
        if m:
            self.mgoal(g, m)
        else:
            self.goal(g)

    def f(self, g):
        self.E.fun(g)


class EngineArgs( YAPEngineArgs ):
    """ Interface to Engine Options class"""

    
class Predicate( YAPPredicate ):
    """ Interface to Generic Predicate"""

    
class PrologPredicate( YAPPrologPredicate ):
    """ Interface to Prolog  Predicate"""

    
    
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

def v():
    return yap.YAPVarTerm()

def numbervars(  q ):
    Dict = {}
    if True:
        engine.goal(show_answer( q.namedVars(), Dict))
        return Dict
    rc = q.namedVarsVector()
    q.r = q.goal().numbervars()
    print( rc )
    o = []
    for i  in rc:
        if len(i) == 2:
            do = str(i[0]) + " = " + str( i[1] ) + "\n"
            o += do
            print(do)
        else:
            do = str(i[0]) + " = " + str( i[1] ) + "\n"
            o += do
            print(do)
    return o

def answer(q):
    try:
        v = q.next()
        if v:
            print( bindings )
        return v
    except Exception as e:
        print(e.args[1])
        return False

def query_prolog(engine, s):
    import pdb; pdb.set_trace()
    #
    # construct a query from a one-line string
    # q is opaque to Python
    bindings = {}
    q = engine.query(python_query(s, bindings))
    # vs is the list of variables
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
    ask = True
    # launch the query
    while answer(q):
        # deterministic = one solution
        if q.deterministic():
            # done
            q.close()
            return True, True
        if ask:
            s = input("more(;),  all(*), no(\\n), python(#) ?").lstrip()
            if s.startswith(';') or s.startswith('y'):
                continue
            elif s.startswith('#'):
                try:
                    exec(s.lstrip('#'))
                except:
                    raise
            elif s.startswith('*') or s.startswith('a'):
                ask = False
                continue
            else:
                break
    print("No (more) answers")
    q.close()
    return

def live(**kwargs):
    loop = True
    while loop:
        try:
            s = input("?- ")
            if not s:
                loop = False
            else:
                query_prolog(engine, s)
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
#

if __name__ == "__main__":
    engine = boot_yap()
    handler = numbervars
    live()
