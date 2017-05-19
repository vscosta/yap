
import yap
import os.path
import sys
# debugging support.
# import pdb
from collections import namedtuple

yap_lib_path = os.path.dirname(__file__)

use_module = namedtuple( 'use_module', 'file')
bindvars = namedtuple( 'bindvars', 'list')
library = namedtuple( 'library', 'list')
v = namedtuple( '_', 'slot')
yap_query = namedtuple( 'yap_query', 'query owner')


def numbervars(  engine, l ):
    rc = engine.fun(bindvars(l))
    o = []
    for i  in rc:
        if i[0] == "=":
            o = o + [i[1]]
        else:
            o = o +[i]
    return o

    def answer(q):
        try:
            return q.next()
        except Exception as e:
            print(e.args[1])
            return False

    #
    # construct a query from a one-line string
    # q is opaque to Python
    if g0:
        q = g0
    else:
        q = engine.run_query(s)
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
    # ask = True
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

def boot_yap(**kwargs):
    yap_lib_path = os.path.dirname(__file__)
    args = yap.YAPEngineArgs()
    args.setYapShareDir(os.path.join(yap_lib_path,"prolog"))
    args.setYapLibDir(yap_lib_path)
    args.setSavedState(os.path.join(yap_lib_path,"startup.yss"))
    engine = yap.YAPEngine(args)
    engine.goal( use_module(library('yapi') ) )
    return engine

def live(**kwargs):
    boot_yap(**kwargs)
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
     live()
