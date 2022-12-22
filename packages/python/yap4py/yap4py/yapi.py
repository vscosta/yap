import readline
import copy
import sys
import asyncio

try:
    from yap4py.yap import *
except Exception as e:
    print(e)
    sys.exit(0)
from yap4py.systuples import python_query, python_show_query, show_answer, v0, compile, yap_flag, set_prolog_flag, load_text, load_file
from yap4py.queries import TopQuery, Query
from os.path import join, dirname

import sys

yap_lib_path = dirname(__file__)

async def print_out(s):
    sys.stdout.write(s.encode())
    await sys.stdout.drain()

async def print_err(s):
    sys.stdout.write(s.encode())
    await sys.stderr.drain()



class Engine( YAPEngine ):

    def __init__(self, args=None,self_contained=False,**kwargs):
        # type: (object) -> object
        if not args:
            args = EngineArgs(**kwargs)
            args.setEmbedded(True)
        if self_contained:
            yap_lib_path = dirname(__file__)
            args.setYapShareDir(join(yap_lib_path, "prolog"))
            args.setYapPLDIR(yap_lib_path)
            args.setSavedState(join(yap_lib_path, "startup.yss"))
        YAPEngine.__init__(self, args)
        self.run(set_prolog_flag("verbose_load",False))
        self.load_library('yapi')
        self.run(set_prolog_flag("verbose_load",True))

    def run(self, g, m=None, release=False):
        if m:
            self.mgoal(g, m, release)
        else:
            self.goal(g, release)

    def load_file(self, name, m=None):
        self.run(load_file(name, m))
            
    def prolog_text(self, file, m=None):
        self.run(load_text( file, m))

class EngineArgs( YAPEngineArgs ):
    """ Interface to EngineOptions class"""
    def __init__(self, args=None,**kwargs):
        super().__init__()



class v(YAPVarTerm,v0):
    def __init__(self):
        YAPVarTerm.__init__()

    def binding(self):
        return self.term()


class YAPShell:

    def numbervars( self ):
        Dict = {}
        self.engine.goal(show_answer( self, Dict), True)
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
        g = None
        #import pdb; pdb.set_trace()
        #
        # construct a query from a one-line string
        # q is opaque to Python
        #
        # q = engine.query(python_query(self, s))
        #
        #        # vs is the list of variables
        # you can print it out, the left-side is the variable name,
        # the right side wraps a handle to a variable
        # import pdb; pdb.set_trace()
        #     #pdb.set_trace()
        # atom match either symbols, or if no symbol exists, sttrings, In this case
        # variable names should match strings
        #for eq in vs:
        #    if not isinstance(eq[0],str):
        #        print( "Error: Variable Name matches a Python Symbol")
        #        return
        try:
            engine = self.engine
            
            loop = False
            bindings = []
            engine.q = TopQuery(engine, query)
            q = engine.q
            for _ in q:
                if q.bindings:
                    bindings += [self.q.bindings]
                    print(  q.bindings )
                if q.done():
                    return True, bindings
                if loop:
                    continue
                s = input("more(;), all(*), no(\\n), python(#)?  ").lstrip()
                if s.startswith(';') or s.startswith('y'):
                    continue
                elif s.startswith('#'):
                    try:
                        exec(s.lstrip('#'))
                    except:
                        raise
                elif s.startswith('*') or s.startswith('a'):
                    loop = True
                    continue
                else:
                    break
            if self.engine.q:
                self.engine.q.close()
                self.engine.q = None
            print("No (more) answers")
            return True, bindings
        except Exception as e:
            if not self.engine.q:
                return False, None
            self.engine.q.close()
            self.engine.q = None
            print("Exception",e)
            e.errorNo = 0
            raise

    def live(self, engine, **kwargs):
        try:
            loop = True
            self.engine.q = None
            while loop:
                try:
                    s = input("?- ")
                    if not s:
                        continue
                    else:
                        self.query_prolog(s)
                except SyntaxError as err:
                    print("Syntax Error error: {0}".format(err))
                    continue
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
        except Exception as e:
            print("Exception",e)
            e.errorNo = 0
            raise


    #
    # initialize engine
    # engine = yap.YAPEngine();
    # engine = yap.YAPEngine(yap.YAPParams());
    #
    def __init__(self, engine, **kwargs):
        # pdb; pdb.set_trace()
        self.engine = engine

        self.live(engine)
        self.engine.q = None


def main():
    engine = Engine()
    YAPShell(engine)

if __name__ == "__main__":
    main()
