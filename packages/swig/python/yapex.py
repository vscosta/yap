# python commands

import sys
import yap

#
# initialize engine
engine = yap.YAPEngine();
# engine = yap.YAPEngine(yap.YAPParams());

def query( s ):
    q = engine.query(s)
    while q.next():
        vs = q.namedVars()
        if vs.length() == 0:
            print( "yes" )
            return
        else:
            while vs.length() > 0:
                eq = vs.car()
                print( eq.getArg(1).text() + " = " + eq.getArg(2).text() )
                vs = vs.cdr()
            if q.deterministic():
                return
            s = input("next: ?")
            if s.find(';') != 0 :
                return
    print( "no more answers" )
    return

def live():
    loop = True
    while loop:
        try:
            s = input("?- ")
            query( s )
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


live()
