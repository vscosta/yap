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
            s = raw_input("next: ?")
            if s.find(';') != 0 :
                return
    print( "no more answers" )
    return

def live():
    loop = True
    while loop:
        s = raw_input("?- ")
        query( s )
