# python commands

import sys
import yap

engine = yap.YAPEngine();
# engine = yap.YAPEngine(yap.YAPParams());

def go():
    while True:
        s = raw_input("Prolog Query: ")
        try: 
            q = engine.query(s)
        except:
            print "Oops!  That was no valid number.  Try again..."  
        while q.next():
            vs = q.namedVars();
            while vs.length() > 0:
                eq = vs.car()
                print eq.getArg(1).text() + " = " + eq.getArg(2).text()
                vs = vs.cdr()
        print "no more answers"
