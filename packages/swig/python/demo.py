# python commands

import sys
import yap

engine = yap.YAPEngine();
# engine = yap.YAPEngine(yap.YAPParams());

def go():
    while True:
        s = raw_input("Prolog Query: ")
        q = engine.query(s)
        while q.next():
            vs = q.namedVars();
            if vs.length() == 0:
              print "yes"
            else:
                while vs.length() > 0:
                    eq = vs.car()
                    print eq.getArg(1).text() + " = " + eq.getArg(2).text()
                    vs = vs.cdr()
        print "no more answers"
