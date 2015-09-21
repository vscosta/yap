# python commands

import yap

engine = yap.YAPEngine();
# engine = yap.YAPEngine(yap.YAPParams());

while True:
  s = raw_input("Prolog Query: ")
  q = engine.query(s)
  print "success"
  while q.next():
    ar = q.namedVars();
    print ar
