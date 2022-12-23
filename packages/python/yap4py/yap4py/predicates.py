
from yap4py.yap import YAPEngine, YAPPredicate, YAPPrologPredicate
from collections import namedtuple

class Predicate( YAPPredicate ):
    """ Interface to Generic Predicate"""

    def __init__(self, t, module=None):
        super().__init__(t)

library = namedtuple('library', 'listfiles')


class LoadLibrary(Predicate):
    def __init__(self, eng):
        self.engine = eng
        self.goal = namedtuple('load_library', 'name' )

    def run(self, c):
        self.engine.run(self.goal(library(c)))

    def __str__(self):
        return self.goal.__str__()
    
load_library = LoadLibrary(YAPEngine).run


class Asserta:
    def __init__(self, eng):
        self.engine = eng
        self.goal = namedtuple('asserta', 'clause' )

    def run(self, c):
        self.engine.run(self.goal(c))

    def __str__(self):
        return self.goal.__str__()
    
asserta = Asserta(YAPEngine).run

class Assertz(Predicate):
    def __init__(self, eng):
        self.engine = eng
        self.goal = namedtuple('assertz', 'clause' )

    def run(self, c):
        self.engine.run(self.goal(c))

    def __str__(self):
        return self.goal.__str__()
    
assertz = Assertz(YAPEngine).run

class PrologPredicate( YAPPrologPredicate ):
    """ Interface to Prolog  Predicate"""

    def __init__(self, t, module=None):
        super().__init__(t)
