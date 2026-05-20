"""
@file queries.py

@defgroup PyQueries A more Pythonic approach to query construction that allows for iteration
@ingroup YAP4Py

"""
from collections import namedtuple

from yap4py.yap import YAPQuery
from yap4py.systuples import top_goal

class Query (YAPQuery):
    """Python support for querying iteratively.

    Goal is a predicate instantiated under a specific environment """
    def __init__(self, engine, g):
        super().__init__( g  )
        self.gate = None
        self.bindings = []
        self.delays = []
        self.errors = []
        self.engine = engine

    def __iter__(self):
        return self
    

    def set_gate(self,gate):
        self.gate = gate
        
    def done(self):
        gate = self.gate
        completed = gate == "fail" or gate == "exit" or gate == "!"
        return completed

    def __next__(self):
        if self.done() or not self.next():
            raise StopIteration()
        return self

def top_query (eng, s):
    """Python support for protected querying iteratively"""
    goal = top_goal( eng, s)
    return Query(eng, goal)

    
def name( name, arity):
    try:
        s = []
        for i in range(arity):
            s += ["A" + str(i)]
        if  arity > 0 and name.isidentifier(): # and not keyword.iskeyword(name):
            return namedtuple(name, s)
    except:
        return None
