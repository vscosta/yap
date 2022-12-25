
from yap4py.yap import YAPQuery, YAPEngine
from collections import namedtuple

class Query (YAPQuery):
    """Goal is a predicate instantiated under a specific environment """
    def __init__(self, engine, g):
        self.gate = None
        self.bindings = []
        self.delays = []
        self.errors = []
        self.engine = engine
        super().__init__(g)

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

def name( name, arity):
    try:
        if  arity > 0 and name.isidentifier(): # and not keyword.iskeyword(name):
            s = []
            for i in range(arity):
                s += ["A" + str(i)]
            return namedtuple(name, s)
    except:
        return None

class TopQuery(Query):
    
    def __init__(self, eng,g):
        self.top = namedtuple('top_goal', 'query g' )
        self.engine = eng
        super().__init__(eng,self.top(self,g))
        
    def run(self, s):
        return Query( self.engine, s)

    def __str__(self):
        return self.top.__str__(self)
    



