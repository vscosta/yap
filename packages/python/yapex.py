
import yap
import sys

# this class is not being used
# we rely on namedtuples instead.


class T(tuple):

    """Represents a non-interned Prolog atom"""
    def __new__(self, s, tple):
        self.tuple.__new__(self, tple)
        self.name = s

    def __repr__(self):
        return "yapex.T(" + self.name + " , " + tuple.__repr__(self) + ")"

    def __str__(self):
        return str(self.name) + str(self.tuple)


def query_prolog(engine, s):
    q = engine.query(s)
    ask = True
    while q.next():
        vs = q.namedVarsCopy()
        if vs:
            i = 0
            for eq in vs:
                name = eq[0]
                bind = eq[1]
                if bind.isVar():
                    var = yap.YAPAtom('$VAR')
                    f = yap.YAPFunctor(var, 1)
                    bind.unify(yap.YAPApplTerm(f, (name)))
                else:
                    i = bind.numberVars(i, True)
                    print(name.text() + " = " + bind.text())
        print("yes")
        if q.deterministic():
            q.close()
            return
        if ask:
            s = input("more(;/y),  all(!/a), no ?").lstrip()
            if s.startswith(';') or s.startswith('y'):
                continue
            elif s.startswith('#'):
                exec(s.lstrip('#'))
            elif s.startswith('a'):
                ask = False
            else:
                break
        print("No (more) answers")
        q.close()
    return


def live():
    engine = yap.YAPEngine()
    loop = True
    while loop:
        try:
            s = input("?- ")
            if not s:
                loop = False
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
live()
