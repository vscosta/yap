
import yap
import sys
# debugging support.
import pdb

def query_prolog(engine, s):

    def answer(q):
        try:
            return q.next()
        except Exception as e:
            print(e.args[1])
            return False

    #
    #construct a query from a one-line string
    # q is opaque to Python
    q = engine.query(s)
    # vs is the list of variables
    # you can print it out, the left-side is the variable name,
    # the right side wraps a handle to a variable
    vs = q.namedVars()
    # atom match either symbols, or if no symbol exists, sttrings, In this case
    # variable names should match strings
    for eq in vs:
        if not isinstance(eq[0],str):
            print( "Error: Variable Name matches a Python Symbol")
            return
    ask = True
    # launch the query
    while answer(q):
        # this new vs should contain bindings to vars
        vs=  q.namedVars()
        #numbervars
        i=0
        # iteratw
        for eq in vs:
            name = eq[0]
            # this is tricky, we're going to bind the variables in the term so thay we can
            # output X=Y. The Python way is to use dictionares.
            #Instead, we use the T function to tranform the Python term back to Prolog
            binding = yap.T(eq[1])
            if binding.isVar():
                binding.unify(name)
            else:
                i = binding.numberVars(i, True)
                print(name + " = " + binding.text())
            #ok, that was Prolog code
        print("yes")
        # deterministic = one solution
        if q.deterministic():
            # done
            q.close()
            return
        if ask:
            s = input("more(;),  all(*), no(\\n), python(#) ?").lstrip()
            if s.startswith(';') or s.startswith('y'):
                continue
            elif s.startswith('#'):
                try:
                    exec(s.lstrip('#'))
                except:
                    raise
            elif s.startswith('*') or s.startswith('a'):
                ask = False
                continue
            else:
                break
    print("No (more) answers")
    q.close()
    return


def live():
    engine = yap.YAPEngine()
    loop = True
    pdb.set_trace()
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
#live()
