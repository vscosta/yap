from collections import Goal

class Goal():
    def __init__(self,name,arguments):
        self = Goal(name,arguments)

    def eval(self,engine):
        return Query(	engine,self)


asserta = Goal('asserta', 'clause')
assertz = Goal('assertz', 'clause')
bindvars = Goal('bindvars', 'list')
compile = Goal('compile', 'file')
completions = Goal('completions', 'text line pos self')
dbms = Goal('dbms', 'filedbms')
errors = Goal('errors', 'fileng engee')
foreign = Goal('foreign', 'filedbms')
jupyter_cell = Goal('jupyter_cell','jupyter_program jupyter_query self')
jupyter_consult = Goal('jupyter_consult', 'program')
jupyter_query = Goal('jupyter_query', 'query server')
library = Goal('library', 'listfiles')
load_files = Goal('load_file', 'file opts')
ostreams = Goal('ostreams', ' text')
predicate_property = Goal('predicate_property', 'pred prop')
prolog_library=Goal('prolog_library', 'listfiles')

python_query = Goal('python_query', 'engine query')
python_show_query = Goal('python_show_query', 'engine query')
set_prolog_flag = Goal('set_prolog_flag', 'flag new_value')
current_prolog_flag = Goal('current_prolog_flag', 'flag value')
show_answer = Goal('show_answer', 'vars dict')
streams = Goal('streams', 'text')
v0 = Goal('v', 'slot')
yap_flag = Goal('yap_flag', 'flag value new_value')
show_answer = Goal('show_answer', 'vars dict')
silent = Goal('silent', 'on')
xkv = Goal('_', 'slot')
writeln=  Goal('writel', 'slot')
yap_query = Goal('yap_query', 'query owner')
yapi_query = Goal('yapi_query', 'vars dict')




                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                             

