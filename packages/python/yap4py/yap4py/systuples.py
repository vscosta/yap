
from collections import namedtuple



bindvars = namedtuple('bindvars', 'list')
compile = namedtuple('compile', 'file')
completions = namedtuple('completions', 'text line pos self')
current_prolog_flag = namedtuple('current_prolog_flag', 'flag value')
dbms = namedtuple('dbms', 'filedbms')
errors = namedtuple('errors', 'fileng')
foreign = namedtuple('foreign', 'filedbms')
jupyter_cell = namedtuple('jupyter_cell','jupyter_program jupyter_query self')
jupyter_consult = namedtuple('jupyter_consult', 'program server')
jupyter_query = namedtuple('jupyter_query', 'query server')

load_file = namedtuple('load_file', ['file','module','recover'],defaults=
                       [None,False])
load_files = namedtuple('load_files', 'file opts')
load_text = namedtuple('load_text', 'module recover')
streams = namedtuple('ostreams', ' text')
predicate_property = namedtuple('predicate_property', 'pred prop')
prolog_library=namedtuple('prolog_library', 'listfiles')
python_query = namedtuple('python_query', 'engine query')
python_show_query = namedtuple('python_show_query', 'engine query')
set_prolog_flag = namedtuple('set_prolog_flag', 'flag new_value')
show_answer = namedtuple('show_answer', 'vars dict')
show_answer = namedtuple('show_answer', 'vars dict')
silent = namedtuple('silent', 'on')
streams = namedtuple('streams', 'text')
v0 = namedtuple('v', 'slot')
writeln=  namedtuple('writeln', 'slot')
xkv = namedtuple('_', 'slot')
yap_flag = namedtuple('yap_flag', 'flag value new_value')
yap_query = namedtuple('yap_query', 'query owner')
yapi_query = namedtuple('yapi_query', 'query owner')
