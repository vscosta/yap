
from collections import namedtuple



bindvars = namedtuple('bindvars', 'list')
compile = namedtuple('compile', 'file')
completions = namedtuple('completions', 'text line pos self')
current_prolog_flag = namedtuple('current_prolog_flag', 'flag value')
dbms = namedtuple('dbms', 'filedbms')
errors = namedtuple('errors', 'fileng')
foreign = namedtuple('foreign', 'filedbms')
jupyter_cell = namedtuple('jupyter_cell','jupyter_program jupyter_query self')
jupyter_query = namedtuple('jupyter_query', 'query server')
library = namedtuple('library', 'file')
load_file = namedtuple('load_file', ['file','module','recover'],defaults=
                       [None,False])
streams = namedtuple('ostreams', ' text')
predicate_property = namedtuple('predicate_property', 'pred prop')
prolog_library=namedtuple('prolog_library', 'listfiles')
python_query = namedtuple('python_query', 'engine query')
python_show_query = namedtuple('python_show_query', 'engine query')
show_answer = namedtuple('show_answer', 'vars dict')
silent = namedtuple('silent', 'on')
streams = namedtuple('streams', 'text')
v0 = namedtuple('v', 'slot')
writeln=  namedtuple('writeln', 'slot')
xkv = namedtuple('_', 'slot')
set_prolog_flag = namedtuple('set_prolog_flag', 'flag value new_value')
yap_query = namedtuple('yap_query', 'query owner')
yapi_query = namedtuple('yapi_query', 'query owner')
