
% if your Prolog compiler does not support the ensure_loaded/1 directive
% then simply consult the files

:- ensure_loaded(benchmark).

:- ensure_loaded(plain).

% comment the next line if your Prolog compiler does not support modules
:- ensure_loaded(module).

:- initialization(logtalk_load([object, database])). 
