
:- prolog_load_context(directory, D),
   prolog_load_context(file, F),
   prolog_load_context(source, S),
   prolog_load_context(stream, St),
   format('~s:~w: including file ~s or just loading ~s~n',[D,St,F,S]).

a(1).

:- include(a11).

