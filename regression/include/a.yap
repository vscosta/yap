a(0).

:- prolog_load_context(directory, D),
   prolog_load_context(file, F),
   prolog_load_context(source, S),
   prolog_load_context(stream, St),
   format('~s:~w: including file ~s or just loading ~s~n',[D,St,F,S]).
   
:- include(a1).
:- include(a2).

a(3).

:- prolog_load_context(directory, D),
   prolog_load_context(file, F),
   prolog_load_context(source, S),
   prolog_load_context(stream, St),
   format('~s:~w: including file ~s or just loading ~s~n',[D,St,F,S]).

    :- listing(a).

