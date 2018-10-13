2:- use_module( library(lineutils) ).
3
4main :-
5    unix(argv[Dir,Out]),
6    open(Out,write,O),

 7    go(Dir,O).
 9go(Dir,O) :-

