
find_undefs :-
 %   check_args(undefs, boolean, true, true),
    format('UNDEFINED procedure calls:~n===~n',[]),
    pmodule(M),
    findall(P, undef_in_m(M,P), Ps),
    Ps = [_|_],
      format('  * ~w~n', [M]),
    member(P, Ps),
    format('      + ~w:~n',[P]),
     fail.
find_undefs.

pmodule(M) :-
    findall(M, node(M, _,_,_), Ms),
    sort(Ms, SMs),
    member(M, SMs).

called_in_module(M, P) :-
    findall(P, edge((_ :- _-M:P)), Ps),
    sort(Ps, SPs),
    member(P, SPs).

undef_in_m(M,P) :-
    called_in_module(M, P),
    \+ edge((_-M:P :- _)),
    \+ is_private(_, M, P),
    \+ is_public(_, M, P).

/*
    setof(M, Target^F^Line^NA^undef( ( Target :- F-M:NA ), Line  ), Ms ),
    member( Mod, Ms ),
    format('    module ~a:~n',[Mod]),
    setof(NA, Target^F^Line^undef( ( Target :- F-Mod:NA ), Line  ), Ns ),
    member( NA, Ns ),
    \+ node( Mod , NA , _File1, _ ),
    \+ node( prolog , NA , _File2, _ ),
    format('      predicate ~w:~n',[NA]),
    (
        setof(F-Line, Target^undef( ( Target :- F-Mod:NA ), Line ), FLs ),
	member(F-L, FLs ),
	format('        line ~w, file ~a~n',[L,F]),
	fail
    ;
         setof(F-M,Type^node( M, NA, F, Type ) , FMs ),
	 format('      same name at:~n',[]),
	 member((F-L)-M, FMs ),
	 format('        module ~a, file ~a, line ~d~n',[M,F,L]),
	 fail
    ).
undefs.
*/
