

:- module( concat, [concat_name/2]).

concat_name( List, Name) :- List=[_|_],
	conc_parts( List, Chs, []),
	atom_codes( Name, Chs).
concat_name( F/A, Name) :-
	conc_part( F/A, Chs, []),
	atom_codes( Name, Chs).

conc_parts( []) --> [].
conc_parts( [P]) --> !, conc_part( P).
conc_parts( [P|Ps]) -->
	conc_part( P),
	"_",
	conc_parts( Ps).

conc_part( F/A) --> !, name( F), "/", name( A).
conc_part( X  ) -->    name( X).

name( A) --> {name(A,Chars)}, copy( Chars).

copy( []) --> [].
copy( [C|Cs]) --> [ C ], copy( Cs).

