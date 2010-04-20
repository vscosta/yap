'$expects_dialect'(swi) :-
	eraseall('$dialect'),
	recorda('$dialect',swi,_),
	load_files(library('dialect/swi'),[silent(true),if(not_loaded)]).
'$expects_dialect'(yap) :-
	eraseall('$dialect'),
	recorda('$dialect',yap,_).

