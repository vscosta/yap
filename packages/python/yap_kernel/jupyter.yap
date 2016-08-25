:- if(  current_prolog_flag(apple, true) ).

:- putenv( 'LC_CTYPE', 'en_us:UTF-8').

plot_inline :-
	X := self.inline_plotting,
	nb_setval(inline, X ),
	X = true,
	!,
	:= (
	   import( matplotlib ),
	   matplotlib.use( `nbagg` )
	   ).

:- endif.
