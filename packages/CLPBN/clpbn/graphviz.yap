
:- module(clpbn_gviz,
		[clpbn2gviz/4]).

clpbn2gviz(Stream, Name, Network, Node, Edge, Output) :-
	format(Stream, 'digraph ~w { ~n	graph [ rankdir="LR" ];~n',[Name]),
	output_vars(Stream, Network),
	info_ouput(Stream, Output),
	format(Stream, '}~n',[]).

output_vars(_, []).
output_vars(Stream, [V|Vs]) :-
	output_var(Stream, V),
	output_vars(Stream, Vs).

output_var(Stream, V) :-
	clpbn:get_atts(V,[key(Key),evidence(_)]),
	output_key(Stream,Key),
	format(Stream, ' [ shape=box, style=filled, fillcolor=red, fontsize=18.0  ]~n',[]),
	fail.
output_var(Stream, V) :-
	clpbn:get_atts(V,[key(Key),dist(_,Parents)]),
	Parents = [_|_], !,
	format(Stream, '	',[]),
	output_parents(Stream, Parents),
	format(Stream,' -> ',[]),
	output_key(Stream,Key),
	nl(Stream).
output_var(_, _).

info_ouput(_, []).
info_ouput(Stream, [V|Output]) :-
	clpbn:get_atts(V,[key(Key)]),
	output_key(Stream,Key),
	format(Stream, ' [ shape=box, style=filled, fillcolor=green, fontsize=18.0 ]~n',[]),
	info_ouput(Stream, Output).


output_parents(Stream, [V]) :- !,
	output_v(V,Stream).
output_parents(Stream, L) :-
	format(Stream,'{ ',[]),
	output_parents1(Stream,L),
	format(Stream,'}',[]).

output_parents1(_,[]).
output_parents1(Stream,[V|L]) :-
	output_v(V,Stream),
	put_code(Stream, 0' ), %'
	output_parents1(Stream,L).

output_v(V,Stream) :-
	clpbn:get_atts(V,[key(Key)]),
	output_key(Stream,Key).

output_key(Stream, Key) :-
	output_key(Stream, 0, Key).

output_key(Stream, _, Key) :-
	primitive(Key), !,
	write(Stream, Key).
output_key(Stream, _, Key) :-
	format(Stream, '"~w"', [Key]).

output_key_args(_, _, []).
output_key_args(Stream, I, [Arg|Args]) :-
	format(Stream, '~*c', [I,0'_]),
	output_key(Stream, I, Arg),
	output_key_args(Stream, I, Args).

