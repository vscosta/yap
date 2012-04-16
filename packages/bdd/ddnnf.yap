
:- module(ddnnf,
	[assignments_to_ddnnf/4,
         ddnnf/3,
	 ddnnf_is/2]).

:- source.

:- style_check(all).

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(lineutils)).
:- use_module(library(terms)).
:- use_module(library(cnf)).
:- use_module(library(simpbool)).

assignments_to_ddnnf(List, Vars, LVars, DDNNF) :-
	list2cnfs(List, CNF, []),
%	(numbervars(CNF,1,_), writeln(Vars:CNF), fail ; true),
	open(dimacs, write, S),
	new_variables_in_term(Vars, CNF, LVars),
	append(Vars, LVars, MVars),
	cnf_to_file(CNF, MVars, S),
	close(S),
	% execute c2d at this point, but we're lazy
	unix(system('c2d -dt_method 3 -in dimacs')),
	open('dimacs.nnf',read,R),
	SVars =.. [v|MVars],
%	ones(LVars),
	input_ddnnf(R, SVars, DDNNF),
%	writeln(DDNNF),
	close(R).

list2cnfs([]) --> [].
list2cnfs([(O=Impl)|Impls]) --> !,
	{cvt(O,FO)},
	disj2cnf(Impl, FO),
	list2cnfs(Impls).
list2cnfs([CNF|Impls]) -->
	{ to_format(CNF, Format, []) },
	[Format],
	list2cnfs(Impls).

cvt(O,O) :- var(O), !.
cvt(not(O),-O).

to_format(A) -->
	{ var(A) },
	!,
	[A].
to_format(A+B) -->
	!,
	to_format(A), 
	to_format(B).
to_format(not(A)) -->
	!,
	[-A].
to_format(A) -->
	[A].

 
disj2cnf(B, O) -->
	{ var(B) }, !,
	[[O|R]],
	{ and2cnf(B, R, []) }.
disj2cnf(A+B, O) -->
	!,
	disj2cnf(A, O),
	disj2cnf(B, O).
disj2cnf(B, O) -->
	[[O|R]],
	{ and2cnf(B, R, []) }.

and2cnf(A) -->
	{ var(A) },
	!,
	[-A].
and2cnf(A*B) -->
	!,
	and2cnf(A),
	and2cnf(B).
and2cnf(not(A)) -->
	!,
	[A].
and2cnf(A) -->
	!,
	[-A].

ddnnf(List, Vars, DDNNF) :-
	exps2conj(List, Conj),
	once(cnf(Conj, CNF)),
	(numbervars(CNF,1,_), writeln(Vars:CNF), fail ; true),
	open(dimacs, write, S),
	new_variables_in_term(Vars, CNF, LVars),
	append(Vars, LVars, MVars),
	cnf_to_file(CNF, MVars, S),
	close(S),
	% execute c2d at this point, but we're lazy
	unix(system('c2d -in dimacs')),
	open('dimacs.nnf',read,R),
	SVars =.. [v|MVars],
%	ones(LVars),
	input_ddnnf(R, SVars, DDNNF),
	close(R).

exps2conj([Conj], Conj) :- !.
exps2conj([Head|List], Head*Conj) :-
	exps2conj(List, Conj).

cvt_el(V, V) :- var(V), !.
cvt_el(not(X), -X1) :- !,
	cvt_el(X, X1).
cvt_el(X+Y, X1+Y1) :- !,
	cvt_el(X, X1),
	cvt_el(Y, Y1).
cvt_el(X*Y, X1*Y1) :- !,
	cvt_el(X, X1),
	cvt_el(Y, Y1).
cvt_el(X, X).

cnf_to_file(List, Vars, S) :-
	number_ivars(Vars, 1, M),
	length(List, N),
	M1 is M-1,
	format(S,'p cnf ~d ~d~n',[M1,N]),
	output_list(List, S),
	fail.
cnf_to_file(_List, _Vars, _S).

number_ivars([], M, M).
number_ivars([I0|IVars], I0, M) :-
	I is I0+1,
	number_ivars(IVars, I, M).

output_list([], _S).
output_list([CNF|List], S) :-
	output_cnf(CNF, S),
	output_list(List, S).

output_cnf([], S) :-
	format(S, '0~n', []).
output_cnf([-V|CNF], S) :- !,
	format(S, '-~d ',[V]),
	output_cnf(CNF, S).
output_cnf([V|CNF], S) :-
	format(S, '~d ',[V]),
	output_cnf(CNF, S).

input_ddnnf(Stream, SVars, ddnnf(Out, SVars, Result)) :-
	read_line_to_codes(Stream, Header),
	split(Header, ["nnf",VS,_ES,_NS]),
	number_codes(NVs, VS),
	functor(TempResults, nnf, NVs),
	process_nnf_lines(Stream, SVars, 1, TempResults, Out, Last),
	Last1 is Last-1,
	arg(Last1, TempResults, Result).
	
process_nnf_lines(Stream, SVars, LineNumber, TempResults, O, LL) :-
	read_line_to_codes(Stream, Codes),
	( Codes = end_of_file -> O = [], LL = LineNumber ;
%	(LineNumber > 1 -> N is LineNumber-1, arg(N,TempResults,P), format("~w ",[P]);true),
%	format("~s~n",[Codes]),
	  arg(LineNumber, TempResults, P),
	  process_nnf_line(SVars, TempResults, Exp0, Codes, []),
	  simplify_line(P=Exp0, Lines, O),
	  NewLine is LineNumber+1,
	  process_nnf_lines(Stream, SVars, NewLine, TempResults, Lines, LL)
	).

process_nnf_line(SVars, _TempResults, Exp) --> "L ",
	nnf_leaf(SVars, Exp).
process_nnf_line(_SVars, TempResults, Exp) --> "A ",
	nnf_and_node(TempResults, Exp).
process_nnf_line(_SVars, TempResults, Exp) --> "O ",
	nnf_or_node(TempResults, Exp).

nnf_leaf(SVars, Prob, Codes, []) :-
	number_codes(Number, Codes),
	Abs is abs(Number),
	arg(Abs, SVars, Node),
	(Number < 0 -> Prob = (1-Node) ; Prob = Node).

nnf_and_node(TempResults, Product, Codes, []) :-
	split(Codes, [_|NumberAsStrings]),
	multiply_nodes(NumberAsStrings, TempResults, Product).

multiply_nodes([], _, 1).
multiply_nodes(NumberAsString.NumberAsStrings, TempResults, Product) :-
	number_codes(Pos, NumberAsString),
	Pos1 is Pos+1,
	arg(Pos1, TempResults, P),
	Product = Product0*P,
	multiply_nodes(NumberAsStrings, TempResults, Product0).

nnf_or_node(TempResults, Sum, Codes, []) :-
	split(Codes, [_J,_C|NumberAsStrings]),
	add_nodes(NumberAsStrings, TempResults, Sum).

add_nodes([], _, 0).
add_nodes(NumberAsString.NumberAsStrings, TempResults, Product) :-
	number_codes(Pos, NumberAsString),
	Pos1 is Pos+1,
	arg(Pos1, TempResults, P),
	Product = Product0+P,
	add_nodes(NumberAsStrings, TempResults, Product0).

ones([]).
ones([1|LVars]) :-
	ones(LVars).

simplify_line((A=Exp0), List, Final) :-
	simplify_exp(Exp0, Exp),
	propagate_constants(Exp, A, List, Final).

propagate_constants(Exp, A, Lines, Lines) :- var(Exp), !, A=Exp.
propagate_constants(0, 0, Lines, Lines) :- !.
propagate_constants(1, 1, Lines, Lines) :- !.
propagate_constants(Exp, A, Lines, [(A=Exp)|Lines]).

ddnnf_is(ddnnf(F, _Vs, Out), Out) :-
	ddnnf_is_acc(F).

%ddnnf_is_acc([H=Exp|_]) :- writeln((H=Exp)),fail.
ddnnf_is_acc([]).
ddnnf_is_acc([H=Exp|Attrs]) :-
%	term_variables(Exp, Vs), ones(Vs),
	H is Exp,
	ddnnf_is_acc(Attrs).
