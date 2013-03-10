
:- module(ddnnf,
	[cnf_to_ddnnf/3,
         ddnnf/3,
	 ddnnf_is/2]).

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(lineutils)).
:- use_module(library(terms)).
:- use_module(library(cnf)).
:- use_module(library(simpbool)).

%
% convert a CNF as list with Variables Vars and Existential variables
% in DDNNF, Exs \in LVars into DDNNF with extra existential vars
%
cnf_to_ddnnf(CNF0, PVs, DDNNF) :-
	list2cnf(CNF0, CNF, []),
	mkddnnf(CNF, PVs, DDNNF).

mkddnnf(CNF, PVs, DDNNF) :-
	term_variables(CNF, AllVars),
%	(numbervars(CNF,1,_), writeln(CNF), fail ; true),
	open(dimacs, write, S),
	cnf_to_file(CNF, AllVars, S),
	close(S),
	% execute c2d at this point, but we're lazy%
%	unix(system('c2d -dt_method 3 -in dimacs')),
	unix(system('c2d -visualize -in dimacs')),
%	unix(system('dsharp -Fnnf dimacs.nnf  dimacs')),
	open('dimacs.nnf',read,R),
	SVars =.. [v|AllVars],
%	ones(LVars),
	input_ddnnf(R, SVars, PVs, DDNNF),
%	writeln(DDNNF),
	close(R).

list2cnf([]) --> [].
list2cnf([(O=A)|Impls]) --> !,
	{cvt(O,FO,NO),
	and2cnf(A,Conj,[]) },
	[[FO|Conj]],
	disj(A, NO),
	list2cnf(Impls).
list2cnf([CNF|Impls]) -->
	{ to_format(CNF, Format, []) },
	[Format],
	list2cnf(Impls).

cvt(O,O,-O) :- var(O), !.
cvt(not(O),-O,O).

neg(O,-O) :- var(O), !.
neg(-O,O).

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

disj(A, NO) --> 
	{ var(A) }, !,
	[[NO,A]].
disj(A*B, NO) --> !,
	disj(A, NO),
	disj(B, NO).
disj(A, NO) -->
	[[NO,A]].

%
% convert a boolean expression with Variables Vars and Existential variables
% in DDNNF, Exs \in LVars into DDNNF with extra existential vars
%
% ex: (A*B+not(B))*(C=B) into something complicated 
%
ddnnf(List, PVs, DDNNF) :-
	exps2conj(List, Conj),
	cnf(Conj, CNF),
	mkddnnf(CNF, PVs, DDNNF).

exps2conj((C1,C2), CC1*CC2) :- !,
	exps2conj(C1, CC1),
	exps2conj(C2, CC2).
exps2conj((Conj), CConj) :- 
	cvt_el(Conj, CConj).

cvt_el(V, V) :- var(V), !.
cvt_el(not(X), -X1) :- !,
	cvt_el(X, X1).
cvt_el(X+Y, X1+Y1) :- !,
	cvt_el(X, X1),
	cvt_el(Y, Y1).
cvt_el(X*Y, X1*Y1) :- !,
	cvt_el(X, X1),
	cvt_el(Y, Y1).
cvt_el(X=Y, X1==Y1) :- !,
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

input_ddnnf(Stream, SVars, PVs, ddnnf(Out, SVars, Result)) :-
	read_line_to_codes(Stream, Header),
	split(Header, ["nnf",VS,_ES,_NS]),
	number_codes(NVs, VS),
	functor(TempResults, nnf, NVs),
	process_nnf_lines(Stream, SVars, PVs, 1, TempResults, Out, Last),
	Last1 is Last-1,
	arg(Last1, TempResults, Result).
	
process_nnf_lines(Stream, SVars, PVs, LineNumber, TempResults, O, LL) :-
	read_line_to_codes(Stream, Codes),
	( Codes = end_of_file -> O = [], LL = LineNumber ;
%	(LineNumber > 1 -> N is LineNumber-1, arg(N,TempResults,P), format("~w ",[P]);true),
%	format("~s~n",[Codes]),
	  arg(LineNumber, TempResults, P),
	  process_nnf_line(SVars, PVs, TempResults, Exp0, Codes, []),
	  simplify_line(P=Exp0, Lines, O),
	  NewLine is LineNumber+1,
	  process_nnf_lines(Stream, SVars, PVs, NewLine, TempResults, Lines, LL)
	).

process_nnf_line(SVars, PVs, _TempResults, Exp) --> "L ",
	nnf_leaf(SVars, PVs, Exp).
process_nnf_line(_SVars, _, TempResults, Exp) --> "A ",
	nnf_and_node(TempResults, Exp).
process_nnf_line(_SVars, _, TempResults, Exp) --> "O ",
	nnf_or_node(TempResults, Exp).

nnf_leaf(SVars, PVs, Prob, Codes, []) :-
	number_codes(Number, Codes),
	Abs is abs(Number),
	arg(Abs, SVars, Node),
	(Number < 0 ->
	   (parameter(Node,PVs) -> Prob = 1-Node ; Prob = 1  )
        ;
	    Prob = Node
	).

parameter(F,[F1|_Exs]) :- F == F1, !.
parameter(F,[_|Exs]) :-
	parameter(F, Exs).

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

%
% compute the value of a SP
%
%
ddnnf_is(ddnnf(F, Vs, Out), Out) :-
	term_variables(Vs,LVs),
	ones(LVs),
%(numbervars(F,1,_),writeln(F),fail;true),
	ddnnf_is_acc(F).

%ddnnf_is_acc([H=Exp|_]) :- writeln((H=Exp)),fail.
ddnnf_is_acc([]).
ddnnf_is_acc([H=Exp|Attrs]) :-
	H is Exp,
%writeln(Exp:H),
	ddnnf_is_acc(Attrs).
