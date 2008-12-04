:- style_check(all).
:- yap_flag(unknown,error).
:- source.

:- module('$eval',
	  [compile_arithmetic/2]).

'$compile_arithmetic'((Head :- Body), (Head :- NBody)) :-
	term_variables(Head, LVs),
	process_body(Body, LVs, NBody).

process_body((G,Body), InputVs, NewBody) :-
	arithmetic_exp(G), !,
	term_variables(G, UnsortedExpVs),
	sort(UnsortedExpVs, ExpVs),
	fetch_more(Body, ExpVs, LGs, Gs, _, RBody),
	term_variables(RBody, ExtraVs),
	compile_arith([G|LGs], InputVs, ExtraVs, (G,Gs), ArithComp),
	(
	    RBody = true
	->
	    NewBody = ArithComp
	;
	    NewBody = (ArithComp,MBody),
	    term_variables(InputVs+G, NewInputVs),
	    process_body(RBody, NewInputVs, MBody)
	).
process_body((G,Body), InputVs, (G,NewBody)) :- !,
	term_variables(InputVs+G, NewInputVs),
	process_body(Body, NewInputVs, NewBody).
process_body(G, InputVs, NewBody) :-
	arithmetic_exp(G), !,
	term_variables(G, _),
	compile_arith([G], InputVs, [], G, ArithComp),
	NewBody = ArithComp.
process_body(G, _, G).

fetch_more((G,Gs), ExpVs, [G|LGs], (G,AGs), AllExpVs, RGs) :-
	arithmetic_exp(G),
	term_variables(G,Vs),
	sort(Vs, SVs),
	intersect_vars(SVs,ExpVs), !,
	join_vars(ExpVs,SVs,MoreExpVs),
	fetch_more(Gs, MoreExpVs, LGs, AGs, AllExpVs, RGs).
fetch_more((G,Gs), ExpVs, [], true, ExpVs, (G,Gs)) :- !.
fetch_more(G, ExpVs, [G], (G), MoreExpVs, true) :-
	arithmetic_exp(G),
	term_variables(G,Vs),
	sort(Vs,SVs),
	intersect_vars(SVs,ExpVs), !,
	join_vars(ExpVs,SVs,MoreExpVs).
fetch_more(G, ExpVs, [], true, ExpVs, G).

arithmetic_exp((_ is _)).
arithmetic_exp((_ =:= _)).
arithmetic_exp((_ < _)).
arithmetic_exp((_ > _)).
arithmetic_exp((_ >= _)).
arithmetic_exp((_ =< _)).

intersect_vars([V1|R1],[V2|R2]) :-
	(
	    V1 == V2
	->
	    true
	;
	    V1 @< V2
	->
	    intersect_vars(R1,[V2|R2])
	;
	    intersect_vars([V1|R1],R2)
	).

join_vars([],[],[]).
join_vars([],[V2|R2],[V2|R2]).
join_vars([V1|R1],[],[V1|R1]).
join_vars([V1|R1],[V2|R2],O) :-
	(
	    V1 == V2
	->
	    O = [V1|RO],
	    join_vars(R1, R2, RO)
	;
	    V1 @< V2
	->
	    O = [V1|RO],
	    join_vars(R1,[V2|R2], RO)
	;
	    O = [V2|RO],
	    join_vars([V1|R1],R2, RO)
	).

compile_arith(LGs, InputVs, ExtraVs, Gs, ArithComp) :-
	add_type_slots(InputVs,TypedVs),
	sort(InputVs,S1),
	sort(ExtraVs,S2),
	join_vars(S1, S2, S),
	visit(LGs, TypedVs, NewTypedVs, S, FlatExps, []),
	FlatExps = [_,_|_],
	alloc_regs(NewTypedVs,0,Regs),
	Regs < 32,
	compile_ops(FlatExps, Gs, ArithComp), !.
compile_arith(_, _, _, Gs, Gs).

add_type_slots([],[]).
add_type_slots([V|ExpVs],[t(V,_,_)|TypesVs]) :-
	add_type_slots(ExpVs,TypesVs).

visit([], TypedVs, TypedVs, _) --> [].
visit([Exp|Exps], TypedVs, NewTypedVs, ExtraVs) -->
	visit_pred(Exp, TypedVs, ITypedVs, ExtraVs),
	visit(Exps, ITypedVs, NewTypedVs, ExtraVs).
	
visit_pred((X is _), _, _, _) -->
	{ nonvar(X) }, !,
	{ fail }.
visit_pred((_ is T), _, _, _) -->
	{ var(T) }, !,
	fail.
visit_pred((X is T), TypedVs, ExtraTypedVs, LeftBodyVars) -->
	% check the expression
	visit_exp(T, TypedVs, NewTypedVs, TMP, Type),
	({ tmember(X, TypedVs, Type, TMP) }
        -> 
	 { NewTypedVs = ExtraTypedVs }
        ;
	  { ExtraTypedVs = [t(X,Type,TMP)|NewTypedVs] }
        ),        
	% final code
	( { vmember(X, LeftBodyVars)  } ->
	    [export(TMP,X,Type)]
	;
	    []
	).
visit_pred((X =:= T), TypedVs, NewTypedVs, _) -->
	% check the expression
	visit_exp(X, TypedVs, ITypedVs, TMP1, Type),
	visit_exp(T, ITypedVs, NewTypedVs, TMP2, Type),
	% assign the type to X, if any
	% final code
	[eq(TMP1,TMP2)].
visit_pred((X < T), TypedVs, NewTypedVs, _) -->
	% check the expression
	visit_exp(X, TypedVs, ITypedVs, TMP1, Type),
	visit_exp(T, ITypedVs, NewTypedVs, TMP2, Type),
	% assign the type to X, if any
	% final code
	[lt(TMP1,TMP2)].
visit_pred((X > T), TypedVs, NewTypedVs, _) -->
	% check the expression
	visit_exp(X, TypedVs, ITypedVs, TMP1, Type),
	visit_exp(T, ITypedVs, NewTypedVs, TMP2, Type),
	% assign the type to X, if any
	% final code
	[lt(TMP2,TMP1)].

visit_exp(V, TypedVs, TypedVs, TMP, Type) -->
	{ 
           var(V), !,
           tmember(V, TypedVs, Type, TMP)
	   % must have been defined before
        },
	( 
	    { var(TMP) } ->
	    { TMP = x(_) },
	    [get(TMP,V,Type)]
	;
	    []
	).
visit_exp(V, TypedVs, TypedVs, V, Type) -->
	{ 
           float(V)
       }, !, 
	{ Type  = float }.
visit_exp(V, TypedVs, TypedVs, V, Type) -->
	{ 
           integer(V)
        }, !, 
	{ Type  = int }.
visit_exp(V, TypedVs, NewTypedVs, TMP, Type) -->
	{
          ground(V),
	  noatoms(V),
	  !,
	  NV is V
        },
	visit_exp(NV, TypedVs, NewTypedVs, TMP, Type).
visit_exp(X, TypedVs, TypedVs, TMP, Type) -->
	{ 
           atom(X), !,
	   add_type(X,Type)
        },
        [zerop(TMP,X)].
visit_exp(X+Y, TypedVs, [t(_,Type,TMP)|NewTypedVs], TMP, Type) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, T1),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, T2),
	{ forward_type(T1, T2, Type) },
	{ TMP = x(_) },
	[add(TMP,V1,V2)].
visit_exp(X-Y, TypedVs, [t(_,Type,TMP)|NewTypedVs], TMP, Type) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, T1),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, T2),
	{ forward_type(T1, T2, Type) },
	{ TMP = x(_) },
	[sub(TMP,V1,V2)].
visit_exp(X*Y, TypedVs, [t(_,Type,TMP)|NewTypedVs], TMP, Type) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, T1),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, T2),
	{ forward_type(T1, T2, Type) },
	{ TMP = x(_) },
	[mul(TMP,V1,V2)].
visit_exp(X/Y, TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	[fdiv(TMP,V1,V2)].
visit_exp(X//Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	{ Y\== 0},
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[idiv(TMP,V1,V2)].
visit_exp(X mod Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	{ Y\== 0},
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[mod(TMP,V1,V2)].
visit_exp(X rem Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	{ Y\== 0},
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[rem(TMP,V1,V2)].
visit_exp(X /\ Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[and(TMP,V1,V2)].
visit_exp(X \/ Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[or(TMP,V1,V2)].
visit_exp(X # Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[xor(TMP,V1,V2)].
visit_exp(X << Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[sl(TMP,V1,V2)].
visit_exp(X >> Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, int),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, int),
	{ TMP = x(_) },
	[sr(TMP,V1,V2)].
visit_exp(X ^ Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	{ TMP = x(_) },
	[exp(TMP,V1,V2)].
visit_exp(X ** Y, TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	[exp(TMP,V1,V2)].
visit_exp(exp(X,Y), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	{ TMP = x(_) },
	[exp(TMP,V1,V2)].
visit_exp(max(X,Y), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	[max(TMP,V1,V2)].
visit_exp(min(X,Y), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	{ TMP = x(_) },
	[min(TMP,V1,V2)].
visit_exp(gcd(X,Y), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, float) -->
	!,
	visit_exp(X, TypedVs, ITypedVs, V1, _),
	visit_exp(Y, ITypedVs, NewTypedVs, V2, _),
	[gcd(TMP,V1,V2)].
visit_exp(+X, TypedVs, NewTypedVs, TMP, T) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP, T).
visit_exp(-X, TypedVs, [t(_,T,TMP)|NewTypedVs], TMP, T) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, T),
	{ TMP = x(_) },
	[uminus(TMP,TMP1)].
visit_exp(\(X), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, int),
	{ TMP = x(_) },
	[unot(TMP,TMP1)].
visit_exp(exp(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[exp1(TMP,TMP1)].
visit_exp(log(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[log(TMP,TMP1)].
visit_exp(log10(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[log10(TMP,TMP1)].
visit_exp(sqrt(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[sqrt(TMP,TMP1)].
visit_exp(sin(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[sin(TMP,TMP1)].
visit_exp(cos(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[cos(TMP,TMP1)].
visit_exp(tan(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[tan(TMP,TMP1)].
visit_exp(asin(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[asin(TMP,TMP1)].
visit_exp(atan(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[atan(TMP,TMP1)].
visit_exp(atan2(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[atan2(TMP,TMP1)].
visit_exp(acos(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[acos(TMP,TMP1)].
visit_exp(sinh(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[sinh(TMP,TMP1)].
visit_exp(cosh(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[cosh(TMP,TMP1)].
visit_exp(tanh(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[tanh(TMP,TMP1)].
visit_exp(asinh(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[asinh(TMP,TMP1)].
visit_exp(acosh(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[acosh(TMP,TMP1)].
visit_exp(atanh(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[atanh(TMP,TMP1)].
visit_exp(floor(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[floor(TMP,TMP1)].
visit_exp(abs(X), TypedVs, [t(_,T,TMP)|NewTypedVs], TMP, T) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, T),
	{ TMP = x(_) },
	[abs(TMP,TMP1)].
visit_exp(integer(X), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[integer(TMP,TMP1)].
visit_exp(truncate(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[truncate(TMP,TMP1)].
visit_exp(round(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[round(TMP,TMP1)].
visit_exp(ceiling(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[ceiling(TMP,TMP1)].
visit_exp(msb(X), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, int),
	{ TMP = x(_) },
	[msb(TMP,TMP1)].
visit_exp(random(X), TypedVs, [t(_,int,TMP)|NewTypedVs], TMP, int) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, int),
	{ TMP = x(_) },
	[random(TMP,TMP1)].
visit_exp(lgamma(X), TypedVs, [t(_,float,TMP)|NewTypedVs], TMP, float) -->
	visit_exp(X, TypedVs, NewTypedVs, TMP1, _),
	{ TMP = x(_) },
	[lgamma(TMP,TMP1)].

forward_type(T1, _, Type) :- T1 == float, !, Type = float.
forward_type(_, T2, Type) :- T2 == float, !, Type = float.
forward_type(T1, T2, Type) :- T1 == int, T2 == int, !, Type = int.
forward_type(_, _, _).


tmember(X, [t(X1,Type,Tmp)|_], Type, Tmp) :-
	X == X1, !.
tmember(X, [_|TypesVs], Type, Tmp) :-
         tmember(X, TypesVs, Type, Tmp).

stmember(X, [t(X,Type,Tmp1)|_], Type, Tmp) :-
	Tmp == Tmp1, !.
stmember(X, [_|TypesVs], Type, Tmp) :-
         stmember(X, TypesVs, Type, Tmp).

vmember(X, [X1|_]) :-
	X == X1, !.
vmember(X, [_|Vs]) :-
         vmember(X, Vs).

add_type(random, float).
add_type(pi, float).
add_type(inf, float).
add_type(nan, float).
add_type(random, float).
add_type(cputime, float).
add_type(heapused, int).
add_type(local_sp, int).
add_type(global_sp, int).
add_type(stackfree, int).

noatoms(N) :-
	N =.. [_|Ns],
	noatom_in_list(Ns).

noatom_in_list([]).
noatom_in_list([El|Els]) :-
	\+ atom(El),
	El =.. [_|LEl],
	noatom_in_list(LEl),
	noatom_in_list(Els).

alloc_regs([],R,R).
alloc_regs([t(_,_,no)|NewTypedVs], R0, RF) :- !,
	alloc_regs(NewTypedVs, R0, RF).
alloc_regs([t(_,_,x(R0))|NewTypedVs], R0, RF) :- !,
	R1 is R0+1,
	alloc_regs(NewTypedVs, R1, RF).
alloc_regs([t(_,_,x(_))|NewTypedVs], R0, RF) :-
	alloc_regs(NewTypedVs, R0, RF).

compile_ops([], Gs, '$escape'(Gs)).
compile_ops([Op|Exps], Gs, (COp,More)) :-
	compile_op(Op , COp),
	compile_ops(Exps, Gs, More).

compile_op(export(x(A),V,any), get_opres(A,V)) :- !.
compile_op(export(x(A),V,int), get_opres_int(A,V)).
compile_op(export(x(A),V,float), get_opres_float(A,V)).
compile_op(eq(x(A),F), eqc_float(A,F)) :- float(F), !.
compile_op(eq(x(A),I), eqc_int(A,I)) :- integer(I), !.
compile_op(eq(x(A),x(B)), eq(A,B)).
compile_op(lt(x(A),F), ltc_float(A,F)) :-  float(F), !.
compile_op(lt(x(A),I), ltc_int(A,I)) :- integer(I), !.
compile_op(lt(F,x(A)), gtc_float(A,F)) :-  float(F), !.
compile_op(lt(I,x(A)), gtc_int(A,I)) :- integer(I), !.
compile_op(lt(x(A),x(B)), lt(A,B)).
compile_op(get(x(A),V,any), get_opinp(A,V)) :- !.
compile_op(get(x(A),V,int), get_opinp_int(A,V)) :- !.
compile_op(get(x(A),V,float), get_opinp_float(A,V)).
compile_op(zerop(x(A),Op), zerop(A,Op)).
compile_op(add(x(A),F,x(B)), add_float_c(A,B,F)) :- float(F), !.
compile_op(add(x(A),I,x(B)), add_int_c(A,B,I)) :- integer(I), !.
compile_op(add(x(A),x(B),F), add_float_c(A,B,F)) :- float(F), !.
compile_op(add(x(A),x(B),I), add_int_c(A,B,I)) :- integer(I), !.
compile_op(add(x(A),x(B),x(C)), add(A,B,C)).
compile_op(sub(x(A),F,x(B)), sub_float_c(A,B,F)) :- float(F), !.
compile_op(sub(x(A),I,x(B)), sub_int_c(A,B,I)) :- integer(I), !.
compile_op(sub(x(A),x(B),F), add_float_c(A,B,F1)) :- float(F), !, F1 is -F.
compile_op(sub(x(A),x(B),I), add_int_c(A,B,I1)) :- integer(I), !, I1 is -I.
compile_op(sub(x(A),x(B),x(C)), sub(A,B,C)).
compile_op(mul(x(A),F,x(B)), mul_float_c(A,B,F)) :- float(F), !.
compile_op(mul(x(A),I,x(B)), mul_int_c(A,B,I)) :- integer(I), !.
compile_op(mul(x(A),x(B),F), mul_float_c(A,B,F)) :- float(F), !.
compile_op(mul(x(A),x(B),I), mul_int_c(A,B,I)) :- integer(I), !.
compile_op(mul(x(A),x(B),x(C)), mul(A,B,C)).
compile_op(fdiv(x(A),F,x(B)), fdiv_c1(A,B,F)) :- float(F), !.
compile_op(fdiv(x(A),I,x(B)), fdiv_c1(A,B,F)) :- integer(I), !, F is truncate(I).
compile_op(fdiv(x(A),x(B),F), fdiv_c2(A,B,F)) :- float(F), !.
compile_op(fdiv(x(A),x(B),I), fdiv_c2(A,B,F)) :- integer(I), !, F is truncate(I).
`compile_op(fdiv(x(A),x(B),x(C)), fdiv(A,B,C)).
compile_op(idiv(x(A),I,x(B)), idiv_c1(A,B,I)) :- integer(I), !.
compile_op(idiv(x(A),x(B),I), idiv_c2(A,B,I)) :- integer(I), !.
compile_op(idiv(x(A),x(B),x(C)), idiv(A,B,C)).
compile_op(mod(x(A),I,x(B)), mod_c1(A,B,I)) :- integer(I), !.
compile_op(mod(x(A),x(B),I), mod_c2(A,B,I)) :- integer(I), !.
compile_op(mod(x(A),x(B),x(C)), mod(A,B,C)).
compile_op(rem(x(A),I,x(B)), rem_c1(A,B,I)) :- integer(I), !.
compile_op(rem(x(A),x(B),I), rem_c2(A,B,I)) :- integer(I), !.
compile_op(rem(x(A),x(B),x(C)), rem(A,B,C)).
compile_op(and(x(A),I,x(B)), and_c(A,B,I)) :- integer(I), !.
compile_op(and(x(A),x(B),I), and_c(A,B,I)) :- integer(I), !.
compile_op(and(x(A),x(B),x(C)), and(A,B,C)).
compile_op(or(x(A),I,x(B)), or_c(A,B,I)) :- integer(I), !.
compile_op(or(x(A),x(B),I), or_c(A,B,I)) :- integer(I), !.
compile_op(or(x(A),x(B),x(C)), or(A,B,C)).
compile_op(xor(x(A),I,x(B)), xor_c(A,B,I)) :- integer(I), !.
compile_op(xor(x(A),x(B),I), xor_c(A,B,I)) :- integer(I), !.
compile_op(xor(x(A),x(B),x(C)), xor(A,B,C)).
compile_op(uminus(x(A),x(B)), uminus(A,B)).
compile_op(sr(x(A),I,x(B)), sr_c1(A,B,I)) :- integer(I), !.
compile_op(sr(x(A),x(B),I), sr_c2(A,B,I)) :- integer(I), !.
compile_op(sr(x(A),x(B),x(C)), sr(A,B,C)).
compile_op(sl(x(A),I,x(B)), sl_c1(A,B,I)) :- integer(I), !.
compile_op(sl(x(A),x(B),I), sl_c2(A,B,I)) :- integer(I), !.
compile_op(sl(x(A),x(B),x(C)), sl(A,B,C)).
/*
compile_op(exp(x(A),F,x(B)), exp_c(A,B,F)) :- float(F), !.
compile_op(exp(x(A),I,x(B)), exp_c(A,B,F)) :- integer(I), !, F is truncate(I).
compile_op(exp(x(A),x(B),F), exp_c(A,B,F)) :- float(F), !.
compile_op(exp(x(A),x(B),I), exp_c(A,B,F)) :- integer(I), !, F is truncate(I).
compile_op(exp(x(A),x(B),x(C)), exp(A,B,C)).
compile_op(max(x(A),F,x(B)), max_float_c(A,B,F)) :- float(F), !.
compile_op(max(x(A),I,x(B)), max_int_c(A,B,I)) :- integer(I), !.
compile_op(max(x(A),x(B),F), max_float_c(A,B,F)) :- float(F), !.
compile_op(max(x(A),x(B),I), max_int_c(A,B,I)) :- integer(I), !.
compile_op(max(x(A),x(B),x(C)), max(A,B,C)).
compile_op(min(x(A),F,x(B)), min_float_c(A,B,F)) :- float(F), !.
compile_op(min(x(A),I,x(B)), min_int_c(A,B,I)) :- integer(I), !.
compile_op(min(x(A),x(B),F), min_float_c(A,B,F)) :- float(F), !.
compile_op(min(x(A),x(B),I), min_int_c(A,B,I)) :- integer(I), !.
compile_op(min(x(A),x(B),x(C)), min(A,B,C)).
compile_op(gcd(x(A),I,x(B)), gcd_c(A,B,I)) :- integer(I), !.
compile_op(gcd(x(A),x(B),I), gcd_c(A,B,I)) :- integer(I), !.
compile_op(gcd(x(A),x(B),x(C)), gcd(A,B,C)).
compile_op(unot(x(A),x(B)), unot(A,B)).
compile_op(exp1(x(A),x(B)), exp1(A,B)).
compile_op(log(x(A),x(B)), log(A,B)).
compile_op(log10(x(A),x(B)), log10(A,B)).
compile_op(sqrt(x(A),x(B)), sqrt(A,B)).
compile_op(sin(x(A),x(B)), sin(A,B)).
compile_op(cos(x(A),x(B)), cos(A,B)).
compile_op(tan(x(A),x(B)), tan(A,B)).
compile_op(asin(x(A),x(B)), asin(A,B)).
compile_op(acos(x(A),x(B)), acos(A,B)).
compile_op(atan(x(A),x(B)), atan(A,B)).
compile_op(atan2(x(A),x(B)), atan2(A,B)).
compile_op(sinh(x(A),x(B)), sinh(A,B)).
compile_op(cosh(x(A),x(B)), cosh(A,B)).
compile_op(tanh(x(A),x(B)), tanh(A,B)).
compile_op(asinh(x(A),x(B)), asinh(A,B)).
compile_op(acosh(x(A),x(B)), acosh(A,B)).
compile_op(atanh(x(A),x(B)), atanh(A,B)).
compile_op(floor(x(A),x(B)), floor(A,B)).
compile_op(abs(x(A),x(B)), abs(A,B)).
compile_op(integer(x(A),x(B)), integer(A,B)).
compile_op(truncate(x(A),x(B)), truncate(A,B)).
compile_op(round(x(A),x(B)), round(A,B)).
compile_op(ceiling(x(A),x(B)), ceiling(A,B)).
compile_op(msb(x(A),x(B)), msb(A,B)).
compile_op(random(x(A),x(B)), random(A,B)).
compile_op(lgamma(x(A),x(B)), lgamma(A,B)).
*/