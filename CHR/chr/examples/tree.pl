% rational (finite and infinite) tree handler
% 931119 thom fruehwirth ECRC for eclipse CHR
% 980130, 980312 thom fruehwirth LMU for sicstus CHR, now simpler and better

:- use_module(library(chr)).

% need global order on variables
:- use_module( library('chr/ordering'), [globalize/1,var_compare/3]).
% var is smaller than any non-var term
lt(X,Y):- (var(X),var(Y) -> globalize(X),globalize(Y),var_compare(<,X,Y) ; X@<Y).
le(X,Y):- (var(X) -> true ; X@=<Y).


handler tree.

option(debug_compile,on).
option(already_in_store, off).  
option(already_in_heads, off).   
option(check_guard_bindings, off).

constraints (~)/2, ('#~')/2.
% T1 ~ T2 means: term T1 is syntactically equal to term T2
% T1 #~ T2 means: term T1 is syntactically different from term T2

operator(700,xfx,(~)).
operator(700,xfx,('#~')).

ident @ T ~ T <=> true.
decompose @ T1 ~ T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),     
		T1=..[F|L1],T2=..[F|L2],
		equate(L1,L2).
orient @ T ~ X <=> lt(X,T) | X ~ T.   
simplify @ X ~ T1 \ X ~ T2 <=> le(T1,T2) | T1 ~ T2.

  same_functor(T1,T2):- functor(T1,F,N),functor(T2,F,N).

  equate([],[]).
  equate([X|L1],[Y|L2]):- X ~ Y, equate(L1,L2).


ident @ T #~ T <=> fail.
decompose @ T1 #~ T2 <=> nonvar(T1),nonvar(T2) | 
		(same_functor(T1,T2) ->
		T1=..[F|L1],T2=..[F|L2],
		not_equate(L1,L2)
		;
		true).
orient @ T #~ X <=>  lt(X,T) | X #~ T. 
simplify @ X ~ T1 \ X #~ T2 <=> T1 #~ T2 
                 pragma already_in_heads.

constraints not_equate/2, label/0.

not_equate([],[]) <=> fail.
not_equate([X],[Y]) <=> X #~ Y.
not_equate([X|L1],[X|L2]) <=> fail.
not_equate([X|L1],[Y|L2]), X~Y <=> not_equate(L1,L2).
% not_equate([X|L1],[Y|L2]) <=> ground(X#~Y) | X #~ Y -> true ; not_equate(L1,L2).

label, not_equate([X|L1],[Y|L2])#Id <=> true |  
        (X #~ Y ; X ~ Y, not_equate(L1,L2)),
	label 
    pragma passive(Id).      
                  

/*

% EXAMPLES ------------------------------------------------------------

write(example:10), X#~a,X~b;

write(example:11), A~B,B~C,C~D,D~A;

write(example:12), A~B,B~C,C~D,D#~A;


write(example:20), A~g(A),A~g(g(A));

write(example:21), A#~g(A),A~g(g(A));

write(example:22), A~g(A),A#~g(g(A));

write(example:23), A~g(g(g(A))),A~g(g(A));

write(example:24), A~g(g(g(A))),g(A)~g(g(A));

write(example:25), A#~g(g(g(A))),A~g(g(A));

write(example:26), A~g(g(g(A))),A#~g(g(A));


write(example:30), X~f(T1),T2~f(T2),X~T1,X~T2;

write(example:31), f(X)~T1,T2~f(T2),X~T1,X~T2;

write(example:32), X~f(T1),T1~f(T2),X~T1,X~T2;

write(example:33), f(X)~T1,f(T1)~T2,X~T1,X~T2;

write(example:34), T1~f(T1),T2~f(T2),X~T1,X~T2;


write(example:40), A~f(X),B~f(Y),Y~Z,Z~X,A#~B;

write(example:41), A~X,B~Y,Y~Z,Z~X,A#~B;

write(example:42), X~f(X,Y),X#~f(X,Y);

write(example:43), X~f(X,Y),X#~f(Y,X);

write(example:44), X~f(X,Y),X#~f(Y,X),label;

write(example:45), X~f(A,B),X#~f(a,b),A~a,B~b;

write(example:46), X~f(A,B),X#~f(a,b),A~a,B~b,label;


write(example:50), [X|L] ~ [X,X|L], L ~ [X,X,X,X|Z];

write(example:51), [X|L]#~[X,X|L], L ~ [X,X,X,X|Z],label;

write(example:52), [X|L] ~ [X,X|L], L#~[X,X,X,X|Z],label;

write(example:53), L ~ [X,X|L], L ~ [A,B,C,D];


write(done).


% results of current version with local already_in_heads pragma

example:10
X~b ? ;
example:11
B~A,
A~C,
D~C ? ;
example:12example:20
A~g(A) ? ;
example:21
A#~g(A),
A~g(g(A)) ? ;
example:22example:23
A~g(A) ? ;
example:24
A~g(A) ? ;
example:25
A~g(g(A)),
A#~g(A) ? ;
example:26
A~g(g(g(A))),
A#~g(g(A)) ? ;
example:30
T1~X,
X~T2,
T2~f(T1) ? ;
example:31
T2~f(T2),
T1~X,
X~T2 ? ;
example:32
T1~X,
X~T2,
T2~f(T2) ? ;
example:33
T2~f(T1),
T1~X,
X~T2 ? ;
example:34
T1~X,
X~T2,
T2~f(T1) ? ;
example:40example:41example:42example:43
X~f(X,Y),
not_equate([X,Y],[Y,X]) ? ;
example:44
label,
X~f(X,Y),
Y#~X ? ;
example:45
X~f(A,B),
not_equate([A,B],[a,b]),
A~a,
B~b ? ;
example:46example:50
L~Z,
Z~[X|Z] ? ;
example:51
label,
L~[X,X,X,X|Z],
Z#~[X|Z] ? ;
example:52
label,
L~[X|L],
Z#~L ? ;
example:53done
true ? 

*/


% end of handler tree
