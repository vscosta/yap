% Thom Fruehwirth, LMU, 980129ff, 980312, 980611, 980711

:- use_module( library(chr)).

handler interval.

option(debug_compile,off).
option(already_in_store, off).
option(check_guard_bindings, off).
option(already_in_heads, off). 

% for domain constraints
operator( 700,xfx,'::').
%operator( 600,xfx,':'). % operator already defined in Sicstus Prolog

% for inequality constraints
%operator( 700,xfx,lt). % not implemented
operator( 700,xfx,le).
operator( 700,xfx,ne).
operator( 700,xfx,eq). 

constraints (::)/2, le/2, eq/2, ne/2, add/3, mult/3.
% X::Min:Max - X is between the numbers Min and Max, inclusively
% X must always be a unbound variable (!), and Min and Max evaluable
% (i.e. ground) arithmetic expressions (or numbers)
constraints int/1.
% int(X) says that X is an integer (default is a real)
constraints bool/1.
% bool(X) says that X is a boolean (default is a real)

constraints browse/1.
% watch how domain of X evolves
browse(X), X::A:B ==> write((X::A:B)),nl.

% define the smallest intervals you want to get:
% the smaller, the more precise, the longer the computation
    small(A:B):- A+2.0e-05>=B. 

% Intersection  -------------------------------

redundant @ X::A:B \ X::C:D <=> %var(X),
	(C=<A, B=<D ; A<B,small(A:B), C<D,small(C:D)) 
        |   
        true.

intersect @ X::A:B , X::C:D <=> %var(X) | 
        X::max(A,C):min(B,D).

% Special Cases  -------------------------------

failure @ X::A:B <=> A>B | fail.

compute @ X::A:B <=> \+ (number(A),number(B)) | C is A, D is B, X::C:D.

integer @ int(X), X::A:B ==> \+ (integer(A),integer(B)) | 
      C is integer(ceiling(float(A))), D is integer(floor(float(B))), X::C:D.

bool @ bool(X), X::A:B ==> B<1 | X::0:0.
bool @ bool(X), X::A:B ==> A>0 | X::1:1.
bool @ bool(X) ==> X::0:1.

% Inequality -------------------------------

(le) @ X le Y, X::A:B, Y::C:D ==> Y::A:D, X::A:D.
(eq) @ X eq Y, X::A:B, Y::C:D ==> Y::A:B, X::C:D.
(ne) @ X ne Y, X::A:A, Y::A:A <=> fail.

(ne_int) @ int(X) \ X ne Y, X::A:B <=> A=Y | X::A+1:B.
(ne_int) @ int(X) \ X ne Y, X::A:B <=> B=Y | X::A:B-1.
(ne_int) @ int(X) \ Y ne X, X::A:B <=> A=Y | X::A+1:B.
(ne_int) @ int(X) \ Y ne X, X::A:B <=> B=Y | X::A:B-1.

% Addition X+Y=Z -------------------------------

add @ add(X,Y,Z), X::A:B, Y::C:D, Z::E:F ==>
	 X::E-D:F-C, Y::E-B:F-A, Z::A+C:B+D.

% Multiplication X*Y=Z -------------------------------

         mitnull(A:B) :- A=<0, 0=<B.

mult_z @ mult(X,Y,Z), X::A:B, Y::C:D ==> 
         M1 is A*C, M2 is A*D, M3 is B*C, M4 is B*D,
         Z::min(min(M1,M2),min(M3,M4)):max(max(M1,M2),max(M3,M4)).

mult_y @ mult(X,Y,Z), X::A:B, Z::E:F ==>
         \+ mitnull(A:B) |
         M1 is E/A, M2 is E/B, M3 is F/A, M4 is F/B,
         Y::min(min(M1,M2),min(M3,M4)):max(max(M1,M2),max(M3,M4)).
mult_x @ mult(Y,X,Z), X::A:B, Z::E:F ==>
         \+ mitnull(A:B) |
         M1 is E/A, M2 is E/B, M3 is F/A, M4 is F/B,
         Y::min(min(M1,M2),min(M3,M4)):max(max(M1,M2),max(M3,M4)).

mult_xyz @ mult(X,Y,Z), X::A:B, Y::C:D, Z::E:F ==>
         mitnull(A:B), mitnull(C:D), \+ mitnull(E:F) |
         (A*C<E -> D>0, X::E/D:B ; true),
         (B*D<E -> C<0, X::A:E/C ; true),
         (F<A*D -> C<0, X::F/C:B ; true),
         (F<B*C -> D>0, X::A:F/D ; true).

% Labeling --------------------------------------------------------

constraints split0/1.
constraints split/1.
% repeated split/1:
constraints label/1.

label @ split0(X), X::A:B <=> \+ small(A:B), A<0,0<B |
	       (X::A:0 ; X::0:B). 

label @ split(X), X::A:B <=> \+ small(A:B) |
	       Half is (A+B)/2,
	       (X::A:Half ; X::Half:B).

label @ label(X), X::A:B <=> \+ small(A:B) |
	       Half is (A+B)/2,
	       (X::A:Half ; X::Half:B), 
	       label(X).    



% EXAMPLES ================================================================

/*

?- X::3:5,X::2:4.

X::3:4 ? 

?- X::3:5, Y::2:4, X=Y.

Y = X,
X::3:4 ? 

?- X::3:3.

X::3:3 ? 

?- X le Y, X::3:5,X::2:4.

X le Y,
X::3:4 ? 

?- X le Y, X::3:5, Y::3:5.

X le Y,
X::3:5,
Y::3:5 ? 

?- X le Y, X::3:5, Y::2:4.

X le Y,
Y::3:4,
X::3:4 ? 

?- add(X,Y,Z), X::2:5, Y::3:4, Z::1:7.

Y::3:4,
Z::5:7,
X::2:4,
add(X,Y,Z)?

?- mult(X,Y,Z), X:: -2:3, Y:: -3:4, Z::7:12.

Z::7:12,
X::1.75:3,
Y::2.3333333333333335:4.0,
mult(X,Y,Z) ? ;

?-  mult(X,Y,Z), X:: -2:3, Y:: -3:4, Z:: -12: -9.

?- A::(-3):3, B::(-3):3, C::4:4,  mult(A,B,C), A eq B.

?- A::(-3):3, B::(-3):3, C::4:4,  mult(A,B,C), A eq B, split(A).

?- int(A), A::(-3):3, B::(-3):3, C::4:4,  mult(A,B,C), A eq B, split(A).

?- A::(-3):3, B::(-3):3, C::4:4,  mult(A,B,C), A eq B, 
     split(A),split(A),split(A),split(A).

?- A::(-3):3, B::(-3):3, C::4:4,  mult(A,B,C), A eq B, label(A).

?- int(A),int(B),int(C), mult(A,B,C), A::0:0.3, B::0:0.3, C::0:0.3, 
     A le C, B le C, C le A, C le B, A le B, B le A.

?- int(A),int(B),int(C), mult(A,B,C), A::0:0.3, B::0:0.3, C::0:0.3, 
     A eq B, B eq C.

?- mult(A,B,C), A::0:0.3, B::0:0.3, C::0:0.3, A eq B, B eq C.

A eq B,
B eq C,
C::0.0:4.304672099999998e-9,
B::0.0:4.304672099999998e-9,
A::0.0:4.304672099999998e-9,
mult(A,B,C) ? ;

?-  mult(A,B,C), A::0:0.3, B::0:0.3, C::0:0.3, A le C.

B::0:0.3,
A le C,
C::0.0:1.9682999999999995e-5,
A::0:1.9682999999999995e-5,
mult(A,B,C) ? ;

?- mult(A,B,C), A::(-0.3):0.3, B::(-0.3):0.3, C::(-0.3):0.3, A eq C.

B:: -0.3:0.3,
A eq C,
C:: -5.9048999999999996e-6:5.9048999999999996e-6,
A:: -5.9048999999999996e-6:5.9048999999999996e-6,
mult(A,B,C) ? ;

?- mult(A,B,C), A::(-3):3, B::(-3):3, C::(-3):3, A eq C.
% solutions A=C=0 or B=1, impossible to enumerate

A:: -3:3,
B:: -3:3,
C:: -3:3,
A eq C,
mult(A,B,C) ? ;

?- mult(A,B,AB), A eq B, add(AB,C,F), F::5:5,
     mult(C,D,CD), C eq D, add(CD,A,G), G::3:3,
     A:: -10:10, B:: -10:10, C:: -10:10, D:: -10:10,
     split0(A),split0(C).

?- int(A),
     mult(A,B,AB), A eq B, add(AB,C,F), F::5:5,
     mult(C,D,CD), C eq D, add(CD,A,G), G::3:3,
     A:: -10:10, B:: -10:10, C:: -10:10, D:: -10:10,
     label(A).

?- mult(A,B,AB), A eq B, add(AB,C,F), F::5:5,
     mult(C,D,CD), C eq D, add(CD,A,G), G::3:3,
     A:: -10:10, B:: -10:10, C:: -10:10, D:: -10:10,
     label(A).

*/

% end of handler interval ===================================================