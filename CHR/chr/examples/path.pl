% PATH CONSISTENCY, simple
% thom fruehwirth ECRC 941201, simplified version of time-pc.chr
% 980311 Thom Fruehwirth, LMU, adapted to Sicstus CHR

:- use_module(library(chr)).

handler path.

option(already_in_heads,on).

constraints con/3.
% con(X,Y,C) means that constraint C holds between variables X and Y

intersect_xy_xy @ con(X, Y, C1), con(X, Y, C2) <=> 
	inter(C1, C2, C3),
	con(X,Y,C3).
intersect_xy_yx @ con(X, Y, C1), con(Y, X, CR) <=> 
	invert(CR, C2),  
	inter(C1, C2, C3),
	con(X,Y,C3).

propagate_xy_yz @ con(X, Y, C1), con(Y, Z, C2) ==>
	trans(C1, C2, C3)
 	| 
	con(X, Z, C3).
propagate_xy_xz @ con(X, Y, CR), con(X, Z, C2) ==>
	invert(CR,C1),
	trans(C1, C2, C3)
 	|
	con(Y, Z, C3).
propagate_xy_zy @ con(X, Y, C1), con(Z, Y, CR) ==>
	invert(CR,C2),
	trans(C1, C2, C3)
 	| 
	con(X, Z, C3).


% Example ---------------------------------
% constraints are < and >

invert(<,>).
invert(>,<).

% fail if empty constraint would be produced
inter(C,C,C).

% fail if most general constraint would be produced
trans(C,C,C).

% ?- con(A,B,>),con(A,C,>),con(B,D,>),con(C,D,>).

/*--------------- eof path.pl ----------------------------------------------*/
