% Thom Fruehwirth, LMU, 980129, 980311

:- use_module(library(chr)).

handler pathc.

option(already_in_heads,on).

constraints c/3.
% c(X,Y,N): the distance between variables X and Y is the positive number N

      c(I,J,A),c(I,J,B) <=> C is min(A,B), c(I,J,C).
      c(I,J,A),c(J,K,B) ==> C is A+B, c(I,K,C).

% Only complete if both c(I,J,D) and c(J,I,D) are present for each constraint

/*
% Queries

c(A,B,D).

c(A,B,2),c(A,B,4).

c(A,B,2),c(B,C,3).

c(A,B,2),c(B,A,1).

c(A,B,2),c(B,A,0).

c(A,B,2),c(A,C,3),c(C,B,2).

c(A,B,2),c(A,C,3),c(C,B,4).

c(A,B,2),c(B,C,3),c(C,A,4).

c(A,B,2),c(B,C,3),c(C,A,4),c(B,A,2),c(C,B,3),c(A,C,4).

*/
