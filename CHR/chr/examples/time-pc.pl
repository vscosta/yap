% PATH CONSISTENCY to be used with time.pl
% thom fruehwirth ECRC 921030,930212,930802,930804,930908,931216,931223
% christian holzbaur 961022 more mods for Sicstus
% thom fruehwirth LMU 980206, 980312

:- use_module(library(chr)).
:- use_module( library('chr/ordering'), [globalize/1,var_compare/3]).

nonground( X) :- \+ ground( X).

handler path_consistency.

constraints arc/4, path/6.
% arc(X,Y,L,T) there is an arc in the constraint network between variables X and Y with constraint L of type T
% path(N,X,Y,L,T,I) there is a path in the constraint network between variables X and Y with constraint L of type T

%% start up
add_path @
arc(X,Y,L,T) <=> ground(L),ground(T),length(L,N) |
	globalize(X-Y),	% attach attribute to vars to have order on them
	path(N,X,Y,L,T,1).

%% ground case
ground @
path(N,X,Y,L,T,I) <=> ground(X-Y-L-T) | path1(N,X,Y,L,T,I).

%% simple cases
empty @
path(N,X,Y,L,T,I) <=> empty(N,L,T) | fail.
universal @
path(N,X,Y,L,T,I) <=> universal(N,L,T) | true.
equality @
path(N,X,X,L,T,I) <=> equality(L,T).
unify @
path(1,X,Y,L,T,I) <=> unique(L),equality(L,T) | X=Y. % can cause problems with var order

%% special cases for finite domains
findom_unique @
path(1,X,Y,L,p-p,I) <=> number(X),unique(L) | bind_value(X,Y,L).
findom_x @
path(N,X,Y,L,p-p,I) <=> number(X),X=\=0
        |
	shift_interval(X,L,L1),
        path(N,0,Y,L1,p-p,I).
findom_y @
path(N,Y,X,L,p-p,I) <=> number(X)
        |
        equality([Eq],p-p),transl(L,L2,[Eq],p-p-p), % invert path
        shift_interval(X,L2,L1),
        path(N,0,Y,L1,p-p,I).

%% intersection (has to come before transitivity)
intersect_xy_xy @
path(N1, X, Y, L1, U-V, I), path(N2, X, Y, L2, U-V, J) <=> % 10
	intersection(L1, L2, L3, U-V),
	length(L3, N3),
	K is min(I, J),
	path(N3, X, Y, L3, U-V, K)
    pragma already_in_heads.
intersect_yx_xy @
path(N1, Y, X, L1, U-V, I), path(N2, X, Y, L, V-U, J) <=> % 11
	equality([Eq], V-V), transl(L, L2, [Eq], V-U-V),  % invert 2nd path
	intersection(L1, L2, L3, U-V),
	length(L3, N3),
	K is min(I, J),
	path(N3, Y, X, L3, U-V, K).

%% transitivity
propagate_xy_yz @
path(N1, X, Y, L1, U-V, I), path(N2, Y, Z, L2, V-W, J) ==>
	nonground(Y),
	J=1, (I=1 -> var_compare( <, X, Z) ; true) % or J=1 or N2=1 or X@<Z
        |
	transl(L1, L2, L3, U-V-W),
	length(L3, M),
	K is I+J,
	path(M, X, Z, L3, U-W, K).
propagate_xy_xz @
path(N1, X, Y, L1, U-V, I), path(N2, X, Z, L3, U-W, J) ==>
	nonground(X),
	min(I, J)=:=1, var_compare( <, Y, Z)  	   % or J=1 or N2=1
        |
	transl(L1, L2, L3, U-V-W),
	length(L2, M),
	K is I+J,
	path(M, Y, Z, L2, V-W, K).
propagate_xy_zy @
path(N1, X, Y, L3, U-V, I), path(N2, Z, Y, L2, W-V, J) ==>
	nonground(Y),
	min(I, J)=:=1, var_compare( <, X, Z)      % or J=1 or N2=1
        |
	transl(L1, L2, L3, U-W-V),
	length(L1, M),
	K is I+J,
	path(M, X, Z, L1, U-W, K).


%% labeling by choice of primitive relation
constraints labeling/0.
labeling, path(N, X, Y, L, T, I)#Id <=> N>1 |	
      member(R, L), 
	path(1, X, Y, [R], T, I),
	  labeling
      pragma passive(Id).
	

/*--------------- eof pc.chr ------------------------------------------------*/
