
int_table(_,T ,[h, m, l]) :- int_table(T).

int_table([0.5,
	0.4,
	0.1]).

	   /* h h   h m  h l  m h  m m  m l  l h  l m l l */
grade_table([
             0.2,  0.7, 0.85, 0.1, 0.2, 0.5, 0.01, 0.05,0.1 ,
	     0.6, 0.25, 0.12, 0.3, 0.6,0.35,0.04, 0.15, 0.4 ,
             0.15,0.04, 0.02, 0.4,0.15,0.12, 0.5, 0.6, 0.4,
	     0.05,0.01, 0.01, 0.2,0.05,0.03, 0.45, 0.2, 0.1 ]).

grade_table(I, D, 
	   /* h h   h m  h l  m h  m m  m l  l h  l m l l */
	    p([a,b,c,d], T, [I,D])) :- grade_table(T).
    
sat_table(
       /*   h a  h b  h c   h d  m a   m b  m c  m d   l a   l b  l c  l d */
/*h*/     [0.98, 0.9, 0.8 , 0.6, 0.9,  0.4, 0.2, 0.01, 0.5,  0.2, 0.01, 0.01,
/*m*/	   0.01, 0.09,0.15, 0.3, 0.05, 0.4, 0.3, 0.04, 0.35, 0.3, 0.09, 0.01 ,
/*l*/	   0.01, 0.01,0.05, 0.1, 0.05, 0.2, 0.5, 0.95, 0.15, 0.5, 0.9,  0.98]).	
/*
  A: professor's ability;
  B: student's grade (for course registration).
*/
satisfaction_table(A, G, p([h,m,l], T, [A,G])) :- sat_table(T).


% The idea is quite simple:
%  hs = h -> r = ( 0.9, 0.1, 0)
%  hs = m -> r = ( 0.2, 0.6, 0.2)
%  hs = l -> r = ( 0, 0.1, 0.9)
%
% add all and divide on the number of elements on the table!
%
rating_prob_table([0.9,0.05,0.01,
		   0.09,0.9,0.09,
		   0.01,0.05,0.9]).

abi_table( [0.50, 0.40,  0.10]).

abi_table( _, T) :- abi_table(T).


pop_table( [0.9, 0.2, 0.01,
           0.09, 0.6, 0.09,
           0.01, 0.2, 0.9]).

pop_table(_, T) :- pop_table(T). 

diff_table([0.25, 0.50, 0.25]).

dif_table(_, T) :- diff_table(T). 

