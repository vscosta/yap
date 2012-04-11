:- use_module(library(pfl)).

:- clpbn_horus:set_solver(fove).
%:- clpbn_horus:set_solver(hve).
%:- clpbn_horus:set_solver(bp).
%:- clpbn_horus:set_solver(cbp).


people(joe,nyc).
people(p2, nyc).
people(p3, nyc).


ev(descn(p2, t)).
ev(descn(p3, t)).

% :- [city_7].

bayes city_conservativeness(C)::[y,n] ; cons_table(C) ; [people(_,C)].

bayes gender(P)::[m,f] ; gender_table(P) ; [people(P,_)].

bayes hair_color(P)::[t,f] , city_conservativeness(C) ; hair_color_table(P) ; [people(P,C)].

bayes car_color(P)::[t,f] , hair_color(P) ; car_color_table(P); [people(P,_)].

bayes height(P)::[t,f] , gender(P) ; height_table(P) ; [people(P,_)].

bayes shoe_size(P):[t,f] , height(P) ; shoe_size_table(P); [people(P,_)].

bayes guilty(P)::[y,n] ; guilty_table(P) ; [people(P,_)].

bayes descn(P)::[t,f] , car_color(P), hair_color(P), height(P), guilty(P) ; descn_table(P) ; [people(P,_)].

bayes witness(C)::[t,f] , descn(Joe) , descn(P2) ; wit_table ; [people(_,C), Joe=joe, P2=p2].


cons_table(amsterdam, [0.2, 0.8]) :- !.
cons_table(_, [0.8, 0.2]).


gender_table(_, [0.55, 0.44]).


hair_color_table(_,
     /* conservative_city */
     /* y     n   */
      [ 0.05, 0.1,
        0.95, 0.9 ]).
    

car_color_table(_,
     /* t    f */
      [ 0.9, 0.2,
        0.1, 0.8 ]).
	

height_table(_,
     /* m    f */
      [ 0.6, 0.4,
        0.4, 0.6 ]).
	

shoe_size_table(_,
     /* t    f */
      [ 0.9, 0.1,
        0.1, 0.9 ]).


guilty_table(_, [0.23, 0.77]).


descn_table(_,
     /* color, hair, height, guilt */
     /* ttttt  tttf  ttft  ttff  tfttt tftf  tfft  tfff  ttttt fttf  ftft  ftff  ffttt fftf  ffft  ffff */
      [ 0.99,  0.5,  0.23, 0.88, 0.41, 0.3, 0.76, 0.87, 0.44, 0.43, 0.29, 0.72, 0.33, 0.91, 0.95, 0.92,
        0.01,  0.5,  0.77, 0.12, 0.59, 0.7, 0.24, 0.13, 0.56, 0.57, 0.61, 0.28, 0.77, 0.09, 0.05, 0.08]).


wit_table([0.2, 0.45, 0.24, 0.34,
           0.8, 0.55, 0.76, 0.66]).


runall(G, Wrapper) :-
    findall(G, Wrapper, L),
    execute_all(L).


execute_all([]).
execute_all(G.L) :-
    call(G),
    execute_all(L).


is_joe_guilty(Guilty) :-
    witness(nyc, t),
    runall(X, ev(X)),
    guilty(joe, Guilty).


% ?- is_joe_guilty(Guilty)

