
%conservative_city(nyc, t).

hair_color(joe, t).

car_color(joe, t).

shoe_size(joe, f).

/* Steps:

1. generate N facts lives(I, nyc), 0 <= I < N.

2. generate evidence on descn for N people, *** except for 1 ***

3. Run query ?- guilty(joe, Guilty), witness(joe, t), descn(2,t), descn(3, f), descn(4, f).

query(Guilty) :-
    guilty(joe, Guilty), 
    witness(joe, t),
    descn(2,t),
    descn(3,f), 
    descn(4,f), ....

*/