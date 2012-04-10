
bayes conservative_city(C)::[y,n] ; cons_table(C) ; [city(C)].

bayes gender(P)::[m,f] ; gender_table(P) ; [people(P,_)].

bayes hair_color(P)::[t,f] , conservative_city(C) ; hair_color_table(P) ; [people(P,C)].

bayes car_color(P)::[t,f] , hair_color(P) ; car_color_table(P); [people(P,_)].

bayes height(P)::[t,f] , gender(P) ; height_table(P) ; [people(P,_)].

bayes shoe_size(P):[t,f] , height(P) ; shoe_size_table(P); [people(P,_)].

bayes guilty(P)::[y,n] ; guilty_table(P) ; [people(P,_)].

bayes descn(P)::[t,f] , car_color(P), hair_color(P), height(P), guilty(P) ; descn_table(P) ; [people(P,_)].

bayes witness(C)::[t,f] , descn(Joe) , descn(P2) ; wit_table ; [city(C), Joe=joe, P2=p2].

:- ensure_loaded(tables).

