
conservative_city(City, Cons) :-
	cons_table(City, ConsDist),
	{ Cons = conservative_city(City) with p([y,n], ConsDist) }.


gender(X, Gender) :-
	gender_table(X, GenderDist),
	{ Gender = gender(X) with p([m,f], GenderDist) }.


hair_color(X, Color) :-
	lives(X, City),
	conservative_city(City, Cons),
	hair_color_table(X,ColorTable),
	{ Color = hair_color(X) with
		p([t,f], ColorTable,[Cons]) }.


car_color(X, Color) :-
	hair_color(X, HColor),
	car_color_table(X,CColorTable),
	{ Color = car_color(X) with
		p([t,f], CColorTable,[HColor]) }.


height(X, Height) :-
	gender(X, Gender),
	height_table(X,HeightTable),
	{ Height = height(X) with
		p([t,f], HeightTable,[Gender]) }.


shoe_size(X, Shoesize) :-
	height(X, Height),
	shoe_size_table(X,ShoesizeTable),
	{ Shoesize = shoe_size(X) with
		p([t,f], ShoesizeTable,[Height]) }.


guilty(X, Guilt) :-
	guilty_table(X, GuiltDist),
	{ Guilt = guilty(X) with p([y,n], GuiltDist) }.


descn(X, Descn) :-
	car_color(X, Car),
	hair_color(X, Hair),
	height(X, Height),
	guilty(X, Guilt),
	descn_table(X, DescTable),
	{ Descn = descn(X) with
		p([t,f], DescTable,[Car,Hair,Height,Guilt]) }.


witness(City, Witness) :-
	descn(joe, DescnJ),
	descn(p2, Descn2),
	wit_table(WitTable),
	{ Witness = witness(City) with
		p([t,f], WitTable,[DescnJ, Descn2]) }.


:- ensure_loaded(tables).

