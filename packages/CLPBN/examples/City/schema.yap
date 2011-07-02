
/* base file for school database. Supposed to be called from school_*.yap */

conservative_city(City, Cons) :-
	cons_table(City, ConsDist),
	{ Cons = cons(City) with p([y,n], ConsDist)   }.

gender(X, Gender) :-
	gender_table(City, GenderDist),
	{ Gender = gender(City) with p([m,f], GenderDist)   }.

hair_color(X, Color) :-
	lives(X, City),
	conservative_city(City, Cons),
	gender(X, Gender),
	color_table(X,ColorTable),
	{ Color = color(X) with
		p([t,f], ColorTable,[Gender,Cons]) }.

car_color(X, Color) :-
	hair_color(City, HColor),
	ccolor_table(X,CColorTable),
	{ Color = ccolor(X) with
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
	guilt_table(X, GuiltDist),
	{ Guilt = guilt(X) with p([y,n], GuiltDist)   }.


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
	descn(1, Descn1),
	wit_table(WitTable),
	{ Witness = wit(City) with
		p([t,f], WitTable,[DescnJ, Descn1]) }.


	