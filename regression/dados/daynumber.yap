days(Y,M,D,Total) :-
	years_to_days(Y, D1),
	months_to_day(M, Y, D2),
	Total is D1+D2+D-1.

years_to_days(Y0, DY) :-
	Y1 is Y0-1900,
	DY is Y1*365+((Y1+3)//4).

months_to_day(1,_,0).
months_to_day(2,_,31).
months_to_day(3,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+ED.
months_to_day(4,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+ED.
months_to_day(5,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+ED.
months_to_day(6,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+ED.
months_to_day(7,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+ED.
months_to_day(8,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+31+ED.
months_to_day(9,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+31+31+ED.
months_to_day(10,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+31+31+30+ED.
months_to_day(11,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+31+31+30+31+ED.
months_to_day(12,Y,DM) :-
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+31+31+30+31+30+ED.
months_to_day(13,Y,DM) :- % should never succeed...
	extra_day(Y,ED),
	DM is 31+28+31+30+31+30+31+31+30+31+30+31+ED.

extra_day(Y,1) :- Y mod 4 == 0, !.
extra_day(_,0).

date(Total, Y, M, D) :-
	get_years(Total, Y, DaysLeft),
	get_months(DaysLeft, Y, M, D0),
	D is D0+1.

get_years(Total, Y, DaysLeft) :-
	daysperyear(Y,D0),
	D0 =< Total,
	Y1 is Y+1,
	daysperyear(Y1,D1),
	D1 > Total, !,
	DaysLeft is Total-D0.	

get_months(Total, Y, Month, Days) :-
	months_to_day(Month, Y, DM),
	DM =< Total,
	Month1 is Month+1,
	months_to_day(Month1, Y, DM1),
	DM1 > Total, !,
	Days is Total-DM.	
	
gendays(120,_) :- !.
gendays(I0,D0) :- !,
	J is I0+1900,
	format('days(~d,~d).~n',[J,D0]),
	( I0 mod 4 =:= 0 -> D is D0+366 ; D is D0+365 ),
	I is I0+1,
	gendays(I, D).

daysperyear(1900,0).
daysperyear(1901,366).
daysperyear(1902,731).
daysperyear(1903,1096).
daysperyear(1904,1461).
daysperyear(1905,1827).
daysperyear(1906,2192).
daysperyear(1907,2557).
daysperyear(1908,2922).
daysperyear(1909,3288).
daysperyear(1910,3653).
daysperyear(1911,4018).
daysperyear(1912,4383).
daysperyear(1913,4749).
daysperyear(1914,5114).
daysperyear(1915,5479).
daysperyear(1916,5844).
daysperyear(1917,6210).
daysperyear(1918,6575).
daysperyear(1919,6940).
daysperyear(1920,7305).
daysperyear(1921,7671).
daysperyear(1922,8036).
daysperyear(1923,8401).
daysperyear(1924,8766).
daysperyear(1925,9132).
daysperyear(1926,9497).
daysperyear(1927,9862).
daysperyear(1928,10227).
daysperyear(1929,10593).
daysperyear(1930,10958).
daysperyear(1931,11323).
daysperyear(1932,11688).
daysperyear(1933,12054).
daysperyear(1934,12419).
daysperyear(1935,12784).
daysperyear(1936,13149).
daysperyear(1937,13515).
daysperyear(1938,13880).
daysperyear(1939,14245).
daysperyear(1940,14610).
daysperyear(1941,14976).
daysperyear(1942,15341).
daysperyear(1943,15706).
daysperyear(1944,16071).
daysperyear(1945,16437).
daysperyear(1946,16802).
daysperyear(1947,17167).
daysperyear(1948,17532).
daysperyear(1949,17898).
daysperyear(1950,18263).
daysperyear(1951,18628).
daysperyear(1952,18993).
daysperyear(1953,19359).
daysperyear(1954,19724).
daysperyear(1955,20089).
daysperyear(1956,20454).
daysperyear(1957,20820).
daysperyear(1958,21185).
daysperyear(1959,21550).
daysperyear(1960,21915).
daysperyear(1961,22281).
daysperyear(1962,22646).
daysperyear(1963,23011).
daysperyear(1964,23376).
daysperyear(1965,23742).
daysperyear(1966,24107).
daysperyear(1967,24472).
daysperyear(1968,24837).
daysperyear(1969,25203).
daysperyear(1970,25568).
daysperyear(1971,25933).
daysperyear(1972,26298).
daysperyear(1973,26664).
daysperyear(1974,27029).
daysperyear(1975,27394).
daysperyear(1976,27759).
daysperyear(1977,28125).
daysperyear(1978,28490).
daysperyear(1979,28855).
daysperyear(1980,29220).
daysperyear(1981,29586).
daysperyear(1982,29951).
daysperyear(1983,30316).
daysperyear(1984,30681).
daysperyear(1985,31047).
daysperyear(1986,31412).
daysperyear(1987,31777).
daysperyear(1988,32142).
daysperyear(1989,32508).
daysperyear(1990,32873).
daysperyear(1991,33238).
daysperyear(1992,33603).
daysperyear(1993,33969).
daysperyear(1994,34334).
daysperyear(1995,34699).
daysperyear(1996,35064).
daysperyear(1997,35430).
daysperyear(1998,35795).
daysperyear(1999,36160).
daysperyear(2000,36525).
daysperyear(2001,36891).
daysperyear(2002,37256).
daysperyear(2003,37621).
daysperyear(2004,37986).
daysperyear(2005,38352).
daysperyear(2006,38717).
daysperyear(2007,39082).
daysperyear(2008,39447).
daysperyear(2009,39813).
daysperyear(2010,40178).
daysperyear(2011,40543).
daysperyear(2012,40908).
daysperyear(2013,41274).
daysperyear(2014,41639).
daysperyear(2015,42004).
daysperyear(2016,42369).
daysperyear(2017,42735).
daysperyear(2018,43100).
daysperyear(2019,43465).
