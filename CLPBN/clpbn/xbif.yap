:- module(xbif, [clpbn2xbif/3]).

clpbn2xbif(Stream, Name, Network) :-
	format(Stream, '<!-- DTD for the XMLBIF 0.3 format -->
<!DOCTYPE BIF [
	<!ELEMENT BIF ( NETWORK )*>
	      <!ATTLIST BIF VERSION CDATA #REQUIRED>
	<!ELEMENT NETWORK ( NAME, ( PROPERTY | VARIABLE | DEFINITION )* )>
	<!ELEMENT NAME (#PCDATA)>
	<!ELEMENT VARIABLE ( NAME, ( OUTCOME |  PROPERTY )* ) >
	      <!ATTLIST VARIABLE TYPE (nature|decision|utility) "nature">
	<!ELEMENT OUTCOME (#PCDATA)>
	<!ELEMENT DEFINITION ( FOR | GIVEN | TABLE | PROPERTY )* >
	<!ELEMENT FOR (#PCDATA)>
	<!ELEMENT GIVEN (#PCDATA)>
	<!ELEMENT TABLE (#PCDATA)>
	<!ELEMENT PROPERTY (#PCDATA)>
]>

<BIF VERSION="0.3">
<NETWORK>
<NAME>~w</NAME>]>


<!-- Variables -->',[Name]),
	output_vars(Stream, Network),
	output_dists(Stream, Network),
	format(Stream, '</NETWORK>
</BIF>
',[]).

output_vars(_, []).
output_vars(Stream, [V|Vs]) :-
	output_var(Stream, V),
	output_vars(Stream, Vs).

output_var(Stream, V) :-
	clpbn:get_atts(V,[key(Key),dist(DInfo)]),
	extract_domain(DInfo,Domain),
	format(Stream, '<VARIABLE TYPE="nature">
	<NAME>~w</NAME>~n',[Key]),
	output_domain(Stream, Domain),
	format(Stream, '</VARIABLE>~n~n',[]).

extract_domain(tab(D,_),D).
extract_domain(tab(D,_,_),D).
extract_domain((_->D),D).

output_domain(_, []).
output_domain(Stream, [El|Domain]) :-
	format(Stream, '	<OUTCOME>~q</OUTCOME>~n',[El]),
	output_domain(Stream, Domain).

output_dists(_, []).
output_dists(Stream, [V|Network]) :-
	output_dist(Stream, V),
	output_dists(Stream, Network).


output_dist(Stream, V) :-
	clpbn:get_atts(V,[key(Key),dist((Info))]),
	format(Stream, '<DEFINITION>
	<FOR>~w</FOR>~n',[Key]),
	output_parents(Stream,Info),
	extract_cpt(Info,CPT),
	output_cpt(Stream,CPT),
	format(Stream, '</DEFINITION>~n~n',[]).

output_parents(_,tab(_,_)).
output_parents(Stream,tab(_,_,Ps)) :-
	do_output_parents(Stream,Ps).
output_parents(Stream,([_|_].Ps->_)) :- !,
	do_output_parents(Stream,Ps).
output_parents(_,(_->_)).

do_output_parents(_,[]).
do_output_parents(Stream,[P1|Ps]) :-
	clpbn:get_atts(P1,[key(Key)]),
	format(Stream, '<GIVEN>~w</GIVEN>~n',[Key]),
	do_output_parents(Stream,Ps).

extract_cpt(tab(_,CPT),CPT).
extract_cpt(tab(_,CPT,_),CPT).	
extract_cpt(([C1|Cs]._->_),[C1|Cs]) :- !.
extract_cpt((CPT->_),CPT).

output_cpt(Stream,CPT) :-
	format(Stream, '	<TABLE> ', []),
	output_els(Stream, CPT),
	format(Stream, '</TABLE>~n', []).

output_els(_, []).
output_els(Stream, [El|Els]) :-
	format(Stream,'~f ',[El]),
	output_els(Stream, Els).

