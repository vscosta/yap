%
% XMLBIF support for CLP(BN)
%

:- module(xbif,
		[clpbn2xbif/3]).

:- use_module(library('clpbn/dists'),
		[get_dist_domain/2]).

clpbn2xbif(Stream, Name, Network) :-
	format(Stream, '<?xml version="1.0" encoding="US-ASCII"?>


<!--
	Bayesian network in XMLBIF v0.3 (BayesNet Interchange Format)
	Produced by CLP(BN)
-->



<!-- DTD for the XMLBIF 0.3 format -->
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
<NAME>~w</NAME>

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
	clpbn:get_atts(V,[key(Key),dist(Id,_)]),
	get_dist_domain(Id, Domain),
	format(Stream, '<VARIABLE TYPE="nature">
	<NAME>',[]),
	output_key(Stream,Key),
	format('</NAME>~n',[]),
	output_domain(Stream, Domain),
	format(Stream, '</VARIABLE>~n~n',[]).

output_domain(_, []).
output_domain(Stream, [El|Domain]) :-
	format(Stream, '	<OUTCOME>~q</OUTCOME>~n',[El]),
	output_domain(Stream, Domain).

output_dists(_, []).
output_dists(Stream, [V|Network]) :-
	output_dist(Stream, V),
	output_dists(Stream, Network).


output_dist(Stream, V) :-
	clpbn:get_atts(V,[key(Key),dist(_,CPT,Parents)]),
	format(Stream, '<DEFINITION>
	<FOR>',[]),
	output_key(Stream, Key),
	format('</FOR>~n',[]),
	output_parents(Stream,Parents),
	output_cpt(Stream,CPT),
	format(Stream, '</DEFINITION>~n~n',[]).

output_parents(_,[]).
output_parents(Stream,[P1|Ps]) :-
	clpbn:get_atts(P1,[key(Key)]),
	format(Stream, '<GIVEN>',[]),
	output_key(Stream,Key),
	format('</GIVEN>~n',[]),
	output_parents(Stream,Ps).

output_cpt(Stream,CPT) :-
	format(Stream, '	<TABLE> ', []),
	output_els(Stream, CPT),
	format(Stream, '</TABLE>~n', []).

output_els(_, []).
output_els(Stream, [El|Els]) :-
	format(Stream,'~f ',[El]),
	output_els(Stream, Els).

output_key(Stream, Key) :-
	output_key(Stream, 0, Key).

output_key(Stream, _, Key) :-
	primitive(Key), !,
	write(Stream, Key).
output_key(Stream, I0, Key) :-
	Key =.. [Name|Args],
	write(Stream, Name),
	I is I0+1,
	output_key_args(Stream, I, Args).

output_key_args(_, _, []).
output_key_args(Stream, I, [Arg|Args]) :-
	format(Stream, '~*c', [I,0'_]),
	output_key(Stream, I, Arg),
	output_key_args(Stream, I, Args).

