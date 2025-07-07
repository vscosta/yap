:- dynamic exported/2, defines_module/1.


%:- use_module(library(lists)).
append([], L, L).
append([H|T], L, [H|R]) :-
       append(T, L, R).
append(ListOfLists, List) :-
%	must_be_list( ListOfLists),
	append_(ListOfLists, List).

append_([], []).
append_([L], L).
append_([L1,L2|Ls], L) :-
	append(L1,L2,LI),
	append_([LI|Ls],L).


:- use_module(library(maplist)).

%:- use_module(library(system)).
%:- use_module(library(matrix)).
%:- use_module(library(readutil)).
%:- use_module(library(lineutils)).

/** @pred split(+ _Line_,- _Split_)

Unify  _Words_ with a set of strings obtained from  _Line_ by
using the blank characters  as separators.
*/
split(String, Strings) :-
	split_at_blank(" 	", Strings, String, []).

/** @pred split(+ _Line_,+ _Separators_,- _Split_)



Unify  _Words_ with a set of strings obtained from  _Line_ by
using the character codes in  _Separators_ as separators. As an
example, consider:

```
?- split("Hello * I am free"," *",S).

S = ["Hello","I","am","free"] ?

no
```

*/
split(String, SplitCodes, Strings) :-
	split_at_blank(SplitCodes, Strings, String, []).

split_at_blank(SplitCodes, More) -->
	[C],
	{ member(C, SplitCodes) }, !,
	split_at_blank(SplitCodes, More).
split_at_blank(SplitCodes, [[C|New]| More]) -->
	[C], !,
	split_(SplitCodes, New, More).
split_at_blank(_, []) --> [].

split_(SplitCodes, [], More) -->
	[C],
	{ member(C, SplitCodes) }, !,
	split_at_blank(SplitCodes,More).
split_(SplitCodes, [C|New], Set) -->
	[C], !,
	split_(SplitCodes, New, Set).
split_(_, [], []) --> [].


split(Text, SplitCodes, DoubleQs, SingleQs, Strings) :-
	split_element(SplitCodes, DoubleQs, SingleQs, Strings, Text, []).

split_element(SplitCodes,  DoubleQs, SingleQs, Strings) -->
    [C],
    !,
    split_element(SplitCodes,  DoubleQs, SingleQs, Strings, C).
split_element(_SplitCodes,  _DoubleQs, _SingleQs, []) --> !.
split_element(_SplitCodes,  _DoubleQs, _SingleQs, [[]]) --> [].


encode_text(S,NS) :-
    sub_string(S,Bef,1,End," "),
    !,
    sub_string(S,0,Bef,_,S1),
    encode(S1,NS1),
    sub_string(S,_,End,0,S2),
    encode_text(S2,NS2),
    string_concat([NS1,' ',NS2], NS).
encode_text(S,NS) :-
    encode(S,NS).


encode(Pred/A,String) :-
    !,  
    pred2dox_(Pred, String0),
    format(string(String),'~s_~d',[String0,A]).

encode("","") :-
    !.
encode(P,S) :-
    pred2dox(P,S),
    !.
    encode(S,S).

decode(P,S) :-
    dox2pred(P,S),
    !.
decode(S,S).


pred2dox(S, String) :-
    sub_string(S,_,2,2,S0),
    string_chars(S0,[C1,C0]),
    char_type_digit(C0),
    (C1 == '/'  ; C1 == '_'),
    !,
    sub_string(S,0,_,2,Name),
    pred2dox_(Name,String0),
    format(string(String),'~s_~s',[String0,[C0]]).
pred2dox(Pred, String) :-
    pred2dox_(Pred, String).

pred2dox_(Pred, String) :-
    string_chars(Pred, Chars),
    maplist(char_type_csym, Chars),
    !,
    String = Pred.
pred2dox_(Pred, String) :-
    string_codes(Pred, Codes),
    foldl(addch,Codes, SF,[]),
    string_concat(["YAP"|SF],String).

addch(Code,[SF|Ss],Ss)  :-
format(string(SF),'~*t~d~8+',[0'0,Code]).

dox2pred(String,Pred) :-
    sub_string(String,0,3,Len,`YAP`),
    sub_string(String,3,Len,0,Codes),
    fetch_chars(Len,Codes,Chars),
    string_concat(Chars,Pred).

fetch_chars(Len,Len,[],[]) :- !.
fetch_chars(This,String,[S|Chars]) :-
    sub_string(String,This,8,Left,S),
    fetch_chars(Left,String,Chars).
