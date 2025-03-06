:- dynamic exported/2, defines_module/1.

:- 	   op(800, xfx, <==),
	   op(800, xfx, +==),
	   op(800, xfx, -==),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	    op(700, xfx , (in)),
	    op(700, xfx, (within)),
	    op(700, xfx, (ins)),
        op(450, xfx, '..'), % should bind more tightly than \/
	op(790, fx, (matrix)),
	op(790, fx, array),
	op(780, xfx, of),
	op(700, xfx, [?=]),
	op(200, fx, (@)),
	  op(100, yf, []),
                  op(760, yfx, #<==>),
                  op(750, xfy, #==>),
                  op(750, yfx, #<==),
           op(740, yfx, #\/),
                  op(730, yfx, #\),
                  op(720, yfx, #/\),
                  op(710,  fy, #\),
                  op(705, xfx, where),
                  op(700, xfx, #>),
                  op(700, xfx, #<),
                  op(700, xfx, #>=),
                  op(700, xfx, #=<),
                  op(700, xfx, #=),
                  op(700, xfx, #\=),
                  op(700,  xf, #>),
                  op(700,  xf, #<),
                  op(700,  xf, #>=),
                  op(700,  xf, #=<),
                  op(700,  xf, #=),
                  op(700,  xf, #\=),
		  op(500, yfx, '<=>'),
		  op(500, yfx, '=>'),
                  op(450, xfx, ..), % should bind more tightl	   op(100,fy,$),
	   op(950,fy,:=),
	   op(950,yfx,:=),
	    op(50, yf, []),
            op(50, yf, '()'),
            op(100, xfy, '.'),
            op(100, fy, '.').

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

%:- use_module(library(maplist)).
foldl(Goal, List, V0, V) :-
    foldl_(List, Goal, V0, V).

foldl_([], _, V, V).
foldl_([H|T], Goal, V0, V) :-
    call(Goal, H, V0, V1),
    foldl_(T, Goal, V1, V).

maplist(_, []).
maplist(Pred, [In|ListIn]) :-
    call(Pred, In),
    maplist(Pred, ListIn).

maplist(_, [], []).
maplist(Pred, [In|ListIn], [Out|ListOut]) :-
    call(Pred, In, Out),
    maplist(Pred, ListIn, ListOut).

maplist(_, [], [], []).
maplist(Pred, [A1|L1], [A2|L2], [A3|L3]) :-
    call(Pred, A1, A2, A3),
    maplist(Pred, L1, L2, L3).

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


encode(P/A,String) :-
    !,
    format(string(Pred),'~w',[P/A]),
    pred2dox(Pred, String).

encode(P,S) :-
    pred2dox(P,S),
    !.
    encode(S,S).

decode(P,S) :-
    dox2pred(P,S),
    !.
decode(S,S).


pred2dox(Pred, String) :-
    sub_string(Pred,0,LEN,2,Name), 
    string_chars(Name,Cs),
I is LEN+2,
get_string_char(I,Pred,D),
    foldl(char_to_safe,Cs,PCs,['_',D ]),
    !,
    string_chars(String,  PCs).
pred2dox(Pred,Pred).

dox2pred(String,Pred) :-
    sub_string(String,0,LEN,2,Name),
    string_chars(Name,PCs),
I is LEN+2,
get_string_char(I,String,D),
   char_type(D,digit),
   foldl(char_to_safe,ACs,PCs,[]),
   append(ACs,['/',D],Cs),
    !,
    string_chars(Pred,  Cs).
dox2pred(Pred,Pred).

/* translate >= to UNU */
char_to_safe(C,[],[]) :-
    var(C),
    !.
char_to_safe(C,[D],[]) :-
    var(C),
    !,
    C=D.
char_to_safe(C,['U',A,'U'|Next],Next) :-
    var(C),
    !,
     number_chars(CA,['0',x,A]),
    char_code(C,CA).
char_to_safe(C,['U',A,B,'U'|Next],Next) :-
    var(C),
    !,
    number_chars(CA,['0',x,A,B]),
    char_code(C,CA).
char_to_safe(C,[D|NL],NL) :-
    var(C),
    !,
    C=D.
/* translate >= to UNU */
char_to_safe('U',NL,L) :-
    !,
   format(chars(NL, L), 'U~16rU', [0'U]).
char_to_safe(C,[C|L],L) :-
    char_type(C,alnum),
    !.
char_to_safe('_',['_'|L],L) :-
    !.
char_to_safe(C,NL,L) :-
    char_code(C,Code),
   format(chars(NL, L), 'U~16rU', [Code]).
    
