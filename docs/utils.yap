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
	split_at_blank(SplitCodes, More).
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


encode(P,S) :-
    pred2dox(P,S),
    !.
encode(S,S).

decode(P,S) :-
    dox2pred(P,S),
    !.
decode(S,S).

pi2dox(P/A, String) :-
    format(string(Pred),'~w',[P/A]),
    pred2dox(Pred, String).

pred2dox(Pred, String) :-

    string_chars(Pred,Cs),
    foldl(csafe,Cs,PCs,[]),
    string_chars(String,['P'|PCs]).

dox2pred(String,Pred) :-
    string_chars(String,['P'|Cs]),
    rcov(PCs,Cs),
    string_chars(Pred,PCs).

    
    
csafe(C,LF,L0) :-
    char_to_safe(C,LF,L0),
    !.
csafe(C,[C|L],L).

rcov([D],[D]) :-
    !,
    char_type(D,digit).
rcov([C|NL],L) :-
    char_to_safe(C,L,L0),
    !,
    rcov(NL,L0).
rcov([C|L0],[C|NL]) :-
    rcov(L0,NL).


char_to_safe('=',['_',e,q|L],L).
char_to_safe('<',['_',l,t|L],L).
char_to_safe('>',['_',g,t|L],L).
%char_to_safe('_',['_','u','l'|L],L).
char_to_safe('!',['_',c,t|L],L).
char_to_safe('-',['_',m,n|L],L).
char_to_safe('+',['_',p,l|L],L).
char_to_safe('*',['_',s,t|L],L).
char_to_safe('/',['_',s,l|L],L).
char_to_safe('\\',['_',b,k|L],L).
char_to_safe('$',['_',d,l|L],L).
char_to_safe('[',['_',o,s|L],L).
char_to_safe(']',['_',l,s|L],L).
char_to_safe('^',['_',h,t|L],L).
char_to_safe('%',['_',p,c|L],L).
char_to_safe('&',['_',t,e|L],L).
char_to_safe('(',['_',o,b|L],L).
char_to_safe(')',['_',l,b|L],L).
char_to_safe('.',['_',d,t|L],L).
char_to_safe(',',['_',c,m|L],L).
char_to_safe(';',['_',s,c|L],L).
char_to_safe('|',['_',v,b|L],L).
char_to_safe('\'',['_',q,t|L],L).
char_to_safe('\"',['_',d,q|L],L).
char_to_safe('`',['_',b,q|L],L).
char_to_safe('#',['_',s,q|L],L).
char_to_safe('@',['_',a,t|L],L).
char_to_safe('?',['_',q,m|L],L).
char_to_safe(':',['_',c,o|L],L).
