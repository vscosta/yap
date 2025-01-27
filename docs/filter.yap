#!/home/vsc/.local/bin/yap -L --

/** @file filter.yap
 *
 * This filter extends doxygen with YAP documentation support.
 *
 */


:- dynamic exported/2, defines_module/1.

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(system)).
:- use_module(library(matrix)).
:- use_module(library(readutil)).
:- use_module(library(lineutils)).

:- initialization(main).

valid_suffix('.yap').
valid_suffix('.pl').
valid_suffix('.prolog').
valid_suffix('.P').
valid_suffix('.ypp').

/**
 * @pred main
 *
4* Call the filter.
*/

main :-
    unix(argv([File])),
    absolute_file_name(File, Y, [access(read),file_type(prolog),file_errors(fail),solutions(first)]),
    
    %    valid_suffix(ValidSuffix),
    %    sub_atom(Y,_,_,0,ValidSuffix),
    !,
    file_directory_name(File, Dir),
    working_directory(OldD,Dir),
    open(Y,read,S,[alias(loop_stream)]),
    script(S),
    findall(O, entry(S,O), Info),
    predicates(Info, _, Preds, _),
    insert_module_header,
    maplist(output,Preds),
    working_directory(_,OldD),
    insert_module_tail,
    halt.
main.

/*
atom_concat('cat ',File,Command),
unix(system(Command)).


    read_stream_to_string(S,Text),
    format('~s',[Text]).
*/

%%
%% @pred script(+S)
%%
script(S) :-
    peek_char(S,'#'),
    !,
    read_line_to_string(S,_),
    script(S).
script(_).

/**
 * @pred entry(+Stream, -Units).
 * Obtain text units
 */
entry(S,O) :-
    repeat,
    read_clause(S,T,[comments(Comments),variable_names(Vs)]),
    (
      T == end_of_file
      ->
      O = directive(end_of_file, Comments, Vs),
      !
      ;
      T = ( :- Directive )
      ->
      dxpand(Directive),
      O = directive(Directive, Comments, Vs)
      ;

      T = (( Grammar --> Expansion ))
      ->
      functor(Grammar,Name,Arity),      
      A is Arity+2,
      O = clause(Name/A, Grammar, Expansion, Comments, Vs)
      ;
      T = (( Head :- Body ))
      ->
      functor(Head,Name,Arity),
      O = clause(Name/Arity,Head,Body,Comments,Vs)
      ;
      O=clause(Name/Arity,Head,true,Comments,Vs)
    ).

%% initial directive
 % predicates([H|_],_,_,_) :-
 %     writeln(H),
 %     fail.
predicates([],_,[],[]).
predicates([directive(Directive,Comments,Vs)|More],Ctx,
	   [command([comments(Comments),  body(Directive, Vs,Ctx),[]])|Preds], L0) :-
    !,
    predicates(More,0,Preds,L0).
% change of predicate
predicates([clause(N/A,Head,Body,Comments,Vs)|More],Ctx,
	   [predicate(N/A,[comments(Comments), head_body(Head, Body, Vs)|L0])|Preds], L0) :-
    Ctx \= N/A,
    !,
    predicates(More,N/A,Preds,L0).
% new predicate
predicates([clause(N/A,Head,Body,Comments,Vs)|More],[],

	   [predicate(N/A,[comments(Comments), head_body(Head, Body, Vs)|L0])|Preds], L0) :-
    !,
    predicates(More,N/A,Preds,L0).
% predicate continuation
predicates([clause(N/A,Head,Body,Comments,Vs)|More],N/A,
	   Preds, [comments(Comments), head_body(Head, Body, Vs)|L0]) :-
    predicates(More,N/A,Preds,L0).

output(command([comments(Comments) |_])) :-
    maplist(out_comment(_),Comments ).
output(predicate(N/A,[comments(Comments) |_Clauses])) :-
    atom_chars(N,SNs),
    %    foldl(csafe,Ns,SNs,[]),
    format(atom(N1),'P~s~d',[SNs,A]),
    maplist(out_comment(Found),Comments),
    addcomm(N/A,Found),
    findall(I,between(1,A,I),Is),
    maplist(atomic_concat('int ARG'),Is,NIs),
    (
      A==0 ->
      T = N()
      ;
      T =.. [N1|NIs]
    ),
    (
      false,
      is_exported(N,A)
      ->
      format(' class  ~s { ~w;~n};~n~n~n',[ N1,T])
      ;
      true
    ) .

insert_module_header :-
    %    format('class Predicate;~n~n',[]),
    defines_module(M),
    !,
    format('namespace ~s~n{~n',[M]).
insert_module_header.

insert_module_tail :-
    defines_module(_M),
    !,
    format('}~n',[]).
insert_module_tail.



out_comment(true,C) :-
    string_chars(C,Cs),
    trl_comm(Cs,Cf), 
    !,
    format('~s~n',[Cf]).
out_comment(_,_C).

trl_comm( ['/','*','<',C|L],['/','*','<',C|NL]) :-
    sp(C),
    !,
    trl_pred(L,star,NL).
trl_comm( ['/','*','*',C|L],['/','*','*',C|NL]) :-
    sp(C),
    !,
    trl_pred(L,star,NL).
trl_comm( ['%','%','<',C|L],['/','/','/','<',C|NL]) :-
    sp(C),
    %ntrl_extend(Ext, []),
    %append(L,['\n'|Ext],ExtL),
    !,
    trl_pred(L,lines,NL).
trl_comm( ['%','%',C|L],['/','/','/',C|NL]) :-
    sp(C),
    %rl_extend(Ext,[]),
    % append(L,['\n'|Ext],ExtL),
    !,
    trl_pred(L,lines,NL).
trl_comm(_L,[]).


sp(' ').
sp('\t').
sp('\n').

trl_pred(L,_,NL) :-
    trl_pred(L,LL),
    trl_lines(LL,NL).

trl_pred(RL,NL) :-
    append(Start,['@',p,r,e,d,' '|Decl],RL),
    !,
    skip_blanks(Decl,TDecl),
    word(TDecl,W,Args),
    decl(W,Args,DL),
    append(Start,DL,NL).
trl_pred(RL,RL).

trl_lines(RL,NL) :-
    split(RL,['\n'],Lines),
    !,
    maplist(rcomm, RLines, Lines,_),
    append(RLines,NL).

rcomm(    ['/','/','/',C|L]) -->
    ['%','%',C],
    {sp(C)},
    !,
    r_id(L).
    rcomm(['/','/','/','<',C|L]) -->
['%','%',C],
    {sp(C)},
    !,
    
    r_id(L).
rcomm(  ['/','/',C|L]) -->
   ['%',C],
{sp(C)},
!,
    r_id(L).

rcomm(L) -->
    r_id(L).




%before atom

r_id(Cs) -->
    [A],
    {char_type_lower(A)},                 
    !,
    r_id_atom(At),
    r_id2(Cs,[A|At],RCs),
    r_id(RCs).
r_id([A|Cs]) -->
    [A],
    r_id(Cs).
r_id(['\n']) --> [].
 
r_id2(Cs,A0,RCS) -->
    ['/',D],
    {char_type_digit(D)},
    !,
    { format(chars(Cs,RCS),' @ref P~s~s \"~s/~s\" ',[A0,[D],A0,[D]]) }.
r_id2(Cs, A0,Left) -->
    {append(A0,Left,Cs)}.


r_id_atom([A|A0]) -->
    [A],
    { char_type_csym(A) },
    !,
    r_id_atom(A0).
r_id_atom([]) --> [].

scan_line([],[],[]).
scan_line(['\n'|R],[],R) :-
    !.
scan_line([C|Cs],[C|M],R) :-
    scan_line(Cs,M, R).

skip_blanks([C|Cs], R) :-
    sp(C),
    !,
    skip_blanks(Cs,R).
skip_blanks(Cs,Cs).

word([C|Cs],[C|W],R) :-
    alphanum(C),
    !,
    word(Cs,W,R).
word(R,[],R).

decl(Name,['('|D1],F) :-
    !,
    append(Info,[')'|R1],D1),
    foldl(arity,Info,1,Arity),
%    foldl(csafe,Name,TName,[]),
    format(chars(F,R1F),'@class P~s~d		~n \"~s(~s)\"',  [Name,Arity,Name,Info]),
    trl_pred(R1,R1F).
decl(Name,[C|R1],F) :-
    sp(C),
    !,
    %foldl(csafe,Name,TName,[]),
    format(chars(F,R1F),'@class P~s0		~n ~s',[Name,Name]),
    trl_pred(R1,R1F).

    /* This routine generates two streams from the comment:
 * - the class text.
 * - The source
 */
arity(',',I, I1) :-
    !,
    I1 is I+1.
arity(_,I,I).

csafe(C,LF,L0) :-
    char_to_safe(C,LF,L0),
    !.
csafe(C,[C|L],L).

alphanum(A) :-
    char_type(A,csym),
    !.
alphanum(':').

char_to_safe('=',['_',e,q|L],L).
char_to_safe('<',['_',l,t|L],L).
char_to_safe('>',['_',g,t|L],L).
%char_to_safe('_',['_','_'|L],L).
char_to_safe('!',['_',c,t|L],L).
char_to_safe('-',['_',m,n|L],L).
char_to_safe('+',['_',p,l|L],L).
char_to_safe('*',['_',s,t|L],L).
char_to_safe('/',['_',s,l|L],L).
char_to_safe('$',['_',d,l|L],L).

addcomm(N/A,false) :-
    is_exported(N,A),
    !,
    length(L,A),
    maplist(=('?'),L),
    T =.. [N|L],
    format('~n~n/**   @class P~s~d	~n ~w  @details (undocumented)  **/~n~n~n~n',[N,A,T]).
addcomm(_,_).

:- initialization(main).



dxpand(module(M,Gs)) :-
   module(M),
    assert(defines_module(M)),
    maplist(dxpand,Gs).
dxpand(op(M,Gs,Y)) :-
    op(M,Gs,Y).
dxpand(use_module(M,Gs)) :-
    (:-use_module(M,Gs)).
dxpand(module(user)) :- !.
dxpand(module(M)) :-
    ensure_loaded(M).
dxpand(A/B) :-assert(exported(A,B)).
dxpand(A//B):- B2 is B+2, assert(exported(A,B2)) .

is_exported(N,_) :-
    string(N),
    string_concat(`$`,_,N),
    !,
    fail.
is_exported(N,_) :-
    atom(N),
    sub_atom(N,0,1,_,'$'),
    !,
    fail.
is_exported(_,_) :-
    current_source_module(user,user),
    !.
is_exported(N,A) :-
    exported(N,A).

