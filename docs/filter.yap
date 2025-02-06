#!/home/vsc/.local/bin/yap -L --

/** @file filter.yap
 *
 * This filter extends doxygen with YAP documentation support.
 *
 */

:- include(utils).

:-dynamic pred_found/2.

:- initialization(main).

:- add_to_path('../pl').

:- set_prolog_flag(double_quotes, string).

valid_suffix('.yap').
valid_suffix('.pl').
valid_suffix('.prolog').
valid_suffix('.P').
valid_suffix('.ypp').

/**
 * @pred main
 *
 * Call the filter.
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
     readutil:read_line_to_string(S,_),
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
      (
	dxpand(Directive),
	fail
	;
	O = directive(Directive, Comments, Vs)
      )
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
    maplist(out_comment,Comments ).
output(predicate(N/A,[comments(Comments) |_Clauses])) :-
    atom_chars(N,SNs),
    %    foldl(csafe,Ns,SNs,[]),
    format(atom(N1),'P~s~d',[SNs,A]),
    maplist(out_comment,Comments),
    addcomm(N/A,_Found),
    findall(I,between(1,A,I),Is),
    maplist(atomic_concat('int ARG'),Is,NIs),
    (
      A==0 ->
      T = N
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

out_comment(C) :-
    trl(C,Cf), 
    !,
    format('~s~n',[Cf]).

trl( C,NC) :-
    sub_string(C,0,3,_,"/*<"),
    sub_string(C,3,1,_,Space),
    sp(Space),
    !,
    trl_lines(C,star,NC).
trl( C,NC) :-
    sub_string(C,0,3,_,"/**"),
    sub_string(C,3,1,_,Space),
    sp(Space),
    !,
    trl_lines(C,star,NC).
trl( C,NC) :-
    sub_string(C,0,2,_,"%<"),
    sub_string(C,2,1,Comm,Space),
    sp(Space),
    !,
    sub_string(C,_,Comm,0,L),
    trl_lines(L,star,NL),
    string_concat(["///<",C,NL],NC).
trl( C,NC) :-
    sub_string(C,0,2,_,"%%"),
    sub_string(C,2,1,Comm,Space),
    sp(Space),
    !,
    sub_string(C,_,Comm,0,L),
    trl_lines(L,star,NL),
    string_concat(["///",C,NL],NC).
trl( _C, "").

sp(" ").
sp("\t").
sp("\n").

ws(" ").
ws("\t").

trl_lines(L,_,NL) :-
    (trl_lines(L,NL)).

trl_lines(Lines, O) :-
    sub_string(Lines,Left,1,Right,"\n"),
    !,
    sub_string(Lines,0,Left,_,C),
    trl_line(C,NC),
    sub_string(Lines,_,Right,0, R),
    trl_lines(R,NL),
    string_concat([NC,"\n",NL],O).
trl_lines( Lines, O) :-
    trl_line( Lines,O).

trl_line(  "","\n") :-
    !.
trl_line(  L,LF) :-
    trl_prefix(L,L1),
    trl_pred(L1,L2),
    trl_pi(L2,LF).

trl_prefix(C,NC) :-
    sub_string(C,0,2,_Len,Pref),
(Pref == "%%" -> true ;Pref == "%!"), 
     sub_string(C,2,1,_Comm,Space),
    sp(Space),
!,
    sub_string(C,2,_,0,B),
    string_concat("///",B,NC).
trl_prefix(C,C).

trl_pred(RL,NL) :-
    sub_string(RL,Bef,5,After,"@pred"),
    After1 is After-1,
    sub_string(RL,_,1,After1,WS),
    ws(WS),
    sub_string(RL,_,After1,0,Line0),
    strip_whitespace(Line0,0,Line),
    detect_name(Line,Name,Args,Arity,Extra),
    !,
    number_string(Arity,A),
    atom_string(At,Name),
    assert(pred_found(At,Arity)),
    sub_string(RL,0,Bef,_, Prefix),
    string_concat([Prefix,"@class P",Name,A,"       **\"",Name,Args,"\"** ",Extra],NL).
trl_pred(RL,NL) :-
    sub_string(RL,Bef,10,After,"@infixpred"),
    A0 is Bef+10,
    skip_whitespace(A0,RL,A1),
    A1>A0,
    block(A1,RL,B1),
		skip_whitespace(B1,RL,A2),
	A2>A1,	
		block(A2,RL,B2),
		skip_whitespace(B2,RL,A3),
		A3>A2,block(A3,RL,B3),
			    !,
			    L2 is B2-A2,
			    L1 is B3-A1,
			    sub_string(RL,A2,L2,_,Name),
			    sub_string(RL,A1,L1,_,NameArgs),
			    sub_string(RL,B3,_,0,Extra),
    atom_string(At,Name),
    assert(pred_found(At,2)),
    sub_string(RL,0,Bef,_, Prefix),
    string_concat([Prefix,"@class P",Name,"2       **\"",NameArgs,"\"** ",Extra],NL).
trl_pred(RL,RL).

    strip_whitespace(Line0,I0,Line) :-
    sub_string(Line0,I0,1,_,WS),
    ws(WS),
    !,
    I is I0+1,
    strip_whitespace(Line0,I,Line).
    strip_whitespace(Line,0,Line) :-
    !.
    strip_whitespace(Line0,I0,Line) :-
    sub_string(Line0,I0,_,0,Line).

    skip_whitespace(I0,Line,IF) :-
    sub_string(Line,I0,1,_,WS),
    ws(WS),
    !,
    I is I0+1,
    skip_whitespace(I,Line,IF).
    skip_whitespace(I,_Line,I).

  
    block(I0,Line,IF) :-
    sub_string(Line,I0,1,_,WS),
\+    ws(WS),
    !,
    I is I0+1,
    block(I,Line,IF).
    block(I,_Line,I).

  
    detect_name(Line,Name,Args,Arity,Extra) :-
    sub_string(Line,Bef,1,_,"("),
    sub_string(Line,_,1,After,")"),
    sub_string(Line,0,Bef,_,Name),
    sub_string(Line,Bef,_Sz,After,Args),
    sub_string(Line,_,After,0,Extra),
    findall(I,sub_string(Args,I,1,_,","),Is),
    length([_|Is],Arity).


trl_pi(RL,NL) :-
    sub_string(RL,Left,1,Right,"/"),
    Left1 is Left+1,
    sub_string(RL,Left1,1,_Right,D),
    digit(D),
    back(Left,RL,NPrefix),
NPrefix \= Left,
    !,
    sub_string(RL,0,NPrefix,_,Prefix),
    sub_string(RL,NPrefix,_,Right,Name),
    sub_string(RL,_,Right,0,RR),
    trl_pi(RR,NR),
    string_concat([Prefix,Name,"/",NR],NL).
trl_pi(L,L).
    

back(0,_L,0) :-
    !.
back(I0,S,I0) :-
    I is I0-1.
    sub_string(S,I,1,_,WS),
    ws(WS),
    !,
    back(I0,S,P) :-
     I is I0-1,
    back(I,S,P).
   

digit("0").
digit("1").
digit("2").
digit("3").
digit("4").
digit("5").
digit("6").
digit("7").
digit("8").
digit("9").
    



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
    \+ pred_found(N,A),
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

