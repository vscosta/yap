
:- module(toks_lsp,
	  [symbols/2,
	  showt/3]
	 ).

:- dynamic ( tt/2, modifier/2, validated/1 ) .


:- use_module(library(lists)).
:- use_module(library(maplist)).

init_codes(L) :-
    retractall(tt(x_)),
    foldl(init_code,L,0,_).

init_code(TokType, Id, Id1) :-
    Id1 is Id+1,
    assert(tt(TokType,Id)).

init_modifiers(L) :-
    retractall(modifier(_,_)),
    foldl(init_modifier,L,1,_).

init_modifier(TokType, Id, Id1) :-
    Id1 is Id<<1,
    assert(modifier(TokType,Id)).


symbols(Ts,LTsf) :-
    ins(Ts,1,0,0,LTs,[]),
    tt(method,Mod),
%   foldl(showt, Ts,LTs,_),
    modifier(definition,Def),
    (
      LTs=[TokP,TokL,TokS,Mod,0|LTs0]
    ->
      LTsf=[TokP,TokL,TokS,Mod,Def|LTs0]
    ;
      LTsf =  LTs
    ).

showt(comment(T), [A,B,C,D,E|Ls],NLs) :-
    format(user_error,'~t~d~3+~t~d~3+~t~d~3+~t~d~3+~t~d~3+ ~w~n',[A,B,C,D,E,comment(T)]),
     peek_comments(Ls,D,NLs).
showt(T, [A,B,C,D,E|Ls],Ls) :-
    format(user_error,'~t~d~3+~t~d~3+~t~d~3+~t~d~3+~t~d~3+ ~w~n',[A,B,C,D,E,T]).

peek_comments([A,B,C,D,E|Ls],D,NLs) :-
    !,
    format(user_error,'~t~d~3+~t~d~3+~t~d~3+~t~d~3+~t~d~3+~n',[A,B,C,D,E]),
    peek_comments(Ls,D,NLs) .
peek_comments(Ls,_D,Ls) .


ins([T,T1] ,_L0,_P0,_Lvl) --> { writeln(T:T1),
			       fail }.
ins([] ,_L,_P,_Lvl) --> !.
ins( [t('EOT',L,P,1)|Ts] ,L0,P0, _Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(keyword,V)
    },
    [DL,DP,1,V,0],
    ins(Ts,L,P,0).
ins( [t('EOT',_L,_P,_)|Ts] ,L0,P0, _Lvl) -->
    !,
    ins( Ts ,L0,P0,0).
ins( [t(end_of_file,_L,_P,_Sz)|_Ts] ,_L0,_P0,_Lvl) --> [].
ins([ t(var(_,_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    {
      tt(parameter,V),
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
    },
    {indent(L,Lvl,Ts)},
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).


ins( [t(atom(_A),L,P1,Sz1),t(atom('/'),L,_P2,1) ,t(number(N),L,P3,Sz2)|Ts] ,L0,P0,Lvl) -->
    {integer(N), N>=0},
    !,
    {
      tt(variable,V),
      !,
      DL is L-L0,
      (
	DL>0->DP=P1
      ;
      DP is P1-P0
      )
    },
    [DL,DP,Sz1,V,0 ,
     0,Sz1,1,V,0,
     0,1,Sz2,V,0],
    {indent(L,1,Ts)},
    ins(Ts,L,P3,Lvl).


ins( [t(atom(_M),L,P1,Sz1),
      t(atom(':'),L,_P2,1),
      t(atom(_A),L,_P3,Sz2),
      t(l,L,P4,1)|Ts] ,L0,P0,0) -->
    !,
    { 	tt(method,V),
     	tt(operator,V2),
	DL is L-L0,
	(DL>0->DP=P1;DP is P1-P0)
    },
    [DL,DP,Sz1,V,0,
     0,Sz1,1,V,0,
     0,1,Sz2,V,0,
     0,Sz2,1,V2,0
    ],
    ins(Ts,L,P4,1).
ins( [t(atom(_A),L,P1,Sz1),t(l,L,P2,1)|Ts] ,L0,P0,0)-->

    !,
    {
      tt(method,V),
      tt(operator,V2),
      !,
      DL is L-L0,
      (
	DL>0->DP=P1
      ;
	DP is P1-P0
      )
      },
    [DL,DP,Sz1,V,0 ,
     0,Sz1,1,V2,0],
    {indent(L,1,Ts)},
    ins(Ts,L,P2,1).



ins( [t(atom(_A),L,P1,Sz1),t(l,L,P2,1)|Ts] ,L0,P0,Lvl0) -->
    {Lvl0>0},
    !,
    { tt(variable,V),
      tt(operator,V2),
      DL is L-L0,
      (DL>0->DP=P1
      ;
	DP is P1-P0
      ),
      Lvl is Lvl0+1
      },
    [DL,DP,Sz1,V,0,
    0,Sz1,1,V2,0],
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P2,Lvl).



ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { unlift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl1,Ts)},
    ins(Ts,L,P,Lvl1).

ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { lift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl1,Ts)},
      ins(Ts,L,P,Lvl1).


ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).

ins([ t(atom(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(variable,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).
%%
% comments: handle multi=line
% handle docummentation comments.
ins([ t(comment(A),L,P,_)|Ts] ,L0,P0,Lvl) -->
    {sub_string(A,S,1,RSz,`\n`),
     RSz > 0,
     !,
     Sz is S+1,
     sub_string(A,Sz,RSz,0,Rest),
     DL is L-L0,
     L1 is L+1,
     (DL>0->DP=P;DP is P-P0),
      tt(comment,V),
      check_doc(A,Doc)
    },
    [DL,DP,S,V,Doc],
   ins([ t(comment(Rest,Doc),L1,0
	,RSz)|Ts],L,P,Lvl).
ins([ t(comment(A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(comment,V),
      check_doc(A,Doc)
      },
    [DL,DP,Sz,V,Doc],
     {indent(L,Lvl,Ts)},
   ins(Ts,L,P,Lvl).
ins([ t(comment(A,Doc),L,0,Sz)|Ts] ,_L0,_P0,Lvl) -->
    {sub_string(A,0,1,RSz,`\n`),
     RSz > 0,
     !,
     sub_string(A,1,RSz,0,Rest),
     L1 is L+1
    },
    ins([ t(comment(Rest,Doc),L1,0,RSz)|Ts],L,Sz,Lvl).
ins([ t(comment(A,Doc),L,0,_Sz)|Ts] ,L0,_P0,Lvl) -->
    {sub_string(A,S,1,RSz,`\n`),
     RSz > 0,
     !,
     sub_string(A,_Sz,RSz,0,Rest),
DL is L-L0,
     tt(comment,V)
    },
    [DL,0,S,V,Doc],
    ins([ t(comment(Rest,Doc),L,0,RSz)|Ts],L,0,Lvl).
ins([ t(comment(_A,Doc),L,0,Sz)|Ts] ,L0,_,Lvl) -->
    !,
    {
     DL is L-L0,
     tt(comment,V)
    },
    [DL,0,Sz,V,Doc],
     {indent(L,Lvl,Ts)},
   ins(Ts,L,0,Lvl).

ins([ t(string(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(string,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).

ins([ t(number(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(number,V)
    },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).



ins( [t((Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { unlift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl1,Ts)},
    ins(Ts,L,P,Lvl1).

ins( [t((Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { lift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl1,Ts)},
      ins(Ts,L,P,Lvl1).


ins( [t((Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { operator(Op),
      !,
      DL is L-L0,
      (DL>0 ->DP=P;DP is P-P0),
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).



ins( [t(error,_,_,_Sz)|Ts] ,L,P,Lvl) -->
    !,
  {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).
ins( [H|Ts] ,L,P,Lvl) -->
    {writeln(fail+H)},
    {indent(L,Lvl,Ts)},
    ins(Ts,L,P,Lvl).

indent(_,_,[]):-
    !.
indent(L,_,[t(_,L,_,_)|_]):-
		!.
indent(_L,Lvl,[t(_,_L1,P,_)|_]):-
    DI is 6+2*Lvl,
    _Offset is DI-P,
%(Offset \= 0 -> self.mods.append(t(L1,Offset)) ;true).
    true.

lift_operator(('(')).
lift_operator((l)).
lift_operator(('[')).
lift_operator(('{')).
unlift_operator((')')).
unlift_operator(('}')).
unlift_operator((']')).
operator((':-')).
operator(('<--')).
operator((':')).
operator(('|')).
operator((',')).
operator((';')).


:- initialization
       init_codes(
	   [namespace,
	     (class),
             struct,
             parameter,
             variable,
             method,
             keyword,
             modifier,
             comment,
             string,
             number,
             operator]
		   ).

:- initialization
       init_modifiers([
	 declaration,
	 definition,
	 readonly,
	 static,
	 deprecated,
	 abstract,
	 async,
	 modification,
	 documentation,
	 defaultLibrary
	 ]).


check_doc(S,D) :-
    string_chars(S,Ats),
    (
	Ats = ['/','*','*',C|_],
	char_type_space(C)
    ;
	Ats = ['/','*','*','>',C|_],
	char_type_space(C)
    ;
	Ats = ['%','%',C|_],
	char_type_space(C)
    ;
	Ats = ['%','%','<',C|_],
	char_type_space(C)
    ),
    !,
    modifier(documentation,D).
check_doc(_,0).
