
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
    modifier(definition,Def),
    (
      LTs=[TokP,TokL,TokS,Mod,0|LTs0]
    ->
      LTsf=[TokP,TokL,TokS,Mod,Def|LTs0]
    ;
      LTsf =  LTs
    ).

showt(T, [A,B,C,D,E|Ls],Ls) :-
    format(user_error,'~3d ~3d ~3d ~3d ~3d ~w~n',[A,B,C,D,E,T]).


    ins([T|_] ,_L0,_P0,_Lvl) --> { writeln(T),
       fail }.
ins([] ,_L0,_P0,_Lvl) --> [].

ins([ t(var(_,_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    {	
      tt(parameter,V),
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
    },
    indent(L,Lvl),
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).

/*
ins( [t(atom(_M),L,P1,Sz1),
      t(atom(':'),L2,P2,1),
      t(atom(_A),L3,P3,Sz12),
      t('l',L4,P4,1)|Ts] ,L0,P0,0) -->
    !,
    { 	tt(method,V),
     	tt(namespace,V1),
     	tt(operator,V1),
	DL is L-L0,
	DL2 is L2-L,
	DL3 is L3-L2,
	DL4 is L4-L3,
	(DL->DP=P1
	;
	DP is P1-P0
	)
      },
    [DL,DP,Sz1,V1,0,
     D2L,DP,Sz1,V2,0,
     DL3,DP,Sz1,V,0,
     DL,DP,Sz1,V1,0,
     ],
    Lins([t('(',L,P,Sz)|Ts],L,P1,0).
*/
ins( [t(atom(_A),L,P1,Sz1),t('EOT',L,P,Sz)|Ts] ,L0,P0,0) -->
    !,
    { 	tt(method,V),
      DL is L-L0,
      (DL>0->DP=P1
      ;
	DP is P1-P0
      ),
      level := 0,
     last := 0
      },
    [DL,DP,Sz1,V,0],
    indent(L,Lvl),
    ins([t('EOT',L,P,Sz)|Ts],L,P1,0).
ins( [t(atom(_A),L,P1,Sz1),t('l',L,P,1)|Ts] ,L0,P0,0) -->
    !,
    { tt(method,V),
      DL is L-L0,
      (
	L>0->DP=P1
      ;
	DP is P-P0
      ),
      last := P+1
      },
    [DL,DP,Sz1,V,0],
    indent(L,Lvl),
    ins([t('(',L,P,1)|Ts],L,P1,0).




ins( [t(atom(_A),L,P1,Sz1),t('l',L,P,1)|Ts] ,L0,P0,Lvl0) -->
    {Lvl0>0},
    !,
    { tt(struct,V),
      DL is L-L0,
      (DL>0->DP=P1
      ;
	DP is P-P0
      ),
      last := P+1
      },
    [DL,DP,Sz1,V,0],
    indent(L,P+1),
    indent(L,Lvl),
    ins([t('(',L,P,1)|Ts],L,P1,Lvl0).



ins( [t(atom(_A),L,P,Sz),t(atom('-->'),L1,P1,Sz1)|Ts] ,L0,P0,0) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(classs,V)
      last := 0
    },
    [DL,DP,Sz,V,0],
    indent(L,Lvl),
    ins([t(atom('-->'),L1,P1,Sz1)|Ts],L,P,1).



ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { unlift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1,
      tt(operator,V)
      },
    [DL,DP,1,V,0],
    indent(L,Lvl),
    ins(Ts,Sz,P,Lvl1).

ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { lift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl1),
    indent(L,Lvl),
      ins(Ts,L,P,Lvl1).


ins( [t(atom(Op),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(operator,V),
      
      },
    indent(L,Lvl),
    [DL,DP,Sz,V,
     indent(L,Lvl),0],
ins(Ts,L,P,Lvl).

ins([ t(atom(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(variable,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl),
    ins(Ts,L,P,Lvl).
ins([ t(comment(A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(comment,V),
      check_doc(A,Doc)
      },
    [DL,DP,Sz,V,Doc],
     indent(L,Lvl),
   ins(Ts,L,P,Lvl).

ins([ t(string(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(string,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl),
    ins(Ts,L,P,Lvl).

ins([ t(number(_A),L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(number,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl),
    ins(Ts,L,P,Lvl).


ins( [t('EOT',L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(keyword,V)
      },
    [DL,DP,Sz,V,0],
    ins(Ts,L,P,Lvl).

ins( [t(end_of_file,_L,_P,_Sz)|_Ts] ,_L0,_P0,_Lvl) --> [].



ins( [t(Op,L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { unlift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl1),
    ins(Ts,L,P,Lvl1).

ins( [t(Op,L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { 		    
      lift_operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1,
      last := P+1,
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl1),
    ins(Ts,L,P,Lvl1).

ins( [t(Op,L,P,Sz)|Ts] ,L0,P0,Lvl) -->
    { operator(Op),
      !,
      DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      tt(operator,V)
      },
    [DL,DP,Sz,V,0],
    indent(L,Lvl),
    ins(Ts,L,P,Lvl).
ins( [t(error,_,_,_Sz)|Ts] ,L,P,Lvl) -->
    !,
    indent(L,Lvl),
    ins(Ts,L,P,Lvl).
ins( [_|Ts] ,L,P,Lvl
   ) -->
    indent(L,Lvl),
    ins(Ts,L,P,Lvl).

lift_operator(('(')).
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
    sub_string(`/** `,0,_,_,S),
    !,
    modifier(documentation,D).
check_doc(S,D) :-
    sub_string(`/**>`,0,_,_,S),
    !,
    modifier(documentation,D).
check_doc(S,D) :-
    sub_string(`%%  `,0,_,_,S),
    !,
    modifier(documentation,D).
check_doc(S,D) :-
    sub_string(`%%> `,0,_,_,S),
    !,
    modifier(documentation,D).
check_doc(_,0).



indent(L,_,Ts,Ts) ;-
Ts =[t(_,L,_,_)], 
		!.
indent(L,Lvl,[t(_,L1,P,_)|Ts],Ts) :-
DI is 6+2*Lvl,
Offet is DI-P,
(Offset \= 0 -> self.mods.append(t(L1,Offset)) ;true).
