:- module(lsp, [
	      validate_file/2,
	      scan_file/2
	  ]).

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- reexport(library(python)).
:- use_module(library(hacks)).


validate_file(File,Queue) :-
    assert((user:portray_message(Sev,Msg) :- q_msg(Sev, Queue, Msg))),
    load_files(File,[]),
    retractall(user:portray_message(Sev,Msg)).


q_msg(Sev,Queue, error(Err,Inf)) :-
    Err =.. [F|As],
    error_descriptor(Inf, Desc),
    query_exception(parserLine, Desc, LN), 
    nonvar(LN),
    query_exception(parserPos, Desc, Pos),
    q_msgs(As,Sev,F,S),
    Queue.append(t(S,LN,Pos)).
     

q_msgs([],_Sev, N,S) :-      
    format(string(S ),'~a.',[N]).
q_msgs([A1],_Sev, N, S) :-      
    format(string(S),'~a ~w.',[N,A1]).
q_msgs([A1,A2],_Sev, N, S) :-      
    format(string(S),'~a ~w ~w.',[N,A1,A2]).

scan_file(File,Toks) :-
    open(File,read,S,[alias(data)]),
    repeat,
    read_clause(S,T,[scan(Ts)]),
    (T == end_of_file ->
	 close(S),
	 !
    ;
    ins(Ts,0,0,0,LTs,[]),
    (LTs=[P,L,S,6|LTs0]   -> LTsf=[P,L,S,0|LTs0] ; LTsf =  LTs ),
    Toks.extend(LTsf),
	 fail
     ).


ins([] ,_L0,_P0,Lvl) --> [].

ins([ t(var(A),L,P)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      atom_length(A,N)
      },
    [DL,DP,N,2,0],
    ins(Ts,L,P,Lvl).


ins( [t(atom(_A),L,P1),t('l',L,P)|Ts] ,L0,P0,0) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      N is P-P1
      },
    [DL,DP,N,0,0],
    ins(Ts,L,P,1).

ins( [t(atom(_A),L,P1),t('l',L,P)|Ts] ,L0,P0,Lvl0) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      N is P-P1,
      Lvl is Lvl0+1
      },
    [DL,DP,N,1,0],
    ins(Ts,L,P,Lvl).



ins([ t(atom(A),L,P)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      atom_length(A,N)
      },
    [DL,DP,N,4,0],
    ins(Ts,L,P,Lvl).

ins([ t(atom(A),L,P)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      atom_length(A,N)
      },
    [DL,DP,N,3,0],
    ins(Ts,L,P,Lvl).

ins([ t(string(A),L,P)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      string_length(A,N)
      },
    [DL,DP,N,8,0],
    ins(Ts,L,P,Lvl).


ins([ t(comment(A),L,P)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      string_length(A,N)
      },
    [DL,DP,N,7,0],
    ins(Ts,L,P,Lvl).

ins([ t(number(A),L,P)|Ts] ,L0,P0,Lvl) -->
    !,
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      number_string(A,S),
      string_length(S,N)
      },
    [DL,DP,N,9,0],
    ins(Ts,L,P,Lvl).


ins( [t('(',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl is Lvl+1
      },
    [DL,DP,1,6,0],
    ins(Ts,L,P,Lvl).


ins( [t('{',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1
      },
    [DL,DP,1,6,0],
    ins(Ts,L,P,Lvl1).

ins( [t('[',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl+1
      },
    [DL,DP,1,6,0],
    ins(Ts,L,P,Lvl1).



ins( [t(')',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1
      },
    [DL,DP,1,6,0],
    ins(Ts,L,P,Lvl1).


ins( [t('}',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1
      },
    [DL,DP,1,6,0],
    ins(Ts,L,P,Lvl1).

ins( [t(']',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0),
      Lvl1 is Lvl-1
      },
    [DL,DP,1,6,0],
    ins(Ts,L,P,Lvl1).

ins( [t(',',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
      },
    [DL,DP,1,10,0],
    ins(Ts,L,P,Lvl).

ins( [t(':',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
      },
    [DL,DP,1,10,0],
    ins(Ts,L,P,Lvl).

ins( [t('|',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
      },
    [DL,DP,1,10,0],
    ins(Ts,L,P,Lvl).


ins( [t('EOT',L,P)|Ts] ,L0,P0,Lvl) -->
    { DL is L-L0,
      (DL>0->DP=P;DP is P-P0)
    },
    [DL,DP,1,10,0],
    ins(Ts,L,P,Lvl).


	      
