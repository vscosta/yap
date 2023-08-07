
:- module(scanner, [
    mkgraph/1,
    init_scanner_helper/1,
    close_scanner_helper/1,
    use/10,
    def/8,
    ws/3
  ]).

:- reexport(library(matrix)).
:- use_module(library(maplist)).
:- use_module(library(lists)).

:- use_module(library(toks_lsp)).

:- dynamic( [compiling/1,loaded/1,  use/10, def/8, ws/3] ).
:- multifile( [compiling/1,  use/10, def/8, ws/3] ).

mkgraph(F) :-
	   scanner:def(_,_,_,F,_,_,_,_),
    !,
    init_scanner_helper(Loop),
    ensure_loaded(F),    
    close_scanner_helper(Loop).
mkgraph(F) :-
    init_scanner_helper(Loop),
    compile(F),    
    close_scanner_helper(Loop).
    
init_scanner_helper(Loop) :-
    prolog_flag(compiler_top_level, Loop),
    set_prolog_flag(compiler_top_level, scanner:scanner_loop).

close_scanner_helper(Loop) :-
    set_prolog_flag(compiler_top_level, Loop).


scanner_loop(Stream,Status) :-
    stream_property(Stream,file_name(File)),
    retractall(scanner:use(_,_F,_MI,_F0,_Mod,File,_S0,_E0,_S1,_E1)),
    retractall(scanner:def(_,_,_,File,_,_,_,_)),
    retractall(scanner:dec(_Dom,_F,_M,File,_B,_E,_BT,_FiET)),
    retractall(scanner:ws(File,_,_)),
    retractall(compiling(_)),
    repeat,
    catch(
	 scanner:c_clause(Stream,Status,File),
	 _Error,
	prolog:error_handler),
    !.

c_clause(Stream,Status,File) :- 
    prompt1(': '), prompt(_,'     '),
    Options = [syntax_errors(dec10),variable_names(Vars),term_position(Pos),scan(L)],
    read_clause(Stream, Clause, Options),
    (
      Clause    == end_of_file
    ->
      !
    ;
      compilation_steps(Clause, Status, Vars,File, Pos, L),
      fail
    ).

preprocess_tokens([],[],[],_) :- !.
preprocess_tokens([t(_,L,_,_),t(_,L1,C,_)|_],_,File) :-
   integer(L1),
    L < L1,
    assert(ws(File,L1,C)),
    fail.
preprocess_tokens([t(comment(_),_,_,_)|L],NL, Bs,File) :- !,
    preprocess_tokens(L, NL, Bs,File).
/*
preprocess_tokens([t(',',Le,C,D),t('l',_,_,_)|L],[t(',',Le,C,D)|NL],Bs, File) :- !,
    preprocess_tokens(L, NL, [b|Bs],File).
preprocess_tokens( [t(atom(A),Le,C,D),t('l',_,_,_)|L],[t(atom(A),Le,C,D)|NL],Bs, File) :-

    ( A=='+' ;
      A == '-' ;
      A== '*' ;
      A== '/' ;
      A== '^' ;
      A == ':' ;
      A == ';' ;
      A == '->' ;
      A == ':-' ;
      A == '-->' ;
      A == '{' ;
      A == '<<' ;
      A == '>>' ;
      A == '=<' ;
      A == '<' ;
      A == '>=' ;
      A == '=' ;
      A == '==' ;
      A == '=:=' ;
      A == '=/=' ;
      A == '[' ;
      A == '*->' ),
    !,
    preprocess_tokens(L, NL, [b|Bs],File).
preprocess_tokens([t('l',Le,C,D)|L],[t('l',Le,C,D)|NL],Bs, File) :- !,
    preprocess_tokens(L, NL, [l|Bs],File).
preprocess_tokens([t('(',_,_,_)|L],NL,Bs, File) :- !,
    preprocess_tokens(L, NL, [b|Bs], File).
preprocess_tokens([t(')',Le,C,D)|L],[t(')',Le,C,D)|NL], [l|Bs],File) :- !,
    preprocess_tokens(L, NL, Bs, File).
preprocess_tokens([t(')',_,_,_)|L],NL,[b|Bs], File) :- !,
    preprocess_tokens(L, NL, Bs, File).
*/
preprocess_tokens([t('EOT',_,_,_)],[],[],_File) :- !.
preprocess_tokens([H|T],[H|NT],Bs,File) :-
    preprocess_tokens(T,NT,Bs,File).


compilation_steps(Clause, Status, Vars, Pos, File, L) :-
    catch(    
 (tok__(Clause,L,Pos,File),
   prolog:call_compiler(Clause, Status,Vars,Pos)),
_Error, error_handler).

tok__(Clause,Ts,_,_File) :-
    symbols(Ts, LTsf),
    catch(self.data.extend(LTsf),_,writeln(LTsf)),
    sync(Clause,Domains,Ts, [_]),
    process_domains(Clause,Domains),
    writeln(Domains),
    !.
tok__(_Clause,_,Pos,F) :-
    %arg(2,Pos,L),
    format(user_error,
	   'failed matching  at  ~w:~w.~n',[F,Pos]).
	
%sync(_,_,L,_) :-
%writeln(L),
%fail.
sync(T,NT) -->
    [t(l,_,_,_)],
    !,
    sync(T,NT),
    [t(')',_,_,_)].
sync(T,NT) -->
    [t('(',_,_,_)],
    !,
    sync(T,NT),
    [t(')',_,_,_)].
sync(V,t(V,[],L-C,L-CF,L-C,L-CF)) -->
{var(V)},
    !,
    [t(var(V0,_N),L,C,Sz)],
    {V==V0},
    {CF is C+Sz}.
sync(C,CT) -->
    {primitive(C)},
    !,
    sync_primit(C,CT).
sync(C,CT) -->
    { C= [_|_]; C = {_}; C='[]'(_,_); C='{}'(_); C='()'(_) },
    !,
    sync_seqs(C,CT).
sync(C,CT) -->
    { functor(C,NA,Ar) },
    (
      { Ar == 2 } -> sync_cmpd2(NA,C,CT)
    ;
      { Ar == 1 } -> sync_cmpd1(NA,C,CT)
    ;
      sync_cmpd(NA, Ar, C,CT)
    ).


sync_primit(A,t(A,[],L-C,L-CF,L-C,L-CF)) -->
    {atom(A)},
    !,
    [t(atom(A),L,C,Sz)],
    {CF is C+Sz}.
sync_primit(NT,t(NT,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    {number(NT)},
    [t(atom(-),SL0,_SC0,_Sz),t(number(T),SL0,SC1,Sz)],
    !,
    {
      T is -NT,
      SCF is SC1+Sz}.
sync_primit(Inf,t(Inf,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    [t(atom(-),SL0,_SC0,_Sz),t(atom(inf),SL0,SC1,Sz)],
    { Inf is -inf },
    !,
    {SCF is SC1+Sz, Inf is -inf}.
sync_primit(Inf,t(Inf,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
[t(atom(+),SL0,SC0,_Sz),t(atom(inf),SL0,SC1,Sz)],
    {Inf is inf},
    !,
    {SCF is SC1+Sz, Inf is +inf}.
sync_primit(T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
{number(T)},
    [t(atom(+),SL0,_SC0,_Sz),t(number(T),SL0,SC1,Sz)],
    !,
    {
      SCF is SC1+Sz}.
sync_primit(T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    [t(number(T),SL0,SC0,Sz)],
    {number(T),
      !,
      SCF is SC0+Sz}.
sync_primit(T,t(T,[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    [t(string(T),SL0,SC0,Sz)],
    {string(T)},
    !,
    {SCF is SC0+Sz}.

sync_seqs({T},t({T},[NT],SL-SC,SL-SC1,SL-SC,EL-EC)) -->
    [t('{',SL,SC,Sz)],
    !,
    sync(T,NT),
    [t('}',EL,EC,_)],
    {SC1 is SL+Sz}.

sync_seqs([H|T],t([H|T],[], SL0-SC0,SL0-SCF, SL0-SC0,SL0-SCF)) -->
    [t(string([H|T]),SL0,SC0,Sz)],
    !,
    {SCF is SC0+Sz}.
sync_seqs([H|T],t([H|T],NAs,SL-SC,SL-SC1,SL-SC,End)) -->
    [t('[',SL,SC,Sz)],
    !,
    {SC1 is SC+Sz},
    sync_list([H|T],NAs,End).
sync_seqs([H|T],t([H|T],[NH|NT],BL-BF,BL-BFL,Start,End)) -->
    sync(H,NH),
    [t( atom('.'),BL,BF,Sz)],
    {BFL is BF+Sz,
      NH =t(_,_,_,_,Start,_),
      NT=t(_,_,_,_,_,End)
      },
    sync(T,NT).
    sync_seqs(A(),t(A(),[NA],R0,Rf,Start,EBL-EC)) -->
	sync(A,NA),
	[t( atom('()'),EBL,BF,Sz)],
	!,
	{EC is BF+Sz,
	  NA=t(_,_,R0,Rf,Start,_)}.
    sync_seqs(A[H|T],t(A[H|t],[NB,NA],R0,Rf,Start,End)) -->
    sync(A,NA),
    [t('[',_,_,_)],
  !,
  sync_list([H|T]	,NB,End),
  {NA=t(_,_,R0,Rf,Start,_End)}.
    
    sync_cmpd2(',',(A1,A2),t((A1,A2),[NA1,NA2],EL-EF,EL-EFL,Start,End)) -->
%	{writeln(in0(T))},
    	sync(A1,NA1),

([ t(')',_,_,_) ] -> [] ; []),

    	[t(',',EL,EF,Sz)],
   	 sync(A2,NA2),
	{ NA1=t(A1,_,_,_,Start,_),
	 NA2=t(A2,_,_,_,_,End),
	 EFL is EF+Sz
% 	,writeln(out0(T))
}.
sync_cmpd2(A,T,t(T,[NA1,NA2],EL-EF,EL-EFL,EL-EF,FEL-FEC)) -->
    {T=..[A,A1,A2]},
    	[t(atom(A),EL,EF,Sz)],
	[t(l,_,_,_)],
%	{writeln(in1(T))},
	sync(A1,NA1),
	[t(',',_,_,_)],
    	sync(A2,NA2),
    	[t(')',FEL,FE0,Szf)],
	{
    	EFL is EF+Sz,    
    	FEC is FE0+Szf
%	,writeln(out1:T)
	 }	.
sync_cmpd2(A,T,t(T,[NA1,NA2],EL-EF,EL-EFL,Start,End)) -->
    {T=..[A,A1,A2]},
%    {writeln(in2(T))},
    sync(A1,NA1),
[t(atom(A),EL,EF,Sz)],
    sync(A2,NA2),
	{
    EFL is EF+Sz,
    NA1=t(_,_,_,_,Start,_),
    NA2=t(_,_,_,_,_,End)
%    ,writeln(out2:T)
    }.

%% - a(X)
sync_cmpd1(A,T,t(T,[NA1],EL-EF,EL-EFL,EL-EF,FEL-FE1)) -->
    	[t(atom(A),EL,EF,Sz)],
	[t(l,_,_,_)],
    !,		
    %	{writeln(in1(T))},
    { arg(1,T,A1) },
    sync(A1,NA1),
    	[t(')',FEL,FE0,Szf)],
	{
    	EFL is EF+Sz,
	FE1 is FE0+Szf
%	,writeln(out1:T)
	 }	.
%% - a X
sync_cmpd1(A,T,t(T,[NA1],EL-EF,EL-EFL,EL-EF,End)) -->
    [t(atom(A),EL,EF,Sz)],
    { current_op(_,fx,A) ; current_op(_,fy,A) },
    !,
    { arg(1,T,A1) },
    sync(A1,NA1),
	{
    EFL is EF+Sz,
    NA1=t(_,_,_,_,_,End)
%    ,writeln(out2:T)
    }.
%% - X a
sync_cmpd1(A,T,t(T,[NA1],EL-EF,EL-EFL,Start,EL-EFL)) -->
	!,
    { arg(1,T,A1) },
    sync(A1,NA1),
    [t(atom(A),EL,EF,Sz)],
	{
    EFL is EF+Sz,
    NA1=t(_,_,_,_,Start,_)
%    ,writeln(out2:T)
    }.

sync_cmpd(A,N,T,t(T,NT,StartL-StartC,StartL-StartF,StartL-StartC,End)) -->
	  [t(atom(A),StartL,StartC,Sz),
	  t(l,_,_,_)],		
    { StartF is StartC+Sz},
    sync_args(1,N,T,NT,End).

chk(A,A) :- writeln(A).

sync_list([H|T],[NH|NTs],End) -->
    sync(H,NH),
    sync_tail(T,NTs,End).

sync_tail([],[],EL-ECF) -->
    [t(']',EL,EC,Sz)],
    {ECF is EC+Sz},
    !.
sync_tail(L,NL,End) -->
    [t(',',_,_,_)],
    !,
    sync_list(L,NL,End).
sync_tail(L,NL,EL-ECF) -->
    [t('|',_,_,_)],
    !,
    sync(L,NL),
    [t(']',EL,EC,Sz)],
    {ECF is EC+Sz}.

% sync_args(N,NF,T,_NT,_,[A,B,C,D,E,F|_],_) :-
%    writeln(N:NF:T:[A,B,C,D,E,F]),
%    fail.
sync_args(N,N,T,[NA],EL-ECF)-->
    !,
    {arg(N,T,AT)},
    sync(AT,NA),
    [t(')',EL,EC,Sz)],
    {ECF is EC+Sz}.

sync_args(N,NF,T,[NA|NT],End)-->
    {
      N1 is N+1,
      arg(N,T,AT)
      },
    sync(AT,NA),
[t(',',_EL,_EC,_)],
    sync_args(N1,NF,T,NT,End).

process_domains((:- Directive),t(_,[Child],_,_,_,_)):-
    Child = t(Directive,Domains,B0,E0,Begin,End )	,
    decs(Directive, Domains, B0, E0, Begin, End).
process_domains((H0:-B),t(_,[t(H0,_,A0,B0,_,_), BGs],_,_,BT,ET)) :-
    !,
    stream_property(loop_stream,file_name(File)),		 
    strip_module(H0,Mod,H),	
    functor(H,N,A),
    def_predicate(N/A,Mod,File,A0,B0,BT,ET),
    uses(B,BGs,Mod,N/A).	
process_domains((H0),t(H0,_,A0,B0,BT,ET)) :-
    stream_property(loop_stream,file_name(File)),		 
    strip_module(H0,Mod,H),	
    functor(H,N,A),
    ( system_predicate(N/A) -> OMod = prolog ; OMod = Mod),
    def_predicate(N/A,OMod,File,A0,B0,BT,ET).
     
def_predicate(N/A,Mod,File,_A0,_B0,BT,ET):-
    compiling(Mod:N/A),
    !,
    retract(def(predicate,N/A,Mod,File,A0,B0,BT,_)),
    assert(def(predicate,N/A,Mod,File,A0,B0,BT,ET)).
def_predicate(N/A,Mod,File,A0,B0,BT,ET):-
    assert(def(predicate,N/A,Mod,File,A0,B0,BT,ET)),
    retractall(compiling(_)),
    assert(	compiling(Mod:N/A)).


decs(module(Mod,_Exports),[t(Mod,_,A0,B0,_,_), t(_,ExportLists,_,_,_,_)],_,_,BT,ET) :-
    stream_property(loop_stream,file_name(File)),
    assert(def(module,export,Mod,File,A0,B0,BT,ET)),
    maplist(add_dec(export,Mod,File,BT,ET),ExportLists).

decs(system_module(_Mod,_Exports,_),[t(Mod,_,A0,B0,_,_), t(_,ExportLists,_,_,_,_), t(SysPreds,_,_,_)],_,_,BT,ET) :-
    !,
    stream_property(loop_stream,file_name(File)),
    assert(def(module,export,Mod,File,A0,B0,BT,ET)),
    maplist(add_dec(export,Mod,File,BT,ET),ExportLists),
    maplist(add_dec(export,prolog,File,BT,ET),SysPreds).
dec(_T,_,_,_,_,_).

add_dec(export,Mod,File,BT,ET,t(N/A,_,_,_,B,E)) :-
    !,
    assert(dec(predicate_export,N/A,Mod,File,B,E,BT,ET)).
add_dec(export,Mod,File,BT,ET,t(op(A1,A2,A3),_,_,_,B,E)) :-
    !,
    assert(def(op,A3,Mod,File,B,E,BT,ET)),
    op(A1,A2,A3).
add_dec(_,_,_,_,_,_).

uses(G,_,_,_) :-
    var(G),			
    !.	
uses((G1,G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
    !,
    uses(G1,BG1,Mod,N/A),
    uses(G2,BG2,Mod,N/A).
uses(Mod:G,t(_,[_,BG2],_,_,_,_),_Mod,N/A) :-
    !,
    uses(G,BG2,Mod,N/A).
uses((G1->G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
    !,
    uses(G1,BG1,Mod,N/A),
    uses(G2,BG2,Mod,N/A).
uses((G1*->G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
    !,
    uses(G1,BG1,Mod,N/A),
    uses(G2,BG2,Mod,N/A).										
uses((G1;G2),t(_,[BG1,BG2],_,_,_,_),Mod,N/A) :-
    !,
    uses(G1,BG1,Mod,N/A),
    uses(G2,BG2,Mod,N/A).
uses(G0,BG,Mod,N/A) :-
    fail,
    strip_module(Mod:G0,M,G),
    predicate_property(M:G,meta_predicate(GM)),
    G=.. [_|As],
    GM =.. [_|Ms],
    maplist(add_use( Mod, N/A, M), Ms,As, BG),
    fail.
uses(G0,t(G0,_,S0,E0,S1,E1),Mod,N0/Ar0) :-
    strip_module(Mod:G0,M,G),
    functor(G,N,Ar),
    stream_property(loop_stream,file_name(File)),
    (system_predicate(N/Ar)
    ->
      MI = prolog
    ;
      follow_imports(Mod,G,MI)
    ),
    assert(use(predicate,N/Ar,MI,N0/Ar0,M,File,S0,E0,S1,E1)),
    fail.
uses(_,_,_,_).	

follow_imports(_M,G,prolog) :-
    predicate_property(G,built_in),
    !.
follow_imports(M,G,MI) :-
    predicate_property(M:G,imported_from(M1)),
    !,
    follow_imports(M1,G,MI).
follow_imports(M,_,M).

add_use(Mod, N0/Ar0,M0,M,G,t(G,_,S0,E0,S1,E1)) :-
    number(M),
    !,
    strip_module(M0:G,M1,G1),
    functor(G1,N,Ar),
    ArM is Ar+M,
    stream_property(loop_stream,file_name(File)),
    system_predicate(N/Ar) -> MI = prolog ;  follow_imports(M1,G,MI),
    assert(use(predicate,N/ArM,MI,N0/Ar0,Mod,File,S0,E0,S1,E1)).

add_use(_Mod, _,_,_,_,_,_).



/*
writeln(T),
length(L0,6),
append(L0,_,L),
writeln(L0).
*/
