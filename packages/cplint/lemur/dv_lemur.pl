/*:- use_module(library(terms)).
*/
:- use_module(library(lists)).


/*define the depth of a variable appearing in a clause A B ^ : : : ^ Br as follows.

 Variables appearing in the head of a clause have depth zero. 
 Otherwise, let Bi be the first literal containing the variable V, and let d be the maximal depth of the input variables of Bi
 then the depth of V is d + 1. The depth of a clause is the maximal depth of any variable in the clause.

In questo modo possiamo lasciare il numero massimo di variabili a 4 (e cosi' impara la regola con taughtby) e riduciamo la profondita' massima delle variabili a 2 (in questo modo dovremmo imparare la regola con i due publication nel body e anche quella con taughtby).
Bisogna modificare revise.pl per controllare che gli atomi che si aggiungono nel body non abbiano variabili oltre la profondita' massima.

testa professor(_710033).
body  taughtby(_710343,_710033,_710355).

Vars1   V1=_710033,     V2=_710343,     V3=_710355

testa professor(_710033).
body  yearsinprogram(_710149,_710196).

Vars1   V1=_710033,     V2=_710149,     V3=_710196.

*/



:- op(500,fx,#).
:- op(500,fx,'-#').
%:-['graphics_train.l'].

%setting(maxdepth_var,1).
%funzionamento 
%?- t(DV).
%DV = [[_A,1],[_B,0],[_C,0]]	- lista di coppie [variabile,profondità massima]



t(DV):-%	dv([advisedby(A,B)],[taughtby(C,B,D),ta(C,A,D)],DV).  
	dv([advisedby(A,B)],[publication(C,B),publication(C,A),professor(B),student(A)],DV).

%	dv([professor(A)],[taughtby(B,A,C),taughtby(D,A,E),taughtby(D,A,E)],DV).  %max_var 5	


dv(H,B,DV1):-			%-DV1
	term_variables(H,V),
	head_depth(V,DV0),
	findall((MD-DV),var_depth(B,DV0,DV,0,MD),LDs), % cerchiamo tutte le possibili liste di coppie var-prof che si possono generare in base alle scelte del modeb e poi prendiamo la lista che porta al massimo della profondita' massima
        get_max(LDs,-1,-,DV1).
	


input_variables_b(LitM,InputVars):-
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  modeb(_,Lit1),
	  input_vars(LitM,Lit1,InputVars).

/* decommentare per testare il file da solo */
/*	  input_vars(Lit,Lit1,InputVars):-
		      Lit =.. [_|Vars],
		      Lit1 =.. [_|Types],
		      input_vars1(Vars,Types,InputVars).


	  input_vars1([],_,[]).

	  input_vars1([V|RV],[+_T|RT],[V|RV1]):-
		    !,
	  input_vars1(RV,RT,RV1).

	  input_vars1([_V|RV],[_|RT],RV1):-
	        input_vars1(RV,RT,RV1).
*/


depth_var_head(List,VarsD):-   % exit:depth_var_head([professor(_G131537)],[[_G131537,0]]) ?
  term_variables(List,Vars0),   %List = lista atomi testa, Vars0 = lista variabili estratte dalla testa (term_variables  _710033,_710237,_711016,_710969).
  head_depth(Vars0,VarsD).	%aggiunge la profondità 0 ad ogni variabile, creando sottoliste

head_depth([],[]).
head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).

/*
depth_var_body(VarsH,BL,VarsD):-
	term_variables(List,Vars0),   	     %estrae la lista Vars0 di variabili degli atomi del body in List
	exclude_headvar(VarsH,Vars0,VarsB),  %VarsB: lista variabili v nel body per cui calcolare d(v) - diverse da quelle nella testa per cui vale 0 (Z,W,R)
	set_Uv(VarsB,BL,Sets), 		     %Sets: a ogni var v in VarsB associa il set U_v delle var!=v: lista di liste [v,(set)]
 	max(Sets,VarsH,VarsD).	            %VarsD: a ogni var v associa la profondità, usando le variabili nella testa VarsH che hanno d=0 (crea tuple (v,d))			
	*/

var_depth([],PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],PrevDs,PrevDs1,_MD,MD):-    		%L=body atom
% MD e' la profondita' massima a cui si e' arrivati
  input_variables_b(L,InputVars),          	%variabili di input nell'atomo L
%  write(L),format("~n variabili di input:",[]),write_list(InputVars),  %L=letterale del body=ta(_710237,_710858,_711331) InputVars = variabili di input nel letterale=_710237,_710858.
  term_variables(L, BodyAtomVars),   		   %BodyAtomVars: estrae dal letterale Lit del body la lista di variabili
  output_vars(BodyAtomVars,InputVars,OutputVars),  %OutputVars = BodyAtomVars-InputVars
  depth_InputVars(InputVars,PrevDs,0,MaxD),   %MaxD: massima profondita' delle variabili di input presenti nel letterale
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0),  %Ds: lista di liste [v,d] per tutte le  variabili (assegna D a tutte le variabili)

  %  term_variables(PrevLits,PrevVars),     	%PrevVars: lista variabili nella testa
  %  write(BodyD),
  %  PrevDs1 = [BodyD|PrevDs].
 var_depth(R,PrevDs0,PrevDs1,D,MD).

get_max([],_,Ds,Ds).

get_max([(MD-DsH)|T],MD0,_Ds0,Ds):-
  MD>MD0,!,
  get_max(T,MD,DsH,Ds).

get_max([_H|T],MD,Ds0,Ds):-
	get_max(T,MD,Ds0,Ds).


output_vars(OutVars,[],OutVars):-!.
output_vars(BodyAtomVars,[I|InputVars],OutVars):-	%esclude le variabili di input dalla lista di var del letterale del body
  delete(BodyAtomVars, I, Residue),   			%cancella I da BodyAtomVars
  output_vars(Residue,InputVars, OutVars).

% restituisce in D la profondita' massima delle variabili presenti nella lista passata come primo argomento
depth_InputVars([],_,D,D).
depth_InputVars([I|Input],PrevDs,D0,D):-
	 member_l(PrevDs,I,MD),
	 (MD>D0->
		D1=MD
	;
		D1=D0
         ),
	 depth_InputVars(Input,PrevDs,D1,D).
     
member_l([[L,D]|P],I,D):-   %resituisce in output la profondita' della variabile I
     I==L,!.
member_l([_|P],I,D):-
     member_l(P,I,D).

compute_depth([],_,PD,PD):-!.   %LVarD 
compute_depth([O|Output],D,PD,RestO):-   %LVarD 
	member_l(PD,O,_),!, % variabile gia' presente
	compute_depth(Output,D,PD,RestO).
	  
compute_depth([O|Output],D,PD,[[O,D]|RestO]):-   %LVarD 
	compute_depth(Output,D,PD,RestO).

 %Otherwise, let Bi be the first literal containing the variable V, and let d be the maximal depth of the input variables of Bi: then the depth of V is d+1. The depth of a clause is the maximal depth of any variable in the clause.
 %


 %[[_A,1],[_B,0],[_C,0]]
exceed_depth([],_):-!.
exceed_depth([H|T],MD):-
	nth1(2,H,Dep),	%estrae la profondità
	%setting(maxdepth_var,MD),
%	(Dep>=MD ->
%		format("*****************depth exceeded ~n")
%	;
%		true
%	),
	Dep<MD,
	exceed_depth(T,MD).
