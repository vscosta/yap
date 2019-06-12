%% The SWI-Prolog interface to SAT solver

/*
There are four SAT solver modules available:
    CryptoMinisat 2.5.1    (cryptominisat)
    Minisat 2.0.2          (minisat)
    Glucose 2.2            (glucose)
    Glucose 4.0            (glucose4)

Glucose 2.2 is the default SAT solver.

To change the default SAT solver, the user should use:
   :- nb_setval(satSolver_module,<New Value>).
before loading 'satsolver' module.

For example:
   :- module(myApp, [...]).
   :- nb_setval(satSolver_module, glucose4).
   :- use_module('satsolver',[sat/1]).

*/

:- module(satsolver,[
                        sat/3,          sat/1,
                        satMulti/4,
                        satMaxUnary/4,  satMaxUnary/2,
                        satMinUnary/4,  satMinUnary/2,
                        satMinBinary/4, satMinBinary/2 %%% Binary is MSB first
                       ]).

satSolverLibrary(cryptominisat,'pl-crypminisat'):-!.
satSolverLibrary(minisat,'pl-minisat'):-!.
satSolverLibrary(glucose,'pl-glucose'):-!.
satSolverLibrary(glucose4,'pl-glucose4'):-!.
satSolverLibrary(Value,_):-!, throw(settings_error(satSolver_module(Value))).

% find which SAT solver to use 
:- catch( nb_getval(satSolver_module,Value),
          error(existence_error(_,_),_),
          Value=glucose ),!,
    % translate value to library name
    satSolverLibrary(Value,SATsolver),
    % add predicate (used later in this code)
    dynamic(useSatSolver/1),!,
    assertz(useSatSolver(Value)),!,
    compile_predicates([useSatSolver/1]),!,
    % load SAT solver
    load_foreign_library(SATsolver,install).
        

% useSatSolver('pl-crypminisat').
% useSatSolver('pl-minisat').
% useSatSolver('pl-glucose').
% useSatSolver('pl-glucose4').

% load SAT solver
%:- useSatSolver(SATsolver), load_foreign_library(SATsolver,install).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solve CNF                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sat(CNF):-
   sat(CNF,Solved,_),!,
   Solved=1.

sat([],1,0.0):-!.
sat([[]],0,0.0):-!.
sat(F,Solved,Time):-
        statistics(cputime,StartTime),
        minisat_new_solver,
        minisat_add_clause([1]), % true
        (addCnf2Solver(F,FVars) ->
            (minisat_solve ->
                 minisat_assign_model([1|FVars]),
                 Solved = 1
            ;
                 Solved = 0
            )
        ;
%            writeln('conflict in cnf'),
            Solved=0
        ),
        minisat_delete_solver,
        statistics(cputime,EndTime),!,
        Time is EndTime-StartTime.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solve CNF - Multi Solutions      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satMulti([],_,1,0.0):-!.
satMulti([[]],_,0,0.0):-!.
satMulti(F,MaxSols,SolCount,Time):-
        statistics(cputime,StartTime),
        minisat_new_solver,
        minisat_add_clause([1]), % true
        (addCnf2Solver(F,FVars) ->
            satMultiModels(MaxSols,Models),
            minisat_delete_solver,!,
            length(Models,SolCount),
            (SolCount == 0 ; assignMultiSols(Models,FVars))
        ;
            minisat_delete_solver,!,
%            writeln('conflict in cnf'),
            SolCount=0
        ),
        statistics(cputime,EndTime),!,
        Time is EndTime-StartTime.

satMultiModels(MaxSols,Models):-
        MaxSols > 0,!,
        (minisat_solve ->
             minisat_get_model([_|Model]),
             % found solution
             Models=[Model|MoreModels],
             MaxSols1 is MaxSols - 1,
             ((MaxSols1 > 0, addClause_NotModule(Model)) ->
                 % need another solution
                 satMultiModels(MaxSols1,MoreModels)
               ;
                 % done searching
                 MoreModels=[])
             ;
             % no more solutions
             Models=[]).
satMultiModels(_,[]):-!.

addClause_NotModule(Model):-
        negAll(Model,NoAsgn),
        minisat_add_clause(NoAsgn).

negAll([V|Vs],[NV|NVs]):-!,
       NV is -(V),
       negAll(Vs,NVs).
negAll([],[]).


assignMultiSols(Models,FVars):-!,
       length(FVars,VarLen),
       length(SoFar,VarLen),
       assignAllCloseList(SoFar),!,
       assignMultiSols(Models,SoFar,FVars).

assignMultiSols([],SoFar,SoFar):-!.
assignMultiSols([M|Models],SoFar,Vars):-!,
       addModel2Vars(M,SoFar,NVars),!,
       assignMultiSols(Models,NVars,Vars).

addModel2Vars([],[],[]).
addModel2Vars([M|Ms],[V|Vs],[[NV|V]|NVs]):-
       (M>0 -> NV=1 ; NV= -1),
       addModel2Vars(Ms,Vs,NVs).

assignAllCloseList([]).
assignAllCloseList([[]|L]):-assignAllCloseList(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Minimize / Maximize unary number %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satMinUnary(CNF,Unary):-
    satMinUnary(CNF,Unary,Solved,_RunTime),!,
    Solved=1.

satMaxUnary(CNF,Unary):-
    satMaxUnary(CNF,Unary,Solved,_RunTime),!,
    Solved=1.

satMinUnary(CNF,Unary,Solved,RunTime):-
     reverseNot(Unary,RNUnary),
     satMaxUnary(CNF,RNUnary,Solved,RunTime).

satMaxUnary(CNF,Unary,Solved,Time):-
     statistics(cputime,StartTime),
     minisat_new_solver,
     minisat_add_clause([1]),
     (addCnf2Solver(CNF,Unary,FVars,MaxLits) ->
          (minisat_solve ->
               minisat_get_model(Model),
               satMaxUnaryLoop(MaxLits,Model,FVars),
               Solved = 1
          ;
               Solved = 0
          )
     ;
          Solved=0
     ),
     minisat_delete_solver,
     statistics(cputime,EndTime),!,
     Time is EndTime-StartTime.


satMaxUnaryLoop(MaxLits,Model,FVars):-!,
     ((nextMaxUnaryValue(MaxLits,NewMaxLits), minisat_solve) ->
         minisat_get_model(NewModel),
         satMaxUnaryLoop(NewMaxLits,NewModel,FVars)
     ;
         assign_model([1|FVars],Model)
     ).

nextMaxUnaryValue([X|MaxLits],NewMinLits):-!,
    I is abs(X),
    minisat_get_var_assignment(I,Ival),
    TorF is sign(X)*sign(Ival),
    (TorF == 1 ->
         nextMaxUnaryValue(MaxLits,NewMinLits)
    ;
         add_cnf_clauses([[X]]),
         NewMinLits=MaxLits
    ).
nextMaxUnaryValue([],[]):-!,fail.


reverseNot(Xs,Ys):-
   reverseNot(Xs,[],Ys).
reverseNot([X|Xs],RNXs,Ys):-!,
   reverseNot(Xs,[-X|RNXs],Ys).
reverseNot([],Ys,Ys):-!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Minimize Binary number           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satMinBinary(CNF,Binary):-
   satMinBinary(CNF,Binary,1,_).

% Binary MSB first
satMinBinary(CNF,Binary,Solved,Time):-
     statistics(cputime,StartTime),
     minisat_new_solver,
     minisat_add_clause([1]),
     (addCnf2Solver(CNF,Binary,FVars,MinLits) ->
          (minisat_solve ->
               minisat_get_model(Model),
               satMinBinaryLoop(MinLits,Model,FVars),
               Solved = 1
          ;
               Solved = 0
          )
     ;
          Solved=0
     ),
     minisat_delete_solver,
     statistics(cputime,EndTime),!,
     Time is EndTime-StartTime.


satMinBinaryLoop(MinLits,Model,FVars):-!,
     ((nextMinBinaryValue(MinLits,NewMinLits), minisat_solve) ->
         minisat_get_model(NewModel),
         satMinBinaryLoop(NewMinLits,NewModel,FVars)
     ;
         assign_model([1|FVars],Model)
     ).

nextMinBinaryValue([X|MinLits],NewMinLits):-!,
    I is abs(X),
    minisat_get_var_assignment(I,Ival),
    TorF is sign(X)*sign(Ival),
    (TorF == 1 ->
         getGroundBinary(MinLits,GroundBin),
         groundVecGreatThanVec([1|GroundBin],[X|MinLits],Cnf),
         add_cnf_clauses(Cnf),
         NewMinLits=[X|MinLits]
    ;
         add_cnf_clauses([[-X]]),
         nextMinBinaryValue(MinLits,NewMinLits)
    ).
nextMinBinaryValue([],[]):-!,fail.


getGroundBinary([X|Lits],[TorF|Vals]):-!,
    I is abs(X),
    minisat_get_var_assignment(I,Ival),
    TorF is sign(X)*sign(Ival),
    getGroundBinary(Lits,Vals).
getGroundBinary([],[]):-!.

% GroundVec > Vec
% Vec = binary number MSB first
groundVecGreatThanVec([X|GroundVec],[Y|Vec],[[-Y]|Cnf]):-
    X=:= -1,!,
    groundVecGreatThanVec(GroundVec,Vec,Cnf).
groundVecGreatThanVec([_X|GroundVec],[Y|Vec],Cnf):-!,
    findLastOne(GroundVec,2,1,Last),
    groundVecGreatThanVec(GroundVec,Vec,2,Last,[-Y],Cnf).


findLastOne([X|GroundVec],Indx,LastSeen,Last):-
    Indx1 is Indx + 1,
    (X =:= 1 ->
         findLastOne(GroundVec,Indx1,Indx,Last)
    ;
         findLastOne(GroundVec,Indx1,LastSeen,Last)
    ).
findLastOne([],_,LastSeen,LastSeen):-!.


groundVecGreatThanVec([Xi|GroundVec],[Yi|Vec],I,Last,Clause,Cnf):-
    I1 is I + 1,
    (Xi=:= -1 ->
       Cnf=[[-Yi|Clause]|MCnf],
       groundVecGreatThanVec(GroundVec,Vec,I1,Last,[Yi|Clause],MCnf)
    ;
           (I<Last ->
           groundVecGreatThanVec(GroundVec,Vec,I1,Last,[-Yi|Clause],Cnf)
       ;
    ).
           Cnf=[[-Yi|Clause]]
       )
groundVecGreatThanVec([],[],_I,_Last,Clause,[Clause]):-!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% General Aux Predicates           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic(keptLiterals(_)).
keptLiterals([]).


addCnf2Solver(Cnf,FVars):-
        term_variables(Cnf,FVars),!,
        \+ \+ ( bind2index(FVars,2,FN), add_cnf_clauses(Cnf), add_cnf_clauses([[FN,-FN]])).


addCnf2Solver(Cnf,RememberVars,FVars,DimacsVars):-
        term_variables([Cnf|RememberVars],FVars),!,
        \+ \+ ( bind2index(FVars,2,FN), add_cnf_clauses(Cnf), add_cnf_clauses([[FN,-FN]]), keepLiterals(RememberVars),!),
        keptLiterals(DimacsVars),
        keepLiterals([]),!.

keepLiterals(KeepLiterals):-
     retractall(satsolver:keptLiterals(_)),
     asserta(satsolver:keptLiterals(KeepLiterals)),!.

:- if(useSatSolver(cryptominisat)).
add_cnf_clauses([Cl|Cls]):-!,
     (Cl=[x|RCl] ->
        to_minisat(RCl,MiniSatCl),
        minisat_add_xorclause(MiniSatCl)
     ;
        to_minisat(Cl,MiniSatCl),
        minisat_add_clause(MiniSatCl)
     ),
     add_cnf_clauses(Cls).
add_cnf_clauses([]):-!.
:- else.
add_cnf_clauses([Cl|Cls]):-!,
     to_minisat(Cl,MiniSatCl),
     minisat_add_clause(MiniSatCl),
     add_cnf_clauses(Cls).
add_cnf_clauses([]):-!.
:- endif.

to_minisat([L|Ls],[N|Ns]) :-!,
    N is L,
    to_minisat(Ls,Ns).
to_minisat([],[]):-!.

bind2index([N|Ns],N,FN) :- N1 is N+1, bind2index(Ns,N1,FN).
bind2index([],N,FN):-!, FN is N - 1.

assign_model([],_).
assign_model([V|Vs],[N|Ns]) :- ( N<0 -> V= -1 ; V=1), assign_model(Vs,Ns).
