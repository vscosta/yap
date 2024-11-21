/** @file satsolver.pl
    @brief Prolog interface to SAT solver
*/

    /**
    @defgroup SAT
    @ingroup  YAPPackages
    @brief Calling a SAT solver from Prolog
    @{

	There are four SAT solver modules available:
	CryptoMinisat     (cryptominisat)
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

    :- create_prolog_flag(sat_solver, glucose4, [type(atom), access(read_write), keep(true)]).

    satSolverLibrary(minisat,'MINISAT'):-!.
    satSolverLibrary(glucose,'GLUCOSE'):-!.
    satSolverLibrary(glucose4,'GLUCOSE4'):-!.
    satSolverLibrary(cryptominisat,'CRYPTOMINISAT'):-!.
    satSolverLibrary(Value,_):-!, throw(error(domain_error(satSolver_module,Value),satSolverLibrary(Value,_))).
    


    % find which SAT solver to use 
    :- current_prolog_flag(sat_solver, Value),
    % translate value to library name
    satSolverLibrary(Value,SATsolver),
    % add predicate (used later in this code)
    !,
    % load SAT solver
    load_foreign_files([],[SATsolver]
		       ,install).




    % load SAT solver
    %:- useSatSolver(SATsolver), load_foreign_library(SATsolver,install).

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Solve CNF                        %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    /**
    * sat(+CNf)
    * call a sat solver on the conjunctive normal formula _CNF_
    *
    */
sat(CNF):-
    sat(CNF,Solved,_),!,
   Solved=1.

sat([],1,0.0):-!.
sat([[]],1,0.0):-!.
sat(F,Solved,Time):-
        statistics(cputime,[_,StartTime]),
	term_variables(F,FVars),
        solver_new_solver,
      %  solver_add_clause([1]), % true
        (addCnf2Solver(F,FVars) ->
            (solver_solve ->
                 solver_assign_model(FVars),
                 Solved = 1
            ;
                 Solved = 0
            )
        ;
%            writeln('conflict in cnf'),
            Solved=0
        ),
        solver_delete_solver,
        statistics(cputime,[_,EndTime]),!,
        Time is EndTime-StartTime.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solve CNF - Multi Solutions      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satMulti([],_,1,0.0):-!.
satMulti([[]],_,0,0.0):-!.
satMulti(F,MaxSols,SolCount,Time):-
        statistics(cputime,[_|StartTime]),
        solver_new_solver,
    %    solver_add_clause([1]), % true
        (addCnf2Solver(F,FVars) ->
            satMultiModels(MaxSols,Models),
            solver_delete_solver,!,
            length(Models,SolCount),
            (SolCount == 0 ; assignMultiSols(Models,FVars))
        ;
            solver_delete_solver,!,
%            writeln('conflict in cnf'),
            SolCount=0
        ),
        statistics(cputime,[_|EndTime]),!,
        Time is EndTime-StartTime.

satMultiModels(MaxSols,Models):-
        MaxSols > 0,!,
        (solver_solve ->
             solver_get_model(Model),
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
        solver_add_clause(NoAsgn).

negAll([V|Vs],[NV|NVs]):-!,
    NV is -(V),
       negAll(Vs,NVs).
negAll([],[]).


assignMultiSols(Models,FVars):-
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
    statistics(cputime,[_,StartTime]),
    solver_new_solver,
    %     solver_add_clause([1]),
    (addCnf2Solver(CNF,Unary,FVars,MaxLits) ->
     (solver_solve ->
      solver_get_model(Model),
      satMaxUnaryLoop(MaxLits,Model,FVars),
      Solved = 1
      ;
      Solved = 0
     )
     ;
     Solved=0
    ),
    solver_delete_solver,
    statistics(cputime,[_,EndTime]),!,
    Time is EndTime-StartTime.


    satMaxUnaryLoop(MaxLits,Model,FVars):-!,
    ((nextMaxUnaryValue(MaxLits,NewMaxLits), solver_solve) ->
     solver_get_model(NewModel),
     satMaxUnaryLoop(NewMaxLits,NewModel,FVars)
     ;
     assign_model(FVars,Model)
    ).

    nextMaxUnaryValue([X|MaxLits],NewMinLits):-!,
    I is abs(X),
    solver_get_var_assignment(I,Ival),
    TorF is sign(X)*sign(Ival),
    (TorF == 1 ->
     nextMaxUnaryValue(MaxLits,NewMinLits)
     ;
     %add_cnf_clauses([[X]]),
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
    statistics(cputime,[_,StartTime]),
    solver_new_solver,
    %     solver_add_clause([1]),
    (addCnf2Solver(CNF,Binary,FVars,MinLits) ->
     (solver_solve ->
      solver_get_model(Model),
      satMinBinaryLoop(MinLits,Model,FVars),
      Solved = 1
      ;
      Solved = 0
     )
     ;
     Solved=0
    ),
    solver_delete_solver,
    statistics(cputime,[_,EndTime]),!,
    Time is EndTime-StartTime.


    satMinBinaryLoop(MinLits,Model,FVars):-!,
    ((nextMinBinaryValue(MinLits,NewMinLits), solver_solve) ->
     solver_get_model(NewModel),
     satMinBinaryLoop(NewMinLits,NewModel,FVars)
     ;
     assign_model([1|FVars],Model)
    ).

    nextMinBinaryValue([X|MinLits],NewMinLits):-!,
    I is abs(X),
    solver_get_var_assignment(I,Ival),
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
    solver_get_var_assignment(I,Ival),
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
      Cnf=[[-Yi|Clause]]
     )
    ).
    groundVecGreatThanVec([],[],_I,_Last,Clause,[Clause]):-!.


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% General Aux Predicates           %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    :-dynamic(keptLiterals/1).
    keptLiterals([]).


    addCnf2Solver(Cnf,FVars):-
    term_variables(Cnf,FVars),!,
    \+ \+ ( bind2index(FVars,1,_FN), add_cnf_clauses(Cnf)).


    addCnf2Solver(Cnf,RememberVars,FVars,DimacsVars):-
    term_variables([Cnf|RememberVars],FVars),!,
    \+ \+ ( bind2index(FVars,1,FN), add_cnf_clauses(Cnf), add_cnf_clauses([[FN,-FN]]), keepLiterals(RememberVars),!),
    keptLiterals(DimacsVars),
    keepLiterals([]),!.

    keepLiterals(KeepLiterals):-
    retractall(satsolver:keptLiterals(_)),
    asserta(satsolver:keptLiterals(KeepLiterals)),!.

    :- if(current_prolog_flag(sat_solver,cryptominisat)).
    add_cnf_clauses([Cl|Cls]):-!,
    (Cl=[x|RCl] ->
     to_solver(RCl,SolverCl),
     solver_add_xorclause(SolverCl)
     ;
     to_solver(Cl,SolverCl),
     solver_add_clause(SolverCl)
    ),
    add_cnf_clauses(Cls).
    add_cnf_clauses([]):-!.
    :- else.
    add_cnf_clauses([Cl|Cls]):-!,
    to_solver(Cl,SolverCl),
    solver_add_clause(SolverCl),
    %%%%%     maplist(w,SolverCl),writeln(0),
    add_cnf_clauses(Cls).
    add_cnf_clauses([]):-!.
    :- endif.

    w(I) :- write(I), write(' ').
    
    to_solver([L|Ls],[N|Ns]) :-!,
    N is L,
    to_solver(Ls,Ns).
    to_solver([],[]):-!.

    bind2index([N|Ns],N,FN) :- N1 is N+1, bind2index(Ns,N1,FN).
    bind2index([],N,FN):-!, FN is N - 1.

    assign_model([],_).
    assign_model([V|Vs],[N|Ns]) :- ( N<0 -> V= -1 ; V=1), assign_model(Vs,Ns).

    %% @}
