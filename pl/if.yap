/*************************************************************************
*									 *
  *	 YAP Prolog  							 *
*									 *
  *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		if.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Control Consulting Files in YAP					 *
*									 *
*************************************************************************/
/**
 * @file   if.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Nov 18 14:01:10 2015
 *
 * @brief  loading program chunks into YAP
 *
 *
*/
%% @}

/**

@defgroup Conditional_Compilation Conditional Compilation

@ingroup  YAPConsulting

@{
 Conditional compilation builds on the same principle as
term_expansion/2, goal_expansion/2 and the expansion of
grammar rules to compile sections of the source-code
conditionally. One of the reasons for introducing conditional
compilation is to simplify writing portable code.


Note that these directives can only be appear as separate terms in the
  input.  Typical usage scenarios include:


    Load different libraries on different dialects

    Define a predicate if it is missing as a system predicate

    Realise totally different implementations for a particular
part of the code due to different capabilities.

    Realise different configuration options for your software.

```
:- if(test1).
section_1.
:- elif(test2).
section_2.
:- elif(test3).
section_3.
:- else.
section_else.
  :- endif.
```

*/

'$if_directive'(if(Goal), M, _VL, _Pos, Option) :- 
    '$if'(M:Goal, Option).
'$if_directive'(elif(Goal), M, _VL, _Pos, Option) :-
    '$elif'(M:Goal, Option).
'$if_directive'(else, _M, _VL, _Pos, Option) :-
    '$else'( Option).
'$if_directive'(endif, _M, _VL, _Pos, Option) :-
    '$endif'( Option).

/** @pred    if( : _Goal_)

  Compile subsequent code only if  _Goal_ succeeds.  For enhanced
portability,  _Goal_ is processed by `expand_goal/2` before execution.
If an error occurs, the error is printed and processing proceeds as if
 _Goal_ has failed.

*/
%
% This is complicated because of embedded ifs.
%
'$if'(Goal) :-
    must_be_callable(Goal),
    '$conditional_compilation'(Inp),
    (Inp == skip
   ->
       Mode=done
  ;
    Inp == done
    ->
    Mode=done
    ;
	call(Goal)
    ->
    Mode = run
    ;
    Mode = skip
    ),
    '$conditional_compilation_push'(Mode).

/**
@pred    else
Start `else' branch.

*/
'$else' :-
    '$conditional_compilation'(Inp),
    ( Inp == run
    ->
    Mode = done
    ;
    Inp == skip
    ->
    Mode = run
    ;
    Mode = done
    ),
    '$conditional_compilation_set'(Mode).



/** @pred   elif(+ _Goal_)


Equivalent to `:- else. :-if(Goal) ... :- endif.`  In a sequence
as below, the section below the first matching elif is processed, If
no test succeeds the else branch is processed.
*/
'$elif'(Goal) :-
 	 '$conditional_compilation'(Inp),
   (
   Inp == run
    ->
    Mode = done
    
    ;
	 Inp == done
    ->
    Mode = done
    ;
	call(Goal)
    ->
    Mode = run
    ;
    Mode = skip
      ),
    '$conditional_compilation_set'(Mode).

/** @pred    endif
QEnd of cond  itional compilation.

*/
'$endif' :-
    '$conditional_compilation_pop'.

%% base layer runs 
'$conditional_compilation_init':-
    nb_setval('$conditional_compilation_level',[run]).

'$conditional_compilation_get_state'(state(LB)) :-
    nb_getval('$conditional_compilation_level', LB).

'$conditional_compilation_set_state'(state(LB)) :-
    nb_setval('$conditional_compilation_level', LB).

'$conditional_compilation_push'(Mode) :-
    nb_getval('$conditional_compilation_level', Levels),
    nb_setval('$conditional_compilation_level', [Mode|Levels]).


'$conditional_compilation'(Mode) :-
    nb_getval('$conditional_compilation_level', [Mode|_Levels]).


'$conditional_compilation_skip'(V)  :-
    var(V),
    !,
    nb_getval('$conditional_compilation_level', [L|_Levels]),
    (L == skip
    ;
    L == done),
    !.
'$conditional_compilation_skip'((:-if(G)))  :-
      '$if'(G),
      !.
'$conditional_compilation_skip'((:-elif(G)))  :-
      '$elif'(G),
!.
'$conditional_compilation_skip'((:-else))  :-
      '$else',
!.
'$conditional_compilation_skip'((:-endif))  :-
      '$endif',
!.
'$conditional_compilation_skip'(_)  :-
    nb_getval('$conditional_compilation_level', [L|_Levels]),
    (L == skip
    ;
    L == done),
    !.

'$conditional_compilation_set'(Mode) :-
    nb_getval('$conditional_compilation_level', [_Mode_|Levels]),
    nb_setval('$conditional_compilation_level', [Mode|Levels]).


'$conditional_compilation_pop' :-
    nb_getval('$conditional_compilation_level', [_|Levels]),
    nb_setval('$conditional_compilation_level', Levels).
    
:- '$conditional_compilation_init'.

'$if_call'(G) :-
	catch('$eval_if'(G), _E, error_handler).

'$eval_if'(Goal) :-
	expand_term(Goal,TrueGoal),
	once(TrueGoal).

'$if_directive'((if(_))).
'$if_directive'((else)).
'$if_directive'((elif(_))).
'$if_directive'((endif)).

