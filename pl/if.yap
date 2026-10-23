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

/** @pred    if( : _Goal_)

  Compile subsequent code only if  _Goal_ succeeds.  For enhanced
portability,  _Goal_ is processed by `expand_goal/2` before execution.
If an error occurs, the error is printed and processing proceeds as if
 _Goal_ has failed.

*/
%
% This is complicated because of embedded ifs.
%




'$ifctrl'(if(Goal), run) :-
    Goal,
    !,
    '$conditional_compilation_push'(run).
'$ifctrl'(if(_Goal), run) :-

    '$conditional_compilation_push'(skip).
'$ifctrl'(if(_Goal), done).
'$ifctrl'(if(_Goal), skip).

/** @pred   elif(+ _Goal_)


Equivalent to `:- else. :-if(Goal) ... :- endif.`  In a sequence
as below, the section below the first matching elif is processed, If
no test succeeds the else branch is processed.
*/
'$ifctrl'(elif(_Goal), run) :-
    '$conditional_compilation_set_state'(done ).
'$ifctrl'(elif(_Goal), done).
'$ifctrl'(elif(Goal), skip) :-
    Goal,
    !,
    '$conditional_compilation_push'(run).
'$ifctrl'(elif(_Goal), skip) :-
      '$conditional_compilation_push'(skip).

/**
@pred    else
Start `else' branch.

*/
'$ifctrl'(else, run) :-
    '$conditional_compilation_set_state'( done ).
'$ifctrl'(else, done).
'$ifctrl'(else, skip) :-

    '$conditional_compilation_set_state'(run ).

/** @pred    endif
 End of conditional compilation.

*/
 '$ifctrl'(endif, _) :-
    '$conditional_compilation_pop'.



%% base layer runs 
'$conditional_compilation_init':-
    nb_setval('$conditional_compilation_level',[run]).

'$conditional_compilation_get_state'(LB) :-
    '__NB_getval__'('$conditional_compilation_level', [LB|_], fail), !.

'$conditional_compilation_set_state'(New) :-
    '__NB_getval__'('$conditional_compilation_level', [_|LB], fail),
    nb_setval('$conditional_compilation_level', [New|LB]).

'$conditional_compilation_push'(Mode) :-
    '__NB_getval__'('$conditional_compilation_level', Levels, fail),
    nb_setval('$conditional_compilation_level', [Mode|Levels]).

'$conditional_compilation_pop' :-
    '__NB_getval__'('$conditional_compilation_level', [_|Levels], fail),
    nb_setval('$conditional_compilation_level', Levels).
    


:- '$conditional_compilation_init'.

'$if_directive'((if(_))).
'$if_directive'((else)).
'$if_directive'((elif(_))).
'$if_directive'((endif)).
