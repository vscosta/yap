/**
 * @file log2md.yap
 * @author Vitor Santos Costa
 *
 *
 */

:-      op(650,yfx, <-- ),
     op(650,yfx, <-* ).

:- module( log2md,
    [open_log/1,
    log_title/1,
    log_section/1,
    log_subsection/1,
    log_paragraph/1,
    log_unit/2,
     (<--)/2,
     (<-*)/2,
     log_goal/1,
     log_goal/1 as log_clause,
     out/1,
     out/2,
     outln/1,
     outln/2] ).

:- use_module( library( maplist) ).


/**
 *
 *
 * @defgroup Log2MD Log Output of Tests in Markdown format.
 *
 * @ingroup Regression System Tests
 *
 *  These primitives support writing a user-specified log of execution to an
 *  output file. The output file can be used for testing or debugging.
 *
 *  Primitives include the ability to write a title, a Prolog clause or
 *  goal, and hooks for tracing calls. The log_goal/2 can be used to
 *  start a goal. Arguments of the form `<--/2` and `*->/2` can be used to
 *   track calls.
 *
 * The output format is markdown.
 */

open_log(F) :-
    open( F, write, _Out, [alias(log)]).

  /**
   * @pred log_title( +String ) is det
   *
   * @param [in] S is a Prolog atom or string describing a title.
   *
   */
  log_title( S ) :-
    out( '## Report on ~a~n~n', [S]).

  /**
   * @pred log_section( +String ) is det
   *
   * @param [in] S is a Prolog atom or string describing a title.
   *
   */
  log_section( S ) :-
    out( '### Report on ~a~n~n', [S]).

  /**
   * @pred log_section( +String ) is det
   *
   * @param [in] S is a Prolog atom or string describing a title.
   *
   */
  log_subsection( S ) :-
    out( '#### Report on ~a~n~n', [S]).

  /**
   * @pred log_section( +String ) is det
   *
   * @param [in] S is a Prolog atom or string describing a title.
   *
   */
  log_paragraph( S ) :-
    out( '##### Report on ~a~n~n', [S]).

  /**
   * @pred log_unit( +String, + Level ) is det
   *
   * @param [in] _String_ is a Prolog atom or string describing a title
   * @param [in] _Level_ is an integer number larager than 1 (do notice that )
   *large numbers may be ignored ).
   *
   *
   */
  log_unit( S ) :-
    out( '## Report on ~a~n~n', [S]).

  /**
   * @pred clause( +Term ) is det
   *
   * @param [in] Term is a Prolog clause or goal that it is going to
   * be printed out using portray_clause/2.
   *
   */
  log_goal( DecoratedClause ) :-
    take_decorations(DecoratedClause, Clause),
    out( '~~~~~~~~~n'),
    portray_clause( user_error , Clause ),
    portray_clause(        log , Clause ),
    out( '~~~~~~~~~n', []).

  take_decorations( G, G ) :-
    var(G),
    !.
  take_decorations(_ <-- G, NG ) :-
    !,
    take_decorations( G, NG ).
  take_decorations(_ <-* G, NG ) :-
    !,
    take_decorations( G, NG ).
  take_decorations(G, NG ) :-
    G =.. [F|Args],
    maplist( take_decorations, Args, NArgs ),
    NG =.. [F|NArgs].

  :- meta_predicate ( + <-- 0 ),
          ( + <-* 0 ).

  /**
   * @pred log_goal( +Tag , :Goal )
   *
   * @param [in] evaluate goal _Goal_ with output  before,
   * during and after the goal has been evaluated.
   *
   */
   A <-* Goal :-
    (
      outln(A),
      log_goal( Goal ),
      call( Goal )
    *->
      out('succeded as~n'),                                 log_goal(Goal)
    ;
      out( 'failed~n'),
      fail
    ).

  /**
   * @pred  +Tag <-- :Goal 
   *
   * @param [in] output goal _Goal_ before and after being evaluated, but only
   * taking the first solution. The _Tag_ must be an atom or a string.
   *
   */
  Tag <-- Goal :-
    (
      outln(Tag),
      log_goal( Goal ),
      call( Goal )
    ->
      out('succeded as~n'),
      log_goal(Goal),
      fail
    ;
      out(failed)
    ).


  /**
   * @pred out(+Format, +Args)
   *
   * @param [in] format the string given Args . The output is sent to
   * user_error and to a stream with alias `log`;
   *
   */
  out(Format, Args) :-
     format(        log, Format, Args),
     format( user_error, Format, Args).

  out(Format) :-
      format(        log, Format, []),
      format( user_error, Format, []).

    outln(Format, Args) :-
      out(Format, Args), out('~n').
    outln(Format) :-
      out(Format), out('~n').
