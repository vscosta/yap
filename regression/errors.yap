/** 
 * @file regression/errors.yap
 * author Vitor Santos Costa
 *
 * @defgroup ErrorHandlerTesting Test Error Handler
 * @ingroup Regression System Tests
 *
 * Just check if the error handler is up to scratch..
 */
 
:- style_check(all).

:- discontiguous main/0.

:- initialization(main).

:- initialization( open('errors.md', write, _Out, [alias(user_error)]), now ).

:- format(user_error, ' Report on error handling~n~n', []).

%% + discontiguous
:- format(user_error, ' + warning: style check discontiguous~n~n~*c~n', [4,0'~,4,0'~]).

a(1).
a(2,2).
a(1).

%% + multifile
:- format(user_error, '~*c~n~n + warning: style check multifile~n~*c~n', [4,0'~,4,0'~]).

:- load_files(mu1,[silent(true)]).

:- load_files(mu2,[silent(true)]).

%% + singleton
:- format(user_error, '~*c~n~n + warning: style check singleton~n~*c~n', [4,0'~,4,0'~]).

a(X).  % X
a(_X). % no msg
a :- b(X) ; c(X). %no msg


:- format(user_error, '~*c~n', [4,0'~]).


%% end of tests,
main.