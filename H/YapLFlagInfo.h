/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 2015-		 *
*									 *
**************************************************************************
*									 *
* File:		YapLFlagInfo.h						 *
* Last rev:								 *
* mods:									 *
* comments:	local flag enumeration.				 *
*									 *
*************************************************************************/

/** @file YapLFlagInfo.h

    @ingroup Flags
*/

YAP_FLAG(  AUTOLOAD_FLAG, "autoload", true, booleanFlag, "false" , NULL ),
YAP_FLAG(  BREAK_LEVEL_FLAG, "break_level", true, nat, "0" , NULL ),
YAP_FLAG(  CALL_COUNTING_FLAG, "call_counting", true, booleanFlag, "true" , NULL ), /**< `call_counting`
                                                                                     
                                                                                     Predicates compiled with this flag set maintain a counter on the numbers of proceduree calls and of retries. These counters are  decreasing counters, and they can be used as timers. Three counters are available:
                                                                                     
                                                                                     calls: number of predicate calls since execution started or since system was reset;
                                                                                     retries: number of retries for predicates called since execution started or since counters were reset;
                                                                                     calls_and_retries: count both on predicate calls and retries.
                                                                                     These counters can be used to find out how many calls a certain goal takes to execute. They can also be force the computatiom yp
                                                                                     stopping.
                                                                               
                                                                               If `on` `fileerrors` is `on`, if `off` (default)
                                                                               `fileerrors` is disabled.
                                                                               */
YAP_FLAG(  ENCODING_FLAG, "encoding", true, isatom, "utf-8" , getenc ),
YAP_FLAG(  FILEERRORS_FLAG, "fileerrors", true, booleanFlag, "true" , NULL ), /**< `fileerrors`
                                                                           
                                                                           If `on` `fileerrors` is `on`, if `off` (default)
                                                                           `fileerrors` is disabled.
                                                                           */
YAP_FLAG(  LANGUAGE_MODE_FLAG, "language_mode", true, isatom, "yap" , NULL ), /**< `language_mode`
                                                                           
                                                                           wweter native mode or trying to emulate a different Prolog.
                                                                           */
YAP_FLAG(  REDEFINE_WARNINGS_FLAG, "redefine_warnings", true, booleanFlag, "true" , NULL ), /**< `redefine_warnings `

    If  _Value_ is unbound, tell whether warnings for procedures defined
in several different files are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.
 */
YAP_FLAG(  SINGLE_VAR_WARNINGS_FLAG, "single_var_warnings", true, booleanFlag, "true" , NULL ), /**< `single_var_warnings`
                                                                                             If `true` (default `true`) YAP checks for singleton variables when loading files. A singleton variable is a variable that appears ony once in a clause. The name must start with a capital letter, variables whose name starts with underscore are never considered singleton.
                                                                                             
                                                                                             */
YAP_FLAG(  STACK_DUMP_ON_ERROR_FLAG, "stack_dump_on_error", true, booleanFlag, "false" , NULL ), /**< `stack_dump_on_error `

    If `true` show a stack dump when YAP finds an error. The default is
`off`.
 */
YAP_FLAG(  STREAM_TYPE_CHECK_FLAG, "stream_type_check", true, isatom, "loose" , NULL ),
YAP_FLAG(  SYNTAX_ERRORS_FLAG, "syntax_errors", true, synerr, "error" , NULL ),	/**< `syntax_errors`

    Control action to be taken after syntax errors while executing read/1,
`read/2`, or `read_term/3`:
  + `dec10`
Report the syntax error and retry reading the term.
  + `fail`
Report the syntax error and fail.
  + `error`
Report the syntax error and generate an error (default).
  + `quiet`
Just fail
 */
YAP_FLAG(  TYPEIN_MODULE_FLAG, "typein_module", true, isatom, "user" , typein ),  /**< `typein_module `

    If bound, set the current working or type-in module to the argument,
which must be an atom. If unbound, unify the argument with the current
working module.

 */
  YAP_FLAG(  USER_ERROR_FLAG, "user_error", true, stream, "user_error" , set_error_stream ), /**< `user_error1`

    If the second argument is bound to a stream, set user_error to
this stream. If the second argument is unbound, unify the argument with
the current user_error stream.
By default, the user_error stream is set to a stream
corresponding to the Unix `stderr` stream.
The next example shows how to use this flag:

~~~{.prolog}
    ?- open( '/dev/null', append, Error,
            [alias(mauri_tripa)] ).

    Error = '$stream'(3) ? ;

    no
    ?- set_prolog_flag(user_error, mauri_tripa).

    close(mauri_tripa).

     yes
    ?- 
~~~
    We execute three commands. First, we open a stream in write mode and
give it an alias, in this case `mauri_tripa`. Next, we set
user_error to the stream via the alias. Note that after we did so
prompts from the system were redirected to the stream
`mauri_tripa`. Last, we close the stream. At this point, YAP
automatically redirects the user_error alias to the original
`stderr`.
 */
YAP_FLAG(  USER_INPUT_FLAG, "user_input", true, stream, "user_input" , set_input_stream ),
  YAP_FLAG(  USER_OUTPUT_FLAG, "user_output", true, stream, "user_output" , set_output_stream ),
