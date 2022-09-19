/**
 * @file YapLFlagInfo.h
 *
 * @addtogroup YAPLFlags YAP Thread-local flags
 * @ingroup YAPFlags
 * @{
 *
 * @brief Prolog flags that can take different values in different threads and/or modules.
 *
 *  @enum LOCAL_FLAGS  Local Flags supported by YAP
 *  @brief The table contains a set of  read-write  flags that can tak different values on
 *  different threads and/or modules. They are particularly useful in shaping I/O.
 */
#ifdef DOXYGEN
#define YAP_FLAG(K,N,I,OP,E,H) N

struct local_flag_t {
#endif



    YAP_FLAG(ALLOW_VARIABLE_NAME_AS_FUNCTOR_FLAG, "allow_variable_name_as_functor", true, booleanFlag, "false", NULL),
/**< Allow constructs such as 'Functor( V )'. Functor is parsed as an
   atom. The token `V` is still understood as a variable.<BR>

Originally a SWI-Prolog flag.<BR>
														   */    


    YAP_FLAG(AUTOLOAD_FLAG, "autoload", true, booleanFlag, "false",
             NULL),
/**< set the system to look for undefined procedures */    


    YAP_FLAG(BREAK_LEVEL_FLAG, "break_level", true, nat, "0",
             NULL),
/**< read-only flag, that tells if Prolog is in an inner top-level */    


    YAP_FLAG(CALL_COUNTING_FLAG, "call_counting", true, booleanFlag, "true",
             NULL),
/**< Predicates compiled with this flag set maintain a counter
               on the numbers of proceduree calls and of retries. These counters
               are  decreasing counters, and they can be used as timers. Three
               counters are available:

               * calls: number of predicate calls since execution started or
               since system was reset; retries: number of retries for predicates
               called since execution started or since counters were reset;

               * calls_and_retries: count both on predicate calls and
               retries. These counters can be used to find out how many calls a
               certain goal takes to execute. They can also be force the
               computation to stop.<br>

 		    */    


 YAP_FLAG(COMPILING_FLAG, "compiling", false, compiling,"false", NULL),

/**< Indicates YAP is
 running within the compiler. */
      
  YAP_FLAG(DEBUG_FLAG, "debug",  true, booleanFlag, "false", NULL),
  
/**< @brief  whether debugging is `true` or
   `false`.<br>

 If  _Value_ is bound to `true` enable debugging, and if
   it is bound to `false` disable debugging.<br>
      */

/**  
 */
/**< support for coding systens, YAP relies on UTF-8 internally.<br>
 */    YAP_FLAG(ENCODING_FLAG, "encoding", true, isatom, "utf8", getenc),

    

    

  YAP_FLAG(FILE_ERRORS_FLAG, "file_errors", true, febooleanFlag, "fail",
             NULL),
/**<         `exit` if failing to open a fail generates an excption;
     * or `fail` otherwise.<br>
*/
  


    YAP_FLAG(LANGUAGE_MODE_FLAG, "language_mode", true, isatom, "yap",
             NULL),
/**<
 @brief   whether native mode or trying to emulate a different
                 Prolog.<br>
		    */

   YAP_FLAG(EXPAND_FILE_NAME_FLAG, "expand_file_name", true, booleanFlag,
             "true", NULL),
/**< obtain the absol
 * ute file name before loading a file.<br>

11. */   

    YAP_FLAG(STACK_DUMP_ON_ERROR_FLAG, "stack_dump_on_error", true, booleanFlag,
/**< error handler should generate  a report on stack status. */             "true", NULL),
    

    YAP_FLAG(STREAM_TYPE_CHECK_FLAG, "stream_type_check", true, isatom, "loose",
             NULL),/**< If `true` show a stack dump when YAP finds an error. The default is
        `off`.<br>
	*/
    

    YAP_FLAG(SYNTAX_ERRORS_FLAG, "syntax_errors", true, synerr, "error",
             NULL),
/**<
@brief Control action to be taken after syntax errors when executing read/1,
`read/2`, or `read_term/3`:
+ `dec10` Report the syntax error and retry reading the term.<br>
+ `fail` Report the syntax error and fail.<br>
+ `error` Report the syntax error and generate an error (default).<br>
+ `quiet` Just fail
		  */    
  
    YAP_FLAG(TYPEIN_MODULE_FLAG, "typein_module", true, isatom, "user",
             typein),
/**<
   @brief   If bound, set the current working or type-in module to the argument,
   which must be an atom. If unbound, unify the argument with the current
   type-in module, that is, with the module YAP will execute goals by default.<br>

*/    


    YAP_FLAG(VERBOSE_FLAG, "verbose", true, isatom, "normal", NULL),
    
/**<

    If `normal` allow printing of informational and banner messages,
    such as the ones that are printed when consulting. If `silent`
    disable printing these messages. It is `normal` by default 1except if
    YAP is booted with the `-q` or `-L` flag.<br>

									 */

    YAP_FLAG(VERBOSE_FILE_SEARCH_FLAG, "verbose_file_search", true, booleanFlag,
/**<

       If `true` allow printing of informational messages when
       searching for file names. If `false` disable printing these
       messages. It is `false` by default.<br>
					 */             "false", NULL),
    


    YAP_FLAG(VERBOSE_LOAD_FLAG, "verbose_load", true, booleanFlag, "true", NULL),
/**<

        If `true` allow printing of informational messages when
        consulting files. If `false` disable printing these messages. It
        is `true` by default except if YAP is booted with the `-L` or `L`
        flags.<br>
										*/    

    YAP_FLAG(USER_ERROR_FLAG, "user_error", true, stream, "user_error",
             set_error_stream),
/**<

  If the second argument is bound to a stream, set user_error to
  this stream. If the second argument is unbound, unify the argument with
  the current user_error stream.<br>
  By default, the user_error stream is set to a stream
  corresponding to the Unix `stderr` stream.<br>
  The next example shows how to use this flag:

  ~~~~
  ?- open( '/dev/null', append, Error,
  [alias(mauri_tripa)] ).

  Error = '$stream'(3) ? ;

  no
  ?- set_prolog_flag(user_error, mauri_tripa).

  close(mauri_tripa).

  yes
  ?-
  ~~~~
  We execute three commands. First, we open a stream in write mode and
  give it an alias, in this case `mauri_tripa`. Next, we set
  user_error to the stream via the alias. Note that after we did so
  prompts from the system were redirected to the stream
  `mauri_tripa`. Last, we close the stream. At this point, YAP
  automatically redirects the user_error alias to the original
  `stderr`.<br>
			      */

      YAP_FLAG(USER_INPUT_FLAG, "user_input", true, stream, "user_input",
             set_input_stream),
/**<
    If the second argument is bound to a stream, set user_input to
    this stream. If the second argument is unbound, unify the argument with
    the current user_input stream. See the user_error_flag for more details.<br>
    */

YAP_FLAG(USER_OUTPUT_FLAG, "user_output", true, stream, "user_output",
             set_output_stream),
/**<
If the second argument is bound to a stream, set user_output to
this stream. If the second argument is unbound, unify the argument with
the current user_output stream.<br>
*/

  YAP_FLAG(NUMBERVARS_FUNCTOR_FLAG, "numbervars_functor", true, isatom, "$VAR",NULL)
/** <
This flag allow changing the `$VAR` functor name, making it possible to
have specialized versions of numbervar. 
*/             
 
 #ifdef DOXYGEN
      }
  #endif
    
    
// @}

