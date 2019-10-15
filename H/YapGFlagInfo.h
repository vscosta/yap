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
 * File:		YapGFlagInfo.h * Last rev:
 ** mods: * comments:	global flag enumeration. *
 *									 *
 *************************************************************************/

/**
 * @file   YapGFlagInfo.h
 * @author Vítor Santos Costa
 * @date   Thu Jul 11 12:21:10 2019
 * 
 * @brief  global flags and their default values.
 * 
 * 
 */

/**
 * @addtogroup YAPGFlags YAP Global Flags
 * @ingroup YAPFlags
 * @{
 *
 *
 * @enum GLOBAL_FLAGS
 * @brief YAP implements several Flags supported by YAP. This table includes only
 * flags that have an unique value in YAP. Some of these flags are read-only; they are
 * often used to access system flags.
 *
 */
#ifdef __ANDROID__
#define IN_ANDROID "true"
#else
#define IN_ANDROID "false"
#endif

#if DOXYFILE
#define  START_GLOBAL_FLAGS enum GLOBAL_FLAGS {
#define  END_GLOBAL_FLAGS };
#define  YAP_FLAG(ITEM,NAME,WRITABLE,DEF,INIT,HELPER) ` NAME `


#endif

START_GLOBAL_FLAGS



YAP_FLAG(ADDRESS_BITS_FLAG, "address_bits", false, nat, BITNESS, NULL), /**<      Number of address bits in the machine, either 64 or 32 bits.
									 */
  YAP_FLAG(AGC_MARGIN_FLAG, "agc_margin", true, nat, "10000",
             agc_threshold), /**<

@brief An integer: if this amount of atoms has been created since the last
atom-garbage collection, perform atom garbage collection at the first
opportunity. Initial value is 10,000. May be changed. A value of 0
(zero) disables atom garbage collection.
*/


  YAP_FLAG(ALLOW_ASSERT_FOR_STATIC_PREDICATES,
             "allow_assert_for_static_predicates", true, booleanFlag, "true",
             NULL),/**<    boolean, allow asserting and retracting clauses of static
    predicates. */




  YAP_FLAG(ANSWER_FORMAT_FLAG, "answer_format", true, isatom, "~p", NULL), /**<  @brief how to present answers, default is `~p`. */


  YAP_FLAG(ANDROID_FLAG, "android", false, ro, IN_ANDROID, NULL), /**<
   @brief  read-only boolean, a machine running an Google's Android version of the
   Linux Operating System */



  YAP_FLAG(APPLE_FLAG, "apple", false, ro,
#if __APPLE__
	   "true"
#else
 "false"
#endif
	   , NULL), /**<
		       @brief     read-only boolean, a machine running an Apple Operating System */


    YAP_FLAG(ARCH_FLAG, "arch", false, ro, YAP_ARCH, NULL), /**<
  @brief     read-only atom, it describes the ISA used in this version of YAP.
     Available from YAP_ARCH.
							    */


    YAP_FLAG(ARGV_FLAG, "argv", false, argv, "@boot", NULL), /**<
  @brief     read-only atom, it describes the list with all arguments received by YAP at boot.
     Available from YAP_ARCH.
							     */


  YAP_FLAG(ARITHMETIC_EXCEPTIONS_FLAG, "arithmetic_exceptions", true,
             booleanFlag, "true", NULL), /**<

@brief    Read-write flag telling whether arithmetic exceptions generate
    Prolog exceptions.


The next example shows zero division handling. If flag enabled:

    ~~~
    ?- X is 2/0.
    ERROR!!
    ZERO DIVISOR ERROR- X is Exp
    ~~~

    If disabled:
    ~~~
    ?- X is 2/0.
    X = (+inf).
    ~~~

    It is `true` by default, but it is disabled by packages like CLP(BN) and
    ProbLog.
					 */
   


  YAP_FLAG(BACK_QUOTES_FLAG, "back_quotes", true, isatom, "atom", bqs), /**<
    If  _Value_ is unbound, tell whether a back quoted list of characters
    token is converted to a list of atoms, `chars`, to a list of integers,
    `codes`, or to a single atom, `atom`. If  _Value_ is bound, set to
    the corresponding behavior. The default value is `string`
									*/
   


  YAP_FLAG(BOUNDED_FLAG, "bounded", false, booleanFlag, "false", NULL), /**<

@brief    Read-only flag telling whether integers are bounded. The value depends
    on whether YAP uses the GMP library or not.
									*/
   

  YAP_FLAG(C_CC_FLAG, "c_cc", false, isatom, C_CC, NULL), /**< C compiler used to generate YAP */


  YAP_FLAG(C_CFLAGS_FLAG, "c_cflags", false, isatom, C_CFLAGS, NULL), /**< C Compiler flagss used to compile flags */


  YAP_FLAG(C_LDFLAGS_FLAG, "c_ldflags", false, isatom, C_LDFLAGS, NULL), /**< loader flags used in YAP */


  YAP_FLAG(C_LIBPLSO_FLAG, "c_libplso", false, isatom, C_LIBPLSO, NULL), /**< shared libraries used in YAP */

/** libraries used in YAP */
    YAP_FLAG(C_LIBS_FLAG, "c_libs", false, isatom, C_LIBS, NULL),


  YAP_FLAG(CHAR_CONVERSION_FLAG, "char_conversion", true, booleanFlag,
             "false", NULL), /**<  Writable flag telling whether a character conversion table is used when
    reading terms.

 The default value for this flag is `off` except in
    `sicstus` and `iso` language modes, where it is `on`.
			     */


  YAP_FLAG(CHARACTER_ESCAPES_FLAG, "character_escapes", true, booleanFlag,
             "true", NULL), /**< Writable flag telling whether a character escapes are enabled,
     `true`, or disabled, `false`.

 The default value for this flag is
 `true`. */
   

  YAP_FLAG(COLON_SETS_CALLING_CONTEXT_FLAG, "colon_sets_calling_context",
	   true, booleanFlag, "true", NULL), /**<  whether to use `:` to set the calling module. */
   


    YAP_FLAG(COMPILED_AT_FLAG, "compiled_at", false, isatom, YAP_COMPILED_AT,
             NULL), /**< @brief     Read-only flag that gives the time when the main YAP binary was compiled.

     It is obtained straight from the __TIME__ macro, as defined in the C99.
		    */


  YAP_FLAG(DEBUG_INFO_FLAG, "debug_info", true, booleanFlag, "true", NULL), /**< generate debugging annotations.

The flag is ignored in YAP.
									    */
 

  YAP_FLAG(DEBUG_ON_ERROR_FLAG, "debug_on_error", true, booleanFlag, "false",
             NULL), /**< Says whether to call the debUgger on an exception. False in YAP..
		     */
   
 

    YAP_FLAG(DEBUGGER_PRINT_OPTIONS_FLAG, "debugger_print_options", true,
             list_option,
             "[quoted(true),numbervars(true),portrayed(true),max_depth(10)]" ,
             NULL), /**< controls debugging output.

    If bound, set the argument to the `write_term/3` options the
    debugger uses to write terms. If unbound, show the current options.
 */		   

    YAP_FLAG(DEBUGGER_SHOW_CONTEXT_FLAG, "debugger_show_context", true,
             booleanFlag, "false", NULL), /**< Show ancestors while debugging.
					   */


  YAP_FLAG(DEFAULT_PARENT_MODULE_FLAG, "default_parent_module", true, isatom,
             "user", NULL), /**<
     * A module to be inherited by all other modules. Default is user  that
     * reexports prolog.
     *
     * Set it to `prolog` for SICStus Prolog like resolution, to `user` for
     * SWI-like.
     */
   


  YAP_FLAG(DIALECT_FLAG, "dialect", false, ro, "yap", NULL), /**<

@brief    Read-only flag that always returns `yap`.
							     */
   


  YAP_FLAG(DISCONTIGUOUS_WARNINGS_FLAG, "discontiguous_warnings", true,
             booleanFlag, "true", NULL), /**<

@brief     If `true` (default `true`) YAP checks for definitions of the same predicate
     that are separated by clauses for other predicates. This may indicate that
     different procedures have the same name.

     The declaration discontiguous/1 disables this warning for user-specified
     predicates.

					 */
   


  YAP_FLAG(DOLLAR_AS_LOWER_CASE_FLAG, "dollar_as_lower_case", true,
             booleanFlag, "false", dollar_to_lc), /**<
    If `off` (default)  consider the character `$` a control character, if
vxu    `on` consider `$` a lower case character.
						  */
   


  YAP_FLAG(DOUBLE_QUOTES_FLAG, "double_quotes", true, isatom, "codes", dqs), /**<  iso

    If  _Value_ is unbound, tell whether a double quoted list of characters
    token is converted to a list of atoms, `chars`, to a list of integers,
    `codes`, or to a single atom, `atom`. If  _Value_ is bound, set to
    the corresponding behavior. The default value is `codes`. */
   
    YAP_FLAG(EDITOR_FLAG, "editor", true, isatom, "$EDITOR", NULL),


  YAP_FLAG(EXECUTABLE_FLAG, "executable", false, executable, "@boot", NULL), /**< @brief    Read-only flag. It unifies with an atom that gives the
    original program path.
									     */
   


  YAP_FLAG(FAST_FLAG, "fast", true, booleanFlag, "false", NULL), /**< @brief  If `on` allow fast machine code, if `off` (default) disable it. Only
    available in experimental implementations.
								 */


    YAP_FLAG(FILE_NAME_VARIABLES_FLAG, "file_name_variables", true, booleanFlag,
             "true", NULL), /**< Allow environment variables in file names */


  YAP_FLAG(FLOAT_FORMAT_FLAG, "float_format", true, isatom, "%.16f", NULL), /**< @brief    C-library `printf()` format specification used by write/1 and
    friends to determine how floating point numbers are printed. The
    default is `%.15g`. 

The specified value is passed to `printf()`
    without further checking. For example, if you want less digits
    printed, `%g` will print all floats using 6 digits instead of the
    default 15.
									    */
   


  YAP_FLAG(GC_FLAG, "gc", true, booleanFlag, "true", NULL), /**< controls garbage collection.

    If `false` allow garbage collection (default), if `false` disable it.
							    */
   


  YAP_FLAG(GC_MARGIN_FLAG, "gc_margin", true, nat, "4096", gc_margin), /**< controls  when to do garbage collection.

    Set or show the minimum free stack before starting garbage
    collection. The default is 0, in this case it depends on total stack size.

    If set to `[]` or other non-integer, it disables garbage collection.

								       */
   
  YAP_FLAG(GC_TRACE_FLAG, "gc_trace", true, isatom, "off", NULL),

/**< show activity in garbag collector.

    If `off` (default) do not show information on garbage collection
    and stack shifts, if `on` inform when a garbage collection or stack
    shift happened, if verbose give detailed information on garbage
    collection and stack shifts. Last, if `very_verbose` give detailed
    information on data-structures found during the garbage collection
    process, namely, on choice-points.
 */
   


  YAP_FLAG(GENERATE_DEBUGGING_INFO_FLAG, "generate_debug_info", true,
             booleanFlag, "true", NULL), /**< 

    If `true` (default) generate debugging information for
    procedures, including source mode. If `false` predicates no
    information is generated, although debugging is still possible, and
    source mode is disabled.

					 */


  YAP_FLAG(GMP_VERSION_FLAG, "gmp_version", false, isatom, "4.8.12", NULL), /**< which GMP package was used for infinite precisiion integers. */


  YAP_FLAG(HALT_AFTER_CONSULT_FLAG, "halt_after_consult", false, booleanFlag,
	   "false", NULL), /**< did we boot using -L ? */


  YAP_FLAG(HOME_FLAG, "home", false, isatom, YAP_ROOTDIR, NULL), /**< home `

     the root of the YAP installation, by default `/usr/local` in Unix or
     `c:\Yap` in Windows system. Can only be set at configure time
								 */
   


  YAP_FLAG(HOST_TYPE_FLAG, "host_type", false, isatom, HOST_ALIAS, NULL), /**< host_type `

    Return `configure` system information, including the machine-id
    for which YAP was compiled and Operating System information.
									  */
   


  YAP_FLAG(INDEX_FLAG, "index", true, indexer, "multi", NULL), /**< `index `

    If `on` allow indexing (default), if `off` disable it, if
    `single` allow on first argument only.
							       */
   


  YAP_FLAG(INDEX_SUB_TERM_SEARCH_DEPTH_FLAG, "index_sub_term_search_depth",
             true, nat, "0", NULL), /**< `Index_sub_term_search_depth `

   Maximum bound on searching sub-terms for indexing, if `0` (default) no
   bound.
				    */
   


  YAP_FLAG(INFORMATIONAL_MESSAGES_FLAG, "informational_messages", true,
             isatom, "normal", NULL), /**< `informational_messages `

    If `on` allow printing of informational messages, such as the ones
    that are printed when consulting. If `off` disable printing
    these messages. It is `on` by default except if YAP is booted with
    the `-L` flag.
				      */
   


  YAP_FLAG(INTEGER_ROUNDING_FUNCTION_FLAG, "integer_rounding_function", true,
             isatom, "toward_zero", NULL), /**< controls integer rounding function, is iso `

    Read-only flag telling the rounding function used for integers. Takes the
    value `toward_zero` for the current version of YAP.
					   */


  YAP_FLAG(ISO_FLAG, "iso", true, booleanFlag, "false", NULL), /**< Improve ISO compatibility. */


  YAP_FLAG(JUPYTER_FLAG, "jupyter", false, booleanFlag, "true", NULL), /**< read-only boolean, a machine running Jupyter */


  YAP_FLAG(LANGUAGE_FLAG, "language", true, isatom, "yap", NULL), /**< dialect used.

    Choose whether YAP follows native, closer to C-Prolog, `yap`, iso-prolog,
    `iso` or SICStus Prolog, `sicstus`. The current default is
    `cprolog`. This flag affects update semantics, leashing mode,
    style checking, handling calls to undefined procedures, how directives
    are interpreted, when to use dynamic, character escapes, and how files
    are consulted. Also check the `dialect` option.
								  */
   


   YAP_FLAG(MAX_ARITY_FLAG, "max_arity", false, isatom, "unbounded", NULL), /**< `max_arity is iso `

    Read-only flag telling the maximum arity of a functor. Takes the value
    `unbounded` for the current version of YAP.
									    */
   
    YAP_FLAG(MAX_TAGGED_INTEGER_FLAG, "max_tagged_integer", false, at2n,
             "INT_MAX", NULL),
    YAP_FLAG(MAX_THREADS_FLAG, "max_threads", false, at2n, "MAX_THREADS", NULL),
    YAP_FLAG(MAX_WORKERS_FLAG, "max_workers", false, at2n, "MAX_WORKERS", NULL),
    YAP_FLAG(MIN_TAGGED_INTEGER_FLAG, "min_tagged_integer", false, at2n,
             "INT_MIN", NULL),


    YAP_FLAG(MODULE_INDEPENDENT_OPERATORS_FLAG, "module_independent_operators",
             true, booleanFlag, "false", NULL),



  YAP_FLAG(N_OF_INTEGER_KEYS_IN_DB_FLAG, "n_of_integer_keys_in_db", false, ro,
             "256", NULL),
    YAP_FLAG(OCCURS_CHECK_FLAG, "occurs_check", true, booleanFlag, "false",
             NULL),


  YAP_FLAG(OPEN_EXPANDS_FILENAME_FLAG, "open_expands_filename", true,
             booleanFlag, "false", NULL), /**< `open_expands_filename `

    If `true` the open/3 builtin performs filename-expansion
    before opening a file (SICStus Prolog like). If `false` it does not
    (SWI-Prolog like).
					  */
   



/**< `module_independent_operators `

   If `true` an operator declaration will be valid for every module in the
   program. This is for compatibility with old software that
   might expect module-independent operators.
*/
  YAP_FLAG(OPEN_SHARED_OBJECT_FLAG, "open_shared_object", true, booleanFlag,
             "true", NULL), /**< `open_shared_object `

     If true, `open_shared_object/2` and friends are implemented,
     providing access to shared libraries (`.so` files) or to dynamic link
     libraries (`.DLL` files).
			    */
   
   


  YAP_FLAG(PROLOG_LIBRARY_DIRECTORY_FLAG, "prolog_library_directory", true,
             isatom, "", NULL), /**< if defined, first location where YAP expects to find the YAP Prolog
				   library. Takes precedence over library_directory */
   


  YAP_FLAG(PROLOG_FOREIGN_DIRECTORY_FLAG, "prolog_foreign_directory", true,
             isatom, "", NULL), /**< if defined, first location where YAP expects to find the YAP Prolog
				   shared libraries (DLLS). Takes precedence over executable_directory/2. */
   

    YAP_FLAG(OPTIMISE_FLAG, "optimise", true, booleanFlag, "false", NULL),
    YAP_FLAG(OS_ARGV_FLAG, "os_argv", false, os_argv, "@boot", NULL),
    YAP_FLAG(PID_FLAG, "pid", false, sys_pid, "@boot", NULL),
    YAP_FLAG(PIPE_FLAG, "pipe", true, booleanFlag, "true", NULL),


  YAP_FLAG(PROFILING_FLAG, "profiling", true, booleanFlag, "false", NULL), /**< `profiling `

     If `off` (default) do not compile call counting information for
     procedures. If `on` compile predicates so that they calls and
     retries to the predicate may be counted. Profiling data can be read through
     the call_count_data/3 built-in.
									   */
   


  YAP_FLAG(PROMPT_ALTERNATIVES_ON_FLAG, "prompt_alternatives_on", true,
             isatom, "determinism", NULL), /**< `  pt_alternatives_on(atom,
   changeable) `

   SWI-Compatible option, determines prompting for alternatives in the Prolog
   toplevel. Default is <tt>groundness</tt>, YAP prompts for alternatives if
   and only if the query contains variables. The alternative, default in
   SWI-Prolog is <tt>determinism</tt> which implies the system prompts for
   alternatives if the goal succeeded while leaving choicepoints. */
   
    YAP_FLAG(QUASI_QUOTATIONS_FLAG, "quasi_quotations", true, booleanFlag,
             "true", NULL),


  YAP_FLAG(READLINE_FLAG, "readline", true, booleanFlag, "false",
             Yap_InitReadline), /**< `readline(boolean, changeable)`
    }

    enable the use of the readline library for console interactions, true by
    default if readline was found. */
   


  YAP_FLAG(REDEFINE_WARNINGS_FLAG, "redefine_warnings", true, booleanFlag,
             "true", NULL), /**<

@brief If  _Value_ is unbound, tell whether warnings for procedures defined
in several different files are `on` or
`off`. If  _Value_ is bound to `on` enable these warnings,
and if it is bound to `off` disable them. The default for YAP is
`off`, unless we are in `sicstus` or `iso` mode.
			    */
    YAP_FLAG(REPORT_ERROR_FLAG, "report_error", true, booleanFlag, "true",
             NULL),



  YAP_FLAG(RESOURCE_DATABASE_FLAG, "resource_database", false, isatom,

             YAP_BOOTSTRAP, NULL), /**<`resource_database`
    Name of the resource file (saved-state or Prolog file) used to construct
    the YAP
    run-time environment.
				   */

   


  YAP_FLAG(SAVED_PROGRAM_FLAG, "saved_program", false, booleanFlag, "false",
             NULL), /**<`saved_program`
    if `true` YAP booted from a `yss` file, usually `startup.yss'. If
    `false`, YAP booted from a Prolog file, by default `boot.yap`.
		    */
   


  YAP_FLAG(SHARED_OBJECT_EXTENSION_FLAG, "shared_object_extension", false,
             isatom, SO_EXT, NULL), /**< `shared_object_extension `

    Suffix associated with loadable code.
				    */

   


  YAP_FLAG(SHARED_OBJECT_SEARCH_PATH_FLAG, "shared_object_search_path", true,
             isatom, SO_PATH, NULL), /**<

@brief     Name of the environment variable used by the system to search for shared
     objects.

				     */
   


  YAP_FLAG(SINGLE_QUOTES_FLAG, "single_quotes", true, isatom, "atom", sqf), /**< single_quoted text is usuallly interpreted as atoms. This flag
									       allows other interpretations such as strings */
   



  YAP_FLAG(SINGLE_VAR_WARNINGS_FLAG, "single_var_warnings", true, booleanFlag,
             "true", NULL), /**<
                         If `true` (default `true`) YAP checks for singleton
                         variables when loading files. A singleton variable is a
                         variable that appears ony once in a clause. The name
                         must start with a capital letter, variables whose name
                         starts with underscore are never considered singleton.

			    */


  YAP_FLAG(SIGNALS_FLAG, "signals", true, booleanFlag, "true", setSignals), /**<

@brief    If `true` (default) YAP handles  Signals such as `^C`
    (`SIGINT`).

									    */

   

    YAP_FLAG(SIGNAL_SEGV_FLAG, "signal_segv", true, booleanFlag, "true", Yap_InitSIGSEGV), /**<

@brief      If `true` (default) YAP handles  the segmentation violation signal. If `false`, the signal is ignored.
      SIGSEGV must be disabled by default to
      collaborate with languages like Java, that rely on SEGV for stack manipulation.

											   */


   

  YAP_FLAG(SOURCE_FLAG, "source", true, booleanFlag, "true", NULL), /**<

@brief       If `true` maintain the source for all clauses. Notice that this is trivially
       supported for facts, and always supported for dynamic code.

								    */
   


  YAP_FLAG(STRICT_ISO_FLAG, "strict_iso", true, booleanFlag, "false", NULL), /**<

    If  _Value_ is unbound, tell whether strict ISO compatibility mode
    is `on` or `off`. If  _Value_ is bound to `on` set
    language mode to `iso` and enable strict mode. If  _Value_ is
    bound to `off` disable strict mode, and keep the current language
    mode. The default for YAP is `off`.
    Under strict ISO Prolog mode all calls to non-ISO built-ins generate an
    error. Compilation of clauses that would call non-ISO built-ins will
    also generate errors. Pre-processing for grammar rules is also
    disabled. Module expansion is still performed.
    Arguably, ISO Prolog does not provide all the functionality required
    from a modern Prolog system. Moreover, because most Prolog
    implementations do not fully implement the standard and because the
    standard itself gives the implementor latitude in a few important
    questions, such as the unification algorithm and maximum size for
    numbers there is no guarantee that programs compliant with this mode
    will work the same way in every Prolog and in every platform. We thus
    believe this mode is mostly useful when investigating how a program
    depends on a Prolog's platform specific features.

									     */
   


  YAP_FLAG(SYSTEM_OPTIONS_FLAG, "system_options", false, options,
             SYSTEM_OPTIONS, NULL), /**<  

    This read only flag tells which options were used to compile
    YAP. Currently it informs whether the system supports `big_numbers`,
    `coroutining`, `depth_limit`, `low_level_tracer`,
    `or-parallelism`, `rational_trees`, `readline`, `tabling`,
    `threads`, or the `wam_profiler`.
				    */
   


  YAP_FLAG(SYSTEM_THREAD_ID_FLAG, "system_thread_id", false, sys_thread_id,
             "@boot", NULL), /**< 

   report the thread running YAP

			     */


  YAP_FLAG(TABLING_MODE_FLAG, "tabling_mode", true, isatom, "[]", NULL), /**< 

    Sets or reads the tabling mode for all tabled predicates. Please
    (see Tabling) for the list of options.

									 */
   

    YAP_FLAG(TIMEZONE_FLAG, "timezone", false, ro, "18000", NULL), /**< 

   report the timezone where YAP is running.

								   */


  YAP_FLAG(TOPLEVEL_HOOK_FLAG, "toplevel_hook", true, booleanFlag, "true",
             NULL), /**< 

    If bound, set the argument to a goal to be executed before entering the
    top-level. If unbound show the current goal or `true` if none is
    presented. Only the first solution is considered and the goal is not
    backtracked into.

		    */
  
 
    YAP_FLAG(TOPLEVEL_PRINT_ANON_FLAG, "toplevel_print_anon", true, booleanFlag,
             "true", NULL), /**< 

    If bound, set the argument to a goal to be executed before entering the
    top-level. If unbound show the current goal or `true` if none is
    presented. Only the first solution is considered and the goal is not
    backtracked into.

			    */

    YAP_FLAG(TOPLEVEL_PRINT_OPTIONS_FLAG, "toplevel_print_options", true,
             list_option, "[quoted(true),numbervars(true),portrayed(true)]",
             NULL), /**< 

    If bound, set the argument to a goal to be executed before entering the
    top-level. If unbound show the current goal or `true` if none is
    presented. Only the first solution is considered and the goal is not
    backtracked into.

 */

    YAP_FLAG(TOPLEVEL_PROMPT_FLAG, "toplevel_prompt", true, isatom, "?- ",
             mkprompt), /**< 

    If bound, set the argument to a goal to be executed before entering the
    top-level. If unbound show the current goal or `true` if none is
    presented. Only the first solution is considered and the goal is not
    backtracked into.

			*/
 
   YAP_FLAG(TTY_CONTROL_FLAG, "tty_control", true, booleanFlag, "true", NULL), /**< 

    If bound, set the argument to a goal to be executed before entering the
    top-level. If unbound show the current goal or `true` if none is
    presented. Only the first solution is considered and the goal is not
    backtracked into.

									       */




  YAP_FLAG(UNIX_FLAG, "unix", false, ro, "true", NULL), /**<  

    Define whether YAP should follow `immediate` update
    semantics, as in C-Prolog (default), or
 `logical` update semantics,
    as in Quintus Prolog, SICStus Prolog, or in the ISO standard. There is
    also an intermediate mode, `logical_assert`, where dynamic
    procedures follow logical semantics but the internal data base still
    follows immediate semantics.
 */
   
   
    YAP_FLAG(UPDATE_SEMANTICS_FLAG, "update_semantics", true, isatom, "logical",
             NULL), /**< @brief    Read-only BooleanFlag flag that unifies with `true` if YAP is
    running on an Unix system.  Defined if the C-compiler used to compile
    this version of YAP either defines `__unix__` or `unix`.
		    */


  YAP_FLAG(USER_FLAGS_FLAG, "user_flags", true, isatom, "error", NULL), /**<
    
     Define the behaviour of set_prolog_flag/2 if the flag is not known. Values
     are `silent`, `warning` and `error`. The first two create the flag
     on-the-fly, with `warning` printing a message. The value `error` is
     consistent with ISO: it raises an existence error and does not create the
     flag. See also `create_prolog_flag/3`. The default is`error`, and
     developers are encouraged to use `create_prolog_flag/3` to create flags for
     their library.
									*/
   


  YAP_FLAG(UNKNOWN_FLAG, "unknown", true, isatom, "error", Yap_unknown), /**<

@brief    Corresponds to calling the unknown/2 built-in. Possible ISO values
    are `error`, `fail`, and `warning`. Yap includes the following extensions:
    `fast_fail` does not invoke any handler.
									 */
   
  YAP_FLAG(VERSION_FLAG, "version", false, nat, YAP_NUMERIC_VERSION, NULL),
   


  YAP_FLAG(VERSION_DATA_FLAG, "version_data", false, ro, YAP_TVERSION, NULL), /**< 

   Read-only flag that unifies with a number of the form
   `_Major_ * 100000 + _Minor_ *100 + _Patch_`, where
   _Major_ is the major version,  _Minor_ is the minor version,
   and  _Patch_ is the patch number.
									      */
   


  YAP_FLAG(VERSION_GIT_FLAG, "version_git", false, ro, YAP_GIT_HEAD,
             NULL), /**< 

  this is the unique identifier for the last commit of the current GIT HEAD,
    it xan be used to identify versions that differ on small (or large) updates.
		    */

#if _WIN32
#define YAP_FOR_WIN32 "true"
#else
#define YAP_FOR_WIN32 "false"
#endif

    YAP_FLAG(WIN32_FLAG, "win32", false, ro,YAP_FOR_WIN32,
             NULL), /**<
`true` if YAP was compiled for the WIN32 standard Windows API.
		    */


    YAP_FLAG(WINDOWS_FLAG, "windows", false, ro,YAP_FOR_WIN32,
             NULL), /**<
`true` if YAP was compiled for the WIN32 standard Windows API.
		    */

    YAP_FLAG(WRITE_ATTRIBUTES_FLAG, "write_attributes", true, isatom, "ignore",
             NULL), /**< 

    Read-only boolean Flag flag that unifies with `true` if YAP is
    running on an Windows machine.
		    */
#if __WINDOWS__
   
    YAP_FLAG(WINDOWS_FLAG, "windows", false, ro, "true", NULL),
#endif


  YAP_FLAG(WRITE_STRINGS_FLAG, "write_strings", true, booleanFlag, "false",
             NULL), /**< 

    Writable flag telling whether the system should write lists of
    integers that are writable character codes using the list notation. It
    is `on` if enables or `off` if disabled. The default value for
    this flag is `off`.
		    */

   
    END_GLOBAL_FLAGS

    //! @}
