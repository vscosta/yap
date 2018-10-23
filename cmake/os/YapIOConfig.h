/* Define if you have libreadline */
#ifndef HAVE_LIBREADLINE
/* #undef HAVE_LIBREADLINE */
#endif

/* Define to 1 if you have the <readline/history.h> header file. */
#ifndef HAVE_READLINE_HISTORY_H
/* #undef HAVE_READLINE_HISTORY_H */
#endif

/* Define to 1 if you have the <readline/readline.h> header file. */
#ifndef HAVE_READLINE_READLINE_H
/* #undef HAVE_READLINE_READLINE_H */
#endif

#if  defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_READLINE_H)
#define USE_READLINE 1
#endif

/* Define to 1 if you have the declaration of `rl_catch_signals ', and to 0 if
you don't. */
#ifndef HAVE_DECL_RL_CATCH_SIGNALS_
/* #undef HAVE_DECL_RL_CATCH_SIGNALS */
#endif

/* Define to 1 if you have the declaration of `rl_done ', and to 0 if you
don't. */
#ifndef HAVE_DECL_RL_DONE_
/* #undef HAVE_DECL_RL_DONE_ */
#endif

/* Define to 1 if you have the declaration of `rl_event_hook', and to 0 if you
don't. */
#ifndef HAVE_DECL_RL_EVENT_HOOK
/* #undef HAVE_DECL_RL_EVENT_HOOK */
#endif

/* Define to 1 if you have the declaration of `rl_readline_state', and to 0 if
you don't. */
#ifndef HAVE_DECL_RL_READLINE_STATE
/* #undef HAVE_DECL_RL_READLINE_STATE */
#endif

/* Define to 1 if you have the `rl_begin_undo_group' function. */
#ifndef HAVE_RL_BEGIN_UNDO_GROUP
/* #undef HAVE_RL_BEGIN_UNDO_GROUP */
#endif

/* Define to 1 if you have the `rl_clear_pending_input' function. */
#ifndef HAVE_RL_CLEAR_PENDING_INPUT
/* #undef HAVE_RL_CLEAR_PENDING_INPUT */
#endif

/* Define to 1 if the system has the type `rl_completion_func_t'). */
#ifndef HAVE_RL_COMPLETION_FUNC_T
/* #undef HAVE_RL_COMPLETION_FUNC_T */
#endif

/* Define to 1 if the system has the type `rl_completion_func_t'. */
#ifndef HAVE_RL_COMPLETION_FUNC_T
/* #undef HAVE_RL_COMPLETION_FUNC_T */
#endif

/* Define to 1 if you have the `rl_completion_matches' function. */
#ifndef HAVE_RL_COMPLETION_MATCHES
/* #undef HAVE_RL_COMPLETION_MATCHES */
#endif

/* Define to 1 if you have the `rl_discard_argument' function. */
#ifndef HAVE_RL_DISCARD_ARGUMENT
/* #undef HAVE_RL_DISCARD_ARGUMENT */
#endif

/* Define to 1 if you have the `rl_done' variable. */
#ifndef HAVE_RL_DONE
#define HAVE_RL_DONE 
#endif

/* Define to 1 if you have the `rl_filename_completion_function' function. */
#ifndef HAVE_RL_FILENAME_COMPLETION_FUNCTION
#define HAVE_RL_FILENAME_COMPLETION_FUNCTION 
#endif

/* Define to 1 if you have the `rl_free_line_state' function. */
#ifndef HAVE_RL_FREE_LINE_STATE
/* #undef HAVE_RL_FREE_LINE_STATE */
#endif

/* Define to 1 if the system has the type `rl_hook_func_t'. */
#ifndef HAVE_RL_HOOK_FUNC_T
/* #undef HAVE_RL_HOOK_FUNC_T */
#endif

/* Define to 1 if you have the `rl_insert_close' function. */
#ifndef HAVE_RL_INSERT_CLOSE
/* #undef HAVE_RL_INSERT_CLOSE */
#endif

/* Define to 1 if you have the `rl_reset_after_signal' function. */
#ifndef HAVE_RL_RESET_AFTER_SIGNAL
/* #undef HAVE_RL_RESET_AFTER_SIGNAL */
#endif

/* Define to 1 if you have the `rl_set_keyboard_input_timeout' function. */
#ifndef HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT
/* #undef HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT */
#endif

/* Define to 1 if you have the `rl_set_prompt' function. */
#ifndef HAVE_RL_SET_PROMPT
/* #undef HAVE_RL_SET_PROMPT */
#endif
