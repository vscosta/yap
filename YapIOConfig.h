
// play nice
#ifndef HAVE_PYTHON
/* #undef HAVE_PYTHON */
#endif
#if HAVE_PYTHON
#include <Python.h>
#endif

/* Define if you have libreadline */
#ifndef HAVE_LIBREADLINE
#define HAVE_LIBREADLINE  CACHE;YES;BOOL;Readline works.
#endif

/* Define to 1 if you have the <readline/history.h> header file. */
#ifndef HAVE_READLINE_HISTORY_H
#define HAVE_READLINE_HISTORY_H 1
#endif

/* Define to 1 if you have the <readline/readline.h> header file. */
#ifndef HAVE_READLINE_READLINE_H
#define HAVE_READLINE_READLINE_H  1
#endif

#if  defined(HAVE_READLINE_READLINE_H) && defined(HAVE_LIBREADLINE)
#define USE_READLINE 1
#endif

/* Define to 1 if you have the declaration of `rl_catch_signals ', and to 0 if
you don't. */
#ifndef HAVE_DECL_RL_CATCH_SIGNALS_
#define HAVE_DECL_RL_CATCH_SIGNALS 1
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
#define HAVE_RL_BEGIN_UNDO_GROUP 1
#endif

/* Define to 1 if you have the `rl_clear_pending_input' function. */
#ifndef HAVE_RL_CLEAR_PENDING_INPUT
#define HAVE_RL_CLEAR_PENDING_INPUT 1
#endif

/* Define to 1 if the system has the type `rl_completion_entry_function'). */
#ifndef HAVE_RL_COMPLETION_ENTRY_FUNCTION
/* #undef HAVE_RL_COMPLETION_ENTRY_FUNCTION */
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
#define HAVE_RL_DISCARD_ARGUMENT 1
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
#define HAVE_RL_FREE_LINE_STATE 1
#endif

/* Define to 1 if the system has the type `rl_hook_func_t'. */
#ifndef HAVE_RL_HOOK_FUNC_T
/* #undef HAVE_RL_HOOK_FUNC_T */
#endif

/* Define to 1 if you have the `rl_insert_close' function. */
#ifndef HAVE_RL_INSERT_CLOSE
#define HAVE_RL_INSERT_CLOSE 1
#endif

/* Define to 1 if you have the `rl_reset_after_signal' function. */
#ifndef HAVE_RL_RESET_AFTER_SIGNAL
#define HAVE_RL_RESET_AFTER_SIGNAL 1
#endif

/* Define to 1 if you have the `rl_set_keyboard_input_timeout' function. */
#ifndef HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT
#define HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT 1
#endif

/* Define to 1 if you have the `rl_set_prompt' function. */
#ifndef HAVE_RL_SET_PROMPT
#define HAVE_RL_SET_PROMPT 1
#endif

/* Define to 1 if you have the `rl_set_signals' function. */
#ifndef HAVE_RL_SET_SIGNALS
#define HAVE_RL_SET_SIGNALS 1
#endif
