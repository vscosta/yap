
// play nice
#ifndef HAVE_PYTHON
#cmakedefine HAVE_PYTHON  ${HAVE_PYTHON}
#endif
#if HAVE_PYTHON
#include <Python.h>
#endif

/* Define if you have libreadline */
#ifndef HAVE_LIBREADLINE
#cmakedefine HAVE_LIBREADLINE  ${HAVE_LIBREADLINE}
#endif

/* Define to 1 if you have the <readline/history.h> header file. */
#ifndef HAVE_READLINE_HISTORY_H
#cmakedefine HAVE_READLINE_HISTORY_H ${HAVE_READLINE_HISTORY_H}
#endif

/* Define to 1 if you have the <readline/readline.h> header file. */
#ifndef HAVE_READLINE_READLINE_H
#cmakedefine HAVE_READLINE_READLINE_H  ${HAVE_READLINE_READLINE_H}
#endif

#if  defined(HAVE_READLINE_READLINE_H) && defined(HAVE_LIBREADLINE)
#define USE_READLINE 1
#endif

/* Define to 1 if you have the declaration of `rl_catch_signals ', and to 0 if
you don't. */
#ifndef HAVE_DECL_RL_CATCH_SIGNALS_
#cmakedefine HAVE_DECL_RL_CATCH_SIGNALS ${HAVE_DECL_RL_CATCH_SIGNALS}
#endif

/* Define to 1 if you have the declaration of `rl_done ', and to 0 if you
don't. */
#ifndef HAVE_DECL_RL_DONE_
#cmakedefine HAVE_DECL_RL_DONE_ ${HAVE_DECL_RL_DONE_}
#endif

/* Define to 1 if you have the declaration of `rl_event_hook', and to 0 if you
don't. */
#ifndef HAVE_DECL_RL_EVENT_HOOK
#cmakedefine HAVE_DECL_RL_EVENT_HOOK ${HAVE_DECL_RL_EVENT_HOOK}
#endif

/* Define to 1 if you have the declaration of `rl_readline_state', and to 0 if
you don't. */
#ifndef HAVE_DECL_RL_READLINE_STATE
#cmakedefine HAVE_DECL_RL_READLINE_STATE ${HAVE_DECL_RL_READLINE_STATE}
#endif

        /* Define to 1 if you have the `rl_begin_undo_group' function. */
#ifndef HAVE_RL_BEGIN_UNDO_GROUP
#cmakedefine HAVE_RL_BEGIN_UNDO_GROUP ${HAVE_RL_BEGIN_UNDO_GROUP}
#endif

/* Define to 1 if you have the `rl_clear_pending_input' function. */
#ifndef HAVE_RL_CLEAR_PENDING_INPUT
#cmakedefine HAVE_RL_CLEAR_PENDING_INPUT ${HAVE_RL_CLEAR_PENDING_INPUT}
#endif

/* Define to 1 if the system has the type `rl_completion_entry_function'). */
#ifndef HAVE_RL_COMPLETION_ENTRY_FUNCTION
#cmakedefine HAVE_RL_COMPLETION_ENTRY_FUNCTION ${HAVE_RL_COMPLETION_ENTRY_FUNCTION}
#endif

/* Define to 1 if the system has the type `rl_completion_func_t'. */
#ifndef HAVE_RL_COMPLETION_FUNC_T
#cmakedefine HAVE_RL_COMPLETION_FUNC_T ${HAVE_RL_COMPLETION_FUNC_T}
#endif

/* Define to 1 if you have the `rl_completion_matches' function. */
#ifndef HAVE_RL_COMPLETION_MATCHES
#cmakedefine HAVE_RL_COMPLETION_MATCHES ${HAVE_RL_COMPLETION_MATCHES}
#endif

/* Define to 1 if you have the `rl_discard_argument' function. */
#ifndef HAVE_RL_DISCARD_ARGUMENT
#cmakedefine HAVE_RL_DISCARD_ARGUMENT ${HAVE_RL_DISCARD_ARGUMENT}
#endif

/* Define to 1 if you have the `rl_done' variable. */
#ifndef HAVE_RL_DONE
#define HAVE_RL_DONE ${HAVE_RL_DONE}
#endif

/* Define to 1 if you have the `rl_filename_completion_function' function. */
#ifndef HAVE_RL_FILENAME_COMPLETION_FUNCTION
#define HAVE_RL_FILENAME_COMPLETION_FUNCTION ${HAVE_RL_FILENAME_COMPLETION_FUNCTION}
#endif

/* Define to 1 if you have the `rl_free_line_state' function. */
#ifndef HAVE_RL_FREE_LINE_STATE
#cmakedefine HAVE_RL_FREE_LINE_STATE ${HAVE_RL_FREE_LINE_STATE}
#endif

/* Define to 1 if the system has the type `rl_hook_func_t'. */
#ifndef HAVE_RL_HOOK_FUNC_T
#cmakedefine HAVE_RL_HOOK_FUNC_T ${HAVE_RL_HOOK_FUNC_T}
#endif

/* Define to 1 if you have the `rl_insert_close' function. */
#ifndef HAVE_RL_INSERT_CLOSE
#cmakedefine HAVE_RL_INSERT_CLOSE ${HAVE_RL_INSERT_CLOSE}
#endif

/* Define to 1 if you have the `rl_reset_after_signal' function. */
#ifndef HAVE_RL_RESET_AFTER_SIGNAL
#cmakedefine HAVE_RL_RESET_AFTER_SIGNAL ${HAVE_RL_RESET_AFTER_SIGNAL}
#endif

/* Define to 1 if you have the `rl_set_keyboard_input_timeout' function. */
#ifndef HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT
#cmakedefine HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT ${HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT}
#endif

/* Define to 1 if you have the `rl_set_prompt' function. */
#ifndef HAVE_RL_SET_PROMPT
#cmakedefine HAVE_RL_SET_PROMPT ${HAVE_RL_SET_PROMPT}
#endif

/* Define to 1 if you have the `rl_set_signals' function. */
#ifndef HAVE_RL_SET_SIGNALS
#cmakedefine HAVE_RL_SET_SIGNALS ${HAVE_RL_SET_SIGNALS}
#endif
