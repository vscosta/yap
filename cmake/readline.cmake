# include subdirectories configuration
## after we have all functionality in
#
# ADD_SUBDIRECTORY(console/terminal)



option (WITH_READLINE  "GNU readline console" ON)

if (WITH_READLINE)
 macro_optional_find_package (Readline ON)


include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckVariableExists)


  if (READLINE_FOUND)
  # - Find the readline library
  # This module defines
  #  READLINE_INCLUDE_DIR, path to readline/readline.h, etc.
  #  READLINE_LIBRARIES, the libraries required to use READLINE.
  #  READLINE_FOUND, If false, do not try to use READLINE.
  # also defined, but not for general use are
  # READLINE_readline_LIBRARY, where to find the READLINE library.
  # READLINE_ncurses_LIBRARY, where to find the ncurses library [might not be defined]
 check_include_files( "stdio.h;readline/readline.h" HAVE_READLINE_READLINE_H )
  check_include_files( "stdio.h;readline/history.h"  HAVE_READLINE_HISTORY_H )
  set(YAP_SYSTEM_OPTIONS "readline" ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
    check_function_exists( add_history  HAVE_ADD_HISTORY )
  check_function_exists( rl_begin_undo_group HAVE_RL_BEGIN_UNDO_GROUP)
  check_function_exists( rl_clear_pending_input HAVE_RL_CLEAR_PENDING_INPUT)
  check_function_exists( rl_discard_argument HAVE_RL_DISCARD_ARGUMENT)
  check_symbol_exists( rl_filename_completion_function  stdio.h;readline/readline.h HAVE_RL_FILENAME_COMPLETION_FUNCTION)
  check_function_exists( rl_free_line_state HAVE_RL_FREE_LINE_STATE )
  check_function_exists( rl_insert_close  HAVE_RL_INSERT_CLOSE )
  check_function_exists( rl_reset_after_signal  HAVE_RL_RESET_AFTER_SIGNAL )
  check_function_exists( rl_set_keyboard_input_timeout  HAVE_RL_SET_KEYBOARD_INPUT_TIMEOUT )
  check_function_exists( rl_set_prompt  HAVE_RL_SET_PROMPT)
  check_symbol_exists( rl_catch_signals "stdio.h;readline/readline.h"   HAVE_DECL_RL_CATCH_SIGNALS )
  check_type_size( rl_completion_func_t RL_COMPLETION_FUNC_T    )
  check_symbol_exists( rl_done stdio.h;readline/readline.h  HAVE_DECL_RL_DONE )
  CHECK_TYPE_SIZE( rl_hook_func_t  RL_HOOK_FUNC_T  )
  check_symbol_exists( rl_event_hook stdio.h;readline/readline.h HAVE_DECL_RL_EVENT_HOOK )
  check_symbol_exists( rl_readline_state stdio.h;readline/readline.h HAVE_DECL_RL_READLINE_STATE )
endif (READLINE_FOUND)


endif (WITH_READLINE)
