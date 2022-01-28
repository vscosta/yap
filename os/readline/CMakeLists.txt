
  
OPTION(WITH_READLINE " Enable GNU Readline" ON)


if (WITH_READLINE)


set (YAP_READLINE_SOURCES
  readline.c
 )
 
 list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR})
 
 find_package(Readline)

if (READLINE_FOUND)

#  READLINE_FOUND            System has readline, include and lib dirs found
#  Readline_INCLUDE_DIR      The readline include directories.
#  Readline_LIBRARY          The readline library.

  include_directories ( .. ../H ../include ../OPTYap  ${Readline_INCLUDE_DIR} )

set (POSITION_INDEPENDENT_CODE TRUE)


set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1;  HAVE_LIBREADLINE=1;USE_READLINE=1)

    check_include_files( "stdio.h;readline/readline.h" HAVE_READLINE_READLINE_H )
check_include_files( "stdio.h;readline/history.h"  HAVE_READLINE_HISTORY_H )
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
check_function_exists( rl_set_signals  HAVE_RL_SET_SIGNALS)
check_symbol_exists( rl_catch_signals "stdio.h;readline/readline.h"   HAVE_DECL_RL_CATCH_SIGNALS )
check_type_size( rl_completion_func_t RL_COMPLETION_FUNC_T    )
check_symbol_exists( rl_done stdio.h;readline/readline.h  HAVE_DECL_RL_DONE )
CHECK_TYPE_SIZE( rl_hook_func_t  RL_HOOK_FUNC_T  )
check_symbol_exists( rl_event_hook stdio.h;readline/readline.h HAVE_DECL_RL_EVENT_HOOK )
check_symbol_exists( rl_readline_state stdio.h;readline/readline.h HAVE_DECL_RL_READLINE_STATE )
check_function_exists( add_history HAVE_ADD_HISTORY)
check_function_exists( remove_history HAVE_REMOVE_HISTORY)
check_function_exists( using_history HAVE_USING_HISTORY)



 # add_component (  YAPReadline
 #   ${YAP_READLINE_SOURCES})
    
#target_link_libraries(libYap

# install(TARGETS YAPReadline
#   RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
#   LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#   ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
#   )


configure_file(${CMAKE_SOURCE_DIR}/os/YapIOConfig.h.cmake ${CMAKE_BINARY_DIR}/YapIOConfig.h) 

add_feature_info(ReadLine "" "READLINE ${READLINE_VERSION}  available at ${READLINE_LIBRARY}")

endif()

else()

  add_component (YAPReadline
    ${YAP_READLINE_SOURCES}
    )
endif()
