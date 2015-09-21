/*************************************************************************
 *                   *
 *   YAP Prolog                *
 *                   *
 *  Yap Prolog was developed at NCCUP - Universidade do Porto  *
 *                   *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997  *
 *                   *
 **************************************************************************
 *                   *
 * File:    absmi.c              *
 * Last rev:                 *
 * mods:                   *
 * comments:  Portable abstract machine interpreter that support threaded
 *      execution.           *
 *                   *
 *************************************************************************/

/**
 * @file   absmi-threaded.h
 * @author VITOR SANTOS COSTA <vsc@VITORs-MacBook-Pro.local>
 * @date   Sun Aug  2 23:46:47 2015
 * 
 * @brief  This file implements the support for switch-based execution of
 *         the YAP abstract machine emulator.
 * 
 % Switch-based emulators consist of one or several switches that are at the 
 
 %
 * YAP includes several operations to support threaded emulation:
 *
 *         - a header that defines the type of instruction;
 *
 *         - a transition instruction that moves to the next operation;
 *
 *         - get values well before they are needed; this is still
 * useful for the opcode of the next instruction.
 *         -
 *
ss */

#ifndef ABSMI_THREADED_H

#define ABSMI_THREADED_H

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_START_PREFETCH_W(TYPE)

#define ALWAYS_LOOKAHEAD(WHERE)

#define START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#define DO_PREFETCH(TYPE)

#define DO_PREFETCH_W(TYPE)

#define ALWAYS_END_PREFETCH()

#define ALWAYS_END_PREFETCH_W()

#define END_PREFETCH()

#define END_PREFETCH_W()

 #if YAP_DBG_PREDS


   #define Op(Label,Type)          \
  _##Label:{ (ExpEnv.config_struc.current_displacement) ?  \
  print_instruction(PREG, ON_PROFILED_INTERPRETER) :  \
  print_instruction(PREG, ON_INTERPRETER);    \
  START_PREFETCH(Type)

   #define OpW(Label,Type)         \
  Label:{ (ExpEnv.config_struc.current_displacement) ?  \
  print_instruction(PREG, ON_PROFILED_INTERPRETER) :  \
  print_instruction(PREG, ON_INTERPRETER);    \
  START_PREFETCH_W(Type)

   #define BOp(Label,Type)         \
  Label:{ (ExpEnv.config_struc.current_displacement) ?  \
  print_instruction(PREG, ON_PROFILED_INTERPRETER) :  \
  print_instruction(PREG, ON_INTERPRETER);

   #define PBOp(Label,Type)        \
  Label:{ (ExpEnv.config_struc.current_displacement) ?  \
  print_instruction(PREG, ON_PROFILED_INTERPRETER) :  \
  print_instruction(PREG, ON_INTERPRETER);    \
  INIT_PREFETCH()

   #define OpRW(Label,Type)        \
  Label:{ (ExpEnv.config_struc.current_displacement) ?  \
  print_instruction(PREG, ON_PROFILED_INTERPRETER) :  \
  print_instruction(PREG, ON_INTERPRETER);

 #else /* YAP_DBG_PREDS */


#define JMPNext() goto nextop

#define JMPNextW()  goto nextop_write

#define GONext()  JMPNext()

#define GONextW() JMPNextW()

#define ALWAYS_GONext() GONext()

#define ALWAYS_GONextW() GONextW()

  #define Op(Label,Type)        \
  _##Label:{ START_PREFETCH(Type)

  #define OpW(Label,Type)       \
  _##Label:{ START_PREFETCH_W(Type)

  #define BOp(Label,Type)       \
  _##Label:{

  #define PBOp(Label,Type)      \
  _##Label:{ INIT_PREFETCH()

  #define OpRW(Label,Type)			\
  _##Label:{

 
 #endif /* YAP_DBG_PREDS */



#endif  // ABSMI_THREADED_H
