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
 * @brief  This file implements the support for threaded execution of
 *         the YAP abstract machine emulator.
 * 
 % Threaded emulators keep the address of the instruction as the opcode
 % itself. This makes it faster to execute virtual instructions, but 
 % when grain-size is small it is easy to spend most of the instruction
 % waiting for data.
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
 */

#ifndef ABSMI_THREADED_H

#define ABSMI_THREADED_H

#if USE_THREADED_CODE

#ifndef _NATIVE

#if YAP_JIT

#define DO_PREFETCH(TYPE)           \
  if (ExpEnv.config_struc.current_displacement)       \
    to_go = (void *) OpAddress[Yap_op_from_opcode(NEXTOP(PREG,TYPE)->opc) + ExpEnv.config_struc.current_displacement]; \
  else                  \
    to_go = (void *)(NEXTOP(PREG,TYPE)->opc);

#define DO_PREFETCH_W(TYPE)           \
  if (ExpEnv.config_struc.current_displacement)       \
    to_go = (void *)OpAddress[Yap_op_from_opcode(NEXTOP(PREG,TYPE)->y_u.o.opcw) + ExpEnv.config_struc.current_displacement]; \
  else                  \
    to_go = (void *)(NEXTOP(PREG,TYPE)->y_u.o.opcw);

#else /* YAP_JIT */

#define DO_PREFETCH(TYPE) to_go = (void *)(NEXTOP(PREG,TYPE)->opc)

#define DO_PREFETCH_W(TYPE) to_go = (void *)(NEXTOP(PREG,TYPE)->y_u.o.opcw)

#endif /* YAP_JIT */

#else /* _NATIVE */

#define DO_PREFETCH(TYPE)

#define DO_PREFETCH_W(TYPE)

#endif /* _NATIVE */

#ifndef _NATIVE

#if LIMITED_PREFETCH||USE_PREFETCH

#define ALWAYS_START_PREFETCH(TYPE)   \
  { register void *to_go; DO_PREFETCH(TYPE)

#if YAP_JIT
#define ALWAYS_LOOKAHEAD(WHAT)            \
  {                 \
  register void *to_go;             \
  if (ExpEnv.config_struc.current_displacement)       \
    to_go = (void *) OpAddress[Yap_op_from_opcode(WHAT) + ExpEnv.config_struc.current_displacement]; \
  else                  \
    to_go = (void *)(WHAT);
#else /* YAP_JIT */
#define ALWAYS_LOOKAHEAD(WHAT)      \
  { register void *to_go = (void *)(WHAT);
#endif /* YAP_JIT */

#define ALWAYS_START_PREFETCH_W(TYPE)   \
  { register void *to_go; DO_PREFETCH_W(TYPE)

#else

#define ALWAYS_START_PREFETCH(TYPE) {

#define ALWAYS_START_PREFETCH_W(TYPE) {

#define ALWAYS_LOOKAHEAD(WHERE) {

#endif /* LIMITED_PREFETCH||USE_PREFETCH */

#else /* _NATIVE */

#if LIMITED_PREFETCH||USE_PREFETCH

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_LOOKAHEAD(WHAT)

#define ALWAYS_START_PREFETCH_W(TYPE)

#else

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_START_PREFETCH_W(TYPE)

#define ALWAYS_LOOKAHEAD(WHERE)

#endif /* LIMITED_PREFETCH||USE_PREFETCH */

#endif /* _NATIVE */

#ifndef _NATIVE

#ifdef USE_PREFETCH

#define START_PREFETCH(TYPE) ALWAYS_START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE) ALWAYS_START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()       \
  { register void *to_go;

#define PREFETCH_OP(X)              \
  if (ExpEnv.config_struc.current_displacement)       \
    to_go = (void *) OpAddress[Yap_op_from_opcode((X)->opc) + ExpEnv.config_struc.current_displacement]; \
  else                  \
    to_go = (void *)((X)->opc);

#else

#define START_PREFETCH(TYPE) {

#define START_PREFETCH_W(TYPE) {

#define INIT_PREFETCH() {

#define PREFETCH_OP(X)

#endif  /* USE_PREFETCH */

#else /* _NATIVE */

#ifdef USE_PREFETCH

#define START_PREFETCH(TYPE) ALWAYS_START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE) ALWAYS_START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#else

#define START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#endif  /* USE_PREFETCH */

#endif /* _NATIVE */

/*****************************************************************

  How to jump to the next abstract machine instruction

******************************************************************/


#endif // THREADED_CODE

#endif  // ABSMI_THREADED_H
