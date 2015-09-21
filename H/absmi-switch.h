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
 * Switch-based emulators consist of one or several switches that are at the 
 * core of an execution loop. They are simpler to implement, more compact,
 * and easier to debug.
 *
 *
 */

#ifndef ABSMI_THREADED_H

#define ABSMI_THREADED_H

#ifndef _NATIVE

#define ALWAYS_START_PREFETCH(TYPE) {

#define ALWAYS_START_PREFETCH_W(TYPE) {

#define ALWAYS_LOOKAHEAD(WHERE) {

#define START_PREFETCH(TYPE) {

#define START_PREFETCH_W(TYPE) {

#define INIT_PREFETCH() {

#define PREFETCH_OP(X)

#else

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_START_PREFETCH_W(TYPE)

#define ALWAYS_LOOKAHEAD(WHERE)

#define START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#endif /* _NATIVE */

  #ifndef _NATIVE

#define ALWAYS_END_PREFETCH() }

#define ALWAYS_END_PREFETCH_W() }

#define END_PREFETCH() }

#define END_PREFETCH_W() }

#else

#define ALWAYS_END_PREFETCH()

#define ALWAYS_END_PREFETCH_W()

#define END_PREFETCH()

#define END_PREFETCH_W()

#endif /* _NATIVE */

#define DO_PREFETCH(TYPE)

#define DO_PREFETCH_W(TYPE)

 #define JMPNext() goto nextop

 #define JMPNextW()  goto nextop_write

 #define baGONext()  JMPNext()

#define GONextW() JMPNextW()

#define ALWAYS_GONext() GONext()

#define ALWAYS_GONextW() GONextW()

#define Op(Label,Type)   case _##Label: { START_PREFETCH(Type)

#define OpW(Label,Type)  case  _##Label: { START_PREFETCH_W(Type)

#define BOp(Label,Type)  case _##Label: {

#define PBOp(Label,Type) case _##Label: { INIT_PREFETCH()

#define OpRW(Label,Type) case _##Label: {

#endif  // ABSMI_THREADED_H
