/*************************************************************************
*									 *
*	 Extension for YAP Prolog 					 *
*									 *
* Copyright G.S.Oliveira, A.F.Silva and Universidade Estadual de Maringá *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		jit_debugpreds.c					 *
* comments:	Debugging predicates					 *
*									 *
* Last rev:     2013-10-18                               		 *
*************************************************************************/

#define JIT_CODE 1

#include "jit_predicates.hpp"

#if YAP_DBG_PREDS

static Int  p_no_print_instruction( USES_REGS1 );
static Int  p_no_print_basic_instruction( USES_REGS1 );
static Int  p_no_print_std_instruction( USES_REGS1 );
static Int  p_no_print_standard_instruction( USES_REGS1 );
static Int  p_print_instruction( USES_REGS1 );
static Int  p_print_basic_instruction( USES_REGS1 );
static Int  p_print_std_instruction( USES_REGS1 );
static Int  p_print_standard_instruction( USES_REGS1 );
static Int  p_print_instruction_msg_before( USES_REGS1 );
static Int  p_print_basic_instruction_msg_before( USES_REGS1 );
static Int  p_print_std_instruction_msg_before( USES_REGS1 );
static Int  p_print_standard_instruction_msg_before( USES_REGS1 );
static Int  p_print_instruction_msg_after( USES_REGS1 );
static Int  p_print_basic_instruction_msg_after( USES_REGS1 );
static Int  p_print_std_instruction_msg_after( USES_REGS1 );
static Int  p_print_standard_instruction_msg_after( USES_REGS1 );
static Int  p_print_instruction3( USES_REGS1 );
static Int  p_print_basic_instruction3( USES_REGS1 );
static Int  p_print_std_instruction3( USES_REGS1 );
static Int  p_print_standard_instruction3( USES_REGS1 );
static Int  p_print_profiled_instruction( USES_REGS1 );
static Int  p_print_traced_instruction( USES_REGS1 );
static Int  p_print_pfd_instruction( USES_REGS1 );
static Int  p_print_profiled_instruction_msg_before( USES_REGS1 );
static Int  p_print_traced_instruction_msg_before( USES_REGS1 );
static Int  p_print_pfd_instruction_msg_before( USES_REGS1 );
static Int  p_print_profiled_instruction_msg_after( USES_REGS1 );
static Int  p_print_traced_instruction_msg_after( USES_REGS1 );
static Int  p_print_pfd_instruction_msg_after( USES_REGS1 );
static Int  p_print_profiled_instruction3( USES_REGS1 );
static Int  p_print_traced_instruction3( USES_REGS1 );
static Int  p_print_pfd_instruction3( USES_REGS1 );
static Int  p_print_native_instruction( USES_REGS1 );
static Int  p_print_ntv_instruction( USES_REGS1 );
static Int  p_print_native_instruction_msg_before( USES_REGS1 );
static Int  p_print_ntv_instruction_msg_before( USES_REGS1 );
static Int  p_print_native_instruction_msg_after( USES_REGS1 );
static Int  p_print_ntv_instruction_msg_after( USES_REGS1 );
static Int  p_print_native_instruction3( USES_REGS1 );
static Int  p_print_ntv_instruction3( USES_REGS1 );
static Int  p_no_print_basic_block( USES_REGS1 );
static Int  p_no_print_basicblock( USES_REGS1 );
static Int  p_no_print_bb( USES_REGS1 );
static Int  p_print_basic_block( USES_REGS1 );
static Int  p_print_basicblock( USES_REGS1 );
static Int  p_print_bb( USES_REGS1 );
static Int  p_print_basic_block_msg_before( USES_REGS1 );
static Int  p_print_basicblock_msg_before( USES_REGS1 );
static Int  p_print_bb_msg_before( USES_REGS1 );
static Int  p_print_basic_block_msg_after( USES_REGS1 );
static Int  p_print_basicblock_msg_after( USES_REGS1 );
static Int  p_print_bb_msg_after( USES_REGS1 );
static Int  p_print_basic_block3( USES_REGS1 );
static Int  p_print_basicblock3( USES_REGS1 );
static Int  p_print_bb3( USES_REGS1 );
static Int  p_print_native_basic_block( USES_REGS1 );
static Int  p_print_native_basicblock( USES_REGS1 );
static Int  p_print_native_bb( USES_REGS1 );
static Int  p_print_native_basic_block_msg_before( USES_REGS1 );
static Int  p_print_native_basicblock_msg_before( USES_REGS1 );
static Int  p_print_native_bb_msg_before( USES_REGS1 );
static Int  p_print_native_basic_block_msg_after( USES_REGS1 );
static Int  p_print_native_basicblock_msg_after( USES_REGS1 );
static Int  p_print_native_bb_msg_after( USES_REGS1 );
static Int  p_print_native_basic_block3( USES_REGS1 );
static Int  p_print_native_basicblock3( USES_REGS1 );
static Int  p_print_native_bb3( USES_REGS1 );
static Int  p_no_print_clause( USES_REGS1 );
static Int  p_print_clause( USES_REGS1 );
static Int  p_no_print_intermediate( USES_REGS1 );
static Int  p_print_intermediate( USES_REGS1 );
static Int  p_print_intermediate_to_std( USES_REGS1 );
static Int  p_print_intermediate_to_std1( USES_REGS1 );
static Int  p_print_intermediate_to_file( USES_REGS1 );
static Int  p_print_intermediate_to_file1( USES_REGS1 );
static Int  p_no_print_llva( USES_REGS1 );
static Int  p_print_llva_before( USES_REGS1 );
static Int  p_print_llva_after( USES_REGS1 );
static Int  p_no_print_me( USES_REGS1 );
static Int  p_print_me( USES_REGS1 );
static Int  p_default_debug( USES_REGS1 );
static Int  p_print_default_predicate_msgs( USES_REGS1 );
static Int  p_print_all_predicate_msgs( USES_REGS1 );
static Int  p_print_info_predicate_msgs( USES_REGS1 );
static Int  p_print_success_predicate_msgs( USES_REGS1 );
static Int  p_print_warning_predicate_msgs( USES_REGS1 );
static Int  p_print_error_predicate_msgs( USES_REGS1 );
static Int  p_no_print_all_predicate_msgs( USES_REGS1 );
static Int  p_no_print_info_predicate_msgs( USES_REGS1 );
static Int  p_no_print_success_predicate_msgs( USES_REGS1 );
static Int  p_no_print_warning_predicate_msgs( USES_REGS1 );
static Int  p_no_print_error_predicate_msgs( USES_REGS1 );
static Int  p_print_predicate_msgs( USES_REGS1 );
static Int  p_exit_on_warning( USES_REGS1 );
static Int  p_disable_on_warning( USES_REGS1 );
static Int  p_exit_on_error( USES_REGS1 );
static Int  p_no_exit_on_warning( USES_REGS1 );
static Int  p_enable_on_warning( USES_REGS1 );
static Int  p_no_exit_on_error( USES_REGS1 );


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

static Int
p_no_print_instruction( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
		char *str = (char*)malloc((YAP_AtomNameLength(AtomOfTerm(u))+1)*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          if (ExpEnv.debug_struc.pyaam_##OP.msg_before) free((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before); \
          if (ExpEnv.debug_struc.pyaam_##OP.msg_after) free((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after); \
          ExpEnv.debug_struc.pyaam_##OP.print = (Int)NO_PLACE; \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = 0; \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = 0; \
        }
        #include "YapOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"no_print_instruction / no_print_basic_instruction / no_print_std_instruction / no_print_standard_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"no_print_instruction / no_print_basic_instruction / no_print_std_instruction / no_print_standard_instruction");
    return FALSE;
  }
}

static Int
p_no_print_basic_instruction( USES_REGS1 )
{
  return p_no_print_instruction();
}

static Int
p_no_print_std_instruction( USES_REGS1 )
{
  return p_no_print_instruction();
}

static Int
p_no_print_standard_instruction( USES_REGS1 )
{
  return p_no_print_instruction();
}

static Int
p_print_instruction( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction / print_basic_instruction / print_std_instruction / print_standard_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction / print_basic_instruction / print_std_instruction / print_standard_instruction");
    return FALSE;
  }
}

static Int
p_print_basic_instruction( USES_REGS1 )
{
  return p_print_instruction();
}

static Int
p_print_std_instruction( USES_REGS1 )
{
  return p_print_instruction();
}

static Int
p_print_standard_instruction( USES_REGS1 )
{
  return p_print_instruction();
}

static Int
p_print_instruction_msg_before( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction_msg_before / print_basic_instruction_msg_before / print_std_instruction_msg_before / print_standard_instruction_msg_before");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc((strlen(msgb)+1)*sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), msgb); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction_msg_before / print_basic_instruction_msg_before / print_std_instruction_msg_before / print_standard_instruction_msg_before");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction_msg_before / print_basic_instruction_msg_before / print_std_instruction_msg_before / print_standard_instruction_msg_before");
    return FALSE;
  }
}

static Int
p_print_basic_instruction_msg_before( USES_REGS1 )
{
  return p_print_instruction_msg_before();
}

static Int
p_print_std_instruction_msg_before( USES_REGS1 )
{
  return p_print_instruction_msg_before();
}

static Int
p_print_standard_instruction_msg_before( USES_REGS1 )
{
  return p_print_instruction_msg_before();
}

static Int
p_print_instruction_msg_after( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction_msg_after / print_basic_instruction_msg_after / print_std_instruction_msg_after / print_standard_instruction_msg_after");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc((strlen(msga)+2)*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), msga); \
		  strcat(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction_msg_after / print_basic_instruction_msg_after / print_std_instruction_msg_after / print_standard_instruction_msg_after");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction_msg_after / print_basic_instruction_msg_after / print_std_instruction_msg_after / print_standard_instruction_msg_after");
    return FALSE;
  }
}

static Int
p_print_basic_instruction_msg_after( USES_REGS1 )
{
  return p_print_instruction_msg_after();
}

static Int
p_print_std_instruction_msg_after( USES_REGS1 )
{
  return p_print_instruction_msg_after();
}

static Int
p_print_standard_instruction_msg_after( USES_REGS1 )
{
  return p_print_instruction_msg_after();
}

static Int
p_print_instruction3( USES_REGS1 )
{
  Term t = Deref(ARG3);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction / print_basic_instruction / print_std_instruction / print_standard_instruction");
    return FALSE;
  }

  t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction / print_basic_instruction / print_std_instruction / print_standard_instruction");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc((strlen(msgb)+1)*sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc((strlen(msga)+2)*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), msgb); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), msga); \
		  strcat(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction / print_basic_instruction / print_std_instruction / print_standard_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_instruction / print_basic_instruction / print_std_instruction / print_standard_instruction");
    return FALSE;
  }
}

static Int
p_print_basic_instruction3( USES_REGS1 )
{
  return p_print_instruction3();
}

static Int
p_print_std_instruction3( USES_REGS1 )
{
  return p_print_instruction3();
}

static Int
p_print_standard_instruction3( USES_REGS1 )
{
  return p_print_instruction3();
}

static Int
p_print_profiled_instruction( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_PROFILED_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_PROFILED_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction / print_traced_instruction / print_pfd_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction / print_traced_instruction / print_pfd_instruction");
    return FALSE;
  }
}

static Int
p_print_traced_instruction( USES_REGS1 )
{
  return p_print_profiled_instruction();
}

static Int
p_print_pfd_instruction( USES_REGS1 )
{
  return p_print_profiled_instruction();
}

static Int
p_print_profiled_instruction_msg_before( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction_msg_before / print_traced_instruction_msg_before / print_pfd_instruction_msg_before");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc((strlen(msgb)+1)*sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_PROFILED_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_PROFILED_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), msgb); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction_msg_before / print_traced_instruction_msg_before / print_pfd_instruction_msg_before");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction_msg_before / print_traced_instruction_msg_before / print_pfd_instruction_msg_before");
    return FALSE;
  }
}

static Int
p_print_traced_instruction_msg_before( USES_REGS1 )
{
  return p_print_profiled_instruction_msg_before();
}

static Int
p_print_pfd_instruction_msg_before( USES_REGS1 )
{
  return p_print_profiled_instruction_msg_before();
}

static Int
p_print_profiled_instruction_msg_after( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction_msg_after / print_traced_instruction_msg_after / print_pfd_instruction_msg_after");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc((strlen(msga)+2)*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_PROFILED_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_PROFILED_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), msga); \
		  strcat(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction_msg_after / print_traced_instruction_msg_after / print_pfd_instruction_msg_after");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction_msg_after / print_traced_instruction_msg_after / print_pfd_instruction_msg_after");
    return FALSE;
  }
}

static Int
p_print_traced_instruction_msg_after( USES_REGS1 )
{
  return p_print_profiled_instruction_msg_after();
}

static Int
p_print_pfd_instruction_msg_after( USES_REGS1 )
{
  return p_print_profiled_instruction_msg_after();
}

static Int
p_print_profiled_instruction3( USES_REGS1 )
{
  Term t = Deref(ARG3);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction/ print_traced_instruction / print_pfd_instruction");
    return FALSE;
  }

  t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction/ print_traced_instruction / print_pfd_instruction");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc((strlen(msgb)+1)*sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc((strlen(msga)+2)*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_PROFILED_INTERPRETER; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_PROFILED_INTERPRETER; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), msgb); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), msga); \
		  strcat(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction/ print_traced_instruction / print_pfd_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_profiled_instruction/ print_traced_instruction / print_pfd_instruction");
    return FALSE;
  }
}

static Int
p_print_traced_instruction3( USES_REGS1 )
{
  return p_print_profiled_instruction3();
}

static Int
p_print_pfd_instruction3( USES_REGS1 )
{
  return p_print_profiled_instruction3();
}

static Int
p_print_native_instruction( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction / print_ntv_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction / print_ntv_instruction");
    return FALSE;
  }
}

static Int
p_print_ntv_instruction( USES_REGS1 )
{
  return p_print_native_instruction();
}

static Int
p_print_native_instruction_msg_before( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction_msg_before / print_ntv_instruction_msg_before");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc((strlen(msgb)+1)*sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), msgb); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction_msg_before / print_ntv_instruction_msg_before");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction_msg_before / print_ntv_instruction_msg_before");
    return FALSE;
  }
}

static Int
p_print_ntv_instruction_msg_before( USES_REGS1 )
{
  return p_print_native_instruction_msg_before();
}

static Int
p_print_native_instruction_msg_after( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction_msg_after / print_ntv_instruction_msg_after");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc((strlen(msga)+2)*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), msga); \
		  strcat(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction_msg_after / print_ntv_instruction_msg_after");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction_msg_after / print_ntv_instruction_msg_after");
    return FALSE;
  }
}

static Int
p_print_ntv_instruction_msg_after( USES_REGS1 )
{
  return p_print_native_instruction_msg_after();
}

static Int
p_print_native_instruction3( USES_REGS1 )
{
  Term t = Deref(ARG3);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction / print_ntv_instruction");
    return FALSE;
  }

  t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction / print_ntv_instruction");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strop = (char*)malloc(100*sizeof(char));
	    #define OPCODE(OP,TYPE) \
        strcpy(strop, #OP); \
        UPPER_ENTRY(strop); \
        if (strcmp(str, strop) == 0) { \
          ExpEnv.debug_struc.pyaam_##OP.msg_before = (CELL)malloc((strlen(msgb)+1)*sizeof(char)); \
          ExpEnv.debug_struc.pyaam_##OP.msg_after = (CELL)malloc((strlen(msga)+2)*sizeof(char)); \
          if (!ExpEnv.debug_struc.pyaam_##OP.print) \
            ExpEnv.debug_struc.pyaam_##OP.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pyaam_##OP.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before), msgb); \
          strcpy(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), msga); \
		  strcat(((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after), "\n"); \
        }
        #include "YapAppliedOpcodes.h"
        #undef OPCODE
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction / print_ntv_instruction");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_instruction / print_ntv_instruction");
    return FALSE;
  }
}

static Int
p_print_ntv_instruction3( USES_REGS1 )
{
  return p_print_native_instruction3();
}

static Int
p_no_print_basic_block( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          if (ExpEnv.debug_struc.pbbs_##BB.msg_before) free((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before); \
          if (ExpEnv.debug_struc.pbbs_##BB.msg_after) free((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after);\
          ExpEnv.debug_struc.pbbs_##BB.print = (Int)NO_PLACE; \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = 0; \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = 0; \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"no_print_basic_block / no_print_basicblock / no_print_bb");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"no_print_basic_block / no_print_basicblock / no_print_bb");
    return FALSE;
  }
}

static Int
p_no_print_basicblock( USES_REGS1 )
{
  return p_no_print_basic_block();
}

static Int
p_no_print_bb( USES_REGS1 )
{
  return p_no_print_basic_block();
}

static Int
p_print_basic_block( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block / print_basicblock / print_bb");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block / print_basicblock / print_bb");
    return FALSE;
  }
}

static Int
p_print_basicblock( USES_REGS1 )
{
  return p_print_basic_block();
}

static Int
p_print_bb( USES_REGS1 )
{
  return p_print_basic_block();
}

static Int
p_print_basic_block_msg_before( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block_msg_before / print_basicblock_msg_before / print_bb_msg_before");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block_msg_before / print_basicblock_msg_before / print_bb_msg_before");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block_msg_before / print_basicblock_msg_before / print_bb_msg_before");
    return FALSE;
  }
}

static Int
p_print_basicblock_msg_before( USES_REGS1 )
{
  return p_print_basic_block_msg_before();
}

static Int
p_print_bb_msg_before( USES_REGS1 )
{
  return p_print_basic_block_msg_before();
}

static Int
p_print_basic_block_msg_after( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block_msg_after / print_basicblock_msg_after / print_bb_msg_after");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block_msg_after / print_basicblock_msg_after / print_bb_msg_after");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block_msg_after / print_basicblock_msg_after / print_bb_msg_after");
    return FALSE;
  }
}

static Int
p_print_basicblock_msg_after( USES_REGS1 )
{
  return p_print_basic_block_msg_after();
}

static Int
p_print_bb_msg_after( USES_REGS1 )
{
  return p_print_basic_block_msg_after();
}

static Int
p_print_basic_block3( USES_REGS1 )
{
  Term t = Deref(ARG3);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block / print_basicblock / print_bb");
    return FALSE;
  }

  t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block / print_basicblock / print_bb");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= ((Int)ON_INTERPRETER | (Int)ON_PROFILED_INTERPRETER); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block / print_basicblock / print_bb");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_basic_block / print_basicblock / print_bb");
    return FALSE;
  }
}

static Int
p_print_basicblock3( USES_REGS1 )
{
  return p_print_basic_block3();
}

static Int
p_print_bb3( USES_REGS1 )
{
  return p_print_basic_block3();
}

static Int
p_print_native_basic_block( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    int i = 0, j = 0;
	char *tmp;
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    UPPER_ENTRY(str);
    if (strcmp(str, "ALL") == 0) {
	  #define BBLOCK(BB) \
      ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
      ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
      if (!ExpEnv.debug_struc.pbbs_##BB.print) \
        ExpEnv.debug_struc.pbbs_##BB.print = (Int)ON_NATIVE; \
      else \
        ExpEnv.debug_struc.pbbs_##BB.print |= (Int)ON_NATIVE; \
      strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
      strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n");
      #include "Yap_AppliedBasicBlocks.h"
      #undef BBLOCK
      return TRUE;
    }
    Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
    return FALSE;
  }
  else if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
    return FALSE;
  }
}

static Int
p_print_native_basicblock( USES_REGS1 )
{
  return p_print_native_basic_block();
}

static Int
p_print_native_bb( USES_REGS1 )
{
  return p_print_native_basic_block();
}

static Int
p_print_native_basic_block_msg_before( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block_msg_before / print_native_basicblock_msg_before / print_native_bb_msg_before");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block_msg_before / print_native_basicblock_msg_before / print_native_bb_msg_before");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block_msg_before / print_native_basicblock_msg_before / print_native_bb_msg_before");
    return FALSE;
  }
}

static Int
p_print_native_basicblock_msg_before( USES_REGS1 )
{
  return p_print_native_basic_block_msg_before();
}

static Int
p_print_native_bb_msg_before( USES_REGS1 )
{
  return p_print_native_basic_block_msg_before();
}

static Int
p_print_native_basic_block_msg_after( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block_msg_after / print_native_basicblock_msg_after / print_native_bb_msg_after");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block_msg_after / print_native_basicblock_msg_after / print_native_bb_msg_after");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block_msg_after / print_native_basicblock_msg_after / print_native_bb_msg_after");
    return FALSE;
  }
}

static Int
p_print_native_basicblock_msg_after( USES_REGS1 )
{
  return p_print_native_basic_block_msg_after();
}

static Int
p_print_native_bb_msg_after( USES_REGS1 )
{
  return p_print_native_basic_block_msg_after();
}

static Int
p_print_native_basic_block3( USES_REGS1 )
{
  Term t = Deref(ARG3);
  char *msga;
  if (IsAtomTerm(t)) {
    msga = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msga, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
    return FALSE;
  }

  t = Deref(ARG2);
  char *msgb;
  if (IsAtomTerm(t)) {
    msgb = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msgb, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
    return FALSE;
  }

  t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    char *strbb = (char*)malloc(100*sizeof(char));
	    #define BBLOCK(BB) \
        strcpy(strbb, #BB); \
        UPPER_ENTRY(strbb); \
        if (strcmp(str, strbb) == 0) { \
          ExpEnv.debug_struc.pbbs_##BB.msg_before = (CELL)malloc(sizeof(char)); \
          ExpEnv.debug_struc.pbbs_##BB.msg_after = (CELL)malloc(2*sizeof(char)); \
          if (!ExpEnv.debug_struc.pbbs_##BB.print) \
            ExpEnv.debug_struc.pbbs_##BB.print = (Int)ON_NATIVE; \
          else \
            ExpEnv.debug_struc.pbbs_##BB.print |= (Int)ON_NATIVE; \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_before), ""); \
          strcpy(((char*)ExpEnv.debug_struc.pbbs_##BB.msg_after), "\n"); \
        }
        #include "Yap_AppliedBasicBlocks.h"
        #undef BBLOCK
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
    return FALSE;
  }
}

static Int
p_print_native_basicblock3( USES_REGS1 )
{
  return p_print_native_basic_block3();
}

static Int
p_print_native_bb3( USES_REGS1 )
{
  return p_print_native_basic_block3();
}

static Int
p_no_print_clause( USES_REGS1 )
{
  if (ExpEnv.debug_struc.pmainclause_on_head.msg_before) free((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_before);
  if (ExpEnv.debug_struc.pmainclause_on_head.msg_after) free((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_after);
  ExpEnv.debug_struc.pmainclause_on_head.print = (Int)NO_PLACE;
  ExpEnv.debug_struc.pmainclause_on_head.msg_before = 0;
  ExpEnv.debug_struc.pmainclause_on_head.msg_after = 0;
  return TRUE;
}

static Int
p_print_clause( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
	while (1) {
	  if (IsAtomTerm(u)) {
        int i = 0, j = 0;
		char *tmp;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
	    if (strcmp(str, "STANDARD") == 0 || strcmp(str, "STD") == 0) {
          ExpEnv.debug_struc.pmainclause_on_head.msg_before = (CELL)malloc(sizeof(char));
          ExpEnv.debug_struc.pmainclause_on_head.msg_after = (CELL)malloc(2*sizeof(char));
          if (!ExpEnv.debug_struc.pmainclause_on_head.print)
            ExpEnv.debug_struc.pmainclause_on_head.print = (Int)ON_INTERPRETER;
          else
            ExpEnv.debug_struc.pmainclause_on_head.print |= (Int)ON_INTERPRETER;
          strcpy(((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_before), "");
          strcpy(((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_after), "\n");
        }
        else if (strcmp(str, "PROFILED") == 0 || strcmp(str, "PFD") == 0) {
          ExpEnv.debug_struc.pmainclause_on_head.msg_before = (CELL)malloc(sizeof(char));
          ExpEnv.debug_struc.pmainclause_on_head.msg_after = (CELL)malloc(2*sizeof(char));
          if (!ExpEnv.debug_struc.pmainclause_on_head.print)
           ExpEnv.debug_struc.pmainclause_on_head.print = (Int)ON_PROFILED_INTERPRETER;
          else
            ExpEnv.debug_struc.pmainclause_on_head.print |= (Int)ON_PROFILED_INTERPRETER;
          strcpy(((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_before), "");
          strcpy(((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_after), "\n");
        }
        else if (strcmp(str, "NATIVE") == 0 || strcmp(str, "NTV") == 0) {
          ExpEnv.debug_struc.pmainclause_on_head.msg_before = (CELL)malloc(sizeof(char));
          ExpEnv.debug_struc.pmainclause_on_head.msg_after = (CELL)malloc(2*sizeof(char));
          if (!ExpEnv.debug_struc.pmainclause_on_head.print)
            ExpEnv.debug_struc.pmainclause_on_head.print = (Int)ON_NATIVE;
          else
            ExpEnv.debug_struc.pmainclause_on_head.print |= (Int)ON_NATIVE;
          strcpy(((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_before), "");
          strcpy(((char*)ExpEnv.debug_struc.pmainclause_on_head.msg_after), "\n");
        }
		else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
		}
	  }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
        return FALSE;
      }
	  t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_native_basic_block / print_native_basicblock / print_native_bb");
    return FALSE;
  }
}

static Int
p_no_print_intermediate( USES_REGS1 )
{
  if (ExpEnv.debug_struc.pprint_intermediate.std_name) free((char*)ExpEnv.debug_struc.pprint_intermediate.std_name);
  if (ExpEnv.debug_struc.pprint_intermediate.file_name) free((char*)ExpEnv.debug_struc.pprint_intermediate.file_name);
  ExpEnv.debug_struc.pprint_intermediate.print_to_std = 0;
  ExpEnv.debug_struc.pprint_intermediate.print_to_file = 0;
  ExpEnv.debug_struc.pprint_intermediate.std_name = 0;
  ExpEnv.debug_struc.pprint_intermediate.file_name = 0;
  return TRUE;
}

static Int
p_print_intermediate( USES_REGS1 )
{
  ExpEnv.debug_struc.pprint_intermediate.print_to_std = 1;
  ExpEnv.debug_struc.pprint_intermediate.std_name = (CELL)malloc(7*sizeof(char));
  strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.std_name), "STDOUT");
  return TRUE;
}

static Int
p_print_intermediate_to_std( USES_REGS1 )
{
  ExpEnv.debug_struc.pprint_intermediate.print_to_std = 1;
  ExpEnv.debug_struc.pprint_intermediate.std_name = (CELL)malloc(7*sizeof(char));
  strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.std_name), "STDOUT");
  return TRUE;
}

static Int
p_print_intermediate_to_std1( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    int i = 0, j = 0;
	char *tmp;
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
	UPPER_ENTRY(str);
	if (strcmp(str, "STDOUT") == 0 || strcmp(str, "STDERR") == 0) {
	  ExpEnv.debug_struc.pprint_intermediate.print_to_std = 1;
	  ExpEnv.debug_struc.pprint_intermediate.std_name = (CELL)malloc(strlen(str)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.std_name), str);
	  return TRUE;
    }
    Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
    return FALSE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_intermediate_to_std");
    return FALSE;
  }
}

static Int
p_print_intermediate_to_file( USES_REGS1 )
{
  ExpEnv.debug_struc.pprint_intermediate.print_to_file = 1;
  ExpEnv.debug_struc.pprint_intermediate.file_name = (CELL)malloc(7*sizeof(char));
  if (Yap_ExecutionMode == MIXED_MODE)
    strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.file_name), "trace");
  else
    strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.file_name), "clause");
  return TRUE;
}

static Int
p_print_intermediate_to_file1( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
	ExpEnv.debug_struc.pprint_intermediate.print_to_file = 1;
	ExpEnv.debug_struc.pprint_intermediate.file_name = (CELL)malloc(strlen(str)*sizeof(char));
	strcpy(((char*)ExpEnv.debug_struc.pprint_intermediate.file_name), str);
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_intermediate_to_file");
    return FALSE;
  }
}

static Int
p_no_print_llva( USES_REGS1 )
{
  ExpEnv.debug_struc.pprint_llva.print_llva_before = 0;
  ExpEnv.debug_struc.pprint_llva.print_llva_after = 0;
  return TRUE;
}

static Int
p_print_llva_before( USES_REGS1 )
{
  ExpEnv.debug_struc.pprint_llva.print_llva_before = 1;
  return TRUE;
}

static Int
p_print_llva_after( USES_REGS1 )
{
  ExpEnv.debug_struc.pprint_llva.print_llva_after = 1;
  return TRUE;
}

static Int
p_no_print_me( USES_REGS1 )
{
  Term t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    int i = 0, j = 0;
	char *tmp;
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
	UPPER_ENTRY(str);
	if (strcmp(str, "INTERPRETEDBACKTRACK") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.interpreted_backtrack);
	if (strcmp(str, "PROFILEDINTERPRETEDBACKTRACK") == 0 || strcmp(str, "TRACEDINTERPRETEDBACKTRACK") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack);
	else if (strcmp(str, "NATIVEBACKTRACK") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.native_backtrack);
	else if (strcmp(str, "TREATIHEAP") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap);
	else if (strcmp(str, "TREATNHEAP") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap);
	else if (strcmp(str, "TREATITRAIL") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.interpreted_treat_trail);
	else if (strcmp(str, "TREATNTRAIL") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.native_treat_trail);
	else if (strcmp(str, "CRITICALS") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.criticals);
	else if (strcmp(str, "ATCOMPILATION") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.at_compilation);
	else if (strcmp(str, "ATRECOMPILATION") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.at_recompilation);
	else if (strcmp(str, "NATIVERUNINIT") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.nativerun_init);
	else if (strcmp(str, "NATIVERUNEXITBYSUCCESS") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success);
	else if (strcmp(str, "NATIVERUNEXITBYFAIL") == 0)
	  free((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail);
	else if (strcmp(str, "NATIVERUNEXIT") == 0) {
	  free((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success);
	  free((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail);
	}
    else {
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"no_print_me");
    return FALSE;
  }
}

static Int
p_print_me( USES_REGS1 )
{
  Term t = Deref(ARG2);
  char *msg;
  if (IsAtomTerm(t)) {
    msg = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(msg, AtomName(AtomOfTerm(t)));
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_me");
    return FALSE;
  }
  t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    int i = 0, j = 0;
	char *tmp;
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
	UPPER_ENTRY(str);
	if (strcmp(str, "INTERPRETEDBACKTRACK") == 0) {
	  ExpEnv.debug_struc.pprint_me.interpreted_backtrack = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.interpreted_backtrack), msg);
	}
	else if (strcmp(str, "PROFILEDINTERPRETEDBACKTRACK") == 0 || strcmp(str, "TRACEDINTERPRETEDBACKTRACK") == 0) {
	  ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack), msg);
	}
	else if (strcmp(str, "NATIVEBACKTRACK") == 0) {
	  ExpEnv.debug_struc.pprint_me.native_backtrack = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.native_backtrack), msg);
	}
	else if (strcmp(str, "TREATIHEAP") == 0) {
	  ExpEnv.debug_struc.pprint_me.interpreted_treat_heap = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap), msg);
	}
	else if (strcmp(str, "TREATNHEAP") == 0) {
	  ExpEnv.debug_struc.pprint_me.native_treat_heap = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.native_treat_heap), msg);
	}
	else if (strcmp(str, "TREATITRAIL") == 0) {
	  ExpEnv.debug_struc.pprint_me.interpreted_treat_trail = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.interpreted_treat_trail), msg);
	}
	else if (strcmp(str, "TREATNTRAIL") == 0) {
	  ExpEnv.debug_struc.pprint_me.native_treat_trail = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.native_treat_trail), msg);
	}
	else if (strcmp(str, "CRITICALS") == 0) {
	  ExpEnv.debug_struc.pprint_me.criticals = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.criticals), msg);
	}
	else if (strcmp(str, "ATCOMPILATION") == 0) {
	  ExpEnv.debug_struc.pprint_me.at_compilation = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.at_compilation), msg);
	}
	else if (strcmp(str, "ATRECOMPILATION") == 0) {
	  ExpEnv.debug_struc.pprint_me.at_recompilation = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.at_recompilation), msg);
	}
	else if (strcmp(str, "NATIVERUNINIT") == 0) {
	  ExpEnv.debug_struc.pprint_me.nativerun_init = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.nativerun_init), msg);
	}
	else if (strcmp(str, "NATIVERUNEXITBYSUCCESS") == 0) {
	  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success), msg);
	}
	else if (strcmp(str, "NATIVERUNEXITBYFAIL") == 0) {
	  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail), msg);
	}
	else if (strcmp(str, "NATIVERUNEXIT") == 0) {
	  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success), msg);
	  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail = (CELL)malloc(strlen(msg)*sizeof(char));
	  strcpy(((char*)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail), msg);
	}
    else {
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
	}
	return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_me");
    return FALSE;
  }
}

static Int
p_default_debug( USES_REGS1 )
{
  #define OPCODE(OP,TYPE) \
  ExpEnv.debug_struc.pyaam_##OP.print = (Int)NO_PLACE; \
  ExpEnv.debug_struc.pyaam_##OP.msg_before = 0; \
  ExpEnv.debug_struc.pyaam_##OP.msg_after = 0;
  #include "YapAppliedOpcodes.h"
  #undef OPCODE

  #define BBLOCK(BB) \
  ExpEnv.debug_struc.pbbs_##BB.print = (Int)NO_PLACE; \
  ExpEnv.debug_struc.pbbs_##BB.msg_before = 0; \
  ExpEnv.debug_struc.pbbs_##BB.msg_after = 0;
  #include "Yap_AppliedBasicBlocks.h"
  #undef BBLOCK

  ExpEnv.debug_struc.pmainclause_on_head.print = (Int)NO_PLACE;
  ExpEnv.debug_struc.pmainclause_on_head.msg_before = 0;
  ExpEnv.debug_struc.pmainclause_on_head.msg_after = 0;

  ExpEnv.debug_struc.pprint_intermediate.print_to_std = 0;
  ExpEnv.debug_struc.pprint_intermediate.print_to_file = 0;
  ExpEnv.debug_struc.pprint_intermediate.std_name = 0;
  ExpEnv.debug_struc.pprint_intermediate.file_name = 0;

  ExpEnv.debug_struc.pprint_llva.print_llva_before = 0;
  ExpEnv.debug_struc.pprint_llva.print_llva_after = 0;

  ExpEnv.debug_struc.pprint_me.interpreted_backtrack = 0;
  ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack = 0;
  ExpEnv.debug_struc.pprint_me.native_backtrack = 0;
  ExpEnv.debug_struc.pprint_me.interpreted_treat_heap = 0;
  ExpEnv.debug_struc.pprint_me.native_treat_heap = 0;
  ExpEnv.debug_struc.pprint_me.interpreted_treat_trail = 0;
  ExpEnv.debug_struc.pprint_me.native_treat_trail = 0;
  ExpEnv.debug_struc.pprint_me.criticals = 0;
  ExpEnv.debug_struc.pprint_me.at_compilation = 0;
  ExpEnv.debug_struc.pprint_me.at_recompilation = 0;
  ExpEnv.debug_struc.pprint_me.nativerun_init = 0;
  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success = 0;
  ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail = 0;

  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;

  ExpEnv.debug_struc.act_predicate_actions.exit_on_warning = 0;
  ExpEnv.debug_struc.act_predicate_actions.disable_on_warning = 1;
  ExpEnv.debug_struc.act_predicate_actions.exit_on_error = 1;

  return TRUE;
}

static Int
p_print_default_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;
  return TRUE;
}

static Int
p_print_all_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 1;
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 1;
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;
  return TRUE;
}

static Int
p_print_info_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 1;
  return TRUE;
}

static Int
p_print_success_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 1;
  return TRUE;
}

static Int
p_print_warning_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
  return TRUE;
}

static Int
p_print_error_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;
  return TRUE;
}

static Int
p_no_print_all_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 0;
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 0;
  return TRUE;
}

static Int
p_no_print_info_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 0;
  return TRUE;
}

static Int
p_no_print_success_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 0;
  return TRUE;
}

static Int
p_no_print_warning_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 0;
  return TRUE;
}

static Int
p_no_print_error_predicate_msgs( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 0;
  return TRUE;
}

static Int
p_print_predicate_msgs( USES_REGS1 )
{
  int i = 0, j = 0;
  char *tmp;
  Term t = Deref(ARG1);
  if (IsAtomTerm(t)) {
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    UPPER_ENTRY(str);
    if (strcmp(str, "ALL") == 0) {
      ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 1;
      ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 1;
      ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
      ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;
      return TRUE;
    }
    Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
    return FALSE;
  }
  else if (IsPairTerm(t)) {
    Term u = HeadOfTermCell(t);
    u = Deref(u);
    while (1) {
      if (IsAtomTerm(u)) {
        int i = 0, j = 0;
        char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(u))*sizeof(char));
        strcpy(str, AtomName(AtomOfTerm(u)));
	    UPPER_ENTRY(str);
        if (strcmp(str, "INFO") == 0 || strcmp(str, "INFORMATION") == 0)
		  ExpEnv.debug_struc.act_predicate_msgs.info_msgs = 1;
        else if (strcmp(str, "SUCCESS") == 0)
		  ExpEnv.debug_struc.act_predicate_msgs.success_msgs = 1;
        else if (strcmp(str, "WARNING") == 0)
		  ExpEnv.debug_struc.act_predicate_msgs.warning_msgs = 1;
		else if (strcmp(str, "ERROR") == 0)
		  ExpEnv.debug_struc.act_predicate_msgs.error_msgs = 1;
        else {
          Yap_Error(OUT_OF_KNOWNRANGE_ERROR,u,"");
          return FALSE;
        }
      }
      else {
        Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_predicate_msgs");
	    return FALSE;
      }
      t = TailOfTermCell(t);
      t = Deref(t);
      if (IsAtomTerm(t)) break;
      u = HeadOfTermCell(t);
      u = Deref(u);
    }
    return TRUE;
  }
  else {
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR,"print_predicate_msgs");
    return FALSE;
  }
}

static Int
p_exit_on_warning( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_actions.exit_on_warning = 1;
  return TRUE;
}

static Int
p_disable_on_warning( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_actions.disable_on_warning = 1;
  return TRUE;
}

static Int
p_exit_on_error( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_actions.exit_on_error = 1;
  return TRUE;
}

static Int
p_no_exit_on_warning( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_actions.exit_on_warning = 0;
  return TRUE;
}

static Int
p_enable_on_warning( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_actions.disable_on_warning = 0;
  return TRUE;
}

static Int
p_no_exit_on_error( USES_REGS1 )
{
  ExpEnv.debug_struc.act_predicate_actions.exit_on_error = 0;
  return TRUE;
}

#pragma GCC diagnostic pop

#endif

void
Yap_InitJitDebugPreds( void )
{
#if YAP_DBG_PREDS
  Yap_InitCPred("no_print_instruction", 1, p_no_print_instruction, SafePredFlag);
  Yap_InitCPred("no_print_basic_instruction", 1, p_no_print_basic_instruction, SafePredFlag);
  Yap_InitCPred("no_print_std_instruction", 1, p_no_print_std_instruction, SafePredFlag);
  Yap_InitCPred("no_print_standard_instruction", 1, p_no_print_standard_instruction, SafePredFlag);
  Yap_InitCPred("print_instruction", 1, p_print_instruction, SafePredFlag);
  Yap_InitCPred("print_basic_instruction", 1, p_print_basic_instruction, SafePredFlag);
  Yap_InitCPred("print_std_instruction", 1, p_print_std_instruction, SafePredFlag);
  Yap_InitCPred("print_standard_instruction", 1, p_print_standard_instruction, SafePredFlag);
  Yap_InitCPred("print_instruction_msg_before", 2, p_print_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_basic_instruction_msg_before", 2, p_print_basic_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_std_instruction_msg_before", 2, p_print_std_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_standard_instruction_msg_before", 2, p_print_standard_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_instruction_msg_after", 2, p_print_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_basic_instruction_msg_after", 2, p_print_basic_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_std_instruction_msg_after", 2, p_print_std_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_standard_instruction_msg_after", 2, p_print_standard_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_instruction", 3, p_print_instruction3, SafePredFlag);
  Yap_InitCPred("print_basic_instruction", 3, p_print_basic_instruction3, SafePredFlag);
  Yap_InitCPred("print_std_instruction", 3, p_print_std_instruction3, SafePredFlag);
  Yap_InitCPred("print_standard_instruction", 3, p_print_standard_instruction3, SafePredFlag);
  Yap_InitCPred("print_profiled_instruction", 1, p_print_profiled_instruction, SafePredFlag);
  Yap_InitCPred("print_traced_instruction", 1, p_print_traced_instruction, SafePredFlag);
  Yap_InitCPred("print_pfd_instruction", 1, p_print_pfd_instruction, SafePredFlag);
  Yap_InitCPred("print_profiled_instruction_msg_before", 2, p_print_profiled_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_traced_instruction_msg_before", 2, p_print_traced_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_pfd_instruction_msg_before", 2, p_print_pfd_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_profiled_instruction_msg_after", 2, p_print_profiled_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_traced_instruction_msg_after", 2, p_print_traced_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_pfd_instruction_msg_after", 2, p_print_pfd_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_profiled_instruction", 3, p_print_profiled_instruction3, SafePredFlag);
  Yap_InitCPred("print_traced_instruction", 3, p_print_traced_instruction3, SafePredFlag);
  Yap_InitCPred("print_pfd_instruction", 3, p_print_pfd_instruction3, SafePredFlag);
  Yap_InitCPred("print_native_instruction", 1, p_print_native_instruction, SafePredFlag);
  Yap_InitCPred("print_ntv_instruction", 1, p_print_ntv_instruction, SafePredFlag);
  Yap_InitCPred("print_native_instruction_msg_before", 2, p_print_native_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_ntv_instruction_msg_before", 2, p_print_ntv_instruction_msg_before, SafePredFlag);
  Yap_InitCPred("print_native_instruction_msg_after", 2, p_print_native_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_ntv_instruction_msg_after", 2, p_print_ntv_instruction_msg_after, SafePredFlag);
  Yap_InitCPred("print_native_instruction", 3, p_print_native_instruction3, SafePredFlag);
  Yap_InitCPred("print_ntv_instruction", 3, p_print_ntv_instruction3, SafePredFlag);
  Yap_InitCPred("no_print_basic_block", 1, p_no_print_basic_block, SafePredFlag);
  Yap_InitCPred("no_print_basicblock", 1, p_no_print_basicblock, SafePredFlag);
  Yap_InitCPred("no_print_bb", 1, p_no_print_bb, SafePredFlag);
  Yap_InitCPred("print_basic_block", 1, p_print_basic_block, SafePredFlag);
  Yap_InitCPred("print_basicblock", 1, p_print_basicblock, SafePredFlag);
  Yap_InitCPred("print_bb", 1, p_print_bb, SafePredFlag);
  Yap_InitCPred("print_basic_block_msg_before", 2, p_print_basic_block_msg_before, SafePredFlag);
  Yap_InitCPred("print_basicblock_msg_before", 2, p_print_basicblock_msg_before, SafePredFlag);
  Yap_InitCPred("print_bb_msg_before", 2, p_print_bb_msg_before, SafePredFlag);
  Yap_InitCPred("print_basic_block_msg_after", 2, p_print_basic_block_msg_after, SafePredFlag);
  Yap_InitCPred("print_basicblock_msg_after", 2, p_print_basicblock_msg_after, SafePredFlag);
  Yap_InitCPred("print_bb_msg_after", 2, p_print_bb_msg_after, SafePredFlag);
  Yap_InitCPred("print_basic_block", 3, p_print_basic_block3, SafePredFlag);
  Yap_InitCPred("print_basicblock", 3, p_print_basicblock3, SafePredFlag);
  Yap_InitCPred("print_bb", 3, p_print_bb3, SafePredFlag);
  Yap_InitCPred("print_native_basic_block", 1, p_print_native_basic_block, SafePredFlag);
  Yap_InitCPred("print_native_basicblock", 1, p_print_native_basicblock, SafePredFlag);
  Yap_InitCPred("print_native_bb", 1, p_print_native_bb, SafePredFlag);
  Yap_InitCPred("print_native_basic_block_msg_before", 2, p_print_native_basic_block_msg_before, SafePredFlag);
  Yap_InitCPred("print_native_basicblock_msg_before", 2, p_print_native_basicblock_msg_before, SafePredFlag);
  Yap_InitCPred("print_native_bb_msg_before", 2, p_print_native_bb_msg_before, SafePredFlag);
  Yap_InitCPred("print_native_basic_block_msg_after", 2, p_print_native_basic_block_msg_after, SafePredFlag);
  Yap_InitCPred("print_native_basicblock_msg_after", 2, p_print_native_basicblock_msg_after, SafePredFlag);
  Yap_InitCPred("print_native_bb_msg_after", 2, p_print_native_bb_msg_after, SafePredFlag);
  Yap_InitCPred("print_native_basic_block", 3, p_print_native_basic_block3, SafePredFlag);
  Yap_InitCPred("print_native_basicblock", 3, p_print_native_basicblock3, SafePredFlag);
  Yap_InitCPred("print_native_bb", 3, p_print_native_bb3, SafePredFlag);
  Yap_InitCPred("no_print_clause", 0, p_no_print_clause, SafePredFlag);
  Yap_InitCPred("print_clause", 1, p_print_clause, SafePredFlag);
  Yap_InitCPred("no_print_intermediate", 0, p_no_print_intermediate, SafePredFlag);
  Yap_InitCPred("print_intermediate", 0, p_print_intermediate, SafePredFlag);
  Yap_InitCPred("print_intermediate_to_std", 0, p_print_intermediate_to_std, SafePredFlag);
  Yap_InitCPred("print_intermediate_to_std", 1, p_print_intermediate_to_std1, SafePredFlag);
  Yap_InitCPred("print_intermediate_to_file", 0, p_print_intermediate_to_file, SafePredFlag);
  Yap_InitCPred("print_intermediate_to_file", 1, p_print_intermediate_to_file1, SafePredFlag);
  Yap_InitCPred("no_print_llva", 0, p_no_print_llva, SafePredFlag);
  Yap_InitCPred("print_llva_before", 0, p_print_llva_before, SafePredFlag);
  Yap_InitCPred("print_llva_after", 0, p_print_llva_after, SafePredFlag);
  Yap_InitCPred("no_print_me", 1, p_no_print_me, SafePredFlag);
  Yap_InitCPred("print_me", 2, p_print_me, SafePredFlag);
  Yap_InitCPred("default_debug", 0, p_default_debug, SafePredFlag);
  Yap_InitCPred("print_default_predicate_msgs", 0, p_print_default_predicate_msgs, SafePredFlag);
  Yap_InitCPred("print_all_predicate_msgs", 0, p_print_all_predicate_msgs, SafePredFlag);
  Yap_InitCPred("print_info_predicate_msgs", 0, p_print_info_predicate_msgs, SafePredFlag);
  Yap_InitCPred("print_success_predicate_msgs", 0, p_print_success_predicate_msgs, SafePredFlag);
  Yap_InitCPred("print_warning_predicate_msgs", 0, p_print_warning_predicate_msgs, SafePredFlag);
  Yap_InitCPred("print_error_predicate_msgs", 0, p_print_error_predicate_msgs, SafePredFlag);
  Yap_InitCPred("no_print_all_predicate_msgs", 0, p_no_print_all_predicate_msgs, SafePredFlag);
  Yap_InitCPred("no_print_info_predicate_msgs", 0, p_no_print_info_predicate_msgs, SafePredFlag);
  Yap_InitCPred("no_print_success_predicate_msgs", 0, p_no_print_success_predicate_msgs, SafePredFlag);
  Yap_InitCPred("no_print_warning_predicate_msgs", 0, p_no_print_warning_predicate_msgs, SafePredFlag);
  Yap_InitCPred("no_print_error_predicate_msgs", 0, p_no_print_error_predicate_msgs, SafePredFlag);
  Yap_InitCPred("print_predicate_msgs", 1, p_print_predicate_msgs, SafePredFlag);
  Yap_InitCPred("exit_on_warning", 0, p_exit_on_warning, SafePredFlag);
  Yap_InitCPred("disable_on_warning", 0, p_disable_on_warning, SafePredFlag);
  Yap_InitCPred("exit_on_error", 0, p_exit_on_error, SafePredFlag);
  Yap_InitCPred("no_exit_on_warning", 0, p_no_exit_on_warning, SafePredFlag);
  Yap_InitCPred("enable_on_warning", 0, p_enable_on_warning, SafePredFlag);
  Yap_InitCPred("no_exit_on_error", 0, p_no_exit_on_error, SafePredFlag);
#endif

}
