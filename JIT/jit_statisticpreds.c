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
* File:		jit_statisticpreds.c					 *
* comments:	JIT Compiler Statistics predicates			 *
*									 *
* Last rev:     2013-10-18                               		 *
*************************************************************************/

#define JIT_CODE 1

#include "jit_predicates.hpp"

#if YAP_STAT_PREDS

#include <papi.h>

static Int  p_init_low_level_stats( USES_REGS1 );
static Int  p_statistics_jit( USES_REGS1 );

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

static Int
p_init_low_level_stats( USES_REGS1 )
{
  int i = 0, j = 0;
  char *tmp;
  Term t = Deref(ARG1);
  // valid value for ARG1 is just 'atom'
  if (IsAtomTerm(t)) {
    // ARG1 is atom
    // gets string from atom and stores it on 'str'
    char *str = (char*)malloc(YAP_AtomNameLength(AtomOfTerm(t))*sizeof(char));
    strcpy(str, AtomName(AtomOfTerm(t)));
    // Makes upper characters of 'str' (for comparison)
    UPPER_ENTRY(str);

    // Detects one papi event (each event is a set of several performance counters)
    // Such events are: 'conditional_branching', 'cache_requests', 'conditional_store', 'floating_point_operations', 'instruction_counting', 'cache_access', 'data_access', and 'tlb_operations'
    // Event chosen by user is stored on 'ExpEnv.stats_struc.papi_event_type'
    // Only one event can be chosen at a time
    if (strcmp(str, "CONDITIONALBRANCHING") == 0)
      ExpEnv.stats_struc.papi_event_type = 0;
    else if (strcmp(str, "CACHEREQUESTS") == 0)
      ExpEnv.stats_struc.papi_event_type = 1;
    else if (strcmp(str, "CONDITIONALSTORE") == 0)
      ExpEnv.stats_struc.papi_event_type = 2;
    else if (strcmp(str, "FLOATINGPOINTOPERATIONS") == 0)
      ExpEnv.stats_struc.papi_event_type = 3;
    else if (strcmp(str, "INSTRUCTIONCOUNTING") == 0)
      ExpEnv.stats_struc.papi_event_type = 4;
    else if (strcmp(str, "CACHEACCESS") == 0)
      ExpEnv.stats_struc.papi_event_type = 5;
    else if (strcmp(str, "DATAACCESS") == 0)
      ExpEnv.stats_struc.papi_event_type = 6;
    else if (strcmp(str, "TLBOPERATIONS") == 0)
      ExpEnv.stats_struc.papi_event_type = 7;
    else {
      // value passed by argument is out of known range (unknown event)
      Yap_Error(OUT_OF_KNOWNRANGE_ERROR,t,"");
      return FALSE;
    }

  /* Initializing PAPI library */
  if (PAPI_library_init(PAPI_VER_CURRENT) != PAPI_VER_CURRENT) {
    fprintf(stderr, "PAPI library init error!\n");
    exit(1);
  }
  /***/

  /* Create one event set */
  if (PAPI_create_eventset(&ExpEnv.stats_struc.papi_eventset) != PAPI_OK)
    fprintf (stderr, "%s:%d\t ERROR\n", __FILE__, __LINE__);
  /***/

  /* Add counters to event set */
  if (ExpEnv.stats_struc.papi_event_type == 0) {
    // Event type is 'conditional_branching' -- contains 9 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(9*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 9; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Conditional Branching -- Conditional branch instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_CN) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Conditional Branching -- Branch instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Conditional Branching -- Conditional branch instructions mispredicted
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_MSP) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // Conditional Branching -- Conditional branch instructions not taken
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_NTK) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
    // Conditional Branching -- Conditional branch instructions correctly predicted
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_PRC) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[4] = 1;
    // Conditional Branching -- Conditional branch instructions taken
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_TKN) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[5] = 1;
    // Conditional Branching -- Unconditional branch instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BR_UCN) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[6] = 1;
    // Conditional Branching -- Cycles branch units are idle
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BRU_IDL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[7] = 1;
    // Conditional Branching -- Branch target address cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_BTAC_M) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[8] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 1) {
    // Event type is 'cache_requests' -- contains 5 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(5*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 5; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Cache Requests -- Requests for exclusive access to clean cache line
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CA_CLN) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Cache Requests -- Requests for cache line invalidation
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CA_INV) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Cache Requests -- Requests for cache line intervention
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CA_ITV) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // Cache Requests -- Requests for exclusive access to shared cache line
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CA_SHR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
    // Cache Requests -- Requests for a snoop
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CA_SNP) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[4] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 2) {
    // Event type is 'conditional_store' -- contains 3 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(3*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 3; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Conditional Store -- Failed store conditional instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CSR_FAL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Conditional Store -- Successful store conditional instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CSR_SUC) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Conditional Store -- Total store conditional instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_CSR_TOT) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 3) {
    // Event type is 'floating_point_operations' -- contains 14 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(14*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 14; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Floating Point Operations -- Floating point add instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FAD_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Floating Point Operations -- Floating point divide instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FDV_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Floating Point Operations -- FMA instructions completed
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FMA_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // Floating Point Operations -- Floating point multiply instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FML_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
    // Floating Point Operations -- Floating point inverse instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FNV_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[4] = 1;
    // Floating Point Operations -- Floating point instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FP_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[5] = 1;
    // Floating Point Operations -- Floating point operations
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FP_OPS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[6] = 1;
    // Floating Point Operations -- Cycles the FP unit
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FP_STAL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[7] = 1;
    // Floating Point Operations -- Cycles floating point units are idle
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FPU_IDL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[8] = 1;
    // Floating Point Operations -- Floating point square root instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FSQ_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[9] = 1;
    // Floating Point Operations -- Floating point operations executed; optimized to count scaled single precision vector operations
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_SP_OPS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[10] = 1;
    // Floating Point Operations -- Floating point operations executed; optimized to count scaled double precision vector operations
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_DP_OPS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[11] = 1;
    // Floating Point Operations -- Single precision vector/SIMD instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_VEC_SP) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[12] = 1;
    // Floating Point Operations -- Double precision vector/SIMD instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_VEC_DP) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[13] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 4) {
    // Event type is 'instruction_counting' -- contains 9 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(9*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 9; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Instruction Counting -- Cycles with maximum instructions completed
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FUL_CCY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Instruction Counting -- Cycles with maximum instruction issue
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FUL_ICY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Instruction Counting -- Cycles integer units are idle
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_FXU_IDL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // Instruction Counting -- Hardware interrupts
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_HW_INT) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
    // Instruction Counting -- Integer instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_INT_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[4] = 1;
    // Instruction Counting -- Total cycles
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TOT_CYC) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[5] = 1;
    // Instruction Counting -- Instructions issued
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TOT_IIS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[6] = 1;
    // Instruction Counting -- Instructions completed
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TOT_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[7] = 1;
    // Instruction Counting -- Vector/SIMD instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_VEC_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[8] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 5) {
    // Event type is 'cache_access' -- contains 51 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(51*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 51; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Cache Access -- L1 data cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_DCA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Cache Access -- L1 data cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_DCH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Cache Access -- L1 data cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_DCM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // Cache Access -- L1 data cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_DCR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
    // Cache Access -- L1 data cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_DCW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[4] = 1;
    // Cache Access -- L1 instruction cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_ICA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[5] = 1;
    // Cache Access -- L1 instruction cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_ICH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[6] = 1;
    // Cache Access -- L1 instruction cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_ICM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[7] = 1;
    // Cache Access -- L1 instruction cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_ICR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[8] = 1;
    // Cache Access -- L1 instruction cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_ICW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[9] = 1;
    // Cache Access -- L1 load misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_LDM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[10] = 1;
    // Cache Access -- L1 store misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_STM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[11] = 1;
    // Cache Access -- L1 total cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_TCA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[12] = 1;
    // Cache Access -- L1 total cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_TCH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[13] = 1;
    // Cache Access -- L1 total cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_TCM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[14] = 1;
    // Cache Access -- L1 total cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_TCR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[15] = 1;
    // Cache Access -- L1 total cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L1_TCW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[16] = 1;
    // Cache Access -- L2 data cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_DCA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[17] = 1;
    // Cache Access -- L2 data cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_DCH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[18] = 1;
    // Cache Access -- L2 data cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_DCM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[19] = 1;
    // Cache Access -- L2 data cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_DCR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[20] = 1;
    // Cache Access -- L2 data cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_DCW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[21] = 1;
    // Cache Access -- L2 instruction cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_ICA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[22] = 1;
    // Cache Access -- L2 instruction cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_ICH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[23] = 1;
    // Cache Access -- L2 instruction cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_ICM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[24] = 1;
    // Cache Access -- L2 instruction cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_ICR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[25] = 1;
    // Cache Access -- L2 instruction cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_ICW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[26] = 1;
    // Cache Access -- L2 load misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_LDM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[27] = 1;
    // Cache Access -- L2 store misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_STM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[28] = 1;
    // Cache Access -- L2 total cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_TCA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[29] = 1;
    // Cache Access -- L2 total cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_TCH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[30] = 1;
    // Cache Access -- L2 total cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_TCM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[31] = 1;
    // Cache Access -- L2 total cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_TCR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[32] = 1;
    // Cache Access -- L2 total cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L2_TCW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[33] = 1;
    // Cache Access -- L3 data cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_DCA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[34] = 1;
    // Cache Access -- L3 data cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_DCH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[35] = 1;
    // Cache Access -- L3 data cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_DCM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[36] = 1;
    // Cache Access -- L3 data cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_DCR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[37] = 1;
    // Cache Access -- L3 data cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_DCW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[38] = 1;
    // Cache Access -- L3 instruction cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_ICA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[39] = 1;
    // Cache Access -- L3 instruction cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_ICH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[40] = 1;
    // Cache Access -- L3 instruction cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_ICM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[41] = 1;
    // Cache Access -- L3 instruction cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_ICR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[42] = 1;
    // Cache Access -- L3 instruction cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_ICW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[43] = 1;
    // Cache Access -- L3 load misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_LDM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[44] = 1;
    // Cache Access -- L3 store misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_STM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[45] = 1;
    // Cache Access -- L3 total cache accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_TCA) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[46] = 1;
    // Cache Access -- L3 total cache hits
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_TCH) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[47] = 1;
    // Cache Access -- L3 total cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_TCM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[48] = 1;
    // Cache Access -- L3 total cache reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_TCR) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[49] = 1;
    // Cache Access -- L3 total cache writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_L3_TCW) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[50] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 6) {
    // Event type is 'data_access' -- contains 12 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(12*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 12; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // Data Access -- Load instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_LD_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // Data Access -- Load/store instructions completed
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_LST_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // Data Access -- Cycles load/store units are idle
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_LSU_IDL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // Data Access -- Cycles Stalled Waiting for memory reads
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_MEM_RCY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
    // Data Access -- Cycles Stalled Waiting for memory accesses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_MEM_SCY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[4] = 1;
    // Data Access -- Cycles Stalled Waiting for memory writes
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_MEM_WCY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[5] = 1;
    // Data Access -- Data prefetch cache misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_PRF_DM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[6] = 1;
    // Data Access -- Cycles stalled on any resource
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_RES_STL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[7] = 1;
    // Data Access -- Store instructions
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_SR_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[8] = 1;
    // Data Access -- Cycles with no instructions completed
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_STL_CCY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[9] = 1;
    // Data Access -- Cycles with no instruction issue
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_STL_ICY) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[10] = 1;
    // Data Access -- Synchronization instructions completed
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_SYC_INS) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[11] = 1;
  }

  else if (ExpEnv.stats_struc.papi_event_type == 7) {
    // Event type is 'tbl_operations' -- contains 4 performance counters
    ExpEnv.stats_struc.papi_valid_values = (short*)malloc(4*sizeof(short));

    /* Initializing 'ExpEnv.stats_struc.papi_valid_values'. Not all performance counters of an event are available, because they are machine-dependent. 'ExpEnv.stats_struc.papi_valid_values[i]' will be set with '1' if event 'i' is available */
    for (i = 0; i < 4; i++) {
      ExpEnv.stats_struc.papi_valid_values[i] = 0;
    }

    /* Adding performance counters of event*/
    // TLB Operations -- Data translation lookaside buffer misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TLB_DM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[0] = 1;
    // TLB Operations -- Instruction translation lookaside buffer misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TLB_IM) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[1] = 1;
    // TLB Operations -- Translation lookaside buffer shootdowns
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TLB_SD) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[2] = 1;
    // TLB Operations -- Total translation lookaside buffer misses
    if (PAPI_add_event(ExpEnv.stats_struc.papi_eventset, PAPI_TLB_TL) == PAPI_OK)
      ExpEnv.stats_struc.papi_valid_values[3] = 1;
  }

  // Setting 'ExpEnv.stats_struc.papi_initialized'. This flag will be tested on predicate 'statistics_jit/0' and event's results will be emitted if '1'
  ExpEnv.stats_struc.papi_initialized = 1;

  /* Start counting */
  if (PAPI_start(ExpEnv.stats_struc.papi_eventset) != PAPI_OK)
    ExpEnv.stats_struc.papi_initialized = 0;
  /***/

  return TRUE;

  }
  else {
    // ARG1 is not an atom
    Yap_NilError(INVALID_PARAMETER_TYPE_ERROR, "Low-level stats");
    return FALSE;
  }
}

static Int
p_statistics_jit( USES_REGS1 )
{
  if (NativeArea && NativeArea->n) { // This exp will be true only if JIT Compiler was used
    // printing...
    int i, j;
    fprintf(stderr, "------------------------------\n");
    fprintf(stderr, "Statistics for JIT::\n");
    fprintf(stderr, "------------------------------\n");
    for (i = 0; i < NativeArea->n; i++) { // For each slot in NativeArea
      if (NativeArea->area.ok[i] == 1) { // For each compiled code. Some slots in NativeArea may be empty because not all code is compiled (I still do not know why LLVM fails to compile some code)
        fprintf(stderr, "    Trace %d:\n", i+1);

        /* print size of each intermediate code compiled on each (re)compilation */
	for (j = 0; j < NativeArea->area.nrecomp[i]; j++) {
	  fprintf(stderr, "\tSize (%d): %ld bytes.\n", j+1, NativeArea->area.trace_size_bytes[i][j]);
	}

        /* print size of each native code on each (re)compilation */
	for (j = 0; j < NativeArea->area.nrecomp[i]; j++) {
	  fprintf(stderr, "\tNative size (%d): %ld bytes.\n", j+1, NativeArea->area.native_size_bytes[i][j]);
	}

        /* print compile time of each native code on each (re)compilation */
	for (j = 0; j < NativeArea->area.nrecomp[i]; j++) {
	  fprintf(stderr, "\tNative compile time (%d): %.3lf sec.\n", j+1, NativeArea->area.compilation_time[i][j]);
	}

        // get address on IntermediatecodeArea which is stored on first instruction of native code (this instruction is NativeArea->area.pc[i]')
	int taddress = ((yamop*)NativeArea->area.pc[i])->u.jhc.jh->caa.taddress;

        if (taddress != -1) {
          fprintf(stderr, "\tProfiling time: %.3lf sec.\n", IntermediatecodeArea->area.profiling_time[taddress]);
	  fprintf(stderr, "\tRun time: %.3lf sec.\n", NativeArea->t_runs[i]);
          fprintf(stderr, "\t%ld runs\n", NativeArea->runs[i]);
          fprintf(stderr, "\t%ld success\n", NativeArea->success[i]);
	}
      }
    }
    fprintf(stderr, "------------------------------\n");
  }

#if YAP_STAT_PREDS
  // From this point until the end we do:
  //   1. We verify if PAPI was initialized (ExpEnv.stats_struc.papi_initialized). If yes, we do:
  //   2. We verify what event type was used. Based on this, we alloc memory for 'ExpEnv.stats_struc.papi_values'
  //   3. 'ExpEnv.stats_struc.papi_values' is a vector which will be contain performance counters' values
  //   4. As previously mentioned, not all performance counters are available on target machine. For such, the position in 'ExpEnv.stats_struc.papi_values' will be '0'. Therefore not be considered
  //   5. Values of each performance counter will be read by function 'PAPI_read'. Note that one of argument is that 'ExpEnv.stats_struc.papi_values'
  //   6. Finally, values will be printed (stderr)
  if (ExpEnv.stats_struc.papi_initialized) {
    if (ExpEnv.stats_struc.papi_event_type == 0)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(9*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 1)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(5*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 2)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(3*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 3)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(14*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 4)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(9*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 5)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(51*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 6)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(12*sizeof(long long));
    else if (ExpEnv.stats_struc.papi_event_type == 7)
      ExpEnv.stats_struc.papi_values = (long long*)malloc(4*sizeof(long long));

    if (PAPI_read(ExpEnv.stats_struc.papi_eventset, ExpEnv.stats_struc.papi_values) != PAPI_OK)
      fprintf (stderr, "%s:%d\t ERROR\n", __FILE__, __LINE__);

    int k = 0;
    if (ExpEnv.stats_struc.papi_event_type == 0) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tConditional branch instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tBranch instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tConditional branch instructions mispredicted:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tConditional branch instructions not taken:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[4])
        fprintf(stderr, "\tConditional branch instructions correctly predicted:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[5])
        fprintf(stderr, "\tConditional branch instructions taken:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[6])
        fprintf(stderr, "\tUnconditional branch instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[7])
        fprintf(stderr, "\tCycles branch units are idle:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[8])
        fprintf(stderr, "\tBranch target address cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 1) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tRequests for exclusive access to clean cache line:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tRequests for cache line invalidation:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "	CCache Requests -- Requests for cache line intervention:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tRequests for exclusive access to shared cache line:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[4])
        fprintf(stderr, "\tRequests for a snoop:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 2) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tFailed store conditional instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tSuccessful store conditional instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tTotal store conditional instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 3) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tFloating point add instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tFloating point divide instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tFMA instructions completed:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tFloating point multiply instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[4])
        fprintf(stderr, "\tFloating point inverse instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[5])
        fprintf(stderr, "\tFloating point instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[6])
        fprintf(stderr, "\tFloating point operations:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[7])
        fprintf(stderr, "\tCycles the FP unit:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[8])
        fprintf(stderr, "\tCycles floating point units are idle:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[9])
        fprintf(stderr, "\tFloating point square root instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[10])
        fprintf(stderr, "\tFloating point operations executed; optimized to count scaled single precision vector operations:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[11])
        fprintf(stderr, "\tFloating point operations executed; optimized to count scaled double precision vector operations:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[12])
        fprintf(stderr, "\tSingle precision vector/SIMD instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[13])
        fprintf(stderr, "\tDouble precision vector/SIMD instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 4) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tCycles with maximum instructions completed:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "	FInstruction Counting -- Cycles with maximum instruction issue:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tCycles integer units are idle:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tHardware interrupts:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[4])
        fprintf(stderr, "\tInteger instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[5])
        fprintf(stderr, "\tTotal cycles:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[6])
        fprintf(stderr, "\tInstructions issued:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[7])
        fprintf(stderr, "\tInstructions completed:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[8])
        fprintf(stderr, "\tVector/SIMD instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 5) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tL1 data cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tL1 data cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tL1 data cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tL1 data cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[4])
        fprintf(stderr, "\tL1 data cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[5])
        fprintf(stderr, "\tL1 instruction cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[6])
        fprintf(stderr, "\tL1 instruction cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[7])
        fprintf(stderr, "\tL1 instruction cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[8])
        fprintf(stderr, "\tL1 instruction cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[9])
        fprintf(stderr, "\tL1 instruction cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[10])
        fprintf(stderr, "\tL1 load misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[11])
        fprintf(stderr, "\tL1 store misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[12])
        fprintf(stderr, "\tL1 total cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[13])
        fprintf(stderr, "\tL1 total cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[14])
        fprintf(stderr, "\tL1 total cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[15])
        fprintf(stderr, "\tL1 total cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[16])
        fprintf(stderr, "\tL1 total cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[17])
        fprintf(stderr, "\tL2 data cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[18])
        fprintf(stderr, "\tL2 data cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[19])
        fprintf(stderr, "\tL2 data cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[20])
        fprintf(stderr, "\tL2 data cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[21])
        fprintf(stderr, "\tL2 data cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[22])
        fprintf(stderr, "\tL2 instruction cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[23])
        fprintf(stderr, "\tL2 instruction cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[24])
        fprintf(stderr, "\tL2 instruction cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[25])
        fprintf(stderr, "\tL2 instruction cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[26])
        fprintf(stderr, "\tL2 instruction cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[27])
        fprintf(stderr, "\tL2 load misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[28])
        fprintf(stderr, "\tL2 store misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[29])
        fprintf(stderr, "\tL2 total cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[30])
        fprintf(stderr, "\tL2 total cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[31])
        fprintf(stderr, "\tL2 total cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[32])
        fprintf(stderr, "\tL2 total cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[33])
        fprintf(stderr, "\tL2 total cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[34])
        fprintf(stderr, "\tL3 data cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[35])
        fprintf(stderr, "\tL3 data cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[36])
        fprintf(stderr, "\tL3 data cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[37])
        fprintf(stderr, "\tL3 data cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[38])
        fprintf(stderr, "\tL3 data cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[39])
        fprintf(stderr, "\tL3 instruction cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[40])
        fprintf(stderr, "\tL3 instruction cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[41])
        fprintf(stderr, "\tL3 instruction cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[42])
        fprintf(stderr, "\tL3 instruction cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[43])
      fprintf(stderr, "\tL3 instruction cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[44])
        fprintf(stderr, "\tL3 load misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[45])
        fprintf(stderr, "\tL3 store misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[46])
        fprintf(stderr, "\tL3 total cache accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[47])
        fprintf(stderr, "\tL3 total cache hits:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[48])
        fprintf(stderr, "\tL3 total cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[49])
        fprintf(stderr, "\tL3 total cache reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[50])
        fprintf(stderr, "\tL3 total cache writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 6) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tLoad instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tLoad/store instructions completed:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tCycles load/store units are idle:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tCycles Stalled Waiting for memory reads:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[4])
        fprintf(stderr, "\tCycles Stalled Waiting for memory accesses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[5])
        fprintf(stderr, "\tCycles Stalled Waiting for memory writes:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[6])
        fprintf(stderr, "\tData prefetch cache misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[7])
        fprintf(stderr, "\tCycles stalled on any resource:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[8])
        fprintf(stderr, "\tStore instructions:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[9])
        fprintf(stderr, "\tCycles with no instructions completed:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[10])
        fprintf(stderr, "\tCycles with no instruction issue:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[11])
        fprintf(stderr, "\tSynchronization instructions completed:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    else if (ExpEnv.stats_struc.papi_event_type == 7) {
      if (ExpEnv.stats_struc.papi_valid_values[0])
        fprintf(stderr, "\tData translation lookaside buffer misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[1])
        fprintf(stderr, "\tInstruction translation lookaside buffer misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[2])
        fprintf(stderr, "\tTranslation lookaside buffer shootdowns:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
      if (ExpEnv.stats_struc.papi_valid_values[3])
        fprintf(stderr, "\tTotal translation lookaside buffer misses:: %lld\n", ExpEnv.stats_struc.papi_values[k++]);
    }

    fprintf(stderr, "------------------------------\n");
#endif
  }

  return TRUE;
}

#pragma GCC diagnostic pop

#endif

void
Yap_InitJitStatisticPreds(void)
{
#if YAP_STAT_PREDS
  Yap_InitCPred("init_low_level_stats", 1, p_init_low_level_stats, SafePredFlag);
  Yap_InitCPred("statistics_jit", 0, p_statistics_jit, SafePredFlag);
#endif
}
