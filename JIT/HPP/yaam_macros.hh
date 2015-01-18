typedef enum {
  #include "Yap_BasicBlocks.h"
} YAP_BBs;

#ifndef _NATIVE

#include "lastop.h"
#include "sprintblock.h"

static inline Int
set_last_deeply(BlocksContext* b, BlocksContext** last) {
  BlocksContext *last1, *last2;
  while (b) {
    if (b->blockty == MULTIPLE_DESTINY && b->thisp == BLOCKADDRESS && b->u.mdb.id == BLOCK) {
      *last = b;
      return 1;
    }
    if (b->blockty == CONDITIONAL_HEADER) {
	  Int _if = set_last_deeply(b->u.kb._if, &last1);
	  Int _else = set_last_deeply(b->u.kb._else, &last2);
      if (_if || _else) {
		if (_if) *last = last1;
		else *last = last2;
	    return 1;
	  }
    }
    b = (BlocksContext*)b->next;
  }
  *last = NULL;
  return 0;
}

#ifdef USE_GMP
#define FUNCTOR_LARGE_INT "(CELL)f0 == (CELL)FunctorLongInt || (CELL)f0 == (CELL)FunctorBigInt"
#else
#define FUNCTOR_LARGE_INT "(CELL)f0 == (CELL)FunctorLongInt"
#endif

#define SET_DEPTH(yaam_block) \
      { \
	    inst_depth = 0; \
	    if (yaam_block == GET_X_VAL_INSTINIT) { \
	      inst_depth = 2; \
		} \
	  }

#define BACK_TO_HEADER() \
      { \
        while (curblock->prev) { \
          curblock = (BlocksContext*)curblock->prev; \
        } \
        curblock = (BlocksContext*)curblock->u.xb.header; \
      }
      
#define FREE(N) \
      { \
		int j; \
		for (j = 0; j < N; j++) { \
		 free(cexp[j]); \
		} \
		free(cexp); \
		free(condty); \
	  }

#define YAAM_BLOCK_IS_DEEPB_MULTIPLE_DESTINY(yaam_block) \
	  yaam_block == GL_VOID_VALX_GLIST_VOID_VALX_NONVAR_NONVAR || \
	  yaam_block == GL_VOID_VALX_GLIST_VOID_VALX_READ || \
	  yaam_block == GL_VOID_VALY_GLIST_VOID_VALY_NONVAR_NONVAR || \
	  yaam_block == GL_VOID_VALY_GLIST_VOID_VALY_READ || \
	  yaam_block == UNIFY_X_VAL_UVALX_NONVAR_NONVAR || \
	  yaam_block == UNIFY_L_X_VAL_ULVALX_NONVAR_NONVAR || \
	  
#define EMIT_CONDITIONAL2_BLOCK(yaam_block, EXP1, EXP2, F1, F2) \
	  if (first_valid_conditional) { \
	      EMIT_2DEEP_BLOCK_TEST(yaam_block, EXP1, EXP2, F1, F2); \
	      first_valid_conditional = 0; \
	  } \
	  else { \
	      EMIT_DEEPFK_BLOCK_TEST(yaam_block); \
	  }
	  
#define EMIT_CONDITIONAL1_BLOCK(yaam_block, EXP1, F1) \
	  if (first_valid_conditional) { \
	      EMIT_1DEEP_BLOCK_TEST(yaam_block, EXP1, F1); \
	      first_valid_conditional = 0; \
	  } \
	  else { \
	      EMIT_DEEPFK_BLOCK_TEST(yaam_block); \
	  }
      
#if defined(YAPOR) || defined(THREADS)
#define YAAM_BLOCK_IS_SIMPLEB_MULTIPLE_DESTINY(yaam_block) \
        yaam_block == YAAM_UNIFYBOUND || \
		yaam_block == PROFILED_RETRY_LOGICAL_END || \
		yaam_block == PROFILED_TRUST_LOGICAL_END || \
		yaam_block == COUNT_RETRY_LOGICAL_END || \
		yaam_block == COUNT_TRUST_LOGICAL_END || \
		yaam_block == LOCK_LU_END || \
		yaam_block == TRY_AND_MARK_YAPOR_THREADS_NOYAPOR_IF || \
#else /* defined(YAPOR) || defined(THREADS) */
#define YAAM_BLOCK_IS_SIMPLEB_MULTIPLE_DESTINY(yaam_block) \
        yaam_block == YAAM_UNIFYBOUND || \
		yaam_block == PROFILED_RETRY_LOGICAL_END || \
		yaam_block == PROFILED_TRUST_LOGICAL_END || \
		yaam_block == COUNT_RETRY_LOGICAL_END || \
		yaam_block == COUNT_TRUST_LOGICAL_END || \
		yaam_block == LOCK_LU_END
#endif

#if YAP_DBG_PREDS
#define PRINT_BLOCK_MAC(yaam_block) print_block(yaam_block, ON_PROFILED_INTERPRETER);
#else /* YAP_DBG_PREDS */
#define PRINT_BLOCK_MAC(yaam_block)
#endif /* YAP_DBG_PREDS */

#if YAP_STAT_PREDS
#define INIT_SPENT_PROF_TIME_FOR_HEAD(i) \
       struct rusage rustart, ruend; \
       struct timeval timstart, timend; \
       getrusage(RUSAGE_SELF, &rustart);

#define COMPLETE_SPENT_PROF_TIME_FOR_HEAD(i) \
       getrusage(RUSAGE_SELF, &ruend); \
       timstart = rustart.ru_utime; \
       timend = ruend.ru_utime; \
       double spenttime = ((double)timend.tv_sec - (double)timstart.tv_sec) + ((double)timend.tv_usec - (double)timstart.tv_usec) / 1000000.0; \
       IntermediatecodeArea->area.profiling_time[i] += spenttime;

#define INIT_SPENT_PROF_TIME() \
       struct rusage rustart, ruend; \
       struct timeval timstart, timend; \
       getrusage(RUSAGE_SELF, &rustart);

#define COMPLETE_SPENT_PROF_TIME() \
       getrusage(RUSAGE_SELF, &ruend); \
       timstart = rustart.ru_utime; \
       timend = ruend.ru_utime; \
       double spenttime = ((double)timend.tv_sec - (double)timstart.tv_sec) + ((double)timend.tv_usec - (double)timstart.tv_usec) / 1000000.0; \
       IntermediatecodeArea->area.profiling_time[i] += spenttime;

#else /* YAP_STAT_PREDS */
#define INIT_SPENT_PROF_TIME_FOR_HEAD(i)

#define COMPLETE_SPENT_PROF_TIME_FOR_HEAD(i)

#define INIT_SPENT_PROF_TIME()

#define COMPLETE_SPENT_PROF_TIME()

#endif /* YAP_STAT_PREDS */

static void EMIT_MULTIPLE_DESTINY_BLOCK_TEST(YAP_BBs);

static void
EMIT_MULTIPLE_DESTINY_BLOCK_TEST(YAP_BBs yaam_block) {
      if (IntermediatecodeArea->n) {
	PRINT_BLOCK_MAC(yaam_block);
	int i;
	for (i = 0; i < IntermediatecodeArea->n; i++) {
	  if (IntermediatecodeArea->area.t &&
	    IntermediatecodeArea->area.ok &&
	    IntermediatecodeArea->area.isactive &&
	    IntermediatecodeArea->area.isactive[i])
	  {
            INIT_SPENT_PROF_TIME();
	    curtrace = &IntermediatecodeArea->area.t[i];
            BlocksContext *curblock;
	    if (!globalcurblock) curblock = (*curtrace)->bc;
            else curblock = globalcurblock[i];
            while (curblock->next) {
              if ( (curblock->blockty == SIMPLE && curblock->thisp == (CELL)curpreg && curblock->u.sb.id == (UInt)yaam_block) ||
                   (curblock->blockty == MULTIPLE_DESTINY && curblock->thisp == (CELL)curpreg && curblock->u.mdb.id == (UInt)yaam_block) )
                   { break; }
              curblock = (BlocksContext*)curblock->next;
            }
            if (!curblock->next &&
               (curblock->blockty != SIMPLE || curblock->thisp != (CELL)curpreg || curblock->u.sb.id != (UInt)yaam_block) &&
               (curblock->blockty != MULTIPLE_DESTINY || curblock->thisp != (CELL)curpreg || curblock->u.mdb.id != (UInt)yaam_block)
              ) {
              BlocksContext* t = (BlocksContext*)malloc(sizeof(BlocksContext));
              t->thisp = (CELL)curpreg;
              t->next = 0;
              t->prev = (CELL)curblock;
              t->blockty = MULTIPLE_DESTINY;
              t->u.mdb.id = (UInt)yaam_block;
              t->u.mdb.ndest = 0;
              t->u.mdb.nfaillabels = 0;
              t->u.mdb.destiny.p = NULL;
              t->u.mdb.destiny.labels = NULL;
              t->u.mdb.faildestiny.p = NULL;
              t->u.mdb.faildestiny.labels = NULL;
	      (*curtrace)->n += 1;
              curblock->next = (CELL)t;
              IntermediatecodeArea->area.lastblock[i] = t;
            }
            else {
              IntermediatecodeArea->area.lastblock[i] = curblock;
            }
            COMPLETE_SPENT_PROF_TIME();
          }
        }
      }
}
		
static void EMIT_SIMPLE_BLOCK_TEST(YAP_BBs);

static void
EMIT_SIMPLE_BLOCK_TEST(YAP_BBs yaam_block) {
      if (IntermediatecodeArea->n) {
	PRINT_BLOCK_MAC(yaam_block);
	int i;
	for (i = 0; i < IntermediatecodeArea->n; i++) {
	  if (IntermediatecodeArea->area.t &&
	    IntermediatecodeArea->area.ok &&
	    IntermediatecodeArea->area.isactive &&
	    IntermediatecodeArea->area.isactive[i])
	  {
            INIT_SPENT_PROF_TIME();
	    curtrace = &IntermediatecodeArea->area.t[i];
            BlocksContext *curblock;
	    if (!globalcurblock) curblock = (*curtrace)->bc;
            else curblock = globalcurblock[i];
            while (curblock->next) {
              if ( (curblock->blockty == SIMPLE && curblock->thisp == (CELL)curpreg && curblock->u.sb.id == (UInt)yaam_block) ||
                   (curblock->blockty == MULTIPLE_DESTINY && curblock->thisp == (CELL)curpreg && curblock->u.mdb.id == (UInt)yaam_block) )
                   { break; }
              curblock = (BlocksContext*)curblock->next;
            }
            if (!curblock->next &&
               (curblock->blockty != SIMPLE || curblock->thisp != (CELL)curpreg || curblock->u.sb.id != (UInt)yaam_block) &&
               (curblock->blockty != MULTIPLE_DESTINY || curblock->thisp != (CELL)curpreg || curblock->u.mdb.id != (UInt)yaam_block)
              ) {
              BlocksContext* t = (BlocksContext*)malloc(sizeof(BlocksContext));
              t->thisp = (CELL)curpreg;
              t->next = 0;
              t->prev = (CELL)curblock;
              t->blockty = SIMPLE;
              t->u.sb.id = (UInt)yaam_block;
              t->u.sb.label_destiny = NULL;
	      (*curtrace)->n += 1;
              curblock->next = (CELL)t;
              IntermediatecodeArea->area.lastblock[i] = t;
            }
            else {
              IntermediatecodeArea->area.lastblock[i] = curblock;
            }
            COMPLETE_SPENT_PROF_TIME();
          }
        }
      }
}
	  
#define EMIT_3DEEP_BLOCK(yaam_block, EXP1, EXP2, EXP3, N1, N2, N3) \
      if (IntermediatecodeArea->n) { \
		char *tmp = (char*) malloc(2048*sizeof(char)); \
		sprint_block((YAP_BBs)yaam_block, &tmp); \
		fprintf(stderr, "OOps!! Encontrei um bloco não instrumentado corretamente -- %s\n", tmp); \
		free(tmp); \
		exit(1); \
	  }
	  
#define EMIT_2DEEP_BLOCK(yaam_block, EXP1, EXP2, N1, N2) \
      if (IntermediatecodeArea->n) { \
		char *tmp = (char*) malloc(2048*sizeof(char)); \
		sprint_block((YAP_BBs)yaam_block, &tmp); \
		fprintf(stderr, "OOps!! Encontrei um bloco não instrumentado corretamente -- %s\n", tmp); \
		free(tmp); \
		exit(1); \
	  }
	  
#define EMIT_1DEEP_BLOCK(yaam_block, EXP, N) \
      if (IntermediatecodeArea->n) { \
		char *tmp = (char*) malloc(2048*sizeof(char)); \
		sprint_block((YAP_BBs)yaam_block, &tmp); \
		fprintf(stderr, "OOps!! Encontrei um bloco não instrumentado corretamente -- %s\n", tmp); \
		free(tmp); \
		exit(1); \
	  }
				  
#define EMIT_SIMPLE_BLOCK(yaam_block) \
      if (IntermediatecodeArea->n) { \
	char *tmp = (char*) malloc(2048*sizeof(char)); \
	sprint_block((YAP_BBs)yaam_block, &tmp); \
	fprintf(stderr, "OOps!! Encontrei um bloco não instrumentado corretamente -- %s\n", tmp); \
      }
		

#define REDEFINE_DESTINY(A, B) \
      if ((A)->blockty == SIMPLE_ENTRY) { \
        strcpy((A)->u.eb.label_destiny, B->u.eb.label_entry); \
      } \
      else if ((A)->blockty == SIMPLE) { \
        strcpy((A)->u.sb.label_destiny, B->u.eb.label_entry); \
      }

#define SET_FAILDESTINY_AS_MULTIPLE(B, i) \
      int k = 0; \
      while (k < IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels) { \
        if (!strcmp(IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.labels[k], (B)->u.eb.label_entry)) break; \
        k++; \
      } \
      if (k == IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels) { \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels++; \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.p = (UInt*)realloc(IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.p, IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels*sizeof(UInt)); \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.p[IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels-1] = (B)->thisp; \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.labels = (char**)realloc(IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.labels, IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels*sizeof(char*)); \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.labels[IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels-1] = (char*)malloc(1024*sizeof(char)); \
        strcpy(IntermediatecodeArea->area.lastblock[i]->u.mdb.faildestiny.labels[IntermediatecodeArea->area.lastblock[i]->u.mdb.nfaillabels-1], (B)->u.eb.label_entry); \
      }

#define SET_DESTINY_AS_MULTIPLE(B, i) \
      int k = 0; \
      while (k < IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest) { \
        if (!strcmp(IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.labels[k], (B)->u.eb.label_entry)) break; \
        k++; \
      } \
      if (k == IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest) { \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest++; \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.p = (UInt*)realloc(IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.p, IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest*sizeof(UInt)); \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.p[IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest-1] = (B)->thisp; \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.labels = (char**)realloc(IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.labels, IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest*sizeof(char*)); \
        IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.labels[IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest-1] = (char*)malloc(1024*sizeof(char)); \
        strcpy(IntermediatecodeArea->area.lastblock[i]->u.mdb.destiny.labels[IntermediatecodeArea->area.lastblock[i]->u.mdb.ndest-1], (B)->u.eb.label_entry); \
      }

#define SET_DESTINY(yaam_block, B, i) \
      if (IntermediatecodeArea->area.lastblock[i]) { \
        if (IntermediatecodeArea->area.lastblock[i]->blockty == MULTIPLE_DESTINY) { \
          if (yaam_block == LBL_FAIL_INSTINIT) { \
			SET_FAILDESTINY_AS_MULTIPLE((B), i); \
		  } \
		  else { \
            SET_DESTINY_AS_MULTIPLE((B), i); \
	      } \
        } \
        if (IntermediatecodeArea->area.lastblock[i]->blockty == SIMPLE_ENTRY) { \
          if (!IntermediatecodeArea->area.lastblock[i]->u.eb.label_destiny) { \
            IntermediatecodeArea->area.lastblock[i]->u.eb.label_destiny = (char*)malloc(1024*sizeof(char)); \
            strcpy(IntermediatecodeArea->area.lastblock[i]->u.eb.label_destiny, (B)->u.eb.label_entry); \
          } \
        } \
        else if (IntermediatecodeArea->area.lastblock[i]->blockty == SIMPLE) { \
          if (IntermediatecodeArea->area.lastblock[i]->u.sb.id != NoStackExecute_Exception) { \
            if (!IntermediatecodeArea->area.lastblock[i]->u.sb.label_destiny) { \
              IntermediatecodeArea->area.lastblock[i]->u.sb.label_destiny = (char*)malloc(1024*sizeof(char)); \
              strcpy(IntermediatecodeArea->area.lastblock[i]->u.sb.label_destiny, (B)->u.eb.label_entry); \
            } \
          } \
        } \
      }

static void EMIT_HEAD_BLOCK(yamop*);

static void
EMIT_HEAD_BLOCK(yamop* __P) {
      INIT_SPENT_PROF_TIME_FOR_HEAD(__P->u.jhc.jh->caa.taddress);
      if (curtrace) {
        curpreg = __P;
        if (curpreg == headoftrace && curtrace && (*curtrace)->n) {
          SET_DESTINY(JIT_HANDLER_INSTINIT, (*curtrace)->bc, __P->u.jhc.jh->caa.taddress);
        }
        else {
	  (*curtrace)->bc = (BlocksContext*)malloc(sizeof(BlocksContext));
          (*curtrace)->bc->blockty = SIMPLE_ENTRY; 
          (*curtrace)->bc->thisp = (CELL)curpreg;
          (*curtrace)->bc->next = 0;
          (*curtrace)->bc->prev = 0;
          (*curtrace)->bc->u.eb.id = (UInt)JIT_HANDLER_INSTINIT;
          (*curtrace)->bc->u.eb.label_destiny = NULL;
          (*curtrace)->bc->u.eb.label_entry = (char*)malloc(1024*sizeof(char));
          strcpy((*curtrace)->bc->u.eb.label_entry, "LBL_JIT_HANDLER_INSTINIT");
          char *hex = (char*)malloc(33*sizeof(char));
          sprintf(hex, "_%lX", (UInt)curpreg);
          strcat((*curtrace)->bc->u.eb.label_entry, hex);
	  (*curtrace)->n = 1;
        }
      }
      COMPLETE_SPENT_PROF_TIME_FOR_HEAD(__P->u.jhc.jh->caa.taddress);
}

static void EMIT_ENTRY_BLOCK(yamop*, YAP_BBs);

static void
EMIT_ENTRY_BLOCK(yamop* __P, YAP_BBs yaam_block) {
      if (IntermediatecodeArea->n) {
	PRINT_BLOCK_MAC(yaam_block);
	if (globalcurblock) {
	  free(globalcurblock);
	  globalcurblock = NULL;
	}
	int i;
	for (i = 0; i < IntermediatecodeArea->n; i++) {
	  if (IntermediatecodeArea->area.t &&
	    IntermediatecodeArea->area.ok &&
	    IntermediatecodeArea->area.isactive &&
	    IntermediatecodeArea->area.isactive[i])
	  {
            INIT_SPENT_PROF_TIME();
            curtrace = &IntermediatecodeArea->area.t[i];
            curpreg = __P;
            BlocksContext *curblock = (*curtrace)->bc;
            while (curblock->next) {
              if (curblock->blockty == SIMPLE_ENTRY && curblock->thisp == (CELL)curpreg && curblock->u.eb.id == (UInt)yaam_block) {
                break;
              }
              curblock = (BlocksContext*)curblock->next;
            }
            if (!curblock->next &&
                 (curblock->blockty != SIMPLE_ENTRY || curblock->thisp != (CELL)curpreg || curblock->u.eb.id != (UInt)yaam_block)
               ) {
              BlocksContext* t = (BlocksContext*)malloc(sizeof(BlocksContext));
	      t = (BlocksContext*)malloc(sizeof(BlocksContext));
              t->blockty = SIMPLE_ENTRY;
              t->thisp = (CELL)curpreg;
              t->next = 0;
              t->prev = (CELL)curblock;
              t->u.eb.id = (UInt)yaam_block;
              t->u.eb.label_destiny = NULL;
              t->u.eb.label_entry = (char*)malloc(1024*sizeof(char));
              {
                strcpy(t->u.eb.label_entry, "LBL_");
                char *tmp = (char*)malloc(2048*sizeof(char));
                linear_sprint_block(yaam_block, &tmp);
                strcat(t->u.eb.label_entry, tmp);
                char* hex = (char*)malloc(33*sizeof(char));
                sprintf(hex, "_%lX", (UInt)t->thisp);
                strcat(t->u.eb.label_entry, hex);
              }
              SET_DESTINY(yaam_block, t, i);
              if (ineedredefinedest) {
                REDEFINE_DESTINY(IntermediatecodeArea->area.lastblock[i], t);
                ineedredefinedest = 0;
              }
	      (*curtrace)->n += 1;
              curblock->next = (CELL)t;
              IntermediatecodeArea->area.lastblock[i] = t;
            }
            else {
              SET_DESTINY(yaam_block, curblock, i);
              IntermediatecodeArea->area.lastblock[i] = curblock;
            }
            COMPLETE_SPENT_PROF_TIME();
          }
	}
      }
}
	  
#define EMIT_CONDITIONAL_FAIL(EXP) \
      if (IntermediatecodeArea->n) { \
		int i; \
		BlocksContext** temp_globalcurblock = (BlocksContext**)malloc(IntermediatecodeArea->n * sizeof(BlocksContext*)); \
		for (i = 0; i < IntermediatecodeArea->n; i++) { \
	      if (IntermediatecodeArea->area.t && \
			  IntermediatecodeArea->area.ok && \
			  IntermediatecodeArea->area.isactive && \
			  IntermediatecodeArea->area.isactive[i]) \
		  { \
                    INIT_SPENT_PROF_TIME(); \
		    curtrace = &IntermediatecodeArea->area.t[i]; \
            BlocksContext *c; \
			if (!globalcurblock) c = (*curtrace)->bc; \
			else c = globalcurblock[i]; \
            while (c->next) { \
              if (c->blockty == CONDITIONAL_HEADER && c->thisp == (CELL)curpreg && !strcmp(c->u.kb.exp, EXP)) { break; } \
              c = (BlocksContext*)c->next; \
            } \
            if (!c->next && (c->blockty != CONDITIONAL_HEADER || c->thisp != (CELL)curpreg || strcmp(c->u.kb.exp, EXP)) ) { \
              BlocksContext *t = (BlocksContext*)malloc(sizeof(BlocksContext)); \
              t->blockty = CONDITIONAL_HEADER; \
              t->thisp = (CELL)curpreg; \
              t->next = 0; \
              t->prev = (CELL)c; \
              t->u.kb.exp = (char*)malloc(1024*sizeof(char)); \
              strcpy(t->u.kb.exp, EXP); \
              t->u.kb._if = (BlocksContext*)malloc(sizeof(BlocksContext)); \
              t->u.kb._if->blockty = NONE; \
              t->u.kb._if->thisp = (CELL)curpreg; \
              t->u.kb._if->next = 0; \
              t->u.kb._if->prev = 0; \
              t->u.kb._if->u.xb.header = (CELL)t; \
              t->u.kb._else = (BlocksContext*)malloc(sizeof(BlocksContext)); \
              t->u.kb._else->blockty = NONE; \
              t->u.kb._else->thisp = (CELL)curpreg; \
              t->u.kb._else->next = 0; \
              t->u.kb._else->prev = 0; \
              t->u.kb._else->u.xb.header = (CELL)t; \
              c->next = (CELL)t; \
              c = t->u.kb._else; \
	          (*curtrace)->n += 2; \
            } \
            else { \
              c = c->u.kb._else; \
            } \
			temp_globalcurblock[i] = c; \
            COMPLETE_SPENT_PROF_TIME(); \
		  } \
		} \
		globalcurblock = temp_globalcurblock; \
	  }
	  
#define EMIT_CONDITIONAL_SUCCESS(EXP) \
      if (IntermediatecodeArea->n) { \
		int i; \
		BlocksContext** temp_globalcurblock = (BlocksContext**)malloc(IntermediatecodeArea->n * sizeof(BlocksContext*)); \
		for (i = 0; i < IntermediatecodeArea->n; i++) { \
	      if (IntermediatecodeArea->area.t && \
			  IntermediatecodeArea->area.ok && \
			  IntermediatecodeArea->area.isactive && \
			  IntermediatecodeArea->area.isactive[i]) \
		  { \
                    INIT_SPENT_PROF_TIME(); \
		    curtrace = &IntermediatecodeArea->area.t[i]; \
            BlocksContext *c; \
			if (!globalcurblock) c = (*curtrace)->bc; \
			else c = globalcurblock[i]; \
            while (c->next) { \
              if (c->blockty == CONDITIONAL_HEADER && c->thisp == (CELL)curpreg && !strcmp(c->u.kb.exp, EXP)) { break; } \
              c = (BlocksContext*)c->next; \
            } \
            if (!c->next && (c->blockty != CONDITIONAL_HEADER || c->thisp != (CELL)curpreg || strcmp(c->u.kb.exp, EXP)) ) { \
              BlocksContext *t = (BlocksContext*)malloc(sizeof(BlocksContext)); \
              t->blockty = CONDITIONAL_HEADER; \
              t->thisp = (CELL)curpreg; \
              t->next = 0; \
              t->prev = (CELL)c; \
              t->u.kb.exp = (char*)malloc(1024*sizeof(char)); \
              strcpy(t->u.kb.exp, EXP); \
              t->u.kb._if = (BlocksContext*)malloc(sizeof(BlocksContext)); \
              t->u.kb._if->blockty = NONE; \
              t->u.kb._if->thisp = (CELL)curpreg; \
              t->u.kb._if->next = 0; \
              t->u.kb._if->prev = 0; \
              t->u.kb._if->u.xb.header = (CELL)t; \
              t->u.kb._else = (BlocksContext*)malloc(sizeof(BlocksContext)); \
              t->u.kb._else->blockty = NONE; \
              t->u.kb._else->thisp = (CELL)curpreg; \
              t->u.kb._else->next = 0; \
              t->u.kb._else->prev = 0; \
              t->u.kb._else->u.xb.header = (CELL)t; \
              c->next = (CELL)t; \
              c = t->u.kb._if; \
	          (*curtrace)->n += 2; \
            } \
            else { \
              c = c->u.kb._if; \
            } \
			temp_globalcurblock[i] = c; \
            COMPLETE_SPENT_PROF_TIME(); \
		  } \
		} \
		globalcurblock = temp_globalcurblock; \
	  }
		
#define EMIT_CONDITIONAL() \
      { \
        BlocksContext *c = (*curtrace)->bc; \
        COUNT i = 0; \
        while (i < ncexp) { \
          while (c->next) { \
            if (c->blockty == CONDITIONAL_HEADER && c->thisp == (CELL)curpreg && !strcmp(c->u.kb.exp, cexp[i])) { break; } \
            c = (BlocksContext*)c->next; \
          } \
          if (!c->next && (c->blockty != CONDITIONAL_HEADER || c->thisp != (CELL)curpreg || strcmp(c->u.kb.exp, cexp[i])) ) { \
            BlocksContext *t = (BlocksContext*)malloc(sizeof(BlocksContext)); \
            t->blockty = CONDITIONAL_HEADER; \
            t->thisp = (CELL)curpreg; \
            t->next = 0; \
            t->prev = (CELL)c; \
            t->u.kb.exp = (char*)malloc(1024*sizeof(char)); \
            strcpy(t->u.kb.exp, cexp[i]); \
            t->u.kb._if = (BlocksContext*)malloc(sizeof(BlocksContext)); \
            t->u.kb._if->blockty = NONE; \
            t->u.kb._if->thisp = (CELL)curpreg; \
            t->u.kb._if->next = 0; \
            t->u.kb._if->prev = 0; \
            t->u.kb._if->u.xb.header = (CELL)t; \
            t->u.kb._else = (BlocksContext*)malloc(sizeof(BlocksContext)); \
            t->u.kb._else->blockty = NONE; \
            t->u.kb._else->thisp = (CELL)curpreg; \
            t->u.kb._else->next = 0; \
            t->u.kb._else->prev = 0; \
            t->u.kb._else->u.xb.header = (CELL)t; \
            c->next = (CELL)t; \
            if (condty[i] == 0) { \
              c = t->u.kb._if; \
            } \
            else { \
              c = t->u.kb._else; \
            } \
	    (*curtrace)->n += 2; \
          } \
          else { \
            if (condty[i] == 0) { \
              c = c->u.kb._if; \
            } else { \
              c = c->u.kb._else; \
            } \
          } \
          i++; \
        } \
        curblock = c; \
      }

#define IF(EXP) EMIT_THIS_BLOCK_AS_IF(EXP);
#define ELSEIF(EXP) EMIT_THIS_BLOCK_AS_ELSEIF(EXP);
#define ELSE() EMIT_THIS_BLOCK_AS_ELSE();
#define ENDBLOCK() \
      BACK_TO_HEADER();
      
#define SIMPLE_VBLOCK_ND0(block) \
        block == GET_X_VAL_GVALX_NONVAR || \
        block == GET_Y_VAL_GVALY_NONVAR || \
        block == PUT_UNSAFE_PUNSAFE_NONVAR || \
        block == UNIFY_X_LOC_WRITE_UNIFY_X_LOC_NONVAR || \
        block == UNIFY_L_X_LOC_WRITE_ULNIFY_X_LOC_NONVAR || \
        block == UNIFY_Y_LOC_WRITE_UNIFY_Y_LOC_NONVAR || \
        block == UNIFY_L_Y_LOC_WRITE_ULUNIFY_Y_LOC_NONVAR || \
        block == WRITE_X_LOC_W_X_BOUND || \
        block == WRITE_Y_LOC_W_Y_BOUND || \
        block == P_DIF_DIF_NVAR1 || \
        block == P_EQ_P_EQ_NVAR1 || \
        block == P_PLUS_VV_PLUS_VV_NVAR || \
        block == P_PLUS_Y_VV_PLUS_Y_VV_NVAR || \
        block == P_MINUS_VV_MINUS_VV_NVAR || \
        block == P_TIMES_VV_TIMES_VV_NVAR || \
        block == P_DIV_VV_DIV_VV_NVAR || \
        block == P_AND_VV_AND_VV_NVAR || \
        block == P_ARG_VV_TEST_D1 || \
        block == P_ARG_VV_ARG_ARG1_NVAR || \
        block == P_ARG_Y_VV_TEST_D1 || \
        block == P_ARG_Y_VV_ARG_Y_ARG1_NVAR
      
#define MULTIPLE_DESTINY_VBLOCK_ND0(block) \
        block == GET_LIST_GLIST_NONVAR || \
        block == GET_ATOM_GATOM_NONVAR || \
        block == GET_STRUCT_GSTRUCT_NONVAR || \
        block == GET_FLOAT_GFLOAT_NONVAR || \
        block == GL_VOID_VARX_GLIST_VOID_VARX_READ || \
        block == GL_VOID_VARY_GLIST_VOID_VARY_READ || \
        block == UNIFY_ATOM_UATOM_NONVAR || \
        block == UNIFY_L_ATOM_ULATOM_NONVAR || \
        block == UNIFY_LIST_READMODE || \
        block == UNIFY_L_LIST_READMODE || \
        block == UNIFY_STRUCT_READMODE || \
        block == UNIFY_L_STRUC_READMODE
        
#define MULTIPLE_DESTINY_VBLOCK_ND1(block) \
        block == P_ARG_CV_ARG_ARG2_VC_NVAR
        
#define MULTIPLE_DESTINY_VBLOCK_ND0_ND1(block) \
        block == GET_X_VAL_GVALX_NONVAR_NONVAR || \
        block == GET_Y_VAL_GVALY_NONVAR_NONVAR || \
        block == CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT || \
        block == CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT || \
        block == CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT || \
        block == CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT || \
        block == P_DIF_DIF_NVAR1_NVAR2 || \
        block == P_EQ_P_EQ_NVAR1_NVAR2 || \
        block == P_ARG_VV_ARG_ARG2_NVAR || \
        block == P_ARG_Y_VV_ARG_Y_ARG2_NVAR
        
#define MULTIPLE_DESTINY_VBLOCK_D0_ND1(block) \
        block == GET_X_VAL_GVALX_VAR_NONVAR || \
        block == GET_Y_VAL_GVALY_VAR_NONVAR || \
        block == P_EQ_P_EQ_VAR1_NVAR2

static inline int
found_entry(char* key, yamop* p) {
  int i = 0;
  while (i < p->u.jhc.jh->tcc.cf->nentries && strcmp(p->u.jhc.jh->tcc.cf->entries[i], key)) {
    i++;
  }
  if (i == p->u.jhc.jh->tcc.cf->nentries) return 0;
  return 1;
}

static inline void
emit_blocks_buf (BlocksContext* mt, short nident, char buf[], yamop* p) {
    BlocksContext* prevblock = NULL;
    char* tmp = (char*)malloc(2048*sizeof(char));
    int i;
    //yamop* lastp = lastop_of(p);
    short foundlast = 0;
    //CELL oldp;
    short first = 1;
    char *lastlabel = (char*)malloc(1024*sizeof(char));
    while (mt) {
      /*if (foundlast) {
        if (oldp != mt->thisp) break;
      }
      if (mt->thisp == (CELL)lastp) {
        foundlast = 1;
        oldp = mt->thisp;
      }*/
      if (mt->blockty == SIMPLE_ENTRY) {
        //if ((YAP_BBs)mt->u.eb.id == IF_NOT_THEN_INSTINIT) { strcpy(buf, ""); break; }
        p->u.jhc.jh->tcc.cf->emit = 1;
        i = 0;
        /*while (i < p->u.jhc.jh->cf->nemited) {
          if (!strcmp(p->u.jhc.jh->cf->emited_blocks[i], mt->u.eb.label_entry)) {
            p->u.jhc.jh->cf->emit = 0;
            break;
          }
          i += 1;
        }*/
        if (p->u.jhc.jh->tcc.cf->emit) {
          {
            p->u.jhc.jh->tcc.cf->leastonce = 1;
            p->u.jhc.jh->tcc.cf->nemited += 1;
            p->u.jhc.jh->tcc.cf->emited_blocks = (char**)realloc(p->u.jhc.jh->tcc.cf->emited_blocks, p->u.jhc.jh->tcc.cf->nemited*sizeof(char*));
            p->u.jhc.jh->tcc.cf->emited_blocks[p->u.jhc.jh->tcc.cf->nemited-1] = (char*)malloc(1024*sizeof(char));
            strcpy(p->u.jhc.jh->tcc.cf->emited_blocks[p->u.jhc.jh->tcc.cf->nemited-1], mt->u.eb.label_entry);
          }
          strcpy(lastlabel, mt->u.eb.label_entry);
          if (first) {
            sprintf(tmp, "%s:\n{\n", mt->u.eb.label_entry);
            first = 0;
          }
          else sprintf(tmp, "}\n\n%s:\n{\n", mt->u.eb.label_entry);
          strcat(buf, tmp);
#if YAP_DBG_PREDS
		    sprintf(tmp, "print_block((YAP_BBs)%ld, ON_NATIVE);\n", (Int)mt->u.eb.id);
            strcat(buf, tmp);
#endif
          sprint_block((YAP_BBs)mt->u.eb.id, &tmp);
          strcat(buf, tmp);
          if (mt->u.eb.label_destiny) {
            if (strcmp(mt->u.eb.label_destiny, "")) {
              if (found_entry(mt->u.eb.label_destiny, p)) {
                sprintf(tmp, "goto %s;\n", mt->u.eb.label_destiny);
              } else {
                sprintf(tmp, "SUCCESSBACK();\n");
              }
              strcat(buf, tmp);
            }
          }
        }
      }
      else if (mt->blockty == SIMPLE) {
        if (p->u.jhc.jh->tcc.cf->emit && (prevblock->u.sb.id != PUT_X_VAL_INSTINIT || mt->u.sb.id != YAAM_DEREF_BODY_D0PT0)) {
	      Int printif = 0;
          if ((YAP_BBs)mt->u.sb.id == YAAM_DEREF_BODY_D0PT0 || (YAP_BBs)mt->u.sb.id == YAAM_DEREF_BODY_D0PT1 ||
              (YAP_BBs)mt->u.sb.id == YAAM_DEREF_BODY_D0S_SREG || (YAP_BBs)mt->u.sb.id == YAAM_DEREF_BODY_D1PT0 ||
              (YAP_BBs)mt->u.sb.id == YAAM_DEREF_BODY_D1PT1) {
            sprintf(tmp, "/* address = 0x%lX */ ", mt->thisp);
            strcat(buf, tmp);
          }
		  if ((YAP_BBs)mt->u.sb.id == CUT_COROUTINING || (YAP_BBs)mt->u.sb.id == CUT_T_COROUTINING || (YAP_BBs)mt->u.sb.id == CUT_E_COROUTINING) {
		    sprintf(tmp, "#pragma clang diagnostic push\n#pragma clang diagnostic ignored \"-Wunused-value\"\n");
			strcat(buf, tmp);
		  }
#if YAP_DBG_PREDS
		    sprintf(tmp, "print_block((YAP_BBs)%ld, ON_NATIVE);\n", (Int)mt->u.sb.id);
            strcat(buf, tmp);
#endif
          sprint_block((YAP_BBs)mt->u.sb.id, &tmp);
          strcat(buf, tmp);
		  if ((YAP_BBs)mt->u.sb.id == CUT_COROUTINING || (YAP_BBs)mt->u.sb.id == CUT_T_COROUTINING || (YAP_BBs)mt->u.sb.id == CUT_E_COROUTINING) {
		    sprintf(tmp, "#pragma clang diagnostic pop\n");
			strcat(buf, tmp);
		  }
          if(mt->u.sb.label_destiny) {
            if (strcmp(mt->u.sb.label_destiny, "")) {
              if (foundlast) {
			    sprintf(tmp, "SUCCESSBACK();\n");
			    strcat(buf, tmp);
			  }
              else {
                if (found_entry(mt->u.sb.label_destiny, p)) {
				  if (				  
					  (YAP_BBs)mt->u.sb.id != NoStackExecute_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackDExecute_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackCall_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackDeallocate_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackCut_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackCutT_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackCutE_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackCommitX_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackCommitY_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackEither_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackPExecute_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackPExecute2_Exception &&
					  (YAP_BBs)mt->u.sb.id != NoStackPTExecute_Exception)
				  {
                    sprintf(tmp, "goto %s;\n", mt->u.sb.label_destiny);
                    strcat(buf, tmp);
                  }
                } else {
                  sprintf(tmp, "SUCCESSBACK();\n");
                  strcat(buf, tmp);
                }
              }
            }
          }
          if (printif) {
			sprintf(tmp, "}\nelse SUCCESSBACK();\n");
			strcat(buf, tmp);	
	      }
          //if (mt->u.sb.id == DEXECUTE_END_END) break;
        }
      }
      else if (mt->blockty == CONDITIONAL_HEADER) {
        if (p->u.jhc.jh->tcc.cf->emit) {
          if (mt->u.kb._if->next && !(mt->u.kb._else->next))
            emit_blocks_buf(mt->u.kb._if, nident+1, buf, p);
          else if (!(mt->u.kb._if->next) && mt->u.kb._else->next)
            emit_blocks_buf(mt->u.kb._else, nident+1, buf, p);
          else if (mt->u.kb._if->next && mt->u.kb._else->next) {
            sprintf(tmp, "if (%s) {\n", mt->u.kb.exp);
            strcat(buf, tmp);
            emit_blocks_buf(mt->u.kb._if, nident+1, buf, p);
            if (p->u.jhc.jh->tcc.cf->printlabel) {
              sprintf(tmp, "}\nelse {\n%s:;\n", p->u.jhc.jh->tcc.cf->clabel);
              p->u.jhc.jh->tcc.cf->printlabel = 0;
            }
            else {
              sprintf(tmp, "}\nelse {\n");
            }
            strcat(buf, tmp);
            emit_blocks_buf(mt->u.kb._else, nident+1, buf, p);
            sprintf(tmp, "}\n");
            strcat(buf, tmp);
          }
        }
      }
      else if (mt->blockty == MULTIPLE_DESTINY) {
		Int printif = 0;
        if (p->u.jhc.jh->tcc.cf->emit) {
#if YAP_DBG_PREDS
		    sprintf(tmp, "print_block((YAP_BBs)%ld, ON_NATIVE);\n", (Int)mt->u.mdb.id);
            strcat(buf, tmp);
#endif
          sprint_block((YAP_BBs)mt->u.mdb.id, &tmp);
          strcat(buf, tmp);
          if(mt->u.mdb.nfaillabels > 0) {
			sprintf(tmp, "if (FAILED) {\n");
			strcat(buf, tmp);
			for (i = 0; i < mt->u.mdb.nfaillabels; i++) {
			  sprintf(tmp, "  if ((UInt)(*_PREG) == 0x%lX) goto %s;\n",
                      mt->u.mdb.faildestiny.p[i], mt->u.mdb.faildestiny.labels[i]);
              strcat(buf, tmp);
			}
			sprintf(tmp, "}\n");
			strcat(buf, tmp);
		  }
          if(mt->u.mdb.ndest > 0) {
            /*if ((YAP_BBs)mt->u.mdb.id == DEXECUTE_END_END && (CELL)lastp == mt->thisp) {
              sprintf(tmp, "SUCCESSBACK();\n");
              strcat(buf, tmp);
              break;
            }
            else {*/
              //if (found_entry(mt->u.mdb.destiny.labels[0]), p) {
                sprintf(tmp, "if ((UInt)(*_PREG) == 0x%lX) goto %s;\n", mt->u.mdb.destiny.p[0], mt->u.mdb.destiny.labels[0]);
              //} else {
                //sprintf(tmp, "if ((UInt)(*_PREG) == 0x%lX) {\n  SUCCESSBACK();\n}\n", mt->u.mdb.destiny.p[0]);
              //}
              strcat(buf, tmp);
              //if ((YAP_BBs)mt->u.mdb.id == PROCCEED_END || (YAP_BBs)mt->u.mdb.id == DEXECUTE_END_END) {
                int i;
                for (i = 1; i < mt->u.mdb.ndest; i++) {
                  if (found_entry(mt->u.mdb.destiny.labels[i], p)) {
					//if (strcmp(lastlabel, mt->u.mdb.destiny.labels[i])) {
                      sprintf(tmp, "if ((UInt)(*_PREG) == 0x%lX) goto %s;\n",
                              mt->u.mdb.destiny.p[i], mt->u.mdb.destiny.labels[i]);
                    //}
                  } else {
                    sprintf(tmp, "if ((UInt)(*_PREG) == 0x%lX) SUCCESSBACK();\n", mt->u.mdb.destiny.p[i]);
                  }
                  strcat(buf, tmp);
                }
              //}
			  //int k = 0;
			  //while (k < mt->u.mdb.ndest && Yap_op_from_opcode(((yamop*)(mt->u.mdb.destiny.p[k]))->opc) != _jit_handler) { k++; }
			  //if (k == mt->u.mdb.ndest) {
			    sprintf(tmp, "SUCCESSBACK();\n");
			  //}
			  //else {
                            //sprintf(tmp, "else {\n  SUCCESS();\n}\n");
                          //}
              strcat(buf, tmp);
            //}
          }
          else {
            sprintf(tmp, "SUCCESSBACK();\n\n");
            strcat(buf, tmp);
          }
          if (printif) {
			sprintf(tmp, "}\nelse SUCCESSBACK();\n");
			strcat(buf, tmp);	
	      }
        }
      }
      prevblock = mt;
      mt = (BlocksContext*)mt->next;
    }
	free(lastlabel);
	free(tmp);
}

static inline void
fill_entries(BlocksContext* mt, yamop** p) {
  while (mt) {
    if (mt->blockty == SIMPLE_ENTRY) {
      (*p)->u.jhc.jh->tcc.cf->nentries += 1;
      (*p)->u.jhc.jh->tcc.cf->entries = (char**)realloc((*p)->u.jhc.jh->tcc.cf->entries, (*p)->u.jhc.jh->tcc.cf->nentries*sizeof(char*));
      (*p)->u.jhc.jh->tcc.cf->entries[(*p)->u.jhc.jh->tcc.cf->nentries-1] = (char*)malloc(1024*sizeof(char));
      strcpy((*p)->u.jhc.jh->tcc.cf->entries[(*p)->u.jhc.jh->tcc.cf->nentries-1], mt->u.eb.label_entry);
    }
    mt = (BlocksContext*)mt->next;
  }
}

#if YAP_STAT_PREDS
#if YAP_DBG_PREDS
const char* cfile_header_for_trace = "#include <absmi.h>\n#include <arith2.h>\n#include <yaam_macros.hpp>\n#include <print_op.hpp>\n#include <native_header_d.h>\n\nextern CELL nnexec;\nextern int IUnify_complex(CELL*, CELL*, CELL*);\nextern int iequ_complex(CELL*, CELL*, CELL*);\nextern void print_preg(yamop*);\nextern NativeContext* NativeArea;\nextern yamop* HREADPREG;\nextern CELL BLOCK;\nextern CELL BLOCKADDRESS;\nextern CELL FAILED;\n\nvoid* clause(yamop**, yamop**, CELL**, void*[], void*[]);\n\nvoid* clause(yamop** _PREG, yamop** _CPREG, CELL** _SREG, void* external_labels[], void* OpAddress[]) {\nNativeArea->runs[(*_PREG)->u.jhc.jh->caa.naddress] += 1;\n\n\0";
#else /* YAP_DBG_PREDS */
const char* cfile_header_for_trace = "#include <absmi.h>\n#include <arith2.h>\n#include <yaam_macros.hpp>\n#include <print_op.hpp>\n#include <native_header.h>\n\nextern CELL nnexec;\nextern int IUnify_complex(CELL*, CELL*, CELL*);\nextern int iequ_complex(CELL*, CELL*, CELL*);\nextern void print_preg(yamop*);\nextern NativeContext* NativeArea;\nextern yamop* HREADPREG;\nextern CELL BLOCK;\nextern CELL BLOCKADDRESS;\nextern CELL FAILED;\n\nvoid* clause(yamop**, yamop**, CELL**, void*[], void*[]);\n\nvoid* clause(yamop** _PREG, yamop** _CPREG, CELL** _SREG, void* external_labels[], void* OpAddress[]) {\nNativeArea->runs[(*_PREG)->u.jhc.jh->caa.naddress] += 1;\n\n\0";
#endif /* YAP_DBG_PREDS */
#else /* YAP_STAT_PREDS */
#if YAP_DBG_PREDS
const char* cfile_header_for_trace = "#include <absmi.h>\n#include <arith2.h>\n#include <yaam_macros.hpp>\n#include <print_op.hpp>\n#include <native_header_d.h>\n\nextern CELL nnexec;\nextern int IUnify_complex(CELL*, CELL*, CELL*);\nextern int iequ_complex(CELL*, CELL*, CELL*);\nextern void print_preg(yamop*);\nextern NativeContext* NativeArea;\nextern yamop* HREADPREG;\nextern CELL BLOCK;\nextern CELL BLOCKADDRESS;\nextern CELL FAILED;\n\nvoid* clause(yamop**, yamop**, CELL**, void*[], void*[]);\n\nvoid* clause(yamop** _PREG, yamop** _CPREG, CELL** _SREG, void* external_labels[], void* OpAddress[]) {\n\n\0";
#else /* YAP_DBG_PREDS */
const char* cfile_header_for_trace = "#include <absmi.h>\n#include <arith2.h>\n#include <yaam_macros.hpp>\n#include <print_op.hpp>\n#include <native_header.h>\n\nextern CELL nnexec;\nextern int IUnify_complex(CELL*, CELL*, CELL*);\nextern int iequ_complex(CELL*, CELL*, CELL*);\nextern void print_preg(yamop*);\nextern NativeContext* NativeArea;\nextern yamop* HREADPREG;\nextern CELL BLOCK;\nextern CELL BLOCKADDRESS;\nextern CELL FAILED;\n\nvoid* clause(yamop**, yamop**, CELL**, void*[], void*[]);\n\nvoid* clause(yamop** _PREG, yamop** _CPREG, CELL** _SREG, void* external_labels[], void* OpAddress[]) {\n\n\0";
#endif /* YAP_DBG_PREDS */
#endif /* YAP_STAT_PREDS */
const char* cfile_end_for_trace = " | clang -O0 -DCUT_C=1  -DCOROUTINING=1 -DRATIONAL_TREES=1 -DDEBUG=1 -DDEPTH_LIMIT=1 -DTABLING=1 -DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1 -D_NATIVE=1 -I. -I./H -I./include  -I./os -I./OPTYap -I./BEAM -I./MYDDAS -I./HPP -xc -c - -o - -emit-llvm\0";
/*const char* cfile_end = " | clang -O0 -DCUT_C=1  -DCOROUTINING=1 -DRATIONAL_TREES=1 -DDEBUG=1 -DDEPTH_LIMIT=1 -DTABLING=1 -DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1 -D_NATIVE=1 -I. -I./H -I./include  -I./os -I./OPTYap -I./BEAM -I./MYDDAS -I./HPP -emit-llvm -xc -c - -o\0";*/

static inline Int
emit_intermediate_for_trace (TraceContext *tt, CELL *tsize, char **cmd, yamop* p) {
  int i;
  if (tt) {
    p->u.jhc.jh->tcc.cf->entries = NULL;
    p->u.jhc.jh->tcc.cf->nentries = 0;
    fill_entries(tt->bc, &p);
    p->u.jhc.jh->tcc.cf->clabel = (char*)malloc(1024*sizeof(char));
    p->u.jhc.jh->tcc.cf->labelidx = 0;
    p->u.jhc.jh->tcc.cf->printlabel = 0;
    p->u.jhc.jh->tcc.cf->emited_blocks = NULL;
    p->u.jhc.jh->tcc.cf->nemited = 0;
    char *buf = (char*)malloc(0x100000*sizeof(char));
    strcpy(buf, "");
    emit_blocks_buf(tt->bc, 0, buf, p);
	{
	  long int tracesize = strlen(cfile_header_for_trace) + strlen(buf);
	  if (tracesize > 0x1FBD0 || *tsize == tracesize || ((long int)(*tsize * 0.002 + *tsize)) > tracesize) {
		for (i = 0; i < p->u.jhc.jh->tcc.cf->nemited; i++)
	      free(p->u.jhc.jh->tcc.cf->emited_blocks[i]);
	    free(p->u.jhc.jh->tcc.cf->emited_blocks);
		free(buf);
	    free(p->u.jhc.jh->tcc.cf->clabel);
		int i;
		for (i = 0; i < p->u.jhc.jh->tcc.cf->nentries; i++)
		  free(p->u.jhc.jh->tcc.cf->entries[i]);
        free(p->u.jhc.jh->tcc.cf->entries);
		return 0;
	  }
	  *tsize = tracesize;
	}
    *cmd = (char*)malloc(0x100000*sizeof(char));
    if (strcmp(buf, "")) {
      strcpy(*cmd, cfile_header_for_trace);
      strcat(*cmd, buf);
      strcat(*cmd, "}\n\n}");
    } else { strcpy(*cmd, ""); }
    for (i = 0; i < p->u.jhc.jh->tcc.cf->nemited; i++)
	  free(p->u.jhc.jh->tcc.cf->emited_blocks[i]);
	free(p->u.jhc.jh->tcc.cf->emited_blocks);
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_std) {
	  if (strcmp((char*)ExpEnv.debug_struc.pprint_intermediate.std_name, "STDOUT") == 0)
        fprintf(stdout, "%s%s}\n\n}", cfile_header_for_trace, buf);
	  else if (strcmp((char*)ExpEnv.debug_struc.pprint_intermediate.std_name, "STDERR") == 0)
        fprintf(stderr, "%s%s}\n\n}", cfile_header_for_trace, buf);
    }
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_file) {
      char filename[1024];
      sprintf(filename, "%s%ld.c", (char*)ExpEnv.debug_struc.pprint_intermediate.file_name, p->u.jhc.jh->caa.naddress);
      FILE *out = fopen(filename, "w");
      fprintf(out, "%s%s}\n\n}", cfile_header_for_trace, buf);
      fclose(out);
    }
#endif
/*
      This block print the trace.
 
      char filename[1024];
      sprintf(filename, "trace%ld.c", global);
      FILE *out = fopen(filename, "w");
      fprintf(out, "%s%s}\n\n}", cfile_header_for_trace, buf);
      fclose(out);
*/
	free(buf);
	free(p->u.jhc.jh->tcc.cf->clabel);
	int i;
	for (i = 0; i < p->u.jhc.jh->tcc.cf->nentries; i++)
	  free(p->u.jhc.jh->tcc.cf->entries[i]);
	free(p->u.jhc.jh->tcc.cf->entries);
	return strlen(*cmd);
  }
  return 0;
}

void emit_intermediate_for_trace_dbg (TraceContext*, yamop*);

void
emit_intermediate_for_trace_dbg (TraceContext *tt, yamop* p) {
  char *buf;
  CELL x;
  emit_intermediate_for_trace(tt, &x, &buf, p);
  fprintf(stdout, "%s%s}\n\n}", cfile_header_for_trace, buf);
}

#include "nextof.hpp"

const char* cfile_header_for_clause = "#include <absmi.h>\n#include <arith2.h>\n#include <setjmp.h>\n#include <signal.h>\n#include <yaam_macros.hpp>\n#include <print_op.hpp>\n#include <singlecode_basics.h>\n#include <singlecode_primitive_predicates.h>\n#include <singlecode_call.h>\n#include <yaam_call_count_mod.h>\n#include <singlecode_cpred.h>\n#include <singlecode_cut.h>\n#include <yaam_failure_mod.h>\n#include <singlecode_get.h>\n#include <indexing_ext.h>\n#include <indexing_std.h>\n#include <singlecode_misc.h>\n#include <singlecode_pop.h>\n#include <singlecode_put.h>\n#include <singlecode_unify.h>\n#include <singlecode_write.h>\n\nextern CELL nnexec;\nextern int IUnify_complex(CELL*, CELL*, CELL*);\nextern int iequ_complex(CELL*, CELL*, CELL*);\nextern void print_preg(yamop*);\nextern NativeContext* NativeArea;\nextern yamop* HREADPREG;\nextern CELL BLOCK;\nextern CELL BLOCKADDRESS;\nextern CELL FAILED;\n\nvoid* clause(yamop**, yamop**, CELL**, void*[], void*[]);\n\nvoid* clause(yamop** _PREG, yamop** _CPREG, CELL** _SREG, void* external_labels[], void* OpAddress[]) {\n\0";
const char* cfile_end_for_clause = " | clang -O0 -DCUT_C=1  -DCOROUTINING=1 -DRATIONAL_TREES=1 -DDEBUG=1 -DDEPTH_LIMIT=1 -DTABLING=1 -DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1 -D_NATIVE=1 -I. -I./H -I./include  -I./os -I./OPTYap -I./BEAM -I./MYDDAS -I./HPP -xc -c - -o - -emit-llvm\0";
/*const char* cfile_end = " | clang -O0 -DCUT_C=1  -DCOROUTINING=1 -DRATIONAL_TREES=1 -DDEBUG=1 -DDEPTH_LIMIT=1 -DTABLING=1 -DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1 -D_NATIVE=1 -I. -I./H -I./include  -I./os -I./OPTYap -I./BEAM -I./MYDDAS -I./HPP -emit-llvm -xc -c - -o\0";*/

#define ENTRY_HAS_WRITED_CPART(entry) \
        entry == _unify_x_var || \
        entry == _unify_l_x_var || \
        entry == _unify_x_var2 || \
        entry == _unify_l_x_var2 || \
        entry == _unify_y_var || \
        entry == _unify_l_y_var || \
        entry == _unify_x_val || \
        entry == _unify_l_x_val || \
        entry == _unify_x_loc || \
        entry == _unify_l_x_loc || \
        entry == _unify_atom || \
        entry == _unify_l_atom || \
        entry == _unify_void || \
        entry == _unify_n_voids || \
        entry == _unify_l_n_voids || \
        entry == _unify_l_list || \
        entry == _unify_struct || \
        entry == _unify_l_struc || \
        entry == _unify_l_float || \
        entry == _save_pair_x

#define SET_ADDRESS_R() \
    strcat(buf, "static void *NOp_Address_R[] = { "); \
    for (i = 0; i < nentries-1; i++) { \
	  strcat(buf, "&&"); \
	  sprint_op(tmpbuf, "lbl", entries[i], ""); \
	  strcat(buf, tmpbuf); \
	  strcat(buf, ", "); \
    } \
    strcat(buf, "&&"); \
    sprint_op(tmpbuf, "lbl", entries[nentries-1], ""); \
    strcat(buf, tmpbuf); \
    strcat(buf, " };\n\n");
    
#define SET_ADDRESS_W() \
    strcat(buf, "static void *NOp_Address_W[] = { "); \
    for (i = 0; i < nentries-1; i++) { \
	  strcat(buf, "&&"); \
	  sprint_op(tmpbuf, "lbl", entries[i], "_write"); \
	  strcat(buf, tmpbuf); \
	  strcat(buf, ", "); \
    } \
    strcat(buf, "&&"); \
    sprint_op(tmpbuf, "lbl", entries[nentries-1], "_write"); \
    strcat(buf, tmpbuf); \
    strcat(buf, " };\n\n");

static inline Int
fe(op_numbers k, op_numbers* e, COUNT n) {
	int i = 0;
	while (i < n && e[i] != k) i++;
	if (i == n) return -1;
	return i;
}

static inline Int
emit_intermediate_for_clause (yamop *p, char **cmd) {
  yamop *tmp = p;
  yamop *next;
  op_numbers* entries = NULL;
  COUNT i, nentries = 0;
  while (1) {
	op_numbers op = Yap_op_from_opcode(p->opc);
	if (fe(op, entries, nentries) == -1) {
	  nentries += 1;
	  entries = (op_numbers*)realloc(entries, nentries*sizeof(op_numbers));
	  entries[nentries-1] = op;
	}
	if (op == _execute || op == _dexecute || op == _procceed || op == _fcall || op == _call || op == _execute_cpred) break;
	if (op == _either) {
      NextOf(&p);
	}
	else if (op == _cut || op == _cut_t || op == _cut_e || op == _commit_b_x || op == _commit_b_y || op == _p_functor) {
      NextOf(&p);
      NextOf(&p);
	}
	NextOf(&p);
  }
  p = tmp;
  while (1) {
	next = p;
	if (Yap_op_from_opcode(next->opc) == _either) {
	  NextOf(&next);
	}
	else if (Yap_op_from_opcode(next->opc) == _cut || Yap_op_from_opcode(next->opc) == _cut_t || Yap_op_from_opcode(next->opc) == _cut_e || Yap_op_from_opcode(next->opc) == _commit_b_x || Yap_op_from_opcode(next->opc) == _commit_b_y || Yap_op_from_opcode(next->opc) == _p_functor) {
	  NextOf(&next);
	  NextOf(&next);
	}
	NextOf(&next);
	if (Yap_op_from_opcode(p->opc) == _execute || Yap_op_from_opcode(p->opc) == _dexecute || Yap_op_from_opcode(p->opc) == _procceed || Yap_op_from_opcode(p->opc) == _fcall || Yap_op_from_opcode(p->opc) == _call || Yap_op_from_opcode(p->opc) == _execute_cpred) {
	  p->next_native_r = p->next_native_w = -1;
	  break;
	}
	else {
	  op_numbers op = Yap_op_from_opcode(next->opc);
	  p->next_native_r = p->next_native_w = fe(op, entries, nentries);
	}
	if (Yap_op_from_opcode(p->opc) == _either) {
      NextOf(&p);
	}
	else if (Yap_op_from_opcode(p->opc) == _cut || Yap_op_from_opcode(p->opc) == _cut_t || Yap_op_from_opcode(p->opc) == _cut_e || Yap_op_from_opcode(p->opc) == _commit_b_x || Yap_op_from_opcode(p->opc) == _commit_b_y || Yap_op_from_opcode(p->opc) == _p_functor) {
      NextOf(&p);
      NextOf(&p);
	}
	NextOf(&p);
  }
  /*p = tmp;
  while(1){
	op_numbers op = Yap_op_from_opcode(p->opc);
	print_op("", op, " -- ");
	printf("%ld\n", p->next_native_r);
	if (op == _execute || op == _dexecute || op == _procceed || op == _fcall || op == _call || op == _execute_cpred) break;
	if (op == _either) {
      NextOf(&p);
	}
	else if (op == _cut || op == _cut_t || op == _cut_e || op == _commit_b_x || op == _commit_b_y || op == _p_functor) {
      NextOf(&p);
      NextOf(&p);
	}
	NextOf(&p);
  }*/
          
  char buf[2097152];
  char tmpbuf[1024];
  strcpy(buf, "");
  SET_ADDRESS_R();
  SET_ADDRESS_W();
  strcat(buf, "\n");

  for (i = 0; i < nentries; i++) {
	/* read part */
	sprint_op(tmpbuf, "lbl", entries[i], "");
	strcat(buf, tmpbuf);
	strcat(buf, ":\n{\n");
	sprint_op(tmpbuf, "  CELL idx = (*_PREG)->next_native_r;\n  ", entries[i], "_instinit;\n");
	strcat(buf, tmpbuf);
	//sprintf(tmpbuf, "  if (idx != -1) goto *NOp_Address[idx];\n  else BACK();\n");
	//strcat(buf, tmpbuf);
	strcat(buf, "}\n\n");
	
	/* written part */
	if (ENTRY_HAS_WRITED_CPART(entries[i])) {
      sprint_op(tmpbuf, "lbl", entries[i], "_write");
	  strcat(buf, tmpbuf);
	  strcat(buf, ":\n{\n");
	  /*{
	    strcpy(tmplbl, tmpbuf);
	    sprintf(tmpbuf, "  printf(\\\"%s!! -- \\\");\n", tmplbl);
	    strcat(buf, tmpbuf);
	    sprintf(tmpbuf, "  print_op(\\\"\\\", Yap_op_from_opcode((*_PREG)->opc), \\\"!!\\\n\\\");\n");
        strcat(buf, tmpbuf);
      }*/
	  sprint_op(tmpbuf, "  CELL idx = (*_PREG)->next_native_w;\n  ", entries[i], "_write_instinit;\n");
	  strcat(buf, tmpbuf);
	  //sprintf(tmpbuf, "  if (idx != -1) goto *NOp_Address[idx];\n  else BACK();\n");
	  //strcat(buf, tmpbuf);
	  strcat(buf, "}\n\n");
	}
	else {
      sprint_op(tmpbuf, "lbl", entries[i], "_write:;\n\n");
	  strcat(buf, tmpbuf);
	}
  }
  
  *cmd = (char*)malloc(2097152*sizeof(char));
  //strcpy(*cmd, "echo \"");
  strcpy(*cmd, cfile_header_for_clause);
  strcat(*cmd, buf);
  strcat(*cmd, "BACK();\n}");
  //strcat(*cmd, cfile_end_for_clause);
  
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_std) {
	  if (strcmp((char*)ExpEnv.debug_struc.pprint_intermediate.std_name, "STDOUT") == 0)
        fprintf(stdout, "%s%s}\n\n}", cfile_header_for_clause, buf);
	  else if (strcmp((char*)ExpEnv.debug_struc.pprint_intermediate.std_name, "STDERR") == 0)
        fprintf(stderr, "%s%s}\n\n}", cfile_header_for_clause, buf);
    }
    if (ExpEnv.debug_struc.pprint_intermediate.print_to_file) {
      char filename[1024];
      sprintf(filename, "%s%ld.c", (char*)ExpEnv.debug_struc.pprint_intermediate.file_name, p->u.jhc.jh->caa.naddress);
      FILE *out = fopen(filename, "w");
	  if (out) {
        fprintf(out, "%s%s}\n\n}", cfile_header_for_trace, buf);
        fclose(out);
		free(*cmd);
        return 1;
	  }
	  free(*cmd);
      return 0;
    }
#endif
}

static inline void*
recompile(void *pt)
{
  yamop* p = (yamop*)pt;

#if YAP_STAT_PREDS
  struct rusage rustart, ruend;
  getrusage(RUSAGE_SELF, &rustart);
#endif
  Int v = emit_intermediate_for_trace(IntermediatecodeArea->area.t[p->u.jhc.jh->caa.taddress], &(IntermediatecodeArea->area.t[p->u.jhc.jh->caa.taddress]->tracesize), &(p->u.jhc.jh->tcc.cmd), p);
  if (v) {
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.pprint_me.at_recompilation != 0 && ExpEnv.debug_struc.pprint_me.at_recompilation != 0x1) {
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);
      fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.at_recompilation);
    }
#endif
#if YAP_STAT_PREDS
    NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress] += 1;
    NativeArea->area.native_size_bytes[p->u.jhc.jh->caa.naddress] = (CELL*)realloc(NativeArea->area.native_size_bytes[p->u.jhc.jh->caa.naddress], NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]*sizeof(CELL));
    NativeArea->area.trace_size_bytes[p->u.jhc.jh->caa.naddress] = (CELL*)realloc(NativeArea->area.trace_size_bytes[p->u.jhc.jh->caa.naddress], NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]*sizeof(CELL));
    NativeArea->area.trace_size_bytes[p->u.jhc.jh->caa.naddress][NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]-1] = v;
#endif
    NativeArea->area.p[p->u.jhc.jh->caa.naddress] = call_JIT_Compiler(J, p);
    IntermediatecodeArea->area.isactive[p->u.jhc.jh->caa.taddress] = 0;
#if YAP_STAT_PREDS
    getrusage(RUSAGE_SELF, &ruend);
    NativeArea->area.compilation_time[p->u.jhc.jh->caa.naddress] = (double*)realloc(NativeArea->area.compilation_time[p->u.jhc.jh->caa.naddress], NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]*sizeof(double));
    NativeArea->area.compilation_time[p->u.jhc.jh->caa.naddress][NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]-1] =
    (((double)ruend.ru_utime.tv_sec - (double)rustart.ru_utime.tv_sec) + ((double)ruend.ru_utime.tv_usec - (double)rustart.ru_utime.tv_usec) / 1000000.0);
#endif
  }
  return NULL;
}

static inline void*
compile(void *pt)
{
  yamop* p = (yamop*)pt;
  
#if YAP_STAT_PREDS
  struct rusage rustart, ruend;
  getrusage(RUSAGE_SELF, &rustart);
#endif
  p->u.jhc.jh->caa.naddress = NativeArea->n;
  Int v = emit_intermediate_for_trace(IntermediatecodeArea->area.t[p->u.jhc.jh->caa.taddress], &(IntermediatecodeArea->area.t[p->u.jhc.jh->caa.taddress]->tracesize), &(p->u.jhc.jh->tcc.cmd), p);
  if (v) {
#if YAP_DBG_PREDS
    if (ExpEnv.debug_struc.pprint_me.at_compilation != 0 && ExpEnv.debug_struc.pprint_me.at_compilation != 0x1) {
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);
      fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pprint_me.at_compilation);
    }
#endif
    NativeArea->area.p = (void**)realloc(NativeArea->area.p, (NativeArea->n+1)*sizeof(void*));
    NativeArea->area.ok = (COUNT*)realloc(NativeArea->area.ok, (NativeArea->n+1)*sizeof(COUNT));
    NativeArea->area.pc = (CELL*)realloc(NativeArea->area.pc, (NativeArea->n+1)*sizeof(CELL));
#if YAP_STAT_PREDS
    NativeArea->area.nrecomp = (COUNT*)realloc(NativeArea->area.nrecomp, (NativeArea->n+1)*sizeof(COUNT));
    NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress] = 1;
    NativeArea->area.compilation_time = (double**)realloc(NativeArea->area.compilation_time, (NativeArea->n+1)*sizeof(double*));
    NativeArea->area.native_size_bytes = (CELL**)realloc(NativeArea->area.native_size_bytes, (NativeArea->n+1)*sizeof(CELL*));
    NativeArea->area.native_size_bytes[p->u.jhc.jh->caa.naddress] = (CELL*)malloc(NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]*sizeof(CELL));
    NativeArea->area.trace_size_bytes = (CELL**)realloc(NativeArea->area.trace_size_bytes, (NativeArea->n+1)*sizeof(CELL*));
    NativeArea->area.trace_size_bytes[p->u.jhc.jh->caa.naddress] = (CELL*)malloc(NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]*sizeof(CELL));
    NativeArea->area.trace_size_bytes[p->u.jhc.jh->caa.naddress][NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]-1] = v;
    NativeArea->runs = (COUNT*)realloc(NativeArea->runs, (NativeArea->n+1)*sizeof(COUNT));
    NativeArea->t_runs = (double*)realloc(NativeArea->t_runs, (NativeArea->n+1)*sizeof(double));
    NativeArea->success = (COUNT*)realloc(NativeArea->success, (NativeArea->n+1)*sizeof(COUNT));
#endif
    NativeArea->area.ok[p->u.jhc.jh->caa.naddress] = 0;
#if YAP_STAT_PREDS
    NativeArea->success[p->u.jhc.jh->caa.naddress] = 0;
    NativeArea->runs[p->u.jhc.jh->caa.naddress] = 0;
    NativeArea->t_runs[p->u.jhc.jh->caa.naddress] = 0.0;
#endif
    NativeArea->n += 1;
    NativeArea->area.p[p->u.jhc.jh->caa.naddress] = call_JIT_Compiler(J, p);
    NativeArea->area.ok[p->u.jhc.jh->caa.naddress] = 1;
    NativeArea->area.pc[p->u.jhc.jh->caa.naddress] = (CELL)p;
#if YAP_STAT_PREDS
    getrusage(RUSAGE_SELF, &ruend);
    NativeArea->area.compilation_time[p->u.jhc.jh->caa.naddress] = (double*)malloc(NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]*sizeof(double));
    NativeArea->area.compilation_time[p->u.jhc.jh->caa.naddress][NativeArea->area.nrecomp[p->u.jhc.jh->caa.naddress]-1] =
    (((double)ruend.ru_utime.tv_sec - (double)rustart.ru_utime.tv_sec) + ((double)ruend.ru_utime.tv_usec - (double)rustart.ru_utime.tv_usec) / 1000000.0);
#endif
  }
  else {
    p->u.jhc.jh->caa.naddress = -1;
  }
  IntermediatecodeArea->area.isactive[p->u.jhc.jh->caa.taddress] = 0;
  return NULL;
}

#endif /*_NATIVE */
