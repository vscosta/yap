/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

/************************************************************************
**                          Memory management                          **
************************************************************************/

extern int Yap_page_size;

#ifdef USE_PAGES_MALLOC
#include <sys/shm.h>
#endif /* USE_PAGES_MALLOC */

#define SHMMAX 0x2000000  /* 32 Mbytes: works fine with linux */
/* #define SHMMAX  0x400000 - 4 Mbytes: shmget limit for Mac (?) */
/* #define SHMMAX  0x800000 - 8 Mbytes: shmget limit for Solaris (?) */

#if SIZEOF_INT_P == 4
#define ALIGN	                   3
#define ALIGNMASK                  0xfffffffc
#elif SIZEOF_INT_P == 8
#define ALIGN	                   7
#define ALIGNMASK                  0xfffffff8
#else
#define ALIGN	                   OOOOPPS!!! Unknown Pointer Sizeof
#define ALIGNMASK                  OOOOPPS!!! Unknown Pointer Sizeof
#endif /* SIZEOF_INT_P */

#define ADJUST_SIZE(SIZE)          ((SIZE + ALIGN) & ALIGNMASK)
#define ADJUST_SIZE_TO_PAGE(SIZE)  ((SIZE) - (SIZE) % Yap_page_size + Yap_page_size)
#define STRUCT_SIZE(STR_TYPE)      ADJUST_SIZE(sizeof(STR_TYPE))
#define PAGE_HEADER(STR)           (pg_hd_ptr)((unsigned long int)STR - (unsigned long int)STR % Yap_page_size)
#define STRUCT_NEXT(STR)           ((STR)->next)

#define UPDATE_STATS(STAT, VALUE)  STAT += VALUE


#ifdef USE_SYSTEM_MALLOC
/**************************************************************************************
**                                  USE_SYSTEM_MALLOC                                **
**************************************************************************************/
#define ALLOC_BLOCK(STR, SIZE, STR_TYPE)                                              \
        if ((STR = (STR_TYPE *) malloc(SIZE)) == NULL)                                \
          Yap_Error(FATAL_ERROR, TermNil, "ALLOC_BLOCK: malloc error")
#define FREE_BLOCK(STR)                                                               \
        free(STR)
#else
/**************************************************************************************
**                                ! USE_SYSTEM_MALLOC                                **
**************************************************************************************/
#define ALLOC_BLOCK(STR, SIZE, STR_TYPE)                                              \
        { char *block_ptr;                                                            \
          if ((block_ptr = Yap_AllocCodeSpace(SIZE + sizeof(CELL))) != NULL)          \
            *block_ptr = 'y';                                                         \
          else if ((block_ptr = (char *) malloc(SIZE + sizeof(CELL))) != NULL)        \
            *block_ptr = 'm';                                                         \
          else                                                                        \
            Yap_Error(FATAL_ERROR, TermNil, "ALLOC_BLOCK: malloc error");             \
          block_ptr += sizeof(CELL);                                                  \
          STR = (STR_TYPE *) block_ptr;                                               \
        }
#define FREE_BLOCK(STR)                                                               \
        { char *block_ptr = (char *)(STR) - sizeof(CELL);                             \
          if (block_ptr[0] == 'y')                                                    \
            Yap_FreeCodeSpace(block_ptr);                                             \
          else                                                                        \
            free(block_ptr);                                                          \
        }
#endif /******************************************************************************/
#define ALLOC_HASH_BUCKETS(BUCKET_PTR, NUM_BUCKETS)                                   \
        { int i; void **bucket_ptr;                                                   \
          ALLOC_BLOCK(bucket_ptr, NUM_BUCKETS * sizeof(void *), void *);              \
          BUCKET_PTR = (void *) bucket_ptr;                                           \
          for (i = NUM_BUCKETS; i != 0; i--)                                          \
            *bucket_ptr++ = NULL;                                                     \
        }
#define FREE_HASH_BUCKETS(BUCKET_PTR)  FREE_BLOCK(BUCKET_PTR)



#ifndef USE_PAGES_MALLOC
/**************************************************************************************
**                                ! USE_PAGES_MALLOC                                 **
**************************************************************************************/
#define ALLOC_STRUCT(STR, STR_TYPE, STR_PAGES, VOID_PAGES)                            \
        LOCK(Pg_lock(STR_PAGES));                                                     \
        UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                                    \
        UNLOCK(Pg_lock(STR_PAGES));                                                   \
        ALLOC_BLOCK(STR, sizeof(STR_TYPE), STR_TYPE)
#define LOCAL_NEXT_ALLOC_STRUCT(STR, LOCAL_STR, STR_TYPE, STR_PAGES, VOID_PAGES)      \
        ALLOC_STRUCT(STR, STR_TYPE, STR_PAGES, VOID_PAGES)
#define FREE_STRUCT(STR, STR_TYPE, STR_PAGES, VOID_PAGES)                             \
        LOCK(Pg_lock(STR_PAGES));                                                     \
        UPDATE_STATS(Pg_str_in_use(STR_PAGES), -1);                                   \
        UNLOCK(Pg_lock(STR_PAGES));                                                   \
        FREE_BLOCK(STR)
#else
/**************************************************************************************
**                        USE_PAGES_MALLOC && ! LIMIT_TABLING                        **
**************************************************************************************/
#ifndef LIMIT_TABLING
#define ALLOC_STRUCT_TEST_PAGE  if
#define ALLOC_STRUCT_RECOVER_SPACE(PG_HD, STR_PAGES, VOID_PAGES)
#else
/**************************************************************************************
**                         USE_PAGES_MALLOC && LIMIT_TABLING                         **
**************************************************************************************/
#define ALLOC_STRUCT_TEST_PAGE  while
#define ALLOC_STRUCT_RECOVER_SPACE(PG_HD, STR_PAGES, VOID_PAGES)                      \
        if (Pg_free_pg(VOID_PAGES) == NULL &&                                         \
            GLOBAL_max_pages == Pg_pg_alloc(VOID_PAGES)) {                            \
          sg_fr_ptr sg_fr = GLOBAL_check_sg_fr;                                       \
          UNLOCK(Pg_lock(VOID_PAGES));                                                \
          do {                                                                        \
            if (sg_fr)                                                                \
              sg_fr = SgFr_next(sg_fr);                                               \
            else                                                                      \
              sg_fr = GLOBAL_first_sg_fr;                                             \
            if (sg_fr == NULL)                                                        \
              Yap_Error(FATAL_ERROR, TermNil, "no space left (RECOVER_SPACE)");       \
              /* see function 'InteractSIGINT' in file 'sysbits.c' */                 \
              /*   Yap_Error(PURE_ABORT, TermNil, "");             */                 \
              /*   restore_absmi_regs(&Yap_standard_regs);         */                 \
              /*   siglongjmp (LOCAL_RestartEnv, 1);               */                 \
            if (SgFr_first_answer(sg_fr) &&                                           \
                SgFr_first_answer(sg_fr) != SgFr_answer_trie(sg_fr)) {                \
              SgFr_state(sg_fr) = ready;                                              \
	      free_answer_hash_chain(SgFr_hash_chain(sg_fr));                         \
	      SgFr_hash_chain(sg_fr) = NULL;                                          \
	      SgFr_first_answer(sg_fr) = NULL;                                        \
              SgFr_last_answer(sg_fr) = NULL;                                         \
              free_answer_trie(TrNode_child(SgFr_answer_trie(sg_fr)),                 \
                               TRAVERSE_MODE_NORMAL, TRAVERSE_POSITION_FIRST);        \
              TrNode_child(SgFr_answer_trie(sg_fr)) = NULL;                           \
	    }                                                                         \
          } while (Pg_free_pg(VOID_PAGES) == Pg_free_pg(STR_PAGES));                  \
          GLOBAL_check_sg_fr = sg_fr;                                                 \
          LOCK(Pg_lock(STR_PAGES));                                                   \
          PG_HD = Pg_free_pg(STR_PAGES);                                              \
        } else
#endif
/**************************************************************************************
**                                  USE_PAGES_MALLOC                                 **
**************************************************************************************/
#define ALLOC_VOID_PAGES(PG_HD, VOID_PAGES)	                                      \
        { int i, shmid;                                                               \
          pg_hd_ptr aux_pg_hd;                                                        \
          if ((shmid = shmget(IPC_PRIVATE, SHMMAX, SHM_R|SHM_W)) == -1)               \
            Yap_Error(FATAL_ERROR, TermNil, "shmget error (ALLOC_VOID_PAGES)");       \
          if ((PG_HD = (pg_hd_ptr) shmat(shmid, NULL, 0)) == (void *) -1)             \
            Yap_Error(FATAL_ERROR, TermNil, "shmat error (ALLOC_VOID_PAGES)");        \
          if (shmctl(shmid, IPC_RMID, 0) != 0)                                        \
            Yap_Error(FATAL_ERROR, TermNil, "shmctl error (ALLOC_VOID_PAGES)");       \
          aux_pg_hd = (pg_hd_ptr)(((void *)PG_HD) + Yap_page_size);                   \
          Pg_free_pg(VOID_PAGES) = aux_pg_hd;                                         \
          for (i = 2; i < SHMMAX / Yap_page_size; i++) {                              \
            PgHd_next(aux_pg_hd) = (pg_hd_ptr)(((void *)aux_pg_hd) + Yap_page_size);  \
            aux_pg_hd = PgHd_next(aux_pg_hd);                                         \
          }                                                                           \
          PgHd_next(aux_pg_hd) = NULL;                                                \
          UPDATE_STATS(Pg_pg_alloc(VOID_PAGES), SHMMAX / Yap_page_size);              \
          UPDATE_STATS(Pg_str_in_use(VOID_PAGES), 1);                                 \
        }

#define INIT_PAGE(PG_HD, STR_TYPE, STR_PAGES)                   		      \
        { int i;                                                                      \
          STR_TYPE *aux_str;                                                          \
          PgHd_str_in_use(PG_HD) = 0;                                                 \
          PgHd_previous(PG_HD) = NULL;                                                \
          PgHd_next(PG_HD) = NULL;                                                    \
          PgHd_free_str(PG_HD) = (void *) (PG_HD + 1);                                \
          aux_str = (STR_TYPE *) PgHd_free_str(PG_HD);                                \
          for (i = 1; i < Pg_str_per_pg(STR_PAGES); i++) {                            \
            STRUCT_NEXT(aux_str) = aux_str + 1;                                       \
            aux_str++;                                                                \
          }                                                                           \
          STRUCT_NEXT(aux_str) = NULL;                                                \
        }

#define ALLOC_STRUCT_TEST_ALLOC_PAGE(PG_HD, STR_TYPE, STR_PAGES, VOID_PAGES)	      \
        ALLOC_STRUCT_TEST_PAGE (PG_HD == NULL) {  /* if / while */                    \
          UNLOCK(Pg_lock(STR_PAGES));                                                 \
          LOCK(Pg_lock(VOID_PAGES));                                                  \
          /* if (...) { ... */                                                        \
          ALLOC_STRUCT_RECOVER_SPACE(PG_HD, STR_PAGES, VOID_PAGES)                    \
          /* } else */                                                                \
          {	                                                                      \
            PG_HD = Pg_free_pg(VOID_PAGES);                                           \
            if (PG_HD == NULL) {                                                      \
              ALLOC_VOID_PAGES(PG_HD, VOID_PAGES);                                    \
            } else {                          				              \
              Pg_free_pg(VOID_PAGES) = PgHd_next(PG_HD);                              \
              UPDATE_STATS(Pg_str_in_use(VOID_PAGES), 1);                             \
            }                                                                         \
            UNLOCK(Pg_lock(VOID_PAGES));                                              \
            INIT_PAGE(PG_HD, STR_TYPE, STR_PAGES);                                    \
            LOCK(Pg_lock(STR_PAGES));                                                 \
            if ((PgHd_next(PG_HD) = Pg_free_pg(STR_PAGES)) != NULL)                   \
              PgHd_previous(PgHd_next(PG_HD)) = PG_HD;                                \
            Pg_free_pg(STR_PAGES) = PG_HD;                                            \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), 1);                                  \
          }                                                                           \
        }

#define ALLOC_STRUCT(STR, STR_TYPE, STR_PAGES, VOID_PAGES)                            \
        { pg_hd_ptr pg_hd;                                                            \
          LOCK(Pg_lock(STR_PAGES));                                                   \
          pg_hd = Pg_free_pg(STR_PAGES);                                              \
          ALLOC_STRUCT_TEST_ALLOC_PAGE(pg_hd, STR_TYPE, STR_PAGES, VOID_PAGES);       \
          STR = (STR_TYPE *) PgHd_free_str(pg_hd);                                    \
          if ((PgHd_free_str(pg_hd) = (void *) STRUCT_NEXT(STR)) == NULL)             \
            if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)                   \
              PgHd_previous(PgHd_next(pg_hd)) = NULL;                                 \
          UPDATE_STATS(PgHd_str_in_use(pg_hd), 1);                                    \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                                  \
          UNLOCK(Pg_lock(STR_PAGES));                                                 \
	}

#define LOCAL_NEXT_ALLOC_STRUCT(STR, LOCAL_STR, STR_TYPE, STR_PAGES, VOID_PAGES)      \
        STR = LOCAL_STR;		                                              \
        if (STR == NULL) {                                                            \
          pg_hd_ptr pg_hd;                                                            \
          LOCK(Pg_lock(STR_PAGES));                                                   \
          pg_hd = Pg_free_pg(STR_PAGES);                                              \
          ALLOC_STRUCT_TEST_ALLOC_PAGE(pg_hd, STR_TYPE, STR_PAGES, VOID_PAGES);       \
          if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)                     \
            PgHd_previous(PgHd_next(pg_hd)) = NULL;                                   \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), -PgHd_str_in_use(pg_hd));            \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), Pg_str_per_pg(STR_PAGES));           \
          UNLOCK(Pg_lock(STR_PAGES));                                                 \
          STR = (STR_TYPE *) PgHd_free_str(pg_hd);                                    \
          PgHd_free_str(pg_hd) = NULL;                                                \
          PgHd_str_in_use(pg_hd) = Pg_str_per_pg(STR_PAGES);                          \
	}                                                                             \
        LOCAL_STR = STRUCT_NEXT(STR)

#define FREE_PAGE(PG_HD, VOID_PAGES)					              \
        LOCK(Pg_lock(VOID_PAGES));                                                    \
        PgHd_next(PG_HD) = Pg_free_pg(VOID_PAGES);                                    \
        Pg_free_pg(VOID_PAGES) = PG_HD;                                               \
        UPDATE_STATS(Pg_str_in_use(VOID_PAGES), -1);                                  \
        UNLOCK(Pg_lock(VOID_PAGES))

#define FREE_STRUCT(STR, STR_TYPE, STR_PAGES, VOID_PAGES)                             \
        { pg_hd_ptr pg_hd;                                                            \
          pg_hd = PAGE_HEADER(STR);                                                   \
          LOCK(Pg_lock(STR_PAGES));                                                   \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), -1);                                 \
          if (--PgHd_str_in_use(pg_hd) == 0) {                                        \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), -1);                                 \
            if (PgHd_previous(pg_hd)) {                                               \
              if ((PgHd_next(PgHd_previous(pg_hd)) = PgHd_next(pg_hd)) != NULL)       \
                PgHd_previous(PgHd_next(pg_hd)) = PgHd_previous(pg_hd);               \
	    } else {                                                                  \
              if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)                 \
                PgHd_previous(PgHd_next(pg_hd)) = NULL;                               \
	    }                                                                         \
            UNLOCK(Pg_lock(STR_PAGES));                                               \
            FREE_PAGE(pg_hd, VOID_PAGES);                                             \
	  } else {                                                                    \
            if ((STRUCT_NEXT(STR) = (STR_TYPE *) PgHd_free_str(pg_hd)) == NULL) {     \
              PgHd_previous(pg_hd) = NULL;                                            \
              if ((PgHd_next(pg_hd) = Pg_free_pg(STR_PAGES)) != NULL)                 \
                PgHd_previous(PgHd_next(pg_hd)) = pg_hd;                              \
              Pg_free_pg(STR_PAGES) = pg_hd;                                          \
            }                                                                         \
            PgHd_free_str(pg_hd) = (void *) STR;                                      \
            UNLOCK(Pg_lock(STR_PAGES));                                               \
          }                                                                           \
        }
#endif /******************************************************************************/


#define ALLOC_TABLE_ENTRY(STR)         ALLOC_STRUCT(STR, struct table_entry, GLOBAL_pages_tab_ent, GLOBAL_pages_void)
#define FREE_TABLE_ENTRY(STR)           FREE_STRUCT(STR, struct table_entry, GLOBAL_pages_tab_ent, GLOBAL_pages_void)

#define ALLOC_SUBGOAL_ENTRY(STR)       ALLOC_STRUCT(STR, struct subgoal_entry, GLOBAL_pages_sg_ent, GLOBAL_pages_void)
#define FREE_SUBGOAL_ENTRY(STR)         FREE_STRUCT(STR, struct subgoal_entry, GLOBAL_pages_sg_ent, GLOBAL_pages_void)

#if !defined(THREADS_NO_SHARING) && !defined(THREADS_SUBGOAL_SHARING) && !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
#define ALLOC_SUBGOAL_FRAME(STR)       ALLOC_STRUCT(STR, struct subgoal_frame, GLOBAL_pages_sg_fr, GLOBAL_pages_void)
#define FREE_SUBGOAL_FRAME(STR)         FREE_STRUCT(STR, struct subgoal_frame, GLOBAL_pages_sg_fr, GLOBAL_pages_void)
#else
#define ALLOC_SUBGOAL_FRAME(STR)       ALLOC_STRUCT(STR, struct subgoal_frame, LOCAL_pages_sg_fr, LOCAL_pages_void)
#define FREE_SUBGOAL_FRAME(STR)         FREE_STRUCT(STR, struct subgoal_frame, LOCAL_pages_sg_fr, LOCAL_pages_void)
#endif

#if !defined(THREADS_NO_SHARING) && !defined(THREADS_SUBGOAL_SHARING) && !defined(THREADS_FULL_SHARING) && !defined(THREADS_CONSUMER_SHARING)
#define ALLOC_DEPENDENCY_FRAME(STR)    ALLOC_STRUCT(STR, struct dependency_frame, GLOBAL_pages_dep_fr, GLOBAL_pages_void)
#define FREE_DEPENDENCY_FRAME(STR)      FREE_STRUCT(STR, struct dependency_frame, GLOBAL_pages_dep_fr, GLOBAL_pages_void)
#else
#define ALLOC_DEPENDENCY_FRAME(STR)    ALLOC_STRUCT(STR, struct dependency_frame, LOCAL_pages_dep_fr, LOCAL_pages_void)
#define FREE_DEPENDENCY_FRAME(STR)      FREE_STRUCT(STR, struct dependency_frame, LOCAL_pages_dep_fr, LOCAL_pages_void)
#endif

#if !defined(THREADS_NO_SHARING)
#if defined(THREADS_SUBGOAL_SHARING) || defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define ALLOC_SUBGOAL_TRIE_NODE(STR)   LOCAL_NEXT_ALLOC_STRUCT(STR, LOCAL_next_free_sg_node, struct subgoal_trie_node, GLOBAL_pages_sg_node, GLOBAL_pages_void)
#else
#define ALLOC_SUBGOAL_TRIE_NODE(STR)   ALLOC_STRUCT(STR, struct subgoal_trie_node, GLOBAL_pages_sg_node, GLOBAL_pages_void)
#endif
#define FREE_SUBGOAL_TRIE_NODE(STR)     FREE_STRUCT(STR, struct subgoal_trie_node, GLOBAL_pages_sg_node, GLOBAL_pages_void)
#else
#define ALLOC_SUBGOAL_TRIE_NODE(STR)   ALLOC_STRUCT(STR, struct subgoal_trie_node, LOCAL_pages_sg_node, LOCAL_pages_void)
#define FREE_SUBGOAL_TRIE_NODE(STR)     FREE_STRUCT(STR, struct subgoal_trie_node, LOCAL_pages_sg_node, LOCAL_pages_void)
#endif

#if !defined(THREADS_NO_SHARING)
#if defined(THREADS_SUBGOAL_SHARING) || defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define ALLOC_SUBGOAL_TRIE_HASH(STR)   LOCAL_NEXT_ALLOC_STRUCT(STR, LOCAL_next_free_sg_hash, struct subgoal_trie_hash, GLOBAL_pages_sg_hash, GLOBAL_pages_void)
#else
#define ALLOC_SUBGOAL_TRIE_HASH(STR)   ALLOC_STRUCT(STR, struct subgoal_trie_hash, GLOBAL_pages_sg_hash, GLOBAL_pages_void)
#endif
#define FREE_SUBGOAL_TRIE_HASH(STR)     FREE_STRUCT(STR, struct subgoal_trie_hash, GLOBAL_pages_sg_hash, GLOBAL_pages_void)
#else
#define ALLOC_SUBGOAL_TRIE_HASH(STR)   ALLOC_STRUCT(STR, struct subgoal_trie_hash, LOCAL_pages_sg_hash, LOCAL_pages_void)
#define FREE_SUBGOAL_TRIE_HASH(STR)     FREE_STRUCT(STR, struct subgoal_trie_hash, LOCAL_pages_sg_hash, LOCAL_pages_void)
#endif

#if !defined(THREADS_NO_SHARING) && !defined(THREADS_SUBGOAL_SHARING)
#if defined(YAPOR) || defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define ALLOC_ANSWER_TRIE_NODE(STR)    LOCAL_NEXT_ALLOC_STRUCT(STR, LOCAL_next_free_ans_node, struct answer_trie_node, GLOBAL_pages_ans_node, GLOBAL_pages_void)
#else
#define ALLOC_ANSWER_TRIE_NODE(STR)    ALLOC_STRUCT(STR, struct answer_trie_node, GLOBAL_pages_ans_node, GLOBAL_pages_void)
#endif
#define FREE_ANSWER_TRIE_NODE(STR)      FREE_STRUCT(STR, struct answer_trie_node, GLOBAL_pages_ans_node, GLOBAL_pages_void)
#else
#define ALLOC_ANSWER_TRIE_NODE(STR)    ALLOC_STRUCT(STR, struct answer_trie_node, LOCAL_pages_ans_node, LOCAL_pages_void)
#define FREE_ANSWER_TRIE_NODE(STR)      FREE_STRUCT(STR, struct answer_trie_node, LOCAL_pages_ans_node, LOCAL_pages_void)
#endif

#if !defined(THREADS_NO_SHARING) && !defined(THREADS_SUBGOAL_SHARING)
#if defined(THREADS_FULL_SHARING) || defined(THREADS_CONSUMER_SHARING)
#define ALLOC_ANSWER_TRIE_HASH(STR)    LOCAL_NEXT_ALLOC_STRUCT(STR, LOCAL_next_free_ans_hash, struct answer_trie_hash, GLOBAL_pages_ans_hash, GLOBAL_pages_void)
#else
#define ALLOC_ANSWER_TRIE_HASH(STR)    ALLOC_STRUCT(STR, struct answer_trie_hash, GLOBAL_pages_ans_hash, GLOBAL_pages_void)
#endif
#define FREE_ANSWER_TRIE_HASH(STR)      FREE_STRUCT(STR, struct answer_trie_hash, GLOBAL_pages_ans_hash, GLOBAL_pages_void)
#else
#define ALLOC_ANSWER_TRIE_HASH(STR)    ALLOC_STRUCT(STR, struct answer_trie_hash, LOCAL_pages_ans_hash, LOCAL_pages_void)
#define FREE_ANSWER_TRIE_HASH(STR)      FREE_STRUCT(STR, struct answer_trie_hash, LOCAL_pages_ans_hash, LOCAL_pages_void)
#endif

#define ALLOC_ANSWER_REF_NODE(STR)    ALLOC_STRUCT(STR, struct answer_ref_node, LOCAL_pages_ans_ref_node, LOCAL_pages_void)
#define FREE_ANSWER_REF_NODE(STR)      FREE_STRUCT(STR, struct answer_ref_node, LOCAL_pages_ans_ref_node, LOCAL_pages_void)

#define ALLOC_GLOBAL_TRIE_NODE(STR)    ALLOC_STRUCT(STR, struct global_trie_node, GLOBAL_pages_gt_node, GLOBAL_pages_void)
#define FREE_GLOBAL_TRIE_NODE(STR)      FREE_STRUCT(STR, struct global_trie_node, GLOBAL_pages_gt_node, GLOBAL_pages_void)

#define ALLOC_GLOBAL_TRIE_HASH(STR)    ALLOC_STRUCT(STR, struct global_trie_hash, GLOBAL_pages_gt_hash, GLOBAL_pages_void)
#define FREE_GLOBAL_TRIE_HASH(STR)      FREE_STRUCT(STR, struct global_trie_hash, GLOBAL_pages_gt_hash, GLOBAL_pages_void)

#define ALLOC_OR_FRAME(STR)            ALLOC_STRUCT(STR, struct or_frame, GLOBAL_pages_or_fr, GLOBAL_pages_void)
#define FREE_OR_FRAME(STR)              FREE_STRUCT(STR, struct or_frame, GLOBAL_pages_or_fr, GLOBAL_pages_void)

#define ALLOC_QG_SOLUTION_FRAME(STR)   ALLOC_STRUCT(STR, struct query_goal_solution_frame, GLOBAL_pages_qg_sol_fr, GLOBAL_pages_void)
#define FREE_QG_SOLUTION_FRAME(STR)     FREE_STRUCT(STR, struct query_goal_solution_frame, GLOBAL_pages_qg_sol_fr, GLOBAL_pages_void)

#define ALLOC_QG_ANSWER_FRAME(STR)     ALLOC_STRUCT(STR, struct query_goal_answer_frame, GLOBAL_pages_qg_ans_fr, GLOBAL_pages_void)
#define FREE_QG_ANSWER_FRAME(STR)       FREE_STRUCT(STR, struct query_goal_answer_frame, GLOBAL_pages_qg_ans_fr, GLOBAL_pages_void)

#define ALLOC_SUSPENSION_FRAME(STR)    ALLOC_STRUCT(STR, struct suspension_frame, GLOBAL_pages_susp_fr, GLOBAL_pages_void)
#define FREE_SUSPENSION_FRAME(STR)      FREE_BLOCK(SuspFr_global_start(STR));                                            \
                                        FREE_STRUCT(STR, struct suspension_frame, GLOBAL_pages_susp_fr, GLOBAL_pages_void)

#define ALLOC_TG_SOLUTION_FRAME(STR)   ALLOC_STRUCT(STR, struct table_subgoal_solution_frame, GLOBAL_pages_tg_sol_fr, GLOBAL_pages_void)
#define FREE_TG_SOLUTION_FRAME(STR)     FREE_STRUCT(STR, struct table_subgoal_solution_frame, GLOBAL_pages_tg_sol_fr, GLOBAL_pages_void)

#define ALLOC_TG_ANSWER_FRAME(STR)     ALLOC_STRUCT(STR, struct table_subgoal_answer_frame, GLOBAL_pages_tg_ans_fr, GLOBAL_pages_void)
#define FREE_TG_ANSWER_FRAME(STR)       FREE_STRUCT(STR, struct table_subgoal_answer_frame, GLOBAL_pages_tg_ans_fr, GLOBAL_pages_void)



/************************************************************************
**                         Bitmap manipulation                         **
************************************************************************/

#define BITMAP_empty(b)		       ((b) == 0)
#define BITMAP_member(b,n)	       (((b) & (1<<(n))) != 0)
#define BITMAP_alone(b,n)	       ((b) == (1<<(n)))
#define BITMAP_subset(b1,b2)	       (((b1) & (b2)) == b2)
#define BITMAP_same(b1,b2)             ((b1) == (b2))
#define BITMAP_clear(b)	               ((b) = 0)
#define BITMAP_and(b1,b2)              ((b1) &= (b2))
#define BITMAP_minus(b1,b2)            ((b1) &= ~(b2))
#define BITMAP_insert(b,n)	       ((b) |= (1<<(n)))
#define BITMAP_delete(b,n)	       ((b) &= (~(1<<(n))))
#define BITMAP_copy(b1,b2)	       ((b1) = (b2))
#define BITMAP_intersection(b1,b2,b3)  ((b1) = ((b2) & (b3)))
#define BITMAP_difference(b1,b2,b3)    ((b1) = ((b2) & (~(b3))))



/************************************************************************
**                            Debug macros                             **
************************************************************************/

#define INFORMATION_MESSAGE(MESSAGE,ARGS...)                            \
        Sfprintf(Serror, "[ " MESSAGE " ]\n", ##ARGS)

#ifdef YAPOR
#define ERROR_MESSAGE(MESSAGE)                                          \
        Yap_Error(INTERNAL_ERROR, TermNil, "W%d - " MESSAGE, worker_id)
#else
#define ERROR_MESSAGE(MESSAGE)                                          \
        Yap_Error(INTERNAL_ERROR, TermNil, MESSAGE)
#endif /* YAPOR */

#ifdef DEBUG_TABLING
#define TABLING_ERROR_CHECKING(PROCEDURE,TEST)                          \
        if (TEST) ERROR_MESSAGE(#PROCEDURE ": " #TEST)
#else
#define TABLING_ERROR_CHECKING(PROCEDURE,TEST)
#endif /* DEBUG_TABLING */

#ifdef DEBUG_YAPOR
#define YAPOR_ERROR_CHECKING(PROCEDURE,TEST)                            \
        if (TEST) ERROR_MESSAGE(#PROCEDURE ": " #TEST)
#else
#define YAPOR_ERROR_CHECKING(PROCEDURE,TEST)
#endif /* DEBUG_YAPOR */

#ifdef DEBUG_OPTYAP
#define OPTYAP_ERROR_CHECKING(PROCEDURE,TEST)                           \
        if (TEST) ERROR_MESSAGE(#PROCEDURE ": " #TEST)
#else
#define OPTYAP_ERROR_CHECKING(PROCEDURE,TEST)
#endif /* DEBUG_OPTYAP */
