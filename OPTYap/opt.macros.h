/* --------------------------- **
**      Memory management      **
** --------------------------- */

extern int PageSize;

#define H_BASE   ((CELL *) GlobalBase)
#define B_BASE   ((choiceptr) LocalBase)
#define TR_BASE  ((tr_fr_ptr) TrailBase)

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
#define ADJUST_SIZE_TO_PAGE(SIZE)  ((SIZE) - (SIZE) % PageSize + PageSize)
#define STRUCT_SIZE(STR_TYPE)      ADJUST_SIZE(sizeof(STR_TYPE))
#define PAGE_HEADER(STR)           (pg_hd_ptr)((unsigned int)STR - (unsigned int)STR % PageSize)
#define STRUCT_NEXT(STR)           ((STR)->next)



#ifdef STATISTICS
#define UPDATE_STATS(STAT, VALUE)  STAT += VALUE
#else
#define UPDATE_STATS(STAT, VALUE)
#endif /* STATISTICS */ 



#define ALLOC_BLOCK(BLOCK, SIZE)               \
        BLOCK = (void *) AllocAtomSpace(SIZE)
#define FREE_BLOCK(BLOCK)                      \
        FreeCodeSpace((char *) (BLOCK))



#define ALLOC_HASH_BUCKETS(BUCKET_PTR, NUM_BUCKETS)         \
        { int i; void **ptr;                                \
          ALLOC_BLOCK(ptr, NUM_BUCKETS * sizeof(void *));   \
          BUCKET_PTR = (void *) ptr;                        \
          for (i = NUM_BUCKETS; i != 0; i--)                \
            *ptr++ = NULL;                                  \
        }
#define FREE_HASH_BUCKETS(BUCKET_PTR)  FREE_BLOCK(BUCKET_PTR)

#ifdef USE_HEAP

#define alloc_memory_block(SIZE)   (void *)AllocCodeSpace(SIZE)
#define free_memory_block(BLK)   FreeCodeSpace((ADDR)BLK)
#define reset_alloc_block_area()   

#define ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE) STR = (STR_TYPE *)AllocCodeSpace(sizeof(STR_TYPE))

#define ALLOC_NEXT_FREE_STRUCT(STR, STR_PAGES, STR_TYPE) STR = (STR_TYPE *)AllocCodeSpace(sizeof(STR_TYPE))

#define FREE_STRUCT(STR, STR_PAGES, STR_TYPE) FreeCodeSpace((ADDR)(STR))

#else


#define ALLOC_PAGE(PG_HD)                                           \
        LOCK(Pg_lock(GLOBAL_PAGES_void));                           \
        UPDATE_STATS(Pg_requests(GLOBAL_PAGES_void), 1);            \
        UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), 1);          \
        if (Pg_free_pg(GLOBAL_PAGES_void) == NULL) {                \
          if (TopAllocArea == TopWorkArea)                          \
            abort_optyap("no more free alloc space (ALLOC_PAGE)");  \
          UPDATE_STATS(Pg_str_alloc(GLOBAL_PAGES_void), 1);         \
          PG_HD = (pg_hd_ptr)TopAllocArea;                          \
          TopAllocArea += PageSize;                                 \
        } else {                                                    \
          PG_HD = Pg_free_pg(GLOBAL_PAGES_void);                    \
          Pg_free_pg(GLOBAL_PAGES_void) = PgHd_next(PG_HD);         \
        }                                                           \
        UNLOCK(Pg_lock(GLOBAL_PAGES_void))

#define FREE_PAGE(PG_HD)                                            \
        LOCK(Pg_lock(GLOBAL_PAGES_void));                           \
        UPDATE_STATS(Pg_requests(GLOBAL_PAGES_void), 1);            \
        UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), -1);         \
        PgHd_next(PG_HD) = Pg_free_pg(GLOBAL_PAGES_void);           \
        Pg_free_pg(GLOBAL_PAGES_void) = PG_HD;                      \
        UNLOCK(Pg_lock(GLOBAL_PAGES_void))



#define ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)                                     \
        { pg_hd_ptr pg_hd;                                                         \
          LOCK(Pg_lock(STR_PAGES));                                                \
          UPDATE_STATS(Pg_requests(STR_PAGES), 1);                                 \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                               \
          if (Pg_free_pg(STR_PAGES)) {                                             \
            pg_hd = Pg_free_pg(STR_PAGES);                                         \
            PgHd_str_in_use(pg_hd)++;                                              \
            STR = (STR_TYPE *) PgHd_free_str(pg_hd);                               \
            if ((PgHd_free_str(pg_hd) = (void *) STRUCT_NEXT(STR)) == NULL)        \
              Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd);                            \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
          } else {                                                                 \
            int i;                                                                 \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), 1);                               \
            UPDATE_STATS(Pg_str_alloc(STR_PAGES), Pg_str_per_pg(STR_PAGES));       \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
            ALLOC_PAGE(pg_hd);                                                     \
            PgHd_str_in_use(pg_hd) = 1;                                            \
            PgHd_previous(pg_hd) = NULL;                                           \
            STR = (STR_TYPE *) (pg_hd + 1);                                        \
            PgHd_free_str(pg_hd) = (void *) ++STR;                                 \
            for (i = Pg_str_per_pg(STR_PAGES); i != 2; i--) {                      \
              STRUCT_NEXT(STR) = STR + 1;                                          \
              STR++;                                                               \
            }                                                                      \
            STRUCT_NEXT(STR) = NULL;                                               \
            STR = (STR_TYPE *) (pg_hd + 1);                                        \
            LOCK(Pg_lock(STR_PAGES));                                              \
            if ((PgHd_next(pg_hd) = Pg_free_pg(STR_PAGES)) != NULL)                \
              PgHd_previous(PgHd_next(pg_hd)) = pg_hd;                             \
            Pg_free_pg(STR_PAGES) = pg_hd;                                         \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
          }                                                                        \
	}

#define ALLOC_NEXT_FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                           \
        if ((STR = LOCAL_next_free_ans_node) == NULL) {                            \
          pg_hd_ptr pg_hd;                                                         \
          LOCK(Pg_lock(STR_PAGES));                                                \
          UPDATE_STATS(Pg_requests(STR_PAGES), 1);                                 \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), Pg_str_per_pg(STR_PAGES));        \
          if (Pg_free_pg(STR_PAGES)) {                                             \
            pg_hd = Pg_free_pg(STR_PAGES);                                         \
            UPDATE_STATS(Pg_str_in_use(STR_PAGES), -PgHd_str_in_use(pg_hd));       \
            PgHd_str_in_use(pg_hd) = Pg_str_per_pg(STR_PAGES);                     \
            STR = (STR_TYPE *) PgHd_free_str(pg_hd);                               \
            PgHd_free_str(pg_hd) = NULL;                                           \
            Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd);                              \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
          } else {                                                                 \
            int i;                                                                 \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), 1);                               \
            UPDATE_STATS(Pg_str_alloc(STR_PAGES), Pg_str_per_pg(STR_PAGES));       \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
            ALLOC_PAGE(pg_hd);                                                     \
            PgHd_str_in_use(pg_hd) = Pg_str_per_pg(STR_PAGES);                     \
            PgHd_free_str(pg_hd) = NULL;                                           \
            PgHd_previous(pg_hd) = NULL;                                           \
            PgHd_next(pg_hd) = NULL;                                               \
            STR = (STR_TYPE *) (pg_hd + 1);                                        \
            for (i = Pg_str_per_pg(STR_PAGES); i != 1; i--) {                      \
              STRUCT_NEXT(STR) = STR + 1;                                          \
              STR++;                                                               \
            }                                                                      \
            STRUCT_NEXT(STR) = NULL;                                               \
            STR = (STR_TYPE *) (pg_hd + 1);                                        \
          }                                                                        \
	}                                                                          \
        LOCAL_next_free_ans_node = STRUCT_NEXT(STR)

#define FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                      \
        { pg_hd_ptr pg_hd;                                                         \
          pg_hd = PAGE_HEADER(STR);                                                \
          LOCK(Pg_lock(STR_PAGES));                                                \
          UPDATE_STATS(Pg_requests(STR_PAGES), 1);                                 \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), -1);                              \
          if (--PgHd_str_in_use(pg_hd) == 0) {                                     \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), -1);                              \
	    UPDATE_STATS(Pg_str_alloc(STR_PAGES), -Pg_str_per_pg(STR_PAGES));      \
            if (PgHd_previous(pg_hd)) {                                            \
              if ((PgHd_next(PgHd_previous(pg_hd)) = PgHd_next(pg_hd)) != NULL)    \
                PgHd_previous(PgHd_next(pg_hd)) = PgHd_previous(pg_hd);            \
	    } else {                                                               \
              if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)              \
                PgHd_previous(PgHd_next(pg_hd)) = NULL;                            \
	    }                                                                      \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
            FREE_PAGE(pg_hd);                                                      \
	  } else {                                                                 \
            if ((STRUCT_NEXT(STR) = (STR_TYPE *) PgHd_free_str(pg_hd)) == NULL) {  \
              PgHd_previous(pg_hd) = NULL;                                         \
              if ((PgHd_next(pg_hd) = Pg_free_pg(STR_PAGES)) != NULL)              \
                PgHd_previous(PgHd_next(pg_hd)) = pg_hd;                           \
              Pg_free_pg(STR_PAGES) = pg_hd;                                       \
            }                                                                      \
            PgHd_free_str(pg_hd) = (void *) STR;                                   \
            UNLOCK(Pg_lock(STR_PAGES));                                            \
          }                                                                        \
        }

#endif /* TEST*/


#define ALLOC_OR_FRAME(STR)          ALLOC_STRUCT(STR, GLOBAL_PAGES_or_fr, struct or_frame)
#define FREE_OR_FRAME(STR)            FREE_STRUCT(STR, GLOBAL_PAGES_or_fr, struct or_frame)

#define ALLOC_QG_SOLUTION_FRAME(STR) ALLOC_STRUCT(STR, GLOBAL_PAGES_qg_sol_fr, struct query_goal_solution_frame)
#define FREE_QG_SOLUTION_FRAME(STR)   FREE_STRUCT(STR, GLOBAL_PAGES_qg_sol_fr, struct query_goal_solution_frame)

#define ALLOC_QG_ANSWER_FRAME(STR)   ALLOC_STRUCT(STR, GLOBAL_PAGES_qg_ans_fr, struct query_goal_answer_frame)
#define FREE_QG_ANSWER_FRAME(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_qg_ans_fr, struct query_goal_answer_frame)

#define ALLOC_TG_SOLUTION_FRAME(STR) ALLOC_STRUCT(STR, GLOBAL_PAGES_tg_sol_fr, struct table_subgoal_solution_frame)
#define FREE_TG_SOLUTION_FRAME(STR)   FREE_STRUCT(STR, GLOBAL_PAGES_tg_sol_fr, struct table_subgoal_solution_frame)

#define ALLOC_TG_ANSWER_FRAME(STR)   ALLOC_STRUCT(STR, GLOBAL_PAGES_tg_ans_fr, struct table_subgoal_answer_frame)
#define FREE_TG_ANSWER_FRAME(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_tg_ans_fr, struct table_subgoal_answer_frame)

#define ALLOC_TABLE_ENTRY(STR)       ALLOC_STRUCT(STR, GLOBAL_PAGES_tab_ent, struct table_entry)
#define FREE_TABLE_ENTRY(STR)         FREE_STRUCT(STR, GLOBAL_PAGES_tab_ent, struct table_entry)

#define ALLOC_SUBGOAL_TRIE_NODE(STR) ALLOC_STRUCT(STR, GLOBAL_PAGES_sg_node, struct subgoal_trie_node)
#define FREE_SUBGOAL_TRIE_NODE(STR)   FREE_STRUCT(STR, GLOBAL_PAGES_sg_node, struct subgoal_trie_node)

#define ALLOC_SUBGOAL_FRAME(STR)     ALLOC_STRUCT(STR, GLOBAL_PAGES_sg_fr, struct subgoal_frame)
#define FREE_SUBGOAL_FRAME(STR)       FREE_STRUCT(STR, GLOBAL_PAGES_sg_fr, struct subgoal_frame)

#define ALLOC_ANSWER_TRIE_NODE(STR)  ALLOC_NEXT_FREE_STRUCT(STR, GLOBAL_PAGES_ans_node, struct answer_trie_node)
#define FREE_ANSWER_TRIE_NODE(STR)              FREE_STRUCT(STR, GLOBAL_PAGES_ans_node, struct answer_trie_node)

#define ALLOC_DEPENDENCY_FRAME(STR)  ALLOC_STRUCT(STR, GLOBAL_PAGES_dep_fr, struct dependency_frame)
#define FREE_DEPENDENCY_FRAME(STR)    FREE_STRUCT(STR, GLOBAL_PAGES_dep_fr, struct dependency_frame)

#define ALLOC_SUSPENSION_FRAME(STR)  ALLOC_STRUCT(STR, GLOBAL_PAGES_susp_fr, struct suspension_frame)
#define FREE_SUSPENSION_FRAME(STR)    FREE_BLOCK(SuspFr_global_start(STR));                         \
                                      FREE_STRUCT(STR, GLOBAL_PAGES_susp_fr, struct suspension_frame)

#define ALLOC_SUBGOAL_HASH(STR)      ALLOC_STRUCT(STR, GLOBAL_PAGES_sg_hash, struct subgoal_hash)
#define FREE_SUBGOAL_HASH(STR)        FREE_STRUCT(STR, GLOBAL_PAGES_sg_hash, struct subgoal_hash)

#define ALLOC_ANSWER_HASH(STR)       ALLOC_STRUCT(STR, GLOBAL_PAGES_ans_hash, struct answer_hash)
#define FREE_ANSWER_HASH(STR)         FREE_STRUCT(STR, GLOBAL_PAGES_ans_hash, struct answer_hash)



/* ------------------------------------- **
**      Bitmap tests and operations      **
** ------------------------------------- */

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



/* ---------------------------------- **
**      Message and debug macros      **
** ---------------------------------- */

#define INFORMATION_MESSAGE(MESG, ARGS...)  information_message(MESG, ##ARGS)

#ifdef YAPOR_ERRORS
#define YAPOR_ERROR_MESSAGE(MESG, ARGS...)  error_message(MESG, ##ARGS)
#else
#define YAPOR_ERROR_MESSAGE(MESG, ARGS...)
#endif /* YAPOR_ERRORS */

#ifdef TABLING_ERRORS
#define TABLING_ERROR_MESSAGE(MESG, ARGS...)  error_message(MESG, ##ARGS)
#else
#define TABLING_ERROR_MESSAGE(MESG, ARGS...)
#endif /* TABLING_ERRORS */

#ifdef OPTYAP_ERRORS
#define OPTYAP_ERROR_MESSAGE(MESG, ARGS...)  error_message(MESG, ##ARGS)
#else
#define OPTYAP_ERROR_MESSAGE(MESG, ARGS...)
#endif /* OPTYAP_ERRORS */



/* ----------------------- **
**      SimICS macros      **
** ----------------------- */

/*
**  Counter  0. Total time
**  Counter  1. Prolog
**  Counter  2. P Share
**  Counter  3. Scheduler
**  Counter  4. Cut request
**  Counter  5. End operations
**  Counter  6. Cut shared (Counter 1 or 3 or 5)
**  Counter  7. Number of requests to share work (Counter 3)
**  Counter  8. Number of refused requests (Counter 3)
**  Counter  9. Number of tasks (Counter 1)
**  Counter 10. Number of calls (Counter 1)
**  Counter 11. Number of failed TRY_LOCK's
*/

#define START_COUNTER             1
#define STOP_COUNTER              2

#define TOTAL_TIME                0
#define PROLOG                    1
#define SHARE                     2
#define SCHEDULER                 3
#define CUT_REQUEST               4
#define END_OPERATIONS            5
#define CUT_SHARED                6
#define ONE_MORE_REQUEST          7
#define ONE_MORE_REFUSED_REQUEST  8
#define ONE_MORE_TASK             9
#define ONE_MORE_CALL            10

#ifdef SIMICS
#define SIMICS_ATOMIC_SERVICE(COUNTER)           \
        SIMICS_SERVICE(START_COUNTER, COUNTER);  \
        SIMICS_SERVICE(STOP_COUNTER, COUNTER)
#define SIMICS_SERVICE(COMMAND, COUNTER)         \
        do {                                     \
          asm volatile ("sethi %0, %%g0" :       \
          /* no outputs */ :                     \
          "g" ((COMMAND << 16) | COUNTER));      \
	} while(0)
#else
#define SIMICS_ATOMIC_SERVICE(COUNTER)
#define SIMICS_SERVICE(COMMAND, COUNTER)
#endif /* SIMICS */
