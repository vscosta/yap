/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        opt.macros.h 
  version:     $Id: opt.macros.h,v 1.12 2007-04-26 14:11:08 ricroc Exp $   
                                                                     
**********************************************************************/

/* ------------------------------ **
**      Includes and defines      **
** ------------------------------ */

#ifdef SHM_MEMORY_ALLOC_SCHEME
#include <sys/shm.h>

#define SHMMAX 0x2000000  /* 32 Mbytes: works fine with linux */
/* #define SHMMAX  0x400000 - 4 Mbytes: shmget limit for Mac (?) */
/* #define SHMMAX  0x800000 - 8 Mbytes: shmget limit for Solaris (?) */
#endif /* SHM_MEMORY_ALLOC_SCHEME */



/* --------------------------- **
**      Memory management      **
** --------------------------- */

extern int Yap_page_size;

#define H_BASE   ((CELL *) Yap_GlobalBase)
#define B_BASE   ((choiceptr) Yap_LocalBase)
#define TR_BASE  ((tr_fr_ptr) Yap_TrailBase)

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

#ifdef MALLOC_MEMORY_ALLOC_SCHEME  /* ---------------------------------------------------------------- */
#define ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)                                                          \
        UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                                                      \
        if ((STR = (STR_TYPE *)malloc(sizeof(STR_TYPE))) == NULL)                                       \
          Yap_Error(FATAL_ERROR, TermNil, "malloc error (ALLOC_STRUCT)")
#define ALLOC_NEXT_FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                \
        ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)
#define FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                           \
        UPDATE_STATS(Pg_str_in_use(STR_PAGES), -1);                                                     \
        free(STR)
#elif YAP_MEMORY_ALLOC_SCHEME  /* -------------------------------------------------------------------- */
#define ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)                                                          \
        { char *ptr = Yap_AllocCodeSpace(sizeof(STR_TYPE) + sizeof(CELL));                              \
          if (ptr) {                                                                                    \
            *ptr = 'y';                                                                                 \
            ptr += sizeof(CELL);                                                                        \
            STR = (STR_TYPE *)ptr;                                                                      \
          } else {                                                                                      \
            ptr = (char *)malloc(sizeof(STR_TYPE) + sizeof(CELL));                                      \
            if (ptr) {                                                                                  \
              *ptr = 'm';                                                                               \
              ptr += sizeof(CELL);                                                                      \
              STR = (STR_TYPE *)ptr;                                                                    \
            } else {                                                                                    \
              Yap_Error(FATAL_ERROR, TermNil, "malloc error (ALLOC_STRUCT)");                           \
              STR = NULL;                                                                               \
	    }                                                                                           \
          }                                                                                             \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                                                    \
        }
#define ALLOC_NEXT_FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                \
        ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)
#define FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                           \
        { char *ptr = (char *)(STR) - sizeof(CELL);                                                     \
          if (ptr[0] == 'y') {                                                                          \
            Yap_FreeCodeSpace(ptr);                                                                     \
          } else                                                                                        \
            free(ptr);                                                                                  \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), -1);                                                   \
        }
#elif SHM_MEMORY_ALLOC_SCHEME  /* -------------------------------------------------------------------- */
#ifdef LIMIT_TABLING
#define INIT_PAGE(PG_HD, STR_PAGES, STR_TYPE)                                                           \
        { int i;                                                                                        \
          STR_TYPE *aux_str;                                                                            \
          PgHd_str_in_use(PG_HD) = 0;                                                                   \
          PgHd_previous(PG_HD) = NULL;                                                                  \
          aux_str = (STR_TYPE *) (PG_HD + 1);                                                           \
          PgHd_free_str(PG_HD) = (void *) aux_str;                                                      \
          for (i = 1; i < Pg_str_per_pg(STR_PAGES); i++) {                                              \
            STRUCT_NEXT(aux_str) = aux_str + 1;                                                         \
            aux_str++;                                                                                  \
          }                                                                                             \
          STRUCT_NEXT(aux_str) = NULL;                                                                  \
          LOCK(Pg_lock(STR_PAGES));                                                                     \
          if ((PgHd_next(PG_HD) = Pg_free_pg(STR_PAGES)) != NULL)                                       \
            PgHd_previous(PgHd_next(PG_HD)) = PG_HD;                                                    \
          Pg_free_pg(STR_PAGES) = PG_HD;                                                                \
          UPDATE_STATS(Pg_pg_alloc(STR_PAGES), 1);                                                      \
        }

#define ALLOC_PAGE(PG_HD)                                                                               \
        { int i, shmid;                                                                                 \
          pg_hd_ptr aux_pg_hd;                                                                          \
          if ((shmid = shmget(IPC_PRIVATE, SHMMAX, SHM_R|SHM_W)) == -1)                                 \
            Yap_Error(FATAL_ERROR, TermNil, "shmget error (ALLOC_PAGE)");                               \
          if ((PG_HD = (pg_hd_ptr) shmat(shmid, NULL, 0)) == (void *) -1)                               \
            Yap_Error(FATAL_ERROR, TermNil, "shmat error (ALLOC_PAGE)");                                \
          if (shmctl(shmid, IPC_RMID, 0) != 0)                                                          \
            Yap_Error(FATAL_ERROR, TermNil, "shmctl error (ALLOC_PAGE)");                               \
          aux_pg_hd = (pg_hd_ptr)(((void *)PG_HD) + Yap_page_size);                                     \
          Pg_free_pg(GLOBAL_PAGES_void) = aux_pg_hd;                                                    \
          for (i = 2; i < SHMMAX / Yap_page_size; i++) {                                                \
            PgHd_next(aux_pg_hd) = (pg_hd_ptr)(((void *)aux_pg_hd) + Yap_page_size);                    \
            aux_pg_hd = PgHd_next(aux_pg_hd);                                                           \
          }                                                                                             \
          PgHd_next(aux_pg_hd) = NULL;                                                                  \
          UPDATE_STATS(Pg_pg_alloc(GLOBAL_PAGES_void), SHMMAX / Yap_page_size);                         \
        }

#define RECOVER_UNUSED_SPACE(STR_PAGES)                                                                 \
        { sg_fr_ptr sg_fr = GLOBAL_check_sg_fr;                                                         \
          do {                                                                                          \
            if (sg_fr)                                                                                  \
              sg_fr = SgFr_next(sg_fr);                                                                 \
            else                                                                                        \
              sg_fr = GLOBAL_first_sg_fr;                                                               \
            if (sg_fr == NULL)                                                                          \
              Yap_Error(FATAL_ERROR, TermNil, "no space left (RECOVER_UNUSED_SPACE)");                  \
              /* see function 'InteractSIGINT' in file 'sysbits.c' */                                   \
              /* Yap_Error(PURE_ABORT, TermNil, "");               */                                   \
              /* restore_absmi_regs(&Yap_standard_regs);           */                                   \
              /* siglongjmp (Yap_RestartEnv, 1);                   */                                   \
            if (SgFr_first_answer(sg_fr) &&                                                             \
                SgFr_first_answer(sg_fr) != SgFr_answer_trie(sg_fr)) {                                  \
              SgFr_state(sg_fr) = ready;                                                                \
	      free_answer_trie_hash_chain(SgFr_hash_chain(sg_fr));                                      \
	      SgFr_hash_chain(sg_fr) = NULL;                                                            \
	      SgFr_first_answer(sg_fr) = NULL;                                                          \
              SgFr_last_answer(sg_fr) = NULL;                                                           \
              free_answer_trie_branch(TrNode_child(SgFr_answer_trie(sg_fr)), TRAVERSE_POSITION_FIRST);  \
              TrNode_child(SgFr_answer_trie(sg_fr)) = NULL;                                             \
	    }                                                                                           \
          } while (Pg_free_pg(GLOBAL_PAGES_void) == Pg_free_pg(STR_PAGES));                             \
          GLOBAL_check_sg_fr = sg_fr;                                                                   \
        }

#define ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)                                                          \
        { pg_hd_ptr pg_hd;                                                                              \
          LOCK(Pg_lock(STR_PAGES));                                                                     \
          pg_hd = Pg_free_pg(STR_PAGES);                                                                \
          while (pg_hd == NULL) {                                                                       \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
            LOCK(Pg_lock(GLOBAL_PAGES_void));                                                           \
            if (Pg_free_pg(GLOBAL_PAGES_void)) {                                                        \
              pg_hd = Pg_free_pg(GLOBAL_PAGES_void);                                                    \
              Pg_free_pg(GLOBAL_PAGES_void) = PgHd_next(pg_hd);                                         \
              UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), 1);                                        \
              UNLOCK(Pg_lock(GLOBAL_PAGES_void));                                                       \
              INIT_PAGE(pg_hd, STR_PAGES, STR_TYPE);                                                    \
            } else if (GLOBAL_MAX_PAGES != Pg_pg_alloc(GLOBAL_PAGES_void)) {                            \
              ALLOC_PAGE(pg_hd);                                                                        \
              UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), 1);                                        \
              UNLOCK(Pg_lock(GLOBAL_PAGES_void));                                                       \
              INIT_PAGE(pg_hd, STR_PAGES, STR_TYPE);                                                    \
            } else {                                                                                    \
              UNLOCK(Pg_lock(GLOBAL_PAGES_void));                                                       \
              RECOVER_UNUSED_SPACE(STR_PAGES);                                                          \
              LOCK(Pg_lock(STR_PAGES));                                                                 \
              pg_hd = Pg_free_pg(STR_PAGES);                                                            \
            }                                                                                           \
          }                                                                                             \
          PgHd_str_in_use(pg_hd)++;                                                                     \
          STR = (STR_TYPE *) PgHd_free_str(pg_hd);                                                      \
          if ((PgHd_free_str(pg_hd) = (void *) STRUCT_NEXT(STR)) == NULL)                               \
            if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)                                     \
              PgHd_previous(PgHd_next(pg_hd)) = NULL;                                                   \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                                                    \
          UNLOCK(Pg_lock(STR_PAGES));                                                                   \
	}

#define ALLOC_NEXT_FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                \
        if ((STR = LOCAL_next_free_ans_node) == NULL) {                                                 \
          pg_hd_ptr pg_hd;                                                                              \
          LOCK(Pg_lock(STR_PAGES));                                                                     \
          pg_hd = Pg_free_pg(STR_PAGES);                                                                \
          while (pg_hd == NULL) {                                                                       \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
            LOCK(Pg_lock(GLOBAL_PAGES_void));                                                           \
            if (Pg_free_pg(GLOBAL_PAGES_void)) {                                                        \
              pg_hd = Pg_free_pg(GLOBAL_PAGES_void);                                                    \
              Pg_free_pg(GLOBAL_PAGES_void) = PgHd_next(pg_hd);                                         \
              UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), 1);                                        \
              UNLOCK(Pg_lock(GLOBAL_PAGES_void));                                                       \
              INIT_PAGE(pg_hd, STR_PAGES, STR_TYPE);                                                    \
            } else if (GLOBAL_MAX_PAGES != Pg_pg_alloc(GLOBAL_PAGES_void)) {                            \
              ALLOC_PAGE(pg_hd);                                                                        \
              UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), 1);                                        \
              UNLOCK(Pg_lock(GLOBAL_PAGES_void));                                                       \
              INIT_PAGE(pg_hd, STR_PAGES, STR_TYPE);                                                    \
            } else {                                                                                    \
              UNLOCK(Pg_lock(GLOBAL_PAGES_void));                                                       \
              RECOVER_UNUSED_SPACE(STR_PAGES);                                                          \
              LOCK(Pg_lock(STR_PAGES));                                                                 \
              pg_hd = Pg_free_pg(STR_PAGES);                                                            \
            }                                                                                           \
          }                                                                                             \
          PgHd_str_in_use(pg_hd) = Pg_str_per_pg(STR_PAGES);                                            \
          STR = (STR_TYPE *) PgHd_free_str(pg_hd);                                                      \
          PgHd_free_str(pg_hd) = NULL;                                                                  \
          Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd);                                                     \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), -PgHd_str_in_use(pg_hd));                              \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), Pg_str_per_pg(STR_PAGES));                             \
          UNLOCK(Pg_lock(STR_PAGES));                                                                   \
	}                                                                                               \
        LOCAL_next_free_ans_node = STRUCT_NEXT(STR)
#else
#define ALLOC_PAGE(PG_HD)                                                                               \
        LOCK(Pg_lock(GLOBAL_PAGES_void));                                                               \
        if (Pg_free_pg(GLOBAL_PAGES_void) == NULL) {                                                    \
          int i, shmid;                                                                                 \
          pg_hd_ptr pg_hd, aux_pg_hd;                                                                   \
          if ((shmid = shmget(IPC_PRIVATE, SHMMAX, SHM_R|SHM_W)) == -1)                                 \
            Yap_Error(FATAL_ERROR, TermNil, "shmget error (ALLOC_PAGE)");                               \
          if ((pg_hd = (pg_hd_ptr) shmat(shmid, NULL, 0)) == (void *) -1)                               \
            Yap_Error(FATAL_ERROR, TermNil, "shmat error (ALLOC_PAGE)");                                \
          if (shmctl(shmid, IPC_RMID, 0) != 0)                                                          \
            Yap_Error(FATAL_ERROR, TermNil, "shmctl error (ALLOC_PAGE)");                               \
          Pg_free_pg(GLOBAL_PAGES_void) = pg_hd;                                                        \
          for (i = 1; i < SHMMAX / Yap_page_size; i++) {                                                \
            aux_pg_hd = (pg_hd_ptr)(((void *)pg_hd) + Yap_page_size);                                   \
            PgHd_next(pg_hd) = aux_pg_hd;                                                               \
            pg_hd = aux_pg_hd;                                                                          \
          }                                                                                             \
          PgHd_next(pg_hd) = NULL;                                                                      \
          UPDATE_STATS(Pg_pg_alloc(GLOBAL_PAGES_void), SHMMAX / Yap_page_size);                         \
        }                                                                                               \
        UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), 1);                                              \
        PG_HD = Pg_free_pg(GLOBAL_PAGES_void);                                                          \
        Pg_free_pg(GLOBAL_PAGES_void) = PgHd_next(PG_HD);                                               \
        UNLOCK(Pg_lock(GLOBAL_PAGES_void))

#define ALLOC_STRUCT(STR, STR_PAGES, STR_TYPE)                                                          \
        { pg_hd_ptr pg_hd;                                                                              \
          LOCK(Pg_lock(STR_PAGES));                                                                     \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), 1);                                                    \
          if (Pg_free_pg(STR_PAGES)) {                                                                  \
            pg_hd = Pg_free_pg(STR_PAGES);                                                              \
            PgHd_str_in_use(pg_hd)++;                                                                   \
            STR = (STR_TYPE *) PgHd_free_str(pg_hd);                                                    \
            if ((PgHd_free_str(pg_hd) = (void *) STRUCT_NEXT(STR)) == NULL)                             \
              if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)                                   \
                PgHd_previous(PgHd_next(pg_hd)) = NULL;                                                 \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
          } else {                                                                                      \
            int i;                                                                                      \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), 1);                                                    \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
            ALLOC_PAGE(pg_hd);                                                                          \
            PgHd_str_in_use(pg_hd) = 1;                                                                 \
            PgHd_previous(pg_hd) = NULL;                                                                \
            STR = (STR_TYPE *) (pg_hd + 1);                                                             \
            PgHd_free_str(pg_hd) = (void *) ++STR;                                                      \
            for (i = Pg_str_per_pg(STR_PAGES); i != 2; i--) {                                           \
              STRUCT_NEXT(STR) = STR + 1;                                                               \
              STR++;                                                                                    \
            }                                                                                           \
            STRUCT_NEXT(STR) = NULL;                                                                    \
            STR = (STR_TYPE *) (pg_hd + 1);                                                             \
            LOCK(Pg_lock(STR_PAGES));                                                                   \
            if ((PgHd_next(pg_hd) = Pg_free_pg(STR_PAGES)) != NULL)                                     \
              PgHd_previous(PgHd_next(pg_hd)) = pg_hd;                                                  \
            Pg_free_pg(STR_PAGES) = pg_hd;                                                              \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
          }                                                                                             \
	}

#define ALLOC_NEXT_FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                \
        if ((STR = LOCAL_next_free_ans_node) == NULL) {                                                 \
          pg_hd_ptr pg_hd;                                                                              \
          LOCK(Pg_lock(STR_PAGES));                                                                     \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), Pg_str_per_pg(STR_PAGES));                             \
          if (Pg_free_pg(STR_PAGES)) {                                                                  \
            pg_hd = Pg_free_pg(STR_PAGES);                                                              \
            UPDATE_STATS(Pg_str_in_use(STR_PAGES), -PgHd_str_in_use(pg_hd));                            \
            PgHd_str_in_use(pg_hd) = Pg_str_per_pg(STR_PAGES);                                          \
            STR = (STR_TYPE *) PgHd_free_str(pg_hd);                                                    \
            PgHd_free_str(pg_hd) = NULL;                                                                \
            Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd);                                                   \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
          } else {                                                                                      \
            int i;                                                                                      \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), 1);                                                    \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
            ALLOC_PAGE(pg_hd);                                                                          \
            PgHd_str_in_use(pg_hd) = Pg_str_per_pg(STR_PAGES);                                          \
            PgHd_free_str(pg_hd) = NULL;                                                                \
            PgHd_previous(pg_hd) = NULL;                                                                \
            PgHd_next(pg_hd) = NULL;                                                                    \
            STR = (STR_TYPE *) (pg_hd + 1);                                                             \
            for (i = Pg_str_per_pg(STR_PAGES); i != 1; i--) {                                           \
              STRUCT_NEXT(STR) = STR + 1;                                                               \
              STR++;                                                                                    \
            }                                                                                           \
            STRUCT_NEXT(STR) = NULL;                                                                    \
            STR = (STR_TYPE *) (pg_hd + 1);                                                             \
          }                                                                                             \
	}                                                                                               \
        LOCAL_next_free_ans_node = STRUCT_NEXT(STR)
#endif /* LIMIT_TABLING */

#define FREE_PAGE(PG_HD)                                                                                \
        LOCK(Pg_lock(GLOBAL_PAGES_void));                                                               \
        UPDATE_STATS(Pg_str_in_use(GLOBAL_PAGES_void), -1);                                             \
        PgHd_next(PG_HD) = Pg_free_pg(GLOBAL_PAGES_void);                                               \
        Pg_free_pg(GLOBAL_PAGES_void) = PG_HD;                                                          \
        UNLOCK(Pg_lock(GLOBAL_PAGES_void))

#define FREE_STRUCT(STR, STR_PAGES, STR_TYPE)                                                           \
        { pg_hd_ptr pg_hd;                                                                              \
          pg_hd = PAGE_HEADER(STR);                                                                     \
          LOCK(Pg_lock(STR_PAGES));                                                                     \
          UPDATE_STATS(Pg_str_in_use(STR_PAGES), -1);                                                   \
          if (--PgHd_str_in_use(pg_hd) == 0) {                                                          \
            UPDATE_STATS(Pg_pg_alloc(STR_PAGES), -1);                                                   \
            if (PgHd_previous(pg_hd)) {                                                                 \
              if ((PgHd_next(PgHd_previous(pg_hd)) = PgHd_next(pg_hd)) != NULL)                         \
                PgHd_previous(PgHd_next(pg_hd)) = PgHd_previous(pg_hd);                                 \
	    } else {                                                                                    \
              if ((Pg_free_pg(STR_PAGES) = PgHd_next(pg_hd)) != NULL)                                   \
                PgHd_previous(PgHd_next(pg_hd)) = NULL;                                                 \
	    }                                                                                           \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
            FREE_PAGE(pg_hd);                                                                           \
	  } else {                                                                                      \
            if ((STRUCT_NEXT(STR) = (STR_TYPE *) PgHd_free_str(pg_hd)) == NULL) {                       \
              PgHd_previous(pg_hd) = NULL;                                                              \
              if ((PgHd_next(pg_hd) = Pg_free_pg(STR_PAGES)) != NULL)                                   \
                PgHd_previous(PgHd_next(pg_hd)) = pg_hd;                                                \
              Pg_free_pg(STR_PAGES) = pg_hd;                                                            \
            }                                                                                           \
            PgHd_free_str(pg_hd) = (void *) STR;                                                        \
            UNLOCK(Pg_lock(STR_PAGES));                                                                 \
          }                                                                                             \
        }
#endif /* --------------------------- MEMORY_ALLOC_SCHEME -------------------------------------------- */



#ifdef YAPOR
#define ALLOC_BLOCK(BLOCK, SIZE)                                                                        \
        if ((BLOCK = (void *) Yap_AllocCodeSpace(SIZE)) == NULL)                                        \
          Yap_Error(FATAL_ERROR, TermNil, "Yap_AllocCodeSpace error (ALLOC_BLOCK)")
#define FREE_BLOCK(BLOCK)                                                                               \
        Yap_FreeCodeSpace((char *) (BLOCK))
#else /* TABLING */
#define ALLOC_BLOCK(BLOCK, SIZE)                                                                        \
        if ((BLOCK = malloc(SIZE)) == NULL)                                                             \
          Yap_Error(FATAL_ERROR, TermNil, "malloc error (ALLOC_BLOCK)")
#define FREE_BLOCK(BLOCK)                                                                               \
        free(BLOCK)
#endif /* YAPOR - TABLING */

#define ALLOC_HASH_BUCKETS(BUCKET_PTR, NUM_BUCKETS)                                                     \
        { int i; void **ptr;                                                                            \
          ALLOC_BLOCK(ptr, NUM_BUCKETS * sizeof(void *));                                               \
          BUCKET_PTR = (void *) ptr;                                                                    \
          for (i = NUM_BUCKETS; i != 0; i--)                                                            \
            *ptr++ = NULL;                                                                              \
        }
#define FREE_HASH_BUCKETS(BUCKET_PTR)  FREE_BLOCK(BUCKET_PTR)

#define ALLOC_OR_FRAME(STR)            ALLOC_STRUCT(STR, GLOBAL_PAGES_or_fr, struct or_frame)
#define FREE_OR_FRAME(STR)             FREE_STRUCT(STR, GLOBAL_PAGES_or_fr, struct or_frame)

#define ALLOC_QG_SOLUTION_FRAME(STR)   ALLOC_STRUCT(STR, GLOBAL_PAGES_qg_sol_fr, struct query_goal_solution_frame)
#define FREE_QG_SOLUTION_FRAME(STR)    FREE_STRUCT(STR, GLOBAL_PAGES_qg_sol_fr, struct query_goal_solution_frame)

#define ALLOC_QG_ANSWER_FRAME(STR)     ALLOC_STRUCT(STR, GLOBAL_PAGES_qg_ans_fr, struct query_goal_answer_frame)
#define FREE_QG_ANSWER_FRAME(STR)      FREE_STRUCT(STR, GLOBAL_PAGES_qg_ans_fr, struct query_goal_answer_frame)

#define ALLOC_TG_SOLUTION_FRAME(STR)   ALLOC_STRUCT(STR, GLOBAL_PAGES_tg_sol_fr, struct table_subgoal_solution_frame)
#define FREE_TG_SOLUTION_FRAME(STR)    FREE_STRUCT(STR, GLOBAL_PAGES_tg_sol_fr, struct table_subgoal_solution_frame)

#define ALLOC_TG_ANSWER_FRAME(STR)     ALLOC_STRUCT(STR, GLOBAL_PAGES_tg_ans_fr, struct table_subgoal_answer_frame)
#define FREE_TG_ANSWER_FRAME(STR)      FREE_STRUCT(STR, GLOBAL_PAGES_tg_ans_fr, struct table_subgoal_answer_frame)

#define ALLOC_TABLE_ENTRY(STR)         ALLOC_STRUCT(STR, GLOBAL_PAGES_tab_ent, struct table_entry)
#define FREE_TABLE_ENTRY(STR)          FREE_STRUCT(STR, GLOBAL_PAGES_tab_ent, struct table_entry)

#define ALLOC_GLOBAL_TRIE_NODE(STR)    ALLOC_STRUCT(STR, GLOBAL_PAGES_gt_node, struct global_trie_node)
#define FREE_GLOBAL_TRIE_NODE(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_gt_node, struct global_trie_node)

#define ALLOC_SUBGOAL_TRIE_NODE(STR)   ALLOC_STRUCT(STR, GLOBAL_PAGES_sg_node, struct subgoal_trie_node)
#define FREE_SUBGOAL_TRIE_NODE(STR)    FREE_STRUCT(STR, GLOBAL_PAGES_sg_node, struct subgoal_trie_node)

#define ALLOC_SUBGOAL_FRAME(STR)       ALLOC_STRUCT(STR, GLOBAL_PAGES_sg_fr, struct subgoal_frame)
#define FREE_SUBGOAL_FRAME(STR)        FREE_STRUCT(STR, GLOBAL_PAGES_sg_fr, struct subgoal_frame)

#ifdef YAPOR
#define ALLOC_ANSWER_TRIE_NODE(STR)    ALLOC_NEXT_FREE_STRUCT(STR, GLOBAL_PAGES_ans_node, struct answer_trie_node)
#else /* TABLING */
#define ALLOC_ANSWER_TRIE_NODE(STR)    ALLOC_STRUCT(STR, GLOBAL_PAGES_ans_node, struct answer_trie_node)
#endif /* YAPOR - TABLING */
#define FREE_ANSWER_TRIE_NODE(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_ans_node, struct answer_trie_node)

#define ALLOC_DEPENDENCY_FRAME(STR)    ALLOC_STRUCT(STR, GLOBAL_PAGES_dep_fr, struct dependency_frame)
#define FREE_DEPENDENCY_FRAME(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_dep_fr, struct dependency_frame)

#define ALLOC_SUSPENSION_FRAME(STR)    ALLOC_STRUCT(STR, GLOBAL_PAGES_susp_fr, struct suspension_frame)
#define FREE_SUSPENSION_FRAME(STR)     FREE_BLOCK(SuspFr_global_start(STR));                         \
                                       FREE_STRUCT(STR, GLOBAL_PAGES_susp_fr, struct suspension_frame)

#define ALLOC_GLOBAL_TRIE_HASH(STR)    ALLOC_STRUCT(STR, GLOBAL_PAGES_gt_hash, struct global_trie_hash)
#define FREE_GLOBAL_TRIE_HASH(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_gt_hash, struct global_trie_hash)

#define ALLOC_SUBGOAL_TRIE_HASH(STR)   ALLOC_STRUCT(STR, GLOBAL_PAGES_sg_hash, struct subgoal_trie_hash)
#define FREE_SUBGOAL_TRIE_HASH(STR)    FREE_STRUCT(STR, GLOBAL_PAGES_sg_hash, struct subgoal_trie_hash)

#define ALLOC_ANSWER_TRIE_HASH(STR)    ALLOC_STRUCT(STR, GLOBAL_PAGES_ans_hash, struct answer_trie_hash)
#define FREE_ANSWER_TRIE_HASH(STR)     FREE_STRUCT(STR, GLOBAL_PAGES_ans_hash, struct answer_trie_hash)



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
