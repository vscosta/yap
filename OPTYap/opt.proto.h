/* -------------------------------------- **
**      Prototypes for opt.*.c files      **
** -------------------------------------- */

/* -------------- **
**  opt.memory.c  **
** -------------- */

#ifdef YAPOR
#ifdef SHM_MEMORY_MAPPING_SCHEME
void shm_map_memory(int id, int size, void *shmaddr);
#else /* MMAP_MEMORY_MAPPING_SCHEME */
void open_mapfile(long);
void close_mapfile(void);
#endif /* MEMORY_MAPPING_SCHEME */
void map_memory(long HeapArea, long GlobalLocalArea, long TrailAuxArea, int n_workers);
void unmap_memory(void);
void remap_memory(void);
#endif /* YAPOR */


/* ------------ **
**  opt.misc.c  **
** ------------ */

void itos(int i, char *s);
void information_message(const char *mesg,...);
#if defined(YAPOR_ERRORS) || defined(TABLING_ERRORS)
void error_message(const char *mesg, ...);
#endif /* YAPOR_ERRORS || TABLING_ERRORS */


/* ------------ **
**  opt.init.c  **
** ------------ */

void init_global(int n_workers, int sch_loop, int delay_load);
void init_local(void);
void make_root_frames(void);
#ifdef YAPOR
void init_workers(void);
#endif /* YAPOR */


/* ------------- **
**  opt.preds.c  **
** ------------- */

#ifdef YAPOR
void finish_yapor(void);
#endif /* YAPOR */



/* -------------------------------------- **
**      Prototypes for tab.*.c files      **
** -------------------------------------- */

/* ------------- **
**  tab.tries.c  **
** ------------- */

#ifdef TABLING
#include <stdio.h>
sg_fr_ptr subgoal_search(tab_ent_ptr tab_ent, OPREG arity, CELL **Yaddr);
ans_node_ptr answer_search(sg_fr_ptr sg_fr, CELL *subs_ptr);
void load_answer_trie(ans_node_ptr ans_node, CELL *subs_ptr);
void private_completion(sg_fr_ptr sg_fr);
void free_subgoal_trie_branch(sg_node_ptr node, int missing_nodes);
void free_answer_trie_branch(ans_node_ptr node);
void update_answer_trie(sg_fr_ptr sg_fr);
void traverse_trie(FILE *stream, sg_node_ptr sg_node, int pred_arity, Atom pred_atom, int show);
#endif /* TABLING */


/* --------------- **
**  tab.suspend.c  **
** --------------- */

#if defined(TABLING) && defined(YAPOR)
void public_completion(void);
void complete_suspension_frames(or_fr_ptr or_fr);
void suspend_branch(void);
void resume_suspension_frame(susp_fr_ptr resume_fr, or_fr_ptr top_or_fr);
#endif /* TABLING && YAPOR */



/* ------------------------------------- **
**      Prototypes for or.*.c files      **
** ------------------------------------- */

/* ------------- **
**  or.engine.c  **
** ------------- */

#ifdef ENV_COPY
void make_root_choice_point(void);
void free_root_choice_point(void);
int q_share_work(int p);
void p_share_work(void);
#endif /* ENV_COPY */


/* ---------------- **
**  or.cowengine.c  **
** ---------------- */

#ifdef ACOW
void make_root_choice_point(void);
void free_root_choice_point(void);
int q_share_work(int p);
int p_share_work(void);
#endif /* ACOW */


/* ---------------- **
**  or.sbaengine.c  **
** ---------------- */

#ifdef SBA
void make_root_choice_point(void);
void free_root_choice_point(void);
int q_share_work(int p);
void p_share_work(void);
#endif /* SBA */


/* ---------------- **
**  or.scheduler.c  **
** ---------------- */

#ifdef YAPOR
int get_work(void);
#endif /* YAPOR */


/* ---------- **
**  or.cut.c  **
** ---------- */

#ifdef YAPOR
void prune_shared_branch(choiceptr prune_cp);
#endif /* YAPOR */
