/**********************************************************************
                                                               
                       The OPTYap Prolog system                
  OPTYap extends the Yap Prolog system to support or-parallel tabling
                                                               
  Copyright:   R. Rocha and NCC - University of Porto, Portugal
  File:        opt.proto.h
  version:     $Id: opt.proto.h,v 1.12 2005-11-04 01:17:17 vsc Exp $   
                                                                     
**********************************************************************/

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

void Yap_init_global(int max_table_size, int n_workers, int sch_loop, int delay_load);
void Yap_init_local(void);
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


/* ------------- **
**  tab.tries.c  **
** ------------- */

#ifdef TABLING
sg_fr_ptr subgoal_search(yamop *preg, CELL **Yaddr);
ans_node_ptr answer_search(sg_fr_ptr sg_fr, CELL *subs_ptr);
void load_answer(ans_node_ptr ans_node, CELL *subs_ptr);
#ifdef GLOBAL_TRIE
CELL *load_substitution_variable(gt_node_ptr current_node, CELL *aux_stack_ptr);
#endif /* GLOBAL_TRIE */
void private_completion(sg_fr_ptr sg_fr);
#ifdef GLOBAL_TRIE
void free_subgoal_trie_branch(sg_node_ptr node, int nodes_left, int position);
#else
void free_subgoal_trie_branch(sg_node_ptr node, int nodes_left, int nodes_extra, int position);
#endif /* GLOBAL_TRIE */
void free_answer_trie_branch(ans_node_ptr node, int position);
void update_answer_trie(sg_fr_ptr sg_fr);
void show_table(tab_ent_ptr tab_ent, int show_mode);
#ifdef GLOBAL_TRIE
void show_global_trie(void);
#endif /* GLOBAL_TRIE */
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


/* ------------- **
**  or.*engine.c **
** ------------- */

#ifdef YAPOR
void make_root_choice_point(void);
void free_root_choice_point(void);
int q_share_work(int p);
int p_share_work(void);
#endif /* YAPOR */

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
