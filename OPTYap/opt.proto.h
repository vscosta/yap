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

/***************************
**      opt.memory.c      **
***************************/

#ifdef YAPOR
#ifdef SHM_MEMORY_MAPPING_SCHEME
void shm_map_memory(int, int, void *);
#else /* MMAP_MEMORY_MAPPING_SCHEME */
void open_mapfile(long);
void close_mapfile(void);
#endif /* MEMORY_MAPPING_SCHEME */
void map_memory(long, long, long, int);
void unmap_memory(void);
void remap_memory(void);
#endif /* YAPOR */



/*************************
**      opt.init.c      **
*************************/

void Yap_init_global(int, int, int, int);
void Yap_init_local(void);
void make_root_frames(void);
#ifdef YAPOR
void init_workers(void);
#endif /* YAPOR */
void itos(int, char *);



/**************************
**      opt.preds.c      **
**************************/

#ifdef YAPOR
void finish_yapor(void);
#endif /* YAPOR */



/**************************
**      tab.tries.c      **
**************************/

#ifdef TABLING
sg_fr_ptr subgoal_search(yamop *, CELL **);
ans_node_ptr answer_search(sg_fr_ptr, CELL *);
void load_answer(ans_node_ptr, CELL *);
CELL *exec_substitution(gt_node_ptr, CELL *);
void update_answer_trie(sg_fr_ptr);
void free_subgoal_trie(sg_node_ptr, int, int);
void free_answer_trie(ans_node_ptr, int, int);
void free_subgoal_hash_chain(sg_hash_ptr);
void free_answer_hash_chain(ans_hash_ptr);
void show_table(tab_ent_ptr, int);
void show_global_trie(int);
#endif /* TABLING */



/*******************************
**      tab.completion.c      **
*******************************/

#ifdef TABLING 
void private_completion(sg_fr_ptr);
#ifdef YAPOR
void public_completion(void);
void complete_suspension_frames(or_fr_ptr);
void suspend_branch(void);
void resume_suspension_frame(susp_fr_ptr, or_fr_ptr);
#endif /* YAPOR */
#endif /* TABLING */



/**************************
**      or.engine.c      **
**************************/

#ifdef YAPOR
void make_root_choice_point(void);
void free_root_choice_point(void);
int q_share_work(int p);
int p_share_work(void);
#endif /* YAPOR */



/*****************************
**      or.scheduler.c      **
*****************************/

#ifdef YAPOR
int get_work(void);
#endif /* YAPOR */



/***********************
**      or.cut.c      **
***********************/

#ifdef YAPOR
void prune_shared_branch(choiceptr);
#endif /* YAPOR */
