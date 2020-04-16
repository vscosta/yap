/*******************************************************************************************

Copyright (C) 2004,2005,2006,2007,2008 (Nuno A. Fonseca) <nuno.fonseca@gmail.com>

This program is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License 
as published by the Free Software Foundation; either 
version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


Last rev: $Id: yap_rl.c,v 1.1 2008-03-26 23:05:22 nunofonseca Exp $
**************************************************************************/

#include <time.h>
#include <stdio.h>

#include "range_list.h"
#include <YapInterface.h>

#define  IDTYPE YAP_Int
#define  PTR2ID(ptr) ((IDTYPE)(ptr))
#define  ID2PTR(id)  ((RL_Tree*)(id))


/* ############################################################ */
unsigned long int memory_usage=0;
unsigned long int tree_mem=0;

#define STORE_TREE_SIZE(tree) tree_mem=tree->mem_alloc
#define UPDATE_MEM_USAGE(tree) memory_usage+=(tree->mem_alloc>0?tree->mem_alloc-tree_mem:0)
#define FREE_MEM_USAGE(tree) (memory_usage-=tree->mem_alloc)
#define ADD_MEM_USAGE(tree) (memory_usage+=tree->mem_alloc)

/** @pred rl_new( ? Size, ? Tree).
 */
static
YAP_Bool 
rl_new(void) {
  YAP_Term t1=YAP_Deref(YAP_ARG1);
  YAP_Term t2=YAP_Deref(YAP_ARG2);
  RL_Tree* new_tree;
  IDTYPE  newid;

  // Check args
  if (!YAP_IsIntTerm(t1) || !YAP_IsVarTerm(t2)) {
    fprintf(stderr,"Error in rl_new arguments\n");
    return(FALSE);
  }
  //
  new_tree=new_rl(YAP_IntOfTerm(t1));
  if(new_tree==NULL) {
    fprintf(stderr,"Error creating new rl.");
    return (FALSE);
  }
  //printf("New rl %d %p--%u\n",PTR2ID(new_tree),new_tree,(int)new_tree,YAP_IntOfTerm(t1));
  // return reference
  newid=YAP_MkIntTerm(PTR2ID(new_tree));
  if(!YAP_Unify(YAP_Deref(YAP_ARG2),newid)) 
    return (FALSE);
  
  return(TRUE);
}
/* @pred rl_new( ? OldTree, ? NewTree).
 *
 * copy from old tree to mew tree
 */
static
YAP_Bool 
rl_copy(void) {
  YAP_Term t1=YAP_Deref(YAP_ARG1); // src
  YAP_Term t2=YAP_Deref(YAP_ARG2); // dest
  RL_Tree* new_tree;
  IDTYPE  id1,newid;
  RL_Tree* tree;

  // Check args
  if (!YAP_IsIntTerm(t1))
    return(FALSE);
  if (!YAP_IsVarTerm(t2)) 
    return(FALSE);
  //
  id1=YAP_IntOfTerm(t1);
  tree=ID2PTR(id1);
  new_tree=copy_rl(tree);

  if(new_tree==NULL) {
    fprintf(stderr,"Error creating new rl.");
    return (FALSE);
  }
  //
#ifdef STATS
  ADD_MEM_USAGE(new_tree);
#endif

  // return list reference
  newid=YAP_MkIntTerm(PTR2ID(new_tree));
  if(!YAP_Unify(YAP_Deref(YAP_ARG2),newid)) 
    return (FALSE);  
  return(TRUE);
}
/**  @pred rl_size( ? Tree, ? Size).
 *
 * 
 */
static
YAP_Bool 
rl_size(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1),t_size;
  IDTYPE id;
  RL_Tree* tree;
  unsigned int size;

  if (YAP_IsVarTerm(t1))
    return(FALSE);

  id = YAP_IntOfTerm(t1);
  tree=ID2PTR(id);
  
  size=tree->size*sizeof(RL_Node)+sizeof(RL_Tree);
  t_size=YAP_MkIntTerm(size);
  if(!YAP_Unify(YAP_ARG2,t_size) )   
    return (FALSE);
  
  return(TRUE);
}
/** @pred rl_new( ? AllTrees ).
 *
 *
 */
static
YAP_Bool 
rl_mem_usage(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);

  if(!YAP_Unify(t1,YAP_MkIntTerm(memory_usage)) )   
    return (FALSE);
  
  return(TRUE);
}

/**  @pred rl_free(  ? Tree).
 */
static
YAP_Bool 
rl_free(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  IDTYPE id;
  RL_Tree* tree;
  
  // Check args
  if (YAP_IsVarTerm(t1)) 
    return(FALSE);

  id=YAP_IntOfTerm(t1);
  tree=ID2PTR(id);
#ifdef STATS  
  FREE_MEM_USAGE(tree);
#endif

  free_rl(tree);

  return (TRUE);
}

/*
 * @pred rl_set_in(  + Tree, +Value )
 *
 */
static
YAP_Bool 
rl_set_in(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  YAP_Term t2=YAP_Deref(YAP_ARG2);
  IDTYPE id;
  NUM val;
  RL_Tree *tree;

  // Check args
  if (YAP_IsVarTerm(t1) || YAP_IsVarTerm(t2) )
    return(FALSE);

  id = YAP_IntOfTerm(t1);
  val = YAP_IntOfTerm(t2);

  tree=ID2PTR(id);

#ifdef STATS
  STORE_TREE_SIZE(tree);
  set_in_rl(tree,val,IN);
  UPDATE_MEM_USAGE(tree);
#else
  set_in_rl(tree,val,IN);
#endif
  return (TRUE);
}

#ifdef UNUSED
 /*
 *
 *
 */
static
YAP_Bool 
rl_in(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  YAP_Term t2=YAP_Deref(YAP_ARG2);
  IDTYPE id;
  NUM val;
  RL_Tree *tree;

  // Check args
  if (YAP_IsVarTerm(t1) || YAP_IsVarTerm(t2) )
    return(FALSE);

  id = YAP_IntOfTerm(t1);
  val = YAP_IntOfTerm(t2);

  tree=ID2PTR(id);

  if ( in_rl(tree,val) ) 
    return (TRUE);
  return (FALSE);
}
#endif

/*@pred rl_free(  ? Tree).
 *
 *
 */
static
YAP_Bool 
rl_set_out(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  YAP_Term t2=YAP_Deref(YAP_ARG2);
  IDTYPE id;
  NUM val;
  RL_Tree *tree;

  // Check args
  if (YAP_IsVarTerm(t1) || YAP_IsVarTerm(t2) )
    return(FALSE);

  id = YAP_IntOfTerm(t1);
  val = YAP_IntOfTerm(t2);

  tree=ID2PTR(id);
#ifdef STATS
  STORE_TREE_SIZE(tree);
  set_in_rl(tree,val,OUT);
  UPDATE_MEM_USAGE(tree);
#else
  set_in_rl(tree,val,OUT);
#endif
  return (TRUE);
}
/*
 *
 *
 */
static
YAP_Bool 
rl_freeze(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  IDTYPE id;
  RL_Tree *tree;

  // Check args
  if (YAP_IsVarTerm(t1) )
    return(FALSE);

  id = YAP_IntOfTerm(t1);
  tree=ID2PTR(id);
  

#ifdef STATS
  STORE_TREE_SIZE(tree);
  freeze_rl(tree);
  UPDATE_MEM_USAGE(tree);
#else
  freeze_rl(tree);
#endif

  return (TRUE);
}
/**  @pred rl_set_all(  + Tree, Els).
 * @addtogroup rl
 *
 */
  
static
YAP_Bool 
rl_set_all_in(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  IDTYPE id;
  RL_Tree *tree;

  // Check args
  if (YAP_IsVarTerm(t1) )
    return(FALSE);

  id = YAP_IntOfTerm(t1);
  tree=ID2PTR(id);
  
  
#ifdef STATS
  STORE_TREE_SIZE(tree);
  rl_all(tree,IN);
  freeze_rl(tree);
  UPDATE_MEM_USAGE(tree);
#else
  rl_all(tree,IN);
  freeze_rl(tree);
#endif

  return (TRUE);
}
/** @pred rl_print(  + Tree).
 *
 *
 */
static
YAP_Bool 
rl_print(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  IDTYPE id;
  RL_Tree *tree;

  // Check args
  if (YAP_IsVarTerm(t1) ) {
    fprintf(stderr,"Error printing tree..");
    return(FALSE);
  }
  id = YAP_IntOfTerm(t1);
  tree=ID2PTR(id);
  
  display_tree(tree);
  
  return (TRUE);
}


/* ==============================================================================
 *
 *
 */
typedef struct {
    YAP_Term last_solution;  /* the last solution */
} yap_back_data_type;

yap_back_data_type *back_data;

/*
 *
 *
 */
static
YAP_Bool 
rl_b_in2(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  IDTYPE id;
  NUM val;
  RL_Tree *tree;

  YAP_PRESERVED_DATA(back_data,yap_back_data_type);
  id = YAP_IntOfTerm(t1);
  tree=ID2PTR(id);
  val=YAP_IntOfTerm(back_data->last_solution);
  val=rl_next_in_bigger(tree,val);
  if ( val > 0 && YAP_Unify(YAP_Deref(YAP_ARG2),YAP_MkIntTerm(val))) {
    back_data->last_solution=YAP_MkIntTerm(val);
    return TRUE;
  }
  YAP_cut_fail();
  return (FALSE); 
}
static
YAP_Bool   
rl_b_in1(void) {

  YAP_Term t1=YAP_Deref(YAP_ARG1);
  YAP_Term t2=YAP_Deref(YAP_ARG2);
  IDTYPE id;
  NUM val;
  RL_Tree *tree;

  // Check args
  if (!YAP_IsIntTerm(t1)) {
    YAP_cut_fail();
    return(FALSE);
  }
  if ( YAP_IsVarTerm(t2) ) {
    // return all in through backtracking
    YAP_PRESERVE_DATA(back_data,yap_back_data_type);
    back_data->last_solution = YAP_MkIntTerm(0);
    return rl_b_in2();
  } else {
    id = YAP_IntOfTerm(t1);
    tree=ID2PTR(id);
    val = YAP_IntOfTerm(t2);
    if ( in_rl(tree,val) ) {
      YAP_cut_succeed();
      return (TRUE);
    }
    YAP_cut_fail();
    return (FALSE);
  }
}
/* ******************************************************* */
void init_rl(void);

void init_rl(void){

     
 YAP_UserCPredicate("rl_new", rl_new,2);        //  Maximum -> RangeID
 YAP_UserCPredicate("rl_free", rl_free,1);      //  RangeId ->
 YAP_UserCPredicate("rl_size", rl_size,2);      //  RangeId -> Size (in bytes)
 YAP_UserCPredicate("rl_mem", rl_mem_usage,1);  //  -> TotalMemory (in bytes)

 YAP_UserCPredicate("rl_copy", rl_copy,2);      //  RangeId  -> NewRangeId
 YAP_UserCPredicate("rl_set_out", rl_set_out,2);//  RangeId x Number  ->
 YAP_UserBackCPredicate("rl_in", rl_b_in1,rl_b_in2,2,sizeof(yap_back_data_type));   //  +RangeId x ?Number 
 //YAP_UserCPredicate("rl_in", rl_in,2);        //  RangeId x Number  ->
 YAP_UserCPredicate("rl_set_in", rl_set_in,2);  //  RangeIdxNumber  -> 
 YAP_UserCPredicate("rl_set_all_in", rl_set_all_in,1); //  RangeId -> 

 YAP_UserCPredicate("rl_print", rl_print,1);    //  RangeId ->

 YAP_UserCPredicate("rl_freeze", rl_freeze,1);  //  RangeId
 
 // fprintf(stderr,"Range list  module succesfully loaded.");
 //fflush(stderr);
}
