/*******************************************************************************************

Copyright (C) 2004,2005,2006,2007,2008 (Nuno A. Fonseca)
<nuno.fonseca@gmail.com>

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


Last rev: $Id: range_list.c,v 1.1 2008-03-26 23:05:22 nunofonseca Exp $
**************************************************************************/

/**
 * @file range_list.c
 *
 * @brief Nuno Fonseca range list implementation.
 *
 * @namespace rltree
 *
 */

#include "range_list.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

void set_num_bit(unsigned int number, char *storage, STATUS status);
BOOLEAN is_num_bit(unsigned int number, char *storage, STATUS status);

static void set_quadrant(RL_Node *node, short quadrant, QUADRANT_STATUS status);
static QUADRANT_STATUS quadrant_status(RL_Node *node, short quadrant);

static void quadrant_interval(RL_Tree *tree, short quadrant, NUM interval,
                              NUM *quad_interval);
static NUM get_quadrant_node(RL_Tree *tree, NUM node, short quadrant,
                             NUM interval);
static unsigned int tree_size(RL_Tree *tree, NUM node, NUM);

int get_location(RL_Tree *tree, NUM node, short quadrant, NUM interval);

long set_in(NUM number, NUM node, NUM node_num, NUM interval, NUM max,
            RL_Tree *tree, STATUS status);
long compact_node(RL_Tree *, NUM node, NUM next_node, NUM node_interval,
                  NUM next_node_interval, NUM next_node_num, short quadrant,
                  NUM max);

BOOLEAN in_tree(NUM number, RL_Tree *tree, NUM node, NUM node_num,
                NUM interval);
void display_tree(RL_Tree *tree);
void idisplay_tree(RL_Tree *tree, NUM node, NUM node_num, NUM interval,
                   NUM max);
static void display_leaf(RL_Tree *tree, NUM node, NUM node_num, NUM max);

NUM new_node(RL_Tree *tree, NUM node_father, short quadrant, NUM node_num,
             NUM quad_min, NUM quad_max, STATUS);
static void root_intervals(RL_Tree *tree);

NUM next_min(RL_Tree *tree, NUM node, NUM node_num, NUM interval, NUM max,
             NUM min);
NUM tree_minus(RL_Tree *r1, RL_Tree *r2, NUM node1, NUM node2, NUM node_num,
               NUM interval, NUM max);

RL_Tree *minus_rl(RL_Tree *range1, RL_Tree *range2);
void shift_right(RL_Tree *tree, const NUM idx, const long nnodes);
void shift_left(RL_Tree *tree, const NUM idx, const long nnodes);
void intersect_leafs(char *storage1, char *storage2);

// static void print_nodes(RL_Tree* tree);

//
RL_Buffer *buffer = NULL;
unsigned int active_bits[16] = {1,    3,     7,     15,   31,   63,
                                127,  255,   511,   1023, 2047, 4095,
                                8191, 16383, 32767, 65535};

/*****************************************************************************/
/*
 *
 *
 */
RL_Tree *new_rl(NUM max_size) {

  RL_Tree *new;
  RL_Node *buf_ptr;
  short q;
  NUM qi, tmp;

  if (max_size < 2)
    max_size = 2;

  new = (RL_Tree *)malloc(sizeof(RL_Tree));
  if (new == NULL)
    return NULL;

  new->range_max = max_size;
  root_intervals(new);

  // alloc a block for the nodes
  new->root = (RL_Node *)calloc(1, NODE_SIZE);
  new->size = 1;
  new->mem_alloc = NODE_SIZE; // memory allocated

  // reset buffer
  buf_ptr = new->root; // tree_buffer();
  ALL_OUT(
      &buf_ptr[0]); // Initialize all numbers as being out of the range/interval
  buf_ptr[0].i_node.num_subnodes = 1;
  new->root = buf_ptr; // pointer to the buffer

  buf_ptr->i_node.num_subnodes = 1;
  quadrant_interval(new, 1, max_size, &qi);
  tmp = qi + 1;
  for (q = 2; q <= BRANCH_FACTOR; ++q) {
    if (max_size < qi * (q - 1) + 1) // 16 32 48 64 - 32
      set_quadrant(new->root, q, R_IGNORE);
    tmp += qi; // max_size=16  16+1
  }

  return new;
}
/*
 *
 *
 */
RL_Tree *copy_rl(RL_Tree *tree) {

  RL_Tree *new;
  RL_Node *buf_ptr;

  new = (RL_Tree *)malloc(sizeof(RL_Tree));
  buf_ptr = (RL_Node *)calloc(tree->size, NODE_SIZE);
  if (new == NULL) {
    printf("new==NULL");
    free(buf_ptr);
    return NULL;
  }
  if (buf_ptr == NULL) {
    printf("buf_ptr==NULL---%lu", tree->size);
    free(new);
    return NULL;
  }
  memmove(new, tree, sizeof(RL_Tree));
  memmove(buf_ptr, &tree->root[0], tree->size * NODE_SIZE);
  new->root = buf_ptr;
  new->mem_alloc = tree->size *NODE_SIZE;
  return new;
}
/*
 *
 *
 */
void free_rl(RL_Tree *range) {

  // free nodes block
  if (range->mem_alloc != 0)
    free(range->root);
  //
  free(range);
}
/*

 */
RL_Tree *set_in_rl(RL_Tree *tree, NUM number, STATUS status) {

  /* */
  if (number > 0 && number <= tree->range_max)
    set_in(number, ROOT(tree), 1, ROOT_INTERVAL(tree), tree->range_max, tree,
           status);
#ifdef DEBUG
  printf("Setting: %lu  size=%lu\n", number, tree->size);
#endif
  /*if (status==IN && !in_rl(tree,number)) {
    fprintf(stderr,"Error adding %lu to tree: size=%lu
    max=%lu\n",number,tree->size,tree->range_max);
    display_tree(tree);
    exit(1);
    }*/
  return tree;
}
/*
 * Mark all examples in range IN/OUT
 */
void rl_all(RL_Tree *tree, STATUS status) {
  int i;

  for (i = 1; i <= BRANCH_FACTOR; ++i)
    if (quadrant_status(NODE(tree, ROOT(tree)), i) != R_IGNORE) {
      if (status == IN)
        set_quadrant(NODE(tree, ROOT(tree)), i, R_TOTALLY_IN_INTERVAL);
      else
        set_quadrant(NODE(tree, ROOT(tree)), i, R_NOT_IN_INTERVAL);
    }
  tree->size = 1;
}
/*
 *
 *
 */
BOOLEAN in_rl(RL_Tree *tree, NUM number) {
  if (number < 1 && number > tree->range_max)
    return FALSE;
  return in_tree(number, tree, ROOT(tree), 1, ROOT_INTERVAL(tree));
}
/*
 *
 *
 */
BOOLEAN freeze_rl(RL_Tree *range) {

  //  reduce memory usage if possible
  NUM s = range->size * NODE_SIZE;
  if (s < range->mem_alloc) {
    range->root = (RL_Node *)realloc(range->root, s);
    range->mem_alloc = s;
  }
  return TRUE;
}
/*
 * Returns range1 without the numbers in range2
 * Constraint:range1->max==range2->max
 */
RL_Tree *minus_rl(RL_Tree *range1, RL_Tree *range2) {
  if (range1->range_max != range2->range_max)
    return NULL;
  //!!!!tree_minus(range1,range2,ROOT(range1),ROOT(range2),1,ROOT_INTERVAL(range1),range1->range_max);
  return range1;
}

/*
 * Returns next number in tree bigger than min
 */
NUM rl_next_in_bigger(RL_Tree *tree, NUM min) {
  if (tree == NULL) {
    fprintf(stdout, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!%lu\n", min);
  }
  return next_min(tree, ROOT(tree), 1, ROOT_INTERVAL(tree), tree->range_max,
                  min + 1);
}
/* ******************************************************************************
   Private Functions
   ******************************************************************************
   */
/*
static void print_nodes(RL_Tree* tree) {
  RL_Node* nodes=tree->root;
  int j;

  for(j=0;j<tree->size;++j)
    printf("[%d]=%lu\n",j,(unsigned long int)nodes[j].leaf);

}
*/

// treeXquadrantXinterval->quadrant_minXquadrant_max
static void quadrant_interval(RL_Tree *tree, short quadrant, NUM interval,
                              NUM *quad_interval) {

  if (IS_ROOT(tree, interval)) {
    *quad_interval = tree->root_i;
  } else {
    *quad_interval = NEXT_INTERVAL(interval);
  }
}

// numberXtreeXinterval->quadrantXquadrant_minXquadrant_max
static void number_quadrant(NUM number, RL_Tree *tree, NUM node_interval,
                            NUM node_num, short *quadrant, NUM *quad_min,
                            NUM *quad_max) {
  NUM tmp = node_num - 1, quad_interval;
  int i;
  quadrant_interval(tree, 1, node_interval, &quad_interval);
  i = (number - node_num) / quad_interval + 1;
  tmp = node_num - 1 + quad_interval * i;
  *quad_max = tmp;
  *quadrant = i;
  *quad_min = tmp - quad_interval + 1;
  // printf("number=%lu node num=%lu quad_interval=%lu-------> quadrant=%d
  // quad_max=%lu\n",number,node_num,quad_interval,i,tmp);
}

/*
 * returns the index to the quadrant "quadrant" node
 */
static NUM get_quadrant_node(RL_Tree *tree, NUM node, short quadrant,
                             NUM interval) {
  int d = get_location(tree, node, quadrant, interval);
  return node + d;
}
/*                 src    s
 *  src= 1 2 3 4 5 6 _ _
 *  offset= 2
 *  nbytes=6
 *  >>>src= 1 2 1 2 3 4 5 6
 *                    src    s
 */
void shift_right(RL_Tree *tree, const NUM idx, const long nnodes) {
  long n = idx + nnodes;
  RL_Node *s = tree->root;

  if (nnodes <= 0)
    return;
  // print_nodes(tree);
  while (n >= idx) {
    s[n + 1].leaf = s[n].leaf;
    --n;
  }
  // print_nodes(tree);
  // printf(">>----------------\n");
}

void shift_left(RL_Tree *tree, const NUM idx, const long nnodes) {
  long n = idx;
  RL_Node *s = tree->root;

  // printf("sfit left: idx=%u nnodes=%u max=%u\n",idx,nnodes,tree->size);
  if (nnodes <= 0) // last element
    return;

  //  print_nodes(tree);
  while (n < idx + nnodes) {
    s[n].leaf = s[n + 1].leaf;
    ++n;
    ;
  }
  //  print_nodes(tree);
  // printf("<<----------------\n");
}

/*
 *
 *
 */
NUM new_node(RL_Tree *tree, NUM node_father, short quadrant,
             NUM father_interval, NUM quad_min, NUM quad_max, STATUS status) {
  // RL_Node *new,*root_node=tree->root;
  NUM new_interval = +NEXT_INTERVAL(father_interval);
  NUM times;
  NUM new;
  RL_Node *ptr;
  new = get_quadrant_node(tree, node_father, quadrant, father_interval);

  if (tree->mem_alloc != 0) {
    // increase array size and shift elements right
    if (REALLOC_MEM(tree)) {
      // printf("new node:resizing memory: current %lu -> new %lu
      // [%lu]\n",tree->mem_alloc,MEM_SIZE(tree),tree->size);
      ptr = (RL_Node *)realloc(tree->root, MEM_SIZE(tree));
      if (ptr == NULL) {
        fprintf(stderr, "Fatal error:range_list: Unable to allocate memory");
        exit(1);
      }
      tree->root = ptr;
      tree->mem_alloc = MEM_SIZE(tree);
    }
    // SHIFT elements at the right and including the current node one position
    times = tree->size - 1 - new;
    shift_right(tree, new, times);
    //      SHIFT_NODES((void*)new,times*NODE_SIZE);
  }
  // update father reference
  set_quadrant(NODE(tree, node_father), quadrant, R_PARCIALLY_IN_INTERVAL);
  // initialize node
  if (status == IN) {
    ALL_OUT(NODE(tree, new)); // clear all bits
    if (!IS_LEAF(new_interval)) {
      short q;
      RL_Node *node_ptr = NODE(tree, new);
      node_ptr->i_node.num_subnodes = 1;
      for (q = 2; q <= BRANCH_FACTOR; ++q)
        if (MIN(quad_max, tree->range_max) <
            quad_min +
                NEXT_INTERVAL(new_interval) * (q - 1)) // QUADRANT_MAX_VALUE(
          set_quadrant(NODE(tree, new), q, R_IGNORE);
    }
  } else {
    // status ==out
    // SET_LEAF_IN(tree->range_max,NODE(tree,new),quad_min);
    tree->root[new].leaf = ON_BITS(MIN(16, tree->range_max - quad_min + 1));
    if (!IS_LEAF(new_interval)) {
      short q;
      RL_Node *node_ptr = NODE(tree, new);
      node_ptr->i_node.num_subnodes = 1;
      node_ptr->i_node.quadrant_1 = node_ptr->i_node.quadrant_2 =
          node_ptr->i_node.quadrant_3 = node_ptr->i_node.quadrant_4 =
              R_TOTALLY_IN_INTERVAL;
      for (q = 2; q <= BRANCH_FACTOR; ++q)
        if (MIN(quad_max, tree->range_max) <
            quad_min +
                NEXT_INTERVAL(new_interval) * (q - 1)) // QUADRANT_MAX_VALUE(
          set_quadrant(NODE(tree, new), q, R_IGNORE);
    }
  }
  // update tree size
  tree->size++;
  return new;
}

/*
 * returns the offset
 *
 */
int get_location(RL_Tree *tree, NUM node, short quadrant, NUM node_interval) {
  int i, c = 1, tmp;
  NUM next_node;
  NUM next_interval;

  if (quadrant == 1 || IS_LEAF(node_interval))
    return 1;

  //
  if (LAST_LEVEL_INODE(node_interval)) {
    // 1 node = current
    for (i = 1; i < quadrant; ++i) {
      if (quadrant_status(NODE(tree, node), i) == R_PARCIALLY_IN_INTERVAL)
        ++c;
    }
    return c;
  }

  //
  // internal range list nodes
  quadrant_interval(tree, quadrant, node_interval, &next_interval);
  i = 1;
  next_node = node + 1;
  while (i != quadrant && i <= BRANCH_FACTOR) {
    if (quadrant_status(NODE(tree, node), i) == R_PARCIALLY_IN_INTERVAL) {
      tmp = tree_size(tree, next_node, next_interval);
      next_node += tmp;
      c += tmp;
    }
    ++i;
  }

  return c;
}
/*
 * Returns the number of nodes created/deleted.
 *
 * number: number to insert from the interval
 * node:   index of current node
 * node_num: number corresponding to the beginning o the interval represented by
 * node
 * interval: size of the interval represented in the current node
 */
long set_in(NUM number, NUM node, NUM node_num, NUM node_interval, NUM max,
            RL_Tree *tree, STATUS status) {
  NUM next_node;
  long ret_val = tree->size, compacted;
  NUM interval = node_interval;
  NUM quad_min, quad_max;
  short quadrant;
  NUM size;
  /* */
  if (IS_LEAF(interval)) {
    // current node is a leaf
    set_num_bit(number - node_num, (char *)NODE(tree, node), status);
    return 0;
  }
  //
  number_quadrant(number, tree, node_interval, node_num, &quadrant, &quad_min,
                  &quad_max);
  interval = quad_max - quad_min + 1;
  // select next node
  switch (status) {
  case IN:
    // move pointer to next node
    if (quadrant_status(NODE(tree, node), quadrant) == R_NOT_IN_INTERVAL) {
      // new node
      // display_tree(tree);
      next_node = new_node(tree, node, quadrant, node_interval, quad_min,
                           quad_max, status);
    } else if (quadrant_status(NODE(tree, node), quadrant) ==
               R_TOTALLY_IN_INTERVAL)
      return 0;
    else
      next_node = get_quadrant_node(tree, node, quadrant, node_interval);
    break;
  case OUT:
    if (quadrant_status(NODE(tree, node), quadrant) == R_TOTALLY_IN_INTERVAL) {
      // new node
      next_node = new_node(tree, node, quadrant, node_interval, quad_min,
                           quad_max, status);
    } else if (quadrant_status(NODE(tree, node), quadrant) == R_NOT_IN_INTERVAL)
      return 0;
    else
      next_node = get_quadrant_node(tree, node, quadrant, node_interval);
    break;
  default:
    printf("set_in: invalid number status %d\n", status);
    exit(1);
  }
  // insert in tree
  set_in(number, next_node, quad_min, interval, quad_max, tree, status);
  ret_val = tree->size - ret_val; // number of nodes added/removed
  // compact tree: only if we didn't create new nodes
  // compacted=compact_node(tree,node,next_node,node_interval,interval,quad_min,quadrant,MIN(quad_max,tree->range_max));
  compacted = 0;
  if (compacted == -1) {
    // NUM times=tree->size-1-next_node; // -1 because array position 0
    shift_left(tree, next_node, 1);
    // update tree size
    tree->size += compacted;
    ret_val += compacted;
    // ret_val=0;//compacted;
  }
  // update subnodes number
  if (tree->root[node].i_node.num_subnodes == 255)
    size = tree_size(tree, node, interval);
  else
    size = ret_val + tree->root[node].i_node.num_subnodes; // new subnodes value

  if (size > 254)
    tree->root[node].i_node.num_subnodes = 255;
  else
    tree->root[node].i_node.num_subnodes = size;

  //  if (size <0 ) exit(1);
  return ret_val;
}
/*
 * Check if can change quadrant color of node. If it changes, the node is
 * deleted and all nodes at right in the array are shifted one position.
 *
 */
long compact_node(RL_Tree *tree, NUM node, NUM next_node, NUM node_interval,
                  NUM next_node_interval, NUM next_node_num, short quadrant,
                  NUM max) {
  unsigned int j;

  RL_Node *node_ptr = NODE(tree, next_node); // next node pointer

  // Try to compact a leaf
  if (IS_LEAF(next_node_interval)) {
#ifdef DEBUG
    fprintf(stderr, "compact_node: interval node\n");
#endif
    // ALL IN
    if (LEAF_ALL_IN(node_ptr->leaf)) {
      set_quadrant(NODE(tree, node), quadrant, R_TOTALLY_IN_INTERVAL);
      return -1;
    }
    // ALL IN: part II
    // The last node does not need to be all in
    if (max - next_node_num + 1 <= LEAF_SIZE) {
      j = ON_BITS(max - next_node_num + 1); // 153,154,155,156,157,.,.,.,[158
                                            // -> valor do max=200 devia ser 158
      if (node_ptr->leaf == j) {
        set_quadrant(NODE(tree, node), quadrant, R_TOTALLY_IN_INTERVAL);
        return -1;
      }
    }
    // ALL OUT
    if (LEAF_ALL_OUT(node_ptr->leaf)) {
      set_quadrant(NODE(tree, node), quadrant, R_NOT_IN_INTERVAL);
#ifdef DEBUG
      printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>compacted leaf1\n");
#endif
      return -1;
    }
  } else {
#ifdef DEBUG
    fprintf(stderr, "compact_node:range node\n");
#endif
    // INODE - range list node
    if (node_ptr->i_node.num_subnodes > 1) // unable to compact
      return 0;
    // ALL IN
    for (j = 1; j <= BRANCH_FACTOR; ++j)
      if (quadrant_status(NODE(tree, next_node), j) != R_IGNORE &&
          quadrant_status(NODE(tree, next_node), j) != R_TOTALLY_IN_INTERVAL)
        break;

    if (j > BRANCH_FACTOR) {
      set_quadrant(NODE(tree, node), quadrant, R_TOTALLY_IN_INTERVAL);
      return -1;
    }
    // ALL OUT
    for (j = 1; j <= BRANCH_FACTOR; ++j)
      if (quadrant_status(NODE(tree, next_node), j) != R_IGNORE &&
          quadrant_status(NODE(tree, next_node), j) != R_NOT_IN_INTERVAL)
        break;

    if (j > BRANCH_FACTOR) {
      set_quadrant(NODE(tree, node), quadrant, R_NOT_IN_INTERVAL);
      return -1;
    }
  }
  return 0;
}

/*
 * interval: interval associated to the node
 */
static unsigned int tree_size(RL_Tree *tree, NUM node, NUM interval) {
  unsigned int c = 1, tmp;
  int i = 1;
  short status;
  NUM next_interval;
  NUM next_node;
  RL_Node *node_ptr = NODE(tree, node);

  if (IS_LEAF(interval))
    return 1;

  if (node_ptr->i_node.num_subnodes == 255) {
    // compute the size of all subtrees
    next_interval = NEXT_INTERVAL(interval);
    for (i = 1; i <= BRANCH_FACTOR; ++i) {
      status = quadrant_status(NODE(tree, node), i);
      switch (status) {
      case R_PARCIALLY_IN_INTERVAL:
        next_node = node + c; //
        tmp = tree_size(tree, next_node, next_interval);
        c += tmp;
        //      default:
      }
    }
  } else
    c = node_ptr->i_node.num_subnodes;
  return c;
}
/*
 * number >=1 && number <=16
 */
void set_num_bit(unsigned int number, char *storage, STATUS status) {
  if (number >= 8) {
    storage++;
    number = number - 8; // =-8
  }
  if (status == IN)
    BITMAP_insert(*storage, number);
  else
    BITMAP_delete(*storage, number);
}
/*
 */
BOOLEAN is_num_bit(unsigned int number, char *storage, STATUS status) {
  if (number >= 8) {
    storage++;
    number = number - 8; // =-8
  }
  if (status == IN)
    return BITMAP_member(*storage, number);
  else
    return !BITMAP_member(*storage, number);
}
/*
 *
 */
static void set_quadrant(RL_Node *node, short quadrant,
                         QUADRANT_STATUS status) {

  switch (quadrant) {
  case 1:
    node->i_node.quadrant_1 = status;
    break;
  case 2:
    node->i_node.quadrant_2 = status;
    break;
  case 3:
    node->i_node.quadrant_3 = status;
    break;
  case 4:
    node->i_node.quadrant_4 = status;
    break;
  default:
    fprintf(stderr, "ERROR: set_quadrant: invalid quadrant %d(%d)\n", quadrant,
            status);
  }
}
/*
 *
 */
static QUADRANT_STATUS quadrant_status(RL_Node *node, short quadrant) {

  switch (quadrant) {
  case 1:
    return node->i_node.quadrant_1;
  case 2:
    return node->i_node.quadrant_2;
  case 3:
    return node->i_node.quadrant_3;
  case 4:
    return node->i_node.quadrant_4;
  default:
    fprintf(stderr, "ERROR: quadrant_status: invalid quadrant(%d)\n", quadrant);
  }
  return 0;
}
/*
 *
 *
 */
static BOOLEAN in_leaf(NUM number, RL_Tree *tree, NUM node, NUM node_num,
                       NUM max) {

  if (is_num_bit(number - node_num, (char *)NODE(tree, node), IN))
    return TRUE;
  return FALSE;
}

/*
 *
 *
 */
BOOLEAN in_tree(NUM number, RL_Tree *tree, NUM node, NUM node_num,
                NUM node_interval) {
  NUM next_node;
  short quadrant;
  NUM interval = node_interval;
  NUM max = MIN(node_num + interval, tree->range_max);
  NUM quad_min, quad_max;

  /* */
  if (IS_LEAF(interval))
    // current node is a leaf
    return in_leaf(number, tree, node, node_num, max);

  number_quadrant(number, tree, node_interval, node_num, &quadrant, &quad_min,
                  &quad_max);
  interval = quad_max - quad_min + 1;
  node_num = quad_min;

  if (quadrant_status(NODE(tree, node), quadrant) == R_PARCIALLY_IN_INTERVAL) {
    next_node = get_quadrant_node(tree, node, quadrant, node_interval);
    return in_tree(number, tree, next_node, node_num, interval);
  }
  if (quadrant_status(NODE(tree, node), quadrant) == R_TOTALLY_IN_INTERVAL)
    return TRUE;

  return FALSE;
}

/* *************************************************************************************************
 */
/* I/O */
/* *************************************************************************************************
 */

/*
 *
 */
static void display_leaf(RL_Tree *tree, NUM node, NUM node_num, NUM max) {
  int i;
  printf("|");
  // for(i=0;i<LEAF_SIZE && node_num+i<=max;++i)
  for (i = 0; i < LEAF_SIZE; ++i)
    if (is_num_bit(i, (char *)NODE(tree, node), IN))
      printf(",%lu", node_num + i);
    else
      printf(",.");
  printf("|");
}
/*
 *
 */
void display_tree(RL_Tree *tree) {

  // root node
  NUM init, max;
  NUM next_node;
  int i;
  short status;

  NUM qi, tmp = 0;
  next_node = 0; // tree->root;

  printf("Size:%lu -[1,%lu]\n", tree->size, tree->range_max);
  qi = ROOT_INTERVAL(tree) / BRANCH_FACTOR;
  // quadrant_interval(tree,1,tree->range_max,&qi);
  for (i = 1; i <= BRANCH_FACTOR; ++i) {
    tmp += qi;
    //
    init = tmp - qi + 1;
    max = tmp;
    status = quadrant_status(NODE(tree, 0), i);
    switch (status) {
    case R_PARCIALLY_IN_INTERVAL:
      next_node = get_quadrant_node(tree, ROOT(tree), i, qi * BRANCH_FACTOR);
      idisplay_tree(tree, next_node, init, qi, max);
      break;
    case R_TOTALLY_IN_INTERVAL:
      printf(",[%lu-%lu]", init, MIN(max, tree->range_max));
      break;
    case R_IGNORE:
      break;
    default:
      /*  not in */
      printf(",]%lu-%lu[", init, MIN(max, tree->range_max));
    }
  }
  printf("\n");
}
/*
 *
 *
 */
void idisplay_tree(RL_Tree *tree, NUM node, NUM node_num, NUM interval,
                   NUM max) {
  NUM next_node;
  short quadrant;
  NUM interval2;
  NUM node_num2;
  NUM quadrant_max;
  short status;

  if (IS_LEAF(interval))
    return display_leaf(tree, node, node_num, MIN(max, tree->range_max));

  interval2 = NEXT_INTERVAL(interval);
  //
  for (quadrant = 1; quadrant <= BRANCH_FACTOR; ++quadrant) {
    node_num2 = node_num + (quadrant - 1) * interval2;
    quadrant_max = QUADRANT_MAX_VALUE(node_num, quadrant, interval2, max);
    status = quadrant_status(NODE(tree, node), quadrant);
    switch (status) {
    case R_PARCIALLY_IN_INTERVAL:
      next_node = get_quadrant_node(tree, node, quadrant, interval);
      if (IS_LEAF(interval2))
        display_leaf(tree, next_node, node_num2,
                     MIN(quadrant_max, tree->range_max));
      else
        idisplay_tree(tree, next_node, node_num2, interval2, quadrant_max);
      break;
    case R_TOTALLY_IN_INTERVAL:
      printf(",[%lu-%lu]", node_num2, MIN(node_num2 + interval2 - 1, max));
      break;
    case R_IGNORE:
      break;
    default:
      printf(",]%lu-%lu[", node_num2,
             MIN(tree->range_max, node_num2 + interval2 - 1));
    }
  }
}

/* ***************************************************************************************************
 */
static NUM next_in_leaf(RL_Tree *tree, NUM node, NUM node_num, NUM max,
                        NUM min) {
  NUM number;
  number = node_num;
  if (number < min)
    number = min;
  // fprintf(stderr,"next_in_leaf:[%lu,%lu]:min=%lu-->number=%lu\n",node_num,max,min,number);
  for (; number <= max; ++number)
    if (is_num_bit(number - node_num, (char *)NODE(tree, node), IN)) {
      // fprintf(stdout,"next_in_leaf:[%lu,%lu]:min=%lu>>>>number=%lu\n",node_num,max,min,number);
      return number;
    }
  // fprintf(stderr,"!next_in_leaf:[%lu,%lu]:min=%lu-->number=%lu\n",node_num,max,min,number);
  return 0;
}

/*
 * Find next element bigger than min
 *
 */
NUM next_min(RL_Tree *tree, NUM node, NUM node_num, NUM interval, NUM max,
             NUM min) {
  NUM next_node;
  short quadrant;
  NUM interval2;
  NUM node_num2;
  NUM quadrant_max;
  short status;

  if (min > tree->range_max)
    return 0;
  if (IS_LEAF(interval))
    return next_in_leaf(tree, node, node_num, MIN(max, tree->range_max), min);

  interval2 = NEXT_INTERVAL(interval);
  //
  for (quadrant = 1; quadrant <= BRANCH_FACTOR; ++quadrant) {
    NUM found;
    node_num2 = node_num + (quadrant - 1) * interval2;
    quadrant_max = QUADRANT_MAX_VALUE(node_num, quadrant, interval2, max);
    //------------------------------------------
    status = quadrant_status(NODE(tree, node), quadrant);
    switch (status) {
    case R_PARCIALLY_IN_INTERVAL:
      next_node = get_quadrant_node(tree, node, quadrant, interval);
      found =
          next_min(tree, next_node, node_num2, interval2, quadrant_max, min);
      if (found > 0)
        return found;
      break;
    case R_TOTALLY_IN_INTERVAL:
      if (min <= quadrant_max && min >= node_num2)
        return min;
      if (min < node_num2)
        return node_num2;
    }
  }
  return 0;
}

/* *******************************************************************************************************/
/*
 *
 */
void intersect_leafs(char *storage1, char *storage2) {

  BITMAP_difference(*storage1, *storage1, *storage2);
  storage1++;
  storage2++;
  BITMAP_difference(*storage1, *storage1, *storage2);
}
/*
 * Removes the elements in tree1 that are in tree2
 *
 */
/*NUM tree_minus(RL_Tree *tree1,RL_Tree *tree2,NUM node1,NUM node2,NUM
node_num,NUM interval,NUM max) {
  NUM next_node1,next_node2;
  short quadrant;
  NUM interval2;
  NUM node_num2;
  NUM quadrant_max;
  short status1,status2;


  if ( IS_LEAF(interval) ) //
    return intersect_leafs((char*)NODE(tree1,node1),(char*)NODE(tree2,node2));

  interval2=NEXT_INTERVAL(interval);
  //
  for(quadrant=1;quadrant<=BRANCH_FACTOR;++quadrant){
    node_num2=node_num+(quadrant-1)*interval2;
    quadrant_max=QUADRANT_MAX_VALUE(node_num,quadrant,interval2,max);
    //------------------------------------------
    status1=quadrant_status(NODE(tree1,node1),quadrant);
    status2=quadrant_status(NODE(tree2,node2),quadrant);
    if (status2==R_IGNORE || status2==R_NOT_IN_INTERVAL) {
      // do nothing
    } else if ( status2==R_TOTALLY_IN_INTERVAL && (status1==R_IGNORE ||
status1==R_NOT_IN_INTERVAL )) {
      // do nothing
    } else if ( status2==R_TOTALLY_IN_INTERVAL && status1==R_TOTALLY_IN_INTERVAL
) {
      //  delete entire quadrant subtree in tree1
    } else if ( status2==R_PARTIALLY_IN_INTERVAL &&
status1==R_PARTIALLY_IN_INTERVAL){
      // call same function
      next_node1=get_quadrant_node(tree1,node1,quadrant,interval);
      next_node2=get_quadrant_node(tree1,node2,quadrant,interval);
      tree_minus(tree1,tree2,next_node1,next_node2,node_num2,interval2,quadrant_max);
    } else if ( status2==R_PARTIALLY_IN_INTERVAL &&
status1==R_TOTALLY_IN_INTERVAL) {
      // foreach element of tree2, remove it in tree1

    } else {
      // this should never happen!!!!
    }
    switch(status) {
    case R_PARCIALLY_IN_INTERVAL:
      next_node=get_quadrant_node(tree,node,quadrant,interval);
      found=next_min(tree,next_node,node_num2,interval2,quadrant_max,min);
      if ( found>0) return found;
      break;
    case R_TOTALLY_IN_INTERVAL:
      if (min<=quadrant_max && min>=node_num2)
        return min;
      if ( min < node_num2 ) return node_num2;
    }

  }
  return 0;
}*/
/* ***************************************************************************************************
 */
// root level
static NUM norm_tree_size(NUM interval) {
  NUM tmp;
  NUM j = BRANCH_FACTOR;
  ;

  if (interval <= LEAF_SIZE * BRANCH_FACTOR)
    return LEAF_SIZE;
  while (1) {
    tmp = LEAF_SIZE * j;
    if (tmp * BRANCH_FACTOR >= interval)
      break;
    j *= BRANCH_FACTOR;
    ;
  }
  return tmp;
}
//
static void root_intervals(RL_Tree *tree) {
  NUM first_i;

  first_i = norm_tree_size(tree->range_max);
  // k=tree->range_max/first_i+1; // number of large intervals

  tree->root_i = first_i;

  if (tree->root_i * BRANCH_FACTOR < tree->range_max) {
    tree->root_i = tree->root_i * BRANCH_FACTOR;
    // printf("%lu---->>%lu\n",tree->range_max,tree->root_i);
  }
}
