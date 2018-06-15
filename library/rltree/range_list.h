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


Last rev: $Id: range_list.h,v 1.1 2008-03-26 23:05:22 nunofonseca Exp $
**************************************************************************/

/**
 * @file range_list.h
 *
 *  range list core data-structures.
 *
 * @namespace rltree
 *
 */

/*
  Leaf
  Each leaf uses 16 bits ( each bit represents one number )

 */
#define NUM unsigned long
/*
  Node
  Each node (non leaf) uses 8 bits.
  - 8 bits are used to represent the state of the 4 subtrees ( subranges ).
  - 2 bits are used to represent the state for each subtreee

  States of a subtree:
    00 (0) - range not in interval
    11 (3)- all range in interval
    10 (2)- range parcially in interval

  An extra byte is used to keep the number of nodes in the subtrees.
 */
struct s_node {
  // short quadrant;
  unsigned short int quadrant_1 : 2; //
  unsigned short int quadrant_2 : 2;
  unsigned short int quadrant_3 : 2;
  unsigned short int quadrant_4 : 2;
  unsigned short int num_subnodes : 8;
};

typedef enum {
  R_TOTALLY_IN_INTERVAL = 3,
  R_PARCIALLY_IN_INTERVAL = 2,
  R_NOT_IN_INTERVAL = 0,
  R_IGNORE = 1
} QUADRANT_STATUS;

#define BRANCH_FACTOR 4 /* factor of division of the range */
#define LEAF_SIZE 16    /* how many numbers are represented by a leaf */

#define NODE_SIZE sizeof(RL_Node)

#define NODE(tree, idx) (RL_Node *)&tree->root[idx]
#define ROOT(tree) 0

#define IS_ROOT(tree, interval) (tree->range_max <= interval)
#define ROOT_INTERVAL(tree) (tree->root_i * BRANCH_FACTOR)

#define MIN(a, b) ((a < b) ? a : b)

#define ON_BITS(n) (active_bits[n - 1]) // mask to check if bits until n are in
#define SET_LEAF_IN(max, node, quad_i)                                         \
  (node.leaf =                                                                 \
       ON_BITS(max - quad_i + 1)) // mask to check if bits until n are in

#define LEAF_ALL_IN(leaf)                                                      \
  (leaf == 65535) // return true if all numbers in leaf are IN (selected)
#define LEAF_ALL_OUT(leaf)                                                     \
  (leaf == 0) // return true if all numbers in leaf are OUT

#define ALL_OUT(n) memset(n, 0, NODE_SIZE)    // turn out a node
#define ALL_IN(n) memset(n, 32767, NODE_SIZE) // turn in a leaf
#define INODE_CAPACITY                                                         \
  (LEAF_SIZE * BRANCH_FACTOR) // minimum range that a inode stores

// returns the maximum number that a quadrant stores
#define QUADRANT_MAX_VALUE(node_num, quadrant, quadrant_interval, max)         \
  (MIN(node_num + quadrant_interval * quadrant - 1, max))

// returns the interval size for the next level in the tree
#define NEXT_INTERVAL(interval)                                                \
  ((interval <= LEAF_SIZE * BRANCH_FACTOR)                                     \
       ? LEAF_SIZE                                                             \
       : interval / BRANCH_FACTOR + interval % BRANCH_FACTOR)

#define IS_LEAF(interval)                                                      \
  ((interval <= LEAF_SIZE) ? 1 : 0) // check if a interval of type Leaf
#define LAST_LEVEL_INODE(interval)                                             \
  ((interval <= LEAF_SIZE * BRANCH_FACTOR && interval > LEAF_SIZE) ? 1 : 0)

#define REALLOC_MEM(tree) (tree->mem_alloc < (tree->size + 1) * NODE_SIZE)
#define MEM_SIZE(tree) (tree->size + 2) * NODE_SIZE

#define TREE_SIZE(tree) tree->mem_alloc + sizeof(RL_Tree)

typedef union {
  struct s_node i_node;
  unsigned short int leaf;
} RL_Node; /* A node is a internal node (inode) or a leaf depending on their
              depth in the tree */

/*
  Range_List
  Contains the root node, max range size,
*/
struct rl_struct {
  RL_Node *root;
  NUM size;      // number of nodes
  NUM mem_alloc; // memory allocated for *root
  NUM range_max; // maximum value of the interval
  NUM root_i;    // root interval
};
typedef struct rl_struct RL_Tree;

/* Buffer */
struct s_buffer {
  RL_Node *root_node;
  unsigned long size; // memory (in bytes) allocated for root_node
};
typedef struct s_buffer RL_Buffer;

//----------------------------------------------------------------
// Bits operations
#define BITMAP_empty(b) ((b) == 0)
#define BITMAP_member(b, n) (((b) & (1 << (n))) != 0)
#define BITMAP_alone(b, n) ((b) == (1 << (n)))
#define BITMAP_subset(b1, b2) (((b1) & (b2)) == b2)
#define BITMAP_same(b1, b2) ((b1) == (b2))

#define BITMAP_on_all(b) ((b) = 255)

#define BITMAP_clear(b) ((b) = 0)
#define BITMAP_and(b1, b2) ((b1) &= (b2))
#define BITMAP_minus(b1, b2) ((b1) &= ~(b2))
#define BITMAP_insert(b, n) ((b) |= (1 << (n)))
#define BITMAP_delete(b, n) ((b) &= (~(1 << (n))))
#define BITMAP_copy(b1, b2) ((b1) = (b2))
#define BITMAP_intersection(b1, b2, b3) ((b1) = ((b2) & (b3)))
#define BITMAP_difference(b1, b2, b3) ((b1) = ((b2) & (~(b3))))

#
//----------------------------------------------------------------
typedef enum { TRUE = 1, FALSE = 0 } BOOLEAN;
typedef enum { IN = 1, OUT = 0 } STATUS;

//
#define BUFFER_SIZE 1000
/* **********************************************************************************
 */
/* API */
extern RL_Tree *new_rl(NUM max_size);
extern RL_Tree *copy_rl(RL_Tree *tree);
extern void free_rl(RL_Tree *range);

extern void rl_all(RL_Tree *tree, STATUS status);
extern void display_tree(RL_Tree *tree);
extern RL_Tree *set_in_rl(RL_Tree *tree, NUM number, STATUS status);
extern BOOLEAN in_rl(RL_Tree *range, NUM number);
extern BOOLEAN
freeze_rl(RL_Tree *tree); /* write operations on the range are finishe */
extern RL_Tree *intersect_rl(RL_Tree *range1, RL_Tree *range2);

extern NUM
rl_next_in_bigger(RL_Tree *tree,
                  NUM min); /* Returns next number in tree bigger than min */

#define IS_FREEZED(tree) (tree->mem_alloc != 0)
