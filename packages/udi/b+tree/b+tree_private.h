#ifndef __BTREE_PRIVATE_H__
#define __BTREE_PRIVATE_H__ 1

#include "udi_common.h"

// Do not kown where it is defined but I need it
extern int Yap_page_size;
#define MAXCARD (int)((Yap_page_size-(2*sizeof(int)))/ (2 * sizeof(void*)))
#define MINCARD (MAXCARD / 2)

struct Branch
{
  double key;
  void * child;
  /*This B+Tree will allways hold index_t both in branches and leaves*/
};
typedef struct Branch branch_t;

struct Node
{
  int count;
  int level;

//  double key[MAXCARD - 1];
//  void * branch[MAXCARD];

  /* we do not use one key with this representation */
  branch_t branch[FLEXIBLE_SIZE];
  /* in leaf nodes last child is ptr to next node
   * for fast in order run
   */
};
typedef struct Node * node_t;
#define SIZEOF_NODE SIZEOF_FLEXIBLE(struct Node, branch, MAXCARD)

struct Range
{
  double min;
  int le;
  double max;
  int ge;
};
typedef struct Range range_t;

typedef node_t btree_t;

static node_t BTreeNewNode (void);
static void BTreeNodeInit (node_t);
static int BTreeInsertNode(node_t, double *, void **);
static int BTreePickBranch(node_t, double);
static int BTreeAddBranch(node_t, int, double *, void **);
static int BTreeAddLeaf(node_t, double *, void **);
static void BTreeDestroyNode (node_t n);

#include "b+tree.h"

#endif /* __BTREE_PRIVATE_H__ */
