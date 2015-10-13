#ifndef __RTREE_PRIVATE_H__
#define __RTREE_PRIVATE_H__ 1

#include "udi_common.h"

#define NUMDIMS 2 /* we will work in 2d changing this will
                     break some functions */

// Do not kown where it is defined but I need it
extern int Yap_page_size;
#define MAXCARD (int)((Yap_page_size-(2*sizeof(int)))/ sizeof(struct Branch))
#define MINCARD (MAXCARD / 2)

struct Rect
{
  double coords[2*NUMDIMS]; /* x1min, y1min, ... , x1max, y1max, ...*/
};
typedef struct Rect rect_t;

typedef size_t index_t;

struct Branch
{
  rect_t mbr;
  void * child; /*void * so user can store whatever he needs, in case
   of non-leaf ndes it stores the child-pointer*/
};
typedef struct Branch branch_t;

struct Node
{
  int count;
  int level;
  branch_t branch[FLEXIBLE_SIZE];
};
typedef struct Node * node_t;
#define SIZEOF_NODE SIZEOF_FLEXIBLE(struct Node, branch, MAXCARD)

typedef node_t rtree_t;

struct Partition
{
  int n;
  rect_t cover_all;
  rect_t cover[2];
  branch_t buffer[FLEXIBLE_SIZE];
};
typedef struct Partition * partition_t;
#define SIZEOF_PARTITION SIZEOF_FLEXIBLE(struct Partition, buffer, MAXCARD + 1)

/* #define ALIGN(addr, size) (((addr)+(size-1))&(~(size-1))) */

#include "rtree.h"

static node_t RTreeNewNode (void);
static void RTreeDestroyNode (node_t);
static void RTreeNodeInit (node_t);

static int RTreeSearchNode (node_t, rect_t, SearchHitCallback, void *);
static int RTreeInsertNode (node_t, int, rect_t,void *,node_t *);

static int RTreePickBranch (rect_t, node_t);
static int RTreeAddBranch(node_t, branch_t, node_t *);
static void RTreeSplitNode (node_t, branch_t, node_t *);

static void RTreePickSeeds(partition_t, node_t, node_t);
static void RTreeNodeAddBranch(rect_t *, node_t, branch_t);
static void RTreePickNext(partition_t, node_t, node_t);

static rect_t RTreeNodeCover(node_t);

static double RectArea (rect_t);
static rect_t RectCombine (rect_t, rect_t);
static int RectOverlap (rect_t, rect_t);

static partition_t PartitionNew (void);
static void PartitionPush (partition_t, branch_t);
static branch_t PartitionPop (partition_t);
static branch_t PartitionGet (partition_t, int);

#endif /* __RTREE_PRIVATE_H__ */
