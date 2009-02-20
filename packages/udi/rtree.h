#ifndef _RTREE_
#define _RTREE_

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

#define NUMDIMS 2 /* 2d */

struct Rect
{
  double coords[2*NUMDIMS]; /* x1min, y1min, ... , x1max, y1max, ...*/
};
typedef struct Rect rect_t;

struct Branch
{
  rect_t mbr;
  void * child; /*void * so user can store whatever he needs, in case
   of non-leaf ndes it stores the child-pointer*/
};
typedef struct Branch branch_t;

#define PGSIZE 196
#define MAXCARD (int)((PGSIZE-(2*sizeof(int)))/ sizeof(struct Branch))
#define MINCARD (MAXCARD / 2)

struct Node
{
  int count;
  int level;
  branch_t branch[MAXCARD];
};
typedef struct Node * node_t;

typedef node_t rtree_t;

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

/* CallBack to search function */
typedef int (*SearchHitCallback)(rect_t r, void *data, void *arg);

extern rtree_t RTreeNew (void);
extern void RTreeInsert (rtree_t *, rect_t, void *);
extern int RTreeSearch (rtree_t, rect_t, SearchHitCallback, void *);
extern void RTreeDestroy (rtree_t);
extern void RTreePrint(node_t);
extern rect_t RectInit (void);

struct Partition
{
  branch_t buffer[MAXCARD+1];
  int n;
  rect_t cover_all;
  rect_t cover[2];
};
typedef struct Partition partition_t;

#endif /* _RTREE_ */
