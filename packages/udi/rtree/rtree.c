#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <float.h>

#include "rtree_private.h"

rtree_t RTreeNew (void)
{
  rtree_t t;
  t = RTreeNewNode();
  t->level = 0; /*leaf*/
  return t;
}

void RTreeDestroy (rtree_t t)
{
  if (t)
    RTreeDestroyNode (t);
}

static node_t RTreeNewNode (void)
{
  node_t n;

  n = (node_t) malloc (SIZEOF_NODE);
  assert(n);
  RTreeNodeInit(n);
  return n;
}

static void RTreeDestroyNode (node_t node)
{
  int i;
  
  if (node->level == 0) /* leaf level*/
    {
      for (i = 0; i < MAXCARD; i++)
        if (node->branch[i].child)
          continue;/* allow user free data*/
        else
          break;
    }
  else
    {
      for (i = 0; i < MAXCARD; i++)
        if (node->branch[i].child)
          RTreeDestroyNode (node->branch[i].child);
        else
          break;
    }
  free (node);
}

static void RTreeNodeInit (node_t n)
{
  memset((void *) n,0, SIZEOF_NODE);
  n->level = -1;
}

int RTreeSearch (rtree_t t, rect_t s, SearchHitCallback f, void *arg)
{
  assert(t);
  return RTreeSearchNode(t,s,f,arg);
}

static int RTreeSearchNode (node_t n, rect_t s, SearchHitCallback f, void *arg)
{
  int i;
  int c = 0;

  if (n->level > 0)
    {
      for (i = 0; i < MAXCARD; i++)
        if (n->branch[i].child &&
            RectOverlap (s,n->branch[i].mbr))
          c += RTreeSearchNode ((node_t) n->branch[i].child, s, f, arg);
    }
  else
    {
      for (i = 0; i < MAXCARD; i++)
        if (n->branch[i].child &&
            RectOverlap (s,n->branch[i].mbr))
          {
            c ++;
            if (f)
              if ( !f(&(n->branch[i].mbr),n->branch[i].child,arg))
                return c;
          }
    }
  return c;
}

void RTreeInsert (rtree_t *t, rect_t r, void *data)
{
  node_t n2;
  node_t new_root;
  branch_t b;
  assert(t && *t);

  if (RTreeInsertNode(*t, 0, r, data, &n2))
    /* deal with root split */
    {
      new_root = RTreeNewNode();
      new_root->level = (*t)->level + 1;
      b.mbr = RTreeNodeCover(*t);
      b.child = (void *) *t;
      RTreeAddBranch(new_root, b, NULL);
      b.mbr = RTreeNodeCover(n2);
      b.child = (void *) n2;
      RTreeAddBranch(new_root, b, NULL);
      *t = new_root;
    }
}

static int RTreeInsertNode (node_t n, int level,
                            rect_t r, void *data,
                            node_t *new_node)
{
  int i;
  node_t n2;
  branch_t b;

  assert(n && new_node);
  assert(level >= 0 && level <= n->level);
  
  if (n->level > level)
    {
      i = RTreePickBranch(r,n);
      if (!RTreeInsertNode((node_t) n->branch[i].child, level,
                           r, data,&n2)) /* not split */
        {
          n->branch[i].mbr = RectCombine(r,n->branch[i].mbr);
          return FALSE;
        }
      else /* node split */
        {
           n->branch[i].mbr = RTreeNodeCover(n->branch[i].child);
           b.child = n2;
           b.mbr = RTreeNodeCover(n2);
           return RTreeAddBranch(n, b, new_node);
        }
    }
  else /*insert level*/
    {
      b.mbr = r;
      b.child = data;
      return RTreeAddBranch(n, b, new_node);
    }
}

static int RTreeAddBranch(node_t n, branch_t b, node_t *new_node)
{
  int i;

  assert(n);

  if (n->count < MAXCARD) /*split not necessary*/
    {
      for (i = 0; i < MAXCARD; i++)
        if (n->branch[i].child == NULL)
          {
            n->branch[i] = b;
            n->count ++;
            break;
          }
      return FALSE;
    }
  else /*needs to split*/
    {
      assert(new_node);
      RTreeSplitNode (n, b, new_node);
      return TRUE;
    }
}

static int RTreePickBranch (rect_t r, node_t n)
{
  int i;
  double area;
  double inc_area;
  rect_t tmp;
  int best_i;
  double best_inc;
  double best_i_area;

  best_i = 0;
  best_inc = DBL_MAX; /* double Max value */
  best_i_area = DBL_MAX;

  for (i = 0; i < MAXCARD; i++)
    if (n->branch[i].child)
      {
        area = RectArea (n->branch[i].mbr);
        tmp = RectCombine (r, n->branch[i].mbr);
        inc_area = RectArea (tmp) - area; 

        if (inc_area < best_inc)
          {
            best_inc = inc_area;
            best_i = i;
            best_i_area = area;
          }
        else if (inc_area == best_inc && best_i_area > area)
          {
            best_inc = inc_area;
            best_i = i;
            best_i_area = area;
          }
      }
    else
      break;
  return best_i;
}

static void RTreeSplitNode (node_t n, branch_t b, node_t *new_node)
{
  partition_t p;
  int level;
  int i;

  assert(n);
  assert(new_node);

  p = PartitionNew();

  for (i = 0; i < MAXCARD; i ++)
    PartitionPush(p,n->branch[i]);
  PartitionPush(p,b);

  level = n->level;
  RTreeNodeInit(n);
  n->level = level;
  *new_node = RTreeNewNode();
  (*new_node)->level = level;

  RTreePickSeeds(p, n, *new_node);

  while (p->n)
    if (n->count + p->n <= MINCARD)
      /* first group (n) needs all entries */
      RTreeNodeAddBranch(&(p->cover[0]), n, PartitionPop(p));
    else if ((*new_node)->count + p->n <= MINCARD)
      /* second group (new_node) needs all entries */
      RTreeNodeAddBranch(&(p->cover[1]), *new_node, PartitionPop(p));
    else
      RTreePickNext(p, n, *new_node);
}

static void RTreePickNext(partition_t p, node_t n1, node_t n2)
/* linear version */
{
  branch_t b;
  double area[2], inc_area[2];
  rect_t tmp;

  b = PartitionPop(p);

  area[0] = RectArea (p->cover[0]);
  tmp = RectCombine (p->cover[0], b.mbr);
  inc_area[0] = RectArea (tmp) - area[0];

  area[1] = RectArea (p->cover[1]);
  tmp = RectCombine (p->cover[1], b.mbr);
  inc_area[1] = RectArea (tmp) - area[1]; 

  if (inc_area[0] < inc_area[1] ||
      (inc_area[0] == inc_area[1] && area[0] < area[1]))
    RTreeNodeAddBranch(&(p->cover[0]),n1,b);
  else
    RTreeNodeAddBranch(&(p->cover[1]),n2,b);
}

static void RTreePickSeeds(partition_t p, node_t n1, node_t n2)
/* puts in index 0 of each node the resulting entry, forming the two
   groups
   This is the linear version
*/
{
  int dim,high, i;
  int highestLow[NUMDIMS], lowestHigh[NUMDIMS];
  double width[NUMDIMS];
  int seed0, seed1;
  double sep, best_sep;

  assert(p->n == MAXCARD + 1);

  for (dim = 0; dim < NUMDIMS; dim++)
    {
      high = dim + NUMDIMS;
      highestLow[dim] = lowestHigh[dim] = 0;
      for (i = 1; i < MAXCARD +1; i++)
        {
          if (p->buffer[i].mbr.coords[dim] >
              p->buffer[highestLow[dim]].mbr.coords[dim])
            highestLow[dim] = i;
          if (p->buffer[i].mbr.coords[high] < 
              p->buffer[lowestHigh[dim]].mbr.coords[high])
            lowestHigh[dim] = i;
        }
      width[dim] = p->cover_all.coords[high] - p->cover_all.coords[dim];
      assert(width[dim] >= 0);
    }

  seed0 = lowestHigh[0];
  seed1 = highestLow[0];
  best_sep = 0;
  for (dim = 0; dim < NUMDIMS; dim ++)
    {
      high = dim + NUMDIMS;
      
      sep = (p->buffer[highestLow[dim]].mbr.coords[dim] -
             p->buffer[lowestHigh[dim]].mbr.coords[high]) / width[dim];
      if (sep > best_sep)
        {
          seed0 = lowestHigh[dim];
          seed1 = highestLow[dim];
          best_sep = sep;
        }
    }
/*   assert (seed0 != seed1); */
  if (seed0 > seed1)
    {
      RTreeNodeAddBranch(&(p->cover[0]),n1,PartitionGet(p,seed0));
      RTreeNodeAddBranch(&(p->cover[1]),n2,PartitionGet(p,seed1));
    }
  else if (seed0 < seed1)
    {
      RTreeNodeAddBranch(&(p->cover[0]),n1,PartitionGet(p,seed1));
      RTreeNodeAddBranch(&(p->cover[1]),n2,PartitionGet(p,seed0));
    }
}

static void RTreeNodeAddBranch(rect_t *r, node_t n, branch_t b)
{
  int i;

  assert(n);
  assert(n->count < MAXCARD);

  for (i = 0; i < MAXCARD; i++)
    if (n->branch[i].child == NULL)
      {
        n->branch[i] = b;
        n->count ++;
        break;
      }
  *r = RectCombine(*r,b.mbr);
}


void RTreePrint(node_t t)
{
  int i;

  /*  printf("rtree([_,_,_,_,_]).\n"); */
  printf("rtree(%p,%d,[",t,t->level);
  for (i = 0; i < MAXCARD; i++)
    {
      if (t->branch[i].child != NULL)
        {
          printf("(%p,",t->branch[i].child);
                   RectPrint(t->branch[i].mbr);
          printf(")");
        }
      else
        {
          printf("nil");
        }
      if (i < MAXCARD-1)
        printf(",");
    }
  printf("]).\n");

  if (t->level != 0)
    for (i = 0; i < MAXCARD; i++)
      if (t->branch[i].child != NULL)
        RTreePrint((node_t) t->branch[i].child);
      else
        break;
}

/*
 * Partition related
 */

static partition_t PartitionNew (void)
{
  partition_t p;

  p = (partition_t) malloc(SIZEOF_PARTITION);
  /*TODO: check return value*/
  memset((void *) p,0, SIZEOF_PARTITION);
  p->cover[0] = p->cover[1] = p->cover_all = RectInit();
  return p;
}

static void PartitionPush (partition_t p, branch_t b)
{
  assert(p->n < MAXCARD + 1);
  p->buffer[p->n] = b;
  p->n ++;
  p->cover_all = RectCombine(p->cover_all,b.mbr);
}

static branch_t PartitionPop (partition_t p)
{
  assert(p->n > 0);
  p->n --;
  return p->buffer[p->n];
}

static branch_t PartitionGet (partition_t p, int n)
{
  branch_t b;
  assert (p->n > n);
  b = p->buffer[n];
  p->buffer[n] = PartitionPop(p);
  return b;
}

/*
 * Rect related
 */

rect_t RectInit (void)
{
  rect_t r = {{DBL_MAX, DBL_MAX, DBL_MIN, DBL_MIN}};
  return (r);
}

rect_t RectInitCoords (double c[4])
{
  rect_t r;
  r.coords[0] = c[0];
  r.coords[1] = c[1];
  r.coords[2] = c[2];
  r.coords[3] = c[3];
  return (r);
}

static double RectArea (rect_t r)
{
  int i;
  double area;

  for (i = 0,area = 1; i < NUMDIMS; i++)
    area *= r.coords[i+NUMDIMS] - r.coords[i];

/*   area = (r.coords[1] - r.coords[0]) *  */
/*     (r.coords[3] - r.coords[2]); */

  return area;
}

static rect_t RectCombine (rect_t r, rect_t s)
{
  int i;
  rect_t new_rect;

  for (i = 0; i < NUMDIMS; i++)
    {
      new_rect.coords[i] = MIN(r.coords[i],s.coords[i]);
      new_rect.coords[i+NUMDIMS] = MAX(r.coords[i+NUMDIMS],s.coords[i+NUMDIMS]);
    }
  
  return new_rect;
}

static int RectOverlap (rect_t r, rect_t s)
{
  int i;
  
  for (i = 0; i < NUMDIMS; i++)
    if (r.coords[i] > s.coords[i + NUMDIMS] ||
        s.coords[i] > r.coords[i + NUMDIMS])
      return FALSE;
  return TRUE;
}

static rect_t RTreeNodeCover(node_t n)
{
  int i;
  rect_t r = RectInit();

  for (i = 0; i < MAXCARD; i++)
    if (n->branch[i].child)
      {
        r = RectCombine (r, n->branch[i].mbr);
      }
    else
      break;

  return r;
}

void RectPrint (rect_t r)
{
  int i;

  printf("[");
  for (i = 0; i < 2*NUMDIMS; i++)
    {
      printf("%f",r.coords[i]);
      if ( i < 2*NUMDIMS - 1)
        printf(",");
    }
  printf("]");
}
