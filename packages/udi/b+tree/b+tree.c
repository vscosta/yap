#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "b+tree_private.h"

btree_t BTreeNew (void)
{
  btree_t t;
  t = BTreeNewNode();
  t->level = 0; /*leaf*/
  return t;
}

void BTreeDestroy (btree_t t)
{
  if (t)
    BTreeDestroyNode (t);
}

static void BTreeDestroyNode (node_t n)
{
  int i;

  if (n->level == 0)
    {
      for (i = 0; i < n->count; i++)
        ;/* allow user free data*/
    }
  else
    {
      for (i = 0; i < n->count; i++)
        BTreeDestroyNode (n->branch[i].child);
    }
}

static node_t BTreeNewNode (void)
{
  node_t n;

  n = (node_t) malloc (SIZEOF_NODE);
  assert(n);
  BTreeNodeInit(n);
  return n;
}

static void BTreeNodeInit (node_t n)
{
  memset((void *) n,0, SIZEOF_NODE);
  n->level = -1;
}

void *BTreeMin (node_t n, node_t *f, int *i)
{

  if (n->level > 0)
    return BTreeMin((node_t) n->branch[0].child, f, i);
  else
    {
      if(f)
        *f = n;
      if(i)
        *i = 0;
      return n->branch[0].child;
    }
}

void *BTreeMax (node_t n, node_t *f, int *i)
{

  if (n->level > 0)
    return BTreeMax((node_t) n->branch[n->count].child, f, i);
  else
    {
      if(n->count > 0)
	{
	  if(f)
	    *f = n;
	  if(i)
	    *i = n->count - 1;
	  return n->branch[n->count - 1].child;
	}
    }

  if (f)
    *f = NULL;
  if (i)
    *i = -1;
  return NULL;
}

void * BTreeSearch (node_t n, double k, int s, node_t *f, int *i)
{
  int j;

  assert(s == EQ || s == GE || s == GT);

  if (n->level > 0)
    {
      for (j = 0; j < n->count; j++)
        if (n->branch[j].key >= k)
          return BTreeSearch((node_t) n->branch[j].child,k,s,f,i);
      return BTreeSearch((node_t) n->branch[j].child,k,s,f,i);
    }
  else
    {
      if (s == EQ || s == GE) /*== or >=*/
        for (j = 0; j < n->count; j++)
          if (n->branch[j].key == k)
            {
              if (f)
                *f = n;
              if (i)
                *i = j;
              return n->branch[j].child;
            }
      if (s == GE || s == GT) /* >= or > */
        {
          if (f)
            *f = n;
          for (j = 0; j < n->count; j++)
            if (n->branch[j].key > k)
              {
                if (i)
                  *i = j;
                return n->branch[j].child;
              }
        }
    }
  if (f)
    *f = NULL;
  if (i)
    *i = -1;
  return NULL;
}

void *BTreeSearchNext (double max, int s, node_t *n, int *i)
{
  assert(n && i);
  assert(s == LT || s == LE);

  if (*i == (*n)->count - 1)
    {
      if (!(*n)->branch[MAXCARD - 1].child) /*terminou*/
        return NULL;
      *n = (node_t) (*n)->branch[MAXCARD - 1].child;
      *i = 0;
    }
  else
    (*i) ++;

  if ((*n)->branch[*i].key > max ||
      ((*n)->branch[*i].key == max && s == LT))
    return NULL;

  return (*n)->branch[*i].child;
}

void BTreeInsert (btree_t *t, double k, void *ptr)
{
  node_t new_root;

  assert(t && *t);

  if (BTreeInsertNode(*t,&k,&ptr))
    /* deal with root split */
    {
      new_root = BTreeNewNode();
      new_root->level = (*t)->level + 1;
      new_root->count = 1;
      new_root->branch[0].key = k;
      new_root->branch[0].child = (void *) (*t);
      new_root->branch[1].child = ptr;
      *t = new_root;
    }
}

static int BTreeInsertNode(node_t n, double *k, void **ptr)
/*ptr holds data and can return node_t*/
{
  int i;

  assert(n);

  if (n->level > 0)
    {
      i = BTreePickBranch(n,*k);
      if (!BTreeInsertNode((node_t) n->branch[i].child, k, ptr))
        /*not split */
        {
          return FALSE;
        }
      else
        /* node split */
        {
          return BTreeAddBranch(n, i, k, ptr); /*propagate split*/
        }
    }
  else
    {
      return BTreeAddLeaf(n,k,ptr);
    }
}

static int BTreeAddBranch(node_t n, int idx, double *k,
                          void **ptr)
{
  int i,j;
  double key[MAXCARD];
  void *branch[MAXCARD+1];
  int level;
  node_t t;

  if (n->count < MAXCARD - 1)
    {
      i = n->count;
      if (i > 0)
        for(; n->branch[i-1].key > *k ; i--)
          {
            n->branch[i].key = n->branch[i-1].key;
            n->branch[i+1].child = n->branch[i].child;
          }
      n->branch[i].key = *k;
      n->branch[i+1].child = *ptr;
      n->branch[i].child = n->branch[idx].child;
      n->count ++;
      return FALSE;
    }
  else
    {
      for(i = n->count, j = MAXCARD; 
	  n->branch[i - 1].key  > *k; 
	  i--, j--)
        {
          key[j - 1] = n->branch[i - 1].key;
          branch[j] = n->branch[i].child;
        }
      key[j - 1] = *k;
      branch[j - 1] = n->branch[idx].child;
      branch[j] = *ptr;
      j--;
      for(; i > 0;i--,j--)
        {
          key[j-1] = n->branch[i-1].key;
          branch[j-1] = n->branch[i-1].child;
        }

      level = n->level;
      BTreeNodeInit(n);
      n->level = level;
      t = BTreeNewNode();
      t->level = level;

      for (i = 0; i < MAXCARD / 2; i ++)
        {
          n->branch[i].key = key[i];
          n->branch[i].child = branch[i];
          n->count ++;
        }
      n->branch[i].child = branch[i];

      *k = key[i];
      *ptr = t;

      for (j = 0,i++; i < MAXCARD; j ++, i ++)
        {
          t->branch[j].key = key[i];
          t->branch[j].child = branch[i];
          t->count ++;
        }
      t->branch[j].child = branch[i];

      return TRUE;
    }
}

static int BTreePickBranch(node_t n, double k)
{
  int i;

  for (i = 0; i < n->count; i++)
    if (n->branch[i].key > k)
      return i;
  return i;
}

static int BTreeAddLeaf(node_t n, double *k, void **ptr)
{
  int i,j;
  node_t t;
  double key[MAXCARD];
  void *branch[MAXCARD];

  assert(n);

  if (n->count < MAXCARD - 1) /*split not necessary*/
    {
      i = n->count;
      if (i > 0)
        for (; n->branch[i - 1].key > *k; i--)
          {
            n->branch[i].key = n->branch[i-1].key;
            n->branch[i].child = n->branch[i-1].child;
          }
      n->branch[i].key = *k;
      n->branch[i].child = *ptr;
      n->count ++;
      return FALSE;
    }
  else /*needs to split*/
    {
      for(i = n->count - 1, j = MAXCARD - 1; 
	  n->branch[i].key  > *k;
	  i--, j--)
        {
          key[j] = n->branch[i].key;
          branch[j] = n->branch[i].child;
        }
      key[j] = *k;
      branch[j] = *ptr;
      j--;
      for(; i >= 0;i--,j--)
        {
          key[j] = n->branch[i].key;
          branch[j] = n->branch[i].child;
        }

      n->count = 0;
      t = BTreeNewNode();
      t->level = n->level;

      for (i = 0; i <= MAXCARD / 2; i ++)
        {
          n->branch[i].key = key[i];
          n->branch[i].child = branch[i];
          n->count ++;
        }
      *k = key[i-1];
      *ptr = t;
      for (j = 0; i < MAXCARD; j ++, i ++)
        {
          t->branch[j].key = key[i];
          t->branch[j].child = branch[i];
          t->count ++;
        }

      /*linked list*/
      t->branch[MAXCARD -1].child = n->branch[MAXCARD - 1].child;
      n->branch[MAXCARD -1].child = t;

    return TRUE;
    }
}
