#ifndef __BTREE_H__
#define __BTREE_H__

#ifndef __BTREE_PRIVATE_H__
typedef void * btree_t;
typedef void * node_t;
#endif

/*
 * Alocates and initializes a new b+tree structure
 */
extern btree_t BTreeNew (void);

/*
 * Inserts in the b+tree the object with key key
 */
extern void BTreeInsert (btree_t *btree, double key, void *data);

/*
 * Searchs the b+tree for the min key object returning it
 *
 * If nidx(node index) and bidx(branch index) is not NULL
 * ...
 */
extern void * BTreeMin(btree_t btree, node_t *nidx, int *bidx);

/*
 * Searchs the b+tree for the max key object returning it
 *
 * If nidx(node index) and bidx(branch index) is not NULL
 * ...
 */
extern void * BTreeMax(btree_t btree, node_t *nidx, int *bidx);

/* Seach Kinds */
#define EQ 1 /* BTreeSearch(btree, key, EQ, NULL, NULL); */
#define LE 2
#define LT 3
/* First call: BTreeMin(btree, nidx, bidx);
   Next Calls(until NULL is returned):
        BTreeSearchNext(key_max, LT || LE, btree, nidx, bidx);*/
#define GE 4
#define GT 5
/* First call: BTreeSearch(btree, key_min, GE || GT, nidx, bidx);
   Next Calls(until NULL is returned):
        BTreeSearchNext(key_min, LT || LE, btree, nidx, bidx);*/

/* Range Searches
 * First call: BTreeSearch(btree, key_min, GE || GT, nidx, bidx);
 * Next Calls(until NULL is returned):
 *       BTreeSearchNext(key_max, LT || LE, btree, nidx, bidx);
 */

/*
 * Searchs the b+tree for:
 * if kind == EQ finds the key object returning it
 * if kind == GE || GT finds the first valid key returning it
 *
 * Returns NULL on fail to find
 *
 * Other parameter to kind will fail
 *
 * If nidx(node index) and bidx(branch index) is not NULL it those values
 */
extern void * BTreeSearch(btree_t btree, double key, int kind,
                          node_t *nidx, int *bidx);

/*
 * Searches next valid answers given nidx, bidx where set in previous call to
 * BTreeMin or BTreeSearch
 *
 * It will return the valid key objects with kind LE or LT, NULL otherwise
 *
 * nidx(node index) and bidx(branch index) will also be set
 */
extern void * BTreeSearchNext (double key, int kind,
                               node_t *nidx, int *bidx);

/*
 * Destroys b+tree, freeing all the memory allocated to it
 */
extern void BTreeDestroy (btree_t);

/*
 * Debug function, prints b+tree
 */
extern void BTreePrint(btree_t);

#endif /*__BTREE_H__*/
