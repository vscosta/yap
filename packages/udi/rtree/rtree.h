#ifndef _RTREE_
#define _RTREE_ 1

#ifndef __RTREE_PRIVATE_H__
	typedef void * rtree_t;
	typedef void * node_t;

	struct Rect
	{
		double coords[4]; /*TODO: change this from here*/
	};
	typedef struct Rect rect_t;
#endif

typedef int (*SearchHitCallback)(void *, void *data, void *arg);

extern rtree_t RTreeNew (void);
extern void RTreeInsert (rtree_t *, rect_t, void *);
extern int RTreeSearch (rtree_t, rect_t, SearchHitCallback, void *);
extern void RTreeDestroy (rtree_t);
extern void RTreePrint(node_t);
extern rect_t RectInit (void);
extern void RectPrint (rect_t);
extern rect_t RectInitCoords (double *);

#endif /* _RTREE_ */
