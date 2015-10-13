#ifndef _RTREE_UDI_
#define _RTREE_UDI_

#include <YapInterface.h>
#include <udi.h>
#include "rtree.h"

#define SPEC "rtree"
/*Prolog term from :- udi(a(-,rtree,-)).*/

extern void *RtreeUdiInit
	(YAP_Term spec, int arg, int arity);

extern void *RtreeUdiInsert
	(void *control, YAP_Term term, int arg, void *data);

extern int RtreeUdiSearch
	(void *control, int arg, Yap_UdiCallback callback, void *args);

extern int RtreeUdiDestroy(void *control);

void udi_rtree_init(void);

#endif /* _RTREE_UDI_ */
