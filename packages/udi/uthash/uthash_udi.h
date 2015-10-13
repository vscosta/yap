#ifndef _UTHASH_UDI_
#define _UTHASH_UDI_

#include <YapInterface.h>
#include <udi.h>
#include "uthash.h"

#define SPEC "#"
/*Prolog term from :- udi(a(#,-)).*/

extern void *UTHashUdiInit
	(YAP_Term spec, int arg, int arity);

extern void *UTHashUdiInsert
	(void *control, YAP_Term term, int arg, void *data);

extern int UTHashUdiSearch
	(void *control, int arg, Yap_UdiCallback callback, void *args);

extern int UTHashUdiDestroy(void *control);

void udi_uthash_init(void);

#endif /* _UTHASH_UDI_ */
