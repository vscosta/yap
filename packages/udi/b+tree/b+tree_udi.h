#ifndef __BTREE_UDI_H__
#define __BTREE_UDI_H__ 1

#include <YapInterface.h>
#include <udi.h>
#include "b+tree.h"

#define SPEC "btree"
/*Prolog term from :- udi(a(-,btree,-)).*/

extern void *BtreeUdiInit
	(YAP_Term spec, int arg, int arity);

extern void *BtreeUdiInsert
	(void *control, YAP_Term term, int arg, void *data);

extern int BtreeUdiSearch
	(void *control, int arg, Yap_UdiCallback callback, void *args);

extern int BtreeUdiDestroy(void *control);

typedef int (*BTreeSearchAtt) (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);

struct Att
{
  const char *att;
  BTreeSearchAtt proc_att;
};

int BTreeMinAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeMaxAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeEqAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeLtAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeLeAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeGtAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeGeAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);
int BTreeRangeAtt (btree_t tree, YAP_Term constraint, Yap_UdiCallback callback, void *args);

static struct Att att_func[] = 
  {
    {"min",BTreeMinAtt},
    {"max",BTreeMaxAtt},
    {"eq",BTreeEqAtt},
    {"lt",BTreeLtAtt},
    {"le",BTreeLeAtt},
    {"gt",BTreeGtAtt},
    {"ge",BTreeGeAtt},
    {"range",BTreeRangeAtt}
  };

#endif /* __BTREE_UDI_H__ */
