#ifndef _RTREE_UDI_I_
#define _RTREE_UDI_I_

#define NARGS 5
struct Control
{
  int arg;
  void *pred;
  rtree_t tree;
};
typedef struct Control control_t[NARGS];

struct CallbackM
{
  clause_list_t cl;
  void * pred;
};
typedef struct CallbackM * callback_m_t;

#endif /* _RTREE_UDI_I_ */
