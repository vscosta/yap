#include "YapConfig.h"
#include "udi.h"
#include "utarray.h"
#include "uthash.h"

/* Argument Indexing */
struct udi_p_args {
  int arg;                 // indexed arg
  void *idxstr;            // user indexing structure
  UdiControlBlock control; // user indexing structure functions
};
typedef struct udi_p_args *UdiPArg;
UT_icd arg_icd = {sizeof(struct udi_p_args), NULL, NULL, NULL};

/* clauselist */
UT_icd cl_icd = {sizeof(yamop *), NULL, NULL, NULL};

/*
 * All the info we need to enter user indexed code
 * stored in a uthash
 */
struct udi_info {
  PredEntry *p;         // predicate (need to identify asserts)
  UT_array *clauselist; // clause list used on returns
  UT_array *args;       // indexed args
  UT_hash_handle hh;    // uthash handle
};
typedef struct udi_info *UdiInfo;

/* to ease code for a UdiInfo hash table*/
#define HASH_FIND_UdiInfo(head, find, out)                                     \
  HASH_FIND(hh, head, find, sizeof(PredEntry), out)
#define HASH_ADD_UdiInfo(head, p, add)                                         \
  HASH_ADD_KEYPTR(hh, head, p, sizeof(PredEntry *), add)

/* used during init */
static YAP_Int p_new_udi(USES_REGS1);
static YAP_Int p_udi_args_init(Term spec, int arity, UdiInfo blk);

/*
 * Indexing Search and intersection Helpers
 */

/* single indexing helpers (no intersection needed just create clauselist) */
#include "clause_list.h"
struct si_callback_h {
  clause_list_t cl;
  UT_array *clauselist;
  void *pred;
};
typedef struct si_callback_h *si_callback_h_t;

static inline int si_callback(void *key, void *data, void *arg) {
  si_callback_h_t c = (si_callback_h_t)arg;
  yamop **cl = (yamop **)utarray_eltptr(c->clauselist, ((YAP_Int)data) - 1);
  return Yap_ClauseListExtend(c->cl, *cl, c->pred);
}
