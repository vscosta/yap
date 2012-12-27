#include "udi.h"
#include "utarray.h"
#include "uthash.h"

/* Argument Indexing */
struct udi_p_args {
	int arg;                 //indexed arg
	void *idxstr;            //user indexing structure
	UdiControlBlock control; //user indexing structure functions
};
typedef struct udi_p_args *UdiPArg;
UT_icd arg_icd = {sizeof(struct udi_p_args), NULL, NULL, NULL };

/* a pointer utarray list
 * This is a hack, becouse I do no know the real type of clauses
 * Not completely used for now
 */
UT_icd ptr_icd = {sizeof(void *), NULL, NULL, NULL };

/*
 * All the info we need to enter user indexed code
 * stored in a uthash
 */
struct udi_info
{
  PredEntry *p;         //predicate (need to identify asserts)
  UT_array *clauselist; //clause list used on returns
  UT_array *args;       //indexed args
  UT_hash_handle hh;
};
typedef struct udi_info *UdiInfo;

/* to ease code for a UdiInfo hash table*/
#define HASH_FIND_UdiInfo(head,find,out)           \
  HASH_FIND(hh,head,find,sizeof(PredEntry *),out)
#define HASH_ADD_UdiInfo(head,p,add)                 \
  HASH_ADD_KEYPTR(hh,head,p,sizeof(PredEntry *),add)

int Yap_udi_args_init(Term spec, int arity, UdiInfo blk);

/* temporary */
struct CallbackM
{
  clause_list_t cl;
  void * pred;
};
typedef struct CallbackM * callback_m_t;

static inline int callback(void *key, void *data, void *arg)
{
	callback_m_t x;
	x = (callback_m_t) arg;
	return Yap_ClauseListExtend(x->cl,data,x->pred);
}

