#ifndef UP_H
#define UP_H

#include "core/bpx.h"
#include "core/xmalloc.h"
#include "core/stuff.h"
#include "core/idtable.h"
#include "core/idtable_preds.h"
#include "core/error.h"

#ifndef _MSC_VER
#include <unistd.h>
#endif
#ifdef MALLOC_TRACE
#include <mcheck.h>
#endif

/* core binary version */
#define BINARY_VERSION "20070529"

#define INIT_PROB_THRESHOLD 1e-9
#define EPS                 1e-12

#define NULL_TERM ((TERM)(0)) /* reference to null */

/* IEEE 64bit double: 4.94e-324 ... 1.797e+308 (for positive) */
#define HUGE_PROB 1.0e+280
#define TINY_PROB 1.0e-300

/* Data structures for support graphs */
typedef struct ExplGraphPath *EG_PATH_PTR;
struct ExplGraphPath {
    int children_len;
    int sws_len;
    struct ExplGraphNode **children; /* an array of pointers to children nodes */
    struct SwitchInstance **sws; /* an array of pointers to switches */
    double inside;    /* Inside propability of this path */
    double max;       /* Max propability of this path (for Viterbi) */
    struct ExplGraphPath *next; /* next path in a list */
};

typedef struct ViterbiEntry *V_ENT_PTR;
struct ViterbiEntry {
    int goal_id;
    EG_PATH_PTR path_ptr;  /* path for a node */
    int children_len;      /* number of children in the path */
    int *top_n_index;      /* indices of paths in the top-N lists for children */
    double max;  /* max. prob of the path with the sub-paths indicated by top_n_index[] */
};

typedef struct ExplGraphNode *EG_NODE_PTR;
struct ExplGraphNode {
    int id;
    double inside, outside;  /* inside and outside propabilities */
    double max;              /* max probabilities */
    EG_PATH_PTR max_path;    /* pointer to the path with max prob. */
    V_ENT_PTR *top_n;        /* top-N list (for top-N Viterbi) */
    int top_n_len;           /* size of top-N list (for top-N Viterbi) */
    int shared;              /* number of goals which call this subgoal */
    EG_PATH_PTR path_ptr;
    double first_outside;
    char has_first_outside;
    char visited;   /* flag: each node needs to occur at most once  */
};

typedef struct ViterbiList *V_LIST_PTR;
struct ViterbiList {
    V_ENT_PTR entry;
    V_LIST_PTR prev;
    V_LIST_PTR next;
};

/* Data structures for switches (this data structure might have
   a little bit redundancy due to `fixed' and `occ' flags) */
typedef struct SwitchInstance *SW_INS_PTR;
struct SwitchInstance {
    int  id;
    char fixed;   /* parameter is fixed or not */
    char fixed_h; /* hyperparameter is fixed or not */
    char occ;     /* occurring in the current expl graphs or not (temporarily used) */
    double inside;   /* theta (parameter) in ML/MAP  */
    double inside_h; /* alpha (hyperparameter) in VB */
    double smooth;        /* pseudo count which equals alpha - 1.0 */
    double smooth_prolog; /* original pseudo count passed from the Prolog part */
    double pi;
    double best_inside;   /* best theta */
    double best_inside_h; /* best alpha */
    double first_expectation;
    char   has_first_expectation;
    double total_expect;      /* Sigma ru */
    double best_total_expect; /* best Sigma ru */
    int    count;    /* number of occurrences in complete data */
    SW_INS_PTR next; /* connect next instance of the same switch */
};

typedef struct ObservedFactNode *ROOT;
struct ObservedFactNode {
    int id;
    int count; /* number of occurrences */
};

#define CTRLC_PRESSED (toam_signal_vec & INTERRUPT)

/* isfinite()/isnan() on non-C99-complient compilers */

#ifdef _MSC_VER
#include <float.h>
#define isfinite _finite
#define isnan _isnan
#endif

#ifdef LINUX
#ifndef isfinite
#define isfinite finite
#endif
#endif

#endif /* UP_H */
