// @file cuddSuport.h
//
//This file contains declarations of cudd internals for support of
// simplecudd.h. It also calls cudd.h, so you shouldn't need anything else
// to interface with cudd.


// cudd.h should be available from external source.

#include <cudd.h>


/**
 ** @brief Maximum variable index.
 **
 ** @details CUDD_MAXINDEX is defined in such a way that on 32-bit and
 ** 64-bit machines one can cast an index to (int) without generating
 ** a negative number.
 */
#if SIZEOF_VOID_P == 8 && SIZEOF_INT == 4
#define CUDD_MAXINDEX		(((DdHalfWord) ~0) >> 1)
#else
#define CUDD_MAXINDEX		((DdHalfWord) ~0)
#endif

/**
 ** @brief The index of constant nodes.
 **
 ** @details This is a synonim for CUDD_MAXINDEX.
 */
#define CUDD_CONST_INDEX	CUDD_MAXINDEX

/**
   @brief Type that is half the size of a pointer.
*/
#if SIZEOF_VOID_P == 8 && SIZEOF_INT == 4
typedef uint32_t DdHalfWord;
#else
typedef uint16_t DdHalfWord;
#endif

/**
 * @brief The two children of a non-terminal node.
 */
struct DdChildren {
    struct DdNode *T;	/**< then (true) child */
    struct DdNode *E;	/**< else (false) child */
};



/**
 * @brief Decision diagram node.
 */
struct DdNode {
    DdHalfWord index;		/**< variable index */
    DdHalfWord ref;		/**< reference count */
    DdNode *next;		/**< next pointer for unique table */
    union {
	CUDD_VALUE_TYPE value;	/**< for constant (terminal) nodes */
	struct DdChildren kids;	/**< for internal nodes */
    } type;			/**< terminal or internal */
};



/**
  @brief Increases the reference count of a node, if it is not
  saturated.

  @details This being a macro, it is faster than Cudd_Ref, but it
  cannot be used in constructs like cuddRef(a = b()).

  @sideeffect none

  @see Cudd_Ref

*/
#define cuddRef(n) cuddSatInc(Cudd_Regular(n)->ref)

#define cuddV(node) ((node)->type.value)

extern DdNode * cuddAllocNode(DdManager *unique);

#define  statLine(dd)


extern DdNode * cuddCacheLookup1(DdManager *table, DdNode * (*)(DdManager *, DdNode *), DdNode *f);
extern void cuddCacheInsert1(DdManager *table, DdNode * (*)(DdManager *, DdNode *), DdNode *f, DdNode *data);


/**
  @brief Decreases the reference count of a node, if it is not
  saturated.

  @details It is primarily used in recursive procedures to decrease
  the ref count of a result node before returning it. This
  accomplishes the goal of removing the protection applied by a
  previous cuddRef. This being a macro, it is faster than Cudd_Deref,
  but it cannot be used in constructs like cuddDeref(a = b()).

  @sideeffect none

  @see Cudd_Deref

*/
#define cuddDeref(n) cuddSatDec(Cudd_Regular(n)->ref)

/**
  @brief Returns the then child of an internal node.

  @details If <code>node</code> is a constant node, the result is
  unpredictable.  The pointer passed to cuddT must be regular.

  @sideeffect none

  @see Cudd_T

*/
#define cuddT(node) ((node)->type.kids.T)


/**
  @brief Returns the else child of an internal node.

  @details If <code>node</code> is a constant node, the result is
  unpredictable.  The pointer passed to cuddE must be regular.

  @sideeffect none

  @see Cudd_E

*/
#define cuddE(node) ((node)->type.kids.E)



extern DdNode * cuddUniqueInter(DdManager *unique, int index, DdNode *T, DdNode *E);

/**
  @brief Saturating increment operator.

  @details Saturation is only necessary on 32-bit machines, where the
  reference count is only 16-bit wide.

  @sideeffect none

  @see cuddSatDec
  
*/
#if SIZEOF_VOID_P == 8 && SIZEOF_INT == 4
#define cuddSatInc(x) ((x)++)
#else
#define cuddSatInc(x) ((x) += (x) != (DdHalfWord)DD_MAXREF)
#endif


/**
  @brief Saturating decrement operator.

  @details Saturation is only necessary on 32-bit machines, where the
  reference count is only 16-bit wide.

  @sideeffect none

  @see cuddSatInc

*/
#if SIZEOF_VOID_P == 8 && SIZEOF_INT == 4
#define cuddSatDec(x) ((x)--)
#else
#define cuddSatDec(x) ((x) -= (x) != (DdHalfWord)DD_MAXREF)
#endif

extern DdNode * cuddUniqueConst(DdManager *unique, CUDD_VALUE_TYPE value);

