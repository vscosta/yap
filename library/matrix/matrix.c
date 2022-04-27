/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		matrix.c * Last rev:
 ** mods: * comments:	numerical arrays *
 *									 *
 *************************************************************************/
/**
 * @file: library/matrix.c
 * @brief numeric operations in matrices.
 * @author Vítor Santos Costa
 */
#include "YapConfig.h"
#include "YapInterface.h"
#include <math.h>
#if defined(__MINGW32__) || _MSC_VER
#include <windows.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#endif

/**
 * @addtogroup YapMatrix
 * @{
 */

/* maximal number of dimensions, 1024 should be enough */
#define MAX_DIMS 1024

/*
  A matrix is something of the form

  TYPE = {int,double}
  BASE = integer
  #DIMS = an int
  DIM1
  ...
  DIMn
  DATA in C format.

  floating point matrixes may need to be aligned, so we always have an
  extra element at the end.
*/

typedef enum { INT_MATRIX, FLOAT_MATRIX } mat_data_type;

typedef enum {
  MAT_TYPE = 0,
  MAT_BASE = 1,
  MAT_NDIMS = 2,
  MAT_SIZE = 3,
  MAT_ALIGN = 4,
  MAT_DIMS = 5,
} mat_type;

typedef enum {
  MAT_PLUS = 0,
  MAT_SUB = 1,
  MAT_TIMES = 2,
  MAT_DIV = 3,
  MAT_IDIV = 4,
  MAT_ZDIV = 5,
  MAT_LOG = 6,
  MAT_EXP = 7
} op_type;

/// A matrix
typedef struct m {
  int type;
  //> 'i' integers,
  //> 'f' floating-point
  //> 'b' boolean
  //> 't' term
  bool c_ord;
  union {
    double *data;
    intptr_t *ls;
    bool *bools;
    struct DB_TERM **dbterms;
    YAP_Term *terms;
  };
  intptr_t ndims;
  intptr_t sz;
  intptr_t *dims, base;
} M;

static YAP_Functor MFunctorM, MFunctorFloats;
static YAP_Term MTermTrue, MTermFalse, MTermFail;
static YAP_Atom MAtomC;

static YAP_Int *matrix_long_data(intptr_t *mat, intptr_t ndims) {
  return (YAP_Int *)(mat + (MAT_DIMS + ndims));
}

static double *matrix_double_data(intptr_t *mat, intptr_t ndims) {
  return (double *)(mat + (MAT_DIMS + ndims));
}

static intptr_t matrix_get_offset(intptr_t *mat, intptr_t *indx) {
  intptr_t i, pos = mat[MAT_SIZE], off = 0;

  /* find where we are */
  for (i = 0; i < mat[MAT_NDIMS]; i++) {
    int v;
    pos /= mat[MAT_DIMS + i];
    v = indx[i] - mat[MAT_BASE];
    if (v >= mat[MAT_DIMS + i]) {
      return off;
    }
    off += pos * v;
  }
  return off;
}

static void matrix_get_index(intptr_t *mat, intptr_t offset, intptr_t *indx) {
  intptr_t i, pos = mat[MAT_SIZE];

  /* find where we are */
  for (i = 0; i < mat[MAT_NDIMS]; i++) {
    pos /= mat[MAT_DIMS + i];
    indx[i] = offset / pos;
    offset = offset % pos;
  }
}

static size_t GET_OFFSET(YAP_Term t, M *mat, intptr_t *poffset) {
    intptr_t i, off = 0, pos = mat->sz;
    if (YAP_IsIntTerm(t)) {
        *poffset = YAP_IntOfTerm(t);
        return true;
    }
    /* find where we are */
    for (i = 0; i < mat->ndims; i++) {
        size_t v;
        if (!YAP_IsPairTerm(t))
            YAP_Error(TYPE_ERROR, t, NULL);
        v = YAP_IntOfTerm(YAP_HeadOfTerm(t)) - mat->base;
        pos /= mat->dims[i];
        off += pos * v;
        t = YAP_TailOfTerm(t);
    }
    *poffset = off;
    return true;
}


static YAP_Bool GET_INDEX(M *mat, intptr_t off, YAP_Term *tf) {
    intptr_t i, pos = mat->sz;
    YAP_Term t;
    /* find where we are */
    *tf = 0;
  /* find where we are */
  for (i = 0; i < mat->ndims; i++) {
      t = YAP_MkNewPairTerm();
      if (*tf==0)
	*tf = t;
      pos /= mat->dims[i];
      YAP_Unify(YAP_HeadOfTerm(t),YAP_MkIntTerm(off/pos));
      off = off % pos;
      *tf = YAP_TailOfTerm(t);
  };
  return YAP_Unify(t,YAP_ARG3);
}

static void matrix_next_index(intptr_t *dims, intptr_t ndims, intptr_t *indx) {
  intptr_t i;

  /* find where we are */
  for (i = ndims; i > 0;) {
    i--;
    indx[i]++;
    if (indx[i] != dims[i])
      return;
    indx[i] = 0;
  }
}

static int GET_MATRIX(YAP_Term inp, M *o) {
  intptr_t *mat;
  o->base = 0;
  // source: stack
  //
  if (YAP_IsApplTerm(inp)) {
    if ((mat = YAP_BlobOfTerm(inp))) {
      o->type = mat[MAT_TYPE] == FLOAT_MATRIX ? 'f' : 'i';
      o->c_ord = true;
      o->sz = mat[MAT_SIZE];
      o->ndims = mat[MAT_NDIMS];
      o->data = (double *)(mat + (MAT_DIMS + o->ndims));
      o->dims = mat + MAT_DIMS;
      return true;
    } else {
      YAP_Functor f = YAP_FunctorOfTerm(inp);
      // original, generic matrix
      if (f == MFunctorM)
	{
	  YAP_Term bases = YAP_IntOfTerm(YAP_ArgOfTerm(4,inp));
	  o->sz = YAP_IntOfTerm(YAP_ArgOfTerm(3,inp));
	  if (YAP_IsIntTerm(bases))
	    o->base = YAP_IntOfTerm(bases);
	  else
	    o->base = YAP_IntOfTerm(YAP_HeadOfTerm(bases));
	  o->c_ord = o->base == 0;
	  o->ndims = YAP_IntOfTerm(YAP_ArgOfTerm(2,inp));
	  o->dims = malloc(o->ndims*sizeof(intptr_t));
	  YAP_Term l = YAP_ArgOfTerm(1,inp);
	  intptr_t *d = o->dims;
	  while(YAP_IsPairTerm(l)) {
	    *d++ = YAP_IntOfTerm(YAP_HeadOfTerm(l));
	    l = YAP_TailOfTerm(l);
	  }
	  o->terms=YAP_ArgsOfTerm(YAP_ArgOfTerm(inp,5))+1;
	}
      else if (f == MFunctorFloats) // used to pass floats to external code floats(Size,Data))
	{	  
	o->sz = YAP_IntOfTerm(YAP_ArgOfTerm(2, inp));
	o->c_ord = true;
	o->ndims = 1;
	o->dims = &o->sz;
	o->type = 'f';
	o->data = (double *)YAP_IntOfTerm(YAP_ArgOfTerm(1, inp));
	return true;
	}
      else // generic compound term
	{
	o->sz = YAP_ArityOfFunctor(f);
	o->c_ord = true;
	o->ndims = 1;
	o->dims = &o->sz;
	o->type = 't';
	o->terms = YAP_ArgsOfTerm(inp);
	  
	}
    }
  } else if (YAP_IsAtomTerm(inp)) {
    if ((o->data = YAP_FetchArray(inp, &o->sz, &o->type))) {
      // old-style arraysx
      //      printf( "%p %d %c\n",o->data , o->sz, o->type);
      if (o->sz > 0) {
        o->ndims = 1;
        o->dims = &o->sz;
        o->c_ord = true;
        return true;
      }
    }
  }
  return false;
}

static bool IS_MATRIX(YAP_Term inp) {
  intptr_t *mat;
  if (YAP_IsApplTerm(inp)) {
    if ( (mat = YAP_BlobOfTerm(inp))) {
    return
      mat[MAT_TYPE] == FLOAT_MATRIX ||
      mat[MAT_TYPE] == INT_MATRIX;
    }
      YAP_Functor f = YAP_FunctorOfTerm(inp);
      return
	f == MFunctorM ||
	f == MFunctorFloats;
      return (YAP_Term)(f) > 4096; // hack!!
  } else if (YAP_IsAtomTerm(inp)) {
    intptr_t size;    int type; 
    if (YAP_FetchArray(inp, &size, &type)) {
      return true;
    }
    }
      return false;

}

static YAP_Bool is_matrix(void) {
  return  IS_MATRIX(YAP_ARG1);
}


static YAP_Term new_int_matrix(intptr_t ndims, intptr_t dims[],
                               YAP_Int data[]) {
  intptr_t sz;
  intptr_t i, nelems = 1;
  YAP_Term blob;
  intptr_t *mat;
  YAP_Int *bdata;
  int idims[MAX_DIMS];
  
  /* in case we don't have enough room and need to shift the stack, we can't
     really afford to keep a pointer to the global */
  for (i = 0; i < ndims; i++) {
    idims[i] = dims[i];
    nelems *= dims[i];
  }
  sz = ((MAT_DIMS + 1) * sizeof(int) + ndims * sizeof(int) +
        nelems * sizeof(YAP_Int)) /
       sizeof(YAP_CELL);

  blob = YAP_MkBlobTerm(sz);
  if (blob == YAP_TermNil()) {
    return blob;
  }
  mat = (intptr_t *)YAP_BlobOfTerm(blob);
  mat[MAT_TYPE] = INT_MATRIX;
  mat[MAT_BASE] = 0;
  mat[MAT_NDIMS] = ndims;
  mat[MAT_SIZE] = nelems;
  for (i = 0; i < ndims; i++) {
    mat[MAT_DIMS + i] = idims[i];
  }
  bdata = matrix_long_data(mat, ndims);
  if (data)
    memmove((void *)bdata, (void *)data, sizeof(double) * nelems);
  else
    memset(bdata, 0, nelems);
  return blob;
}


static YAP_Term new_float_matrix(intptr_t ndims, intptr_t dims[],
                                 double data[]) {
  intptr_t sz;
  intptr_t i, nelems = 1;
  YAP_Term blob;
  intptr_t *mat;
  double *bdata;
  int idims[MAX_DIMS];

  /* in case we don't have enough room and need to shift the stack, we can't
     really afford to keep a pointer to the global */
  for (i = 0; i < ndims; i++) {
    idims[i] = dims[i];
    nelems *= dims[i];
  }
  sz = ((MAT_DIMS + 1) * sizeof(int) + ndims * sizeof(int) +
        (nelems + 1) * sizeof(double) + (sizeof(YAP_CELL) - 1)) /
       sizeof(YAP_CELL);
  blob = YAP_MkBlobTerm(sz);
  if (blob == YAP_TermNil())
    return blob;
  mat = YAP_BlobOfTerm(blob);
  mat[MAT_TYPE] = FLOAT_MATRIX;
  mat[MAT_BASE] = 0;
  mat[MAT_NDIMS] = ndims;
  mat[MAT_SIZE] = nelems;
  for (i = 0; i < ndims; i++) {
    mat[MAT_DIMS + i] = idims[i];
  }
  bdata = matrix_double_data(mat, ndims);
  if (data)
    memmove((void *)bdata, (void *)data, sizeof(double) * nelems);
  return blob;
}

static YAP_Bool scan_dims(intptr_t ndims, YAP_Term tl,
                          intptr_t dims[MAX_DIMS]) {
  int i;

  for (i = 0; i < ndims; i++) {
    YAP_Term th;
    int d;

    if (!YAP_IsPairTerm(tl)) {
      return FALSE;
    }
    th = YAP_HeadOfTerm(tl);
    if (!YAP_IsIntTerm(th)) {
      /* ERROR */
      return FALSE;
    }
    d = YAP_IntOfTerm(th);
    if (d < 0) {
      /* ERROR */
      return FALSE;
    }
    dims[i] = d;
    tl = YAP_TailOfTerm(tl);
  }
  if (tl != YAP_TermNil()) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static YAP_Bool cp_int_matrix(YAP_Term tl, YAP_Term matrix) {
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  YAP_Int *j = matrix_long_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    YAP_Term th;
    int d;

    if (!YAP_IsPairTerm(tl)) {
      return FALSE;
    }
    th = YAP_HeadOfTerm(tl);
    if (!YAP_IsIntTerm(th)) {
      /* ERROR */
      return FALSE;
    }
    d = YAP_IntOfTerm(th);
    j[i] = d;
    tl = YAP_TailOfTerm(tl);
  }
  if (tl != YAP_TermNil()) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static YAP_Bool cp_float_matrix(YAP_Term tl, YAP_Term matrix) {
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(matrix);
  intptr_t i, nelems = mat[MAT_SIZE];
  double *j = matrix_double_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    YAP_Term th;
    double d;

    if (!YAP_IsPairTerm(tl)) {
      return FALSE;
    }
    th = YAP_HeadOfTerm(tl);
    if (YAP_IsIntTerm(th)) {
      d = YAP_IntOfTerm(th);
    } else if (!YAP_IsFloatTerm(th)) {
      /* ERROR */
      return FALSE;
    } else {
      d = YAP_FloatOfTerm(th);
    }
    j[i] = d;
    tl = YAP_TailOfTerm(tl);
  }
  if (tl != YAP_TermNil()) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}

static YAP_Bool set_int_matrix(YAP_Term matrix, YAP_Int set) {
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  YAP_Int *j = matrix_long_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    j[i] = set;
  }
  return TRUE;
}

static YAP_Bool set_float_matrix(YAP_Term matrix, double set) {
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  double *j = matrix_double_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    j[i] = set;
  }
  return TRUE;
}

static YAP_Bool new_ints_matrix(void) {
  intptr_t ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out;
  intptr_t dims[MAX_DIMS];
  YAP_Term data;

  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_int_matrix(ndims, dims, NULL);
  if (out == YAP_TermNil())
    return FALSE;
  data = YAP_ARG3;
  if (!YAP_IsVarTerm(data) && !cp_int_matrix(data, out))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static YAP_Bool new_ints_matrix_set(void) {
  intptr_t ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out, tset = YAP_ARG3;
  intptr_t dims[MAX_DIMS];
  YAP_Int set;

  if (!YAP_IsIntTerm(tset)) {
    return FALSE;
  }
  set = YAP_IntOfTerm(tset);
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  size_t sz = 1, i;
  for (i = 0; i < ndims; i++)
    sz *= dims[i];
  if (YAP_RequiresExtraStack(sz * sizeof(YAP_CELL))<0)
    return false;
  out = new_int_matrix(ndims, dims, NULL);
  return set_int_matrix(out, set) && YAP_Unify(YAP_ARG4, out);
}

static YAP_Bool new_floats_matrix(void) {
  intptr_t ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out, data = YAP_ARG3;
  intptr_t dims[MAX_DIMS];
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_float_matrix(ndims, dims, NULL);
  if (out == YAP_TermNil())
    return FALSE;
  if (!YAP_IsVarTerm(data) && !cp_float_matrix(data, out))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

  static YAP_Bool new_floats_matrix_set(void) {
  intptr_t ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out, tset = YAP_ARG3;
  intptr_t dims[MAX_DIMS];
  double set;

  if (!YAP_IsFloatTerm(tset)) {
    return FALSE;
  }
  set = YAP_FloatOfTerm(tset);
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_float_matrix(ndims, dims, NULL);
  if (!set_float_matrix(out, set))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static YAP_Term mk_int_list(intptr_t nelems, intptr_t *data) {
  YAP_Term tn = YAP_TermNil();
  YAP_Term tf = tn;
  int i = 0;

  for (i = nelems - 1; i >= 0; i--) {
    tf = YAP_MkPairTerm(YAP_MkIntTerm(data[i]), tf);
    if (tf == tn) {
      /* error */
      return tn;
    }
  }
  return tf;
}

static YAP_Term mk_int_list2(intptr_t nelems, int base, intptr_t *data) {
  YAP_Term tn = YAP_TermNil();
  YAP_Term tf = tn;
  int i = 0;

  for (i = nelems - 1; i >= 0; i--) {
    tf = YAP_MkPairTerm(YAP_MkIntTerm(data[i] + base), tf);
    if (tf == tn) {
      /* error */
      return tn;
    }
  }
  return tf;
}

static YAP_Term mk_rep_int_list(intptr_t nelems, int data) {
  YAP_Term tn = YAP_TermNil();
  YAP_Term tf = tn;
  int i = 0;

  for (i = nelems - 1; i >= 0; i--) {
    tf = YAP_MkPairTerm(YAP_MkIntTerm(data), tf);
    if (tf == tn) {
      /* error */
      return tn;
    }
  }
  return tf;
}

static void matrix_float_set(intptr_t *mat, intptr_t *indx, double nval) {
  intptr_t off = 0;

  off = matrix_get_offset(mat, indx);
  (matrix_double_data(mat, mat[MAT_NDIMS]))[off] = nval;
}

static void matrix_long_set(intptr_t *mat, intptr_t *indx, YAP_Int nval) {
  intptr_t off = matrix_get_offset(mat, indx);
  (matrix_long_data(mat, mat[MAT_NDIMS]))[off] = nval;
}


static YAP_Bool matrix_get_one(void) {
  M mat;
  YAP_Term tf;
  intptr_t offset;
  if (GET_MATRIX(YAP_ARG1, &mat)>=0 && GET_OFFSET(YAP_ARG2, &mat, &offset)) {
    switch (mat.type) {
    case 'f':
      tf = YAP_MkFloatTerm(mat.data[offset]);
      return YAP_Unify(YAP_ARG3, tf);
    case 'i':
      tf = YAP_MkIntTerm(mat.ls[offset]);
      return YAP_Unify(YAP_ARG3, tf);
      return true;
    case 'b':
      return YAP_Unify(YAP_ARG3, (mat.bools[offset] ? MTermTrue : MTermFalse));
      ;
    case 't':
      return YAP_Unify(YAP_ARG3, mat.terms[offset]);
    default:
      return false;
    }
  }
  return false;
}

//> M[off] <== int|float
static YAP_Bool matrix_set_one(void) {
  M mat;
  intptr_t offset;
  if (GET_MATRIX(YAP_ARG1, &mat)<0 || !(GET_OFFSET(YAP_ARG2, &mat, &offset))) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f':
    if (YAP_IsIntTerm(YAP_ARG3)) {
      mat.data[offset] = YAP_IntOfTerm(YAP_ARG3);
    } else if (YAP_IsFloatTerm(YAP_ARG3)) {
      mat.data[offset] = YAP_FloatOfTerm(YAP_ARG3);
    }
    return true;
  case 'i':
    if (YAP_IsIntTerm(YAP_ARG3))
      mat.ls[offset] = YAP_IntOfTerm(YAP_ARG3);
    else if (YAP_IsFloatTerm(YAP_ARG3))
      mat.ls[offset] = YAP_FloatOfTerm(YAP_ARG3);
    return true;
  case 'b':
    if (YAP_ARG3 == MTermTrue || YAP_ARG3 == YAP_MkIntTerm(1)) {
      mat.bools[offset] = true;
      return true;
    } else if (YAP_ARG3 == MTermFalse || YAP_ARG3 == YAP_MkIntTerm(0)) {
      mat.bools[offset] = false;
      return true;
    } else {
      return false;
    }
  case 't':
    mat.terms[offset] = YAP_ARG3;
    return true;
  default:
    return false;
  }
}

static YAP_Term flist(YAP_Term t, double *f) {
    YAP_Term hd = YAP_HeadOfTerm(t);
    if (YAP_IsIntTerm(hd)) {
        *f = YAP_IntOfTerm(hd);
    } else if (YAP_IsFloatTerm(hd)) {
        *f = YAP_FloatOfTerm(hd);
    } else {
        return 0;
    }
    return YAP_TailOfTerm(t);
}

static YAP_Term ilist(YAP_Term t, YAP_Int *f) {
    YAP_Term hd = YAP_HeadOfTerm(t);
    if (YAP_IsIntTerm(hd)) {
        *f = YAP_IntOfTerm(hd);
    } else if (YAP_IsFloatTerm(hd)) {
        *f = YAP_FloatOfTerm(hd);
    } else {
        return 0;
    }
    return YAP_TailOfTerm(t);
}

static YAP_Term blist(YAP_Term t, YAP_Bool *f) {
    YAP_Term hd = YAP_HeadOfTerm(t);
          if (hd == MTermTrue || hd == YAP_MkIntTerm(1)) {
      *f = true;
    } else if (hd == MTermFalse || hd == YAP_MkIntTerm(0)) {
              *f = false;
          }else {
              return 0;
          }
    return YAP_TailOfTerm(t);
}

//> M[off] <== int|float
static YAP_Bool matrix_set_all(void) {
    M mat;
    intptr_t offset, sz ;
    if (GET_MATRIX(YAP_ARG1, &mat)<0) {
        /* Error */
        return false;
    }
    sz = mat.sz;
    switch (mat.type) {
        case 'f': {
            double d;
            if (YAP_IsIntTerm(YAP_ARG2)) {
                d = YAP_IntOfTerm(YAP_ARG2);
            } else if (YAP_IsFloatTerm(YAP_ARG2)) {
                d = YAP_FloatOfTerm(YAP_ARG2);
            } else {
                YAP_Term t = YAP_ARG2;
                for (offset = 0; offset < sz; offset++)
                    t = flist(t, mat.data + offset);
                return true;
            }
            for (offset = 0; offset < sz; offset++)
                mat.data[offset] = d;
            return true;
        }
        case 'b': {
            YAP_Bool d;
          if (YAP_ARG2 == MTermTrue || YAP_ARG2 == YAP_MkIntTerm(1)) {
      d = true;
      return true;
    } else if (YAP_ARG2 == MTermFalse || YAP_ARG2 == YAP_MkIntTerm(0)) {
              d = false;
          }else {
              YAP_Term t = YAP_ARG2;
              for (offset = 0; offset < sz; offset++)
                  t = blist(t, mat.bools+offset);
              return true;
          }
            for (offset = 0; offset < sz; offset++)
                mat.bools[offset] = d;
            return true;
    }
        case 'i':
        {
    YAP_Int d;
            if (YAP_IsIntTerm(YAP_ARG2)) {
                d = YAP_IntOfTerm(YAP_ARG2);
            } else if (YAP_IsFloatTerm(YAP_ARG2)) {
                d = YAP_FloatOfTerm(YAP_ARG2         );
            } else {
                YAP_Term t = YAP_ARG2;
                for (offset = 0; offset < sz; offset++)
                    t = ilist(t, mat.ls+offset);
                return true;
            }
            for (offset = 0; offset < sz; offset++)
                mat.ls[offset] = d;
            return true;
        }
        case 't': {
            YAP_Term d = YAP_ARG2;
            for (offset = 0; offset < sz; offset++)
                mat.terms[offset] = d;
            return true;
        }
        default:
            return false;
    }
}

//> M <== [int|float]
static YAP_Bool matrix_add_to_all(void) {
  M mat;
  intptr_t offset, sz;
  if (GET_MATRIX(YAP_ARG1, &mat)<0) {
    /* Error */
    return false;
  }
  sz = mat.sz;
  switch (mat.type) {
  case 'f': {
    YAP_Float f;
    if (YAP_IsIntTerm(YAP_ARG3)) {
      f = YAP_IntOfTerm(YAP_ARG3);
    } else if (YAP_IsFloatTerm(YAP_ARG3)) {
      f = YAP_FloatOfTerm(YAP_ARG3);
    } else {
      return false;
    }
    for (offset = 0; offset < sz; offset++) {
      mat.data[offset] += f;
    }
  }
    return true;
  case 'i': {
    YAP_Int i;
    if (YAP_IsIntTerm(YAP_ARG3)) {
      i = YAP_IntOfTerm(YAP_ARG3);
    } else if (YAP_IsFloatTerm(YAP_ARG3)) {
      i = YAP_FloatOfTerm(YAP_ARG3);
    } else {
      return false;
    }
    for (offset = 0; offset < sz; offset++) {
      mat.ls[offset] += i;

    }
    return true;
  }
  case 'b':
      return false;
  case 't':
       return false;

   default:
    return false;
  }
}

//> M[off] <== int|float
static YAP_Bool matrix_inc(void) {
  M mat;
  intptr_t offset;
  if (GET_MATRIX(YAP_ARG1, &mat)<0 || !(GET_OFFSET(YAP_ARG2, &mat, &offset))) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f':
    mat.data[offset] += 1.0;
    return true;
  case 'i':
    mat.ls[offset] += 1;
    return true;
  case 'b':
  case 't':
  default:
    return false;
  }
}

//> M[off] <== int|float
static YAP_Bool matrix_inc3(void) {
  M mat;
  intptr_t offset;
  if (GET_MATRIX(YAP_ARG1, &mat)<0 || !(GET_OFFSET(YAP_ARG2, &mat, &offset))) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f':
    mat.data[offset] += 1.0;
    return YAP_Unify(YAP_MkFloatTerm(mat.data[offset]), YAP_ARG3);
  case 'i':
    mat.ls[offset] += 1;
    return YAP_Unify(YAP_MkIntTerm(mat.data[offset]), YAP_ARG3);
  case 'b':
  case 't':
  default:
    return false;
  }
}

/** @pred matrix_dec(+ _Matrix_,+ _Position_,- _Element_)


Decrement the element of  _Matrix_ at position  _Position_ and
unify with  _Element_.


*/

static YAP_Bool matrix_dec(void) {
  M mat;
  intptr_t offset;
  if (GET_MATRIX(YAP_ARG1, &mat)<0 || !(GET_OFFSET(YAP_ARG2, &mat, &offset))) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f':
    mat.data[offset] -= 1.0;
    return true;
  case 'i':
    mat.ls[offset] -= 1;
    return true;
  case 'b':
  case 't':
  default:
    return false;
  }
}

//> M[off] <== int|float
static YAP_Bool matrix_dec3(void) {
  M mat;
  intptr_t offset;
  if (GET_MATRIX(YAP_ARG1, &mat)<0 || !(GET_OFFSET(YAP_ARG2, &mat, &offset))) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f':
    mat.data[offset] -= 1.0;
    return YAP_Unify(YAP_MkFloatTerm(mat.data[offset]), YAP_ARG3);
  case 'i':
    mat.ls[offset] -= 1;
    return YAP_Unify(YAP_MkIntTerm(mat.ls[offset]), YAP_ARG3);
  case 'b':
  case 't':
  default:
    return false;
  }
}


static YAP_Bool matrix_min(void) {
  M mat;
  if (GET_MATRIX(YAP_ARG1, &mat)<0) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f': {
      intptr_t i,  sz = mat.sz;
      double max = mat.data[0];
      for (i = 1; i < sz; i++) {
          if (mat.data[i] < max) {
              max = mat.data[i];
          }
      }
      return YAP_Unify(YAP_MkFloatTerm(max), YAP_ARG2);
  }
  case 'i': {
          intptr_t i, sz = mat.sz;
          YAP_Int max = mat.ls[0];
          for (i = 1; i < sz; i++) {
              if (mat.ls[i] < max) {
                  max = mat.ls[i];
              }
          }
          return YAP_Unify(YAP_MkIntTerm(max), YAP_ARG2);
      }
  case 'b':
  case 't':
  default:
    return false;
  }
}


static YAP_Bool matrix_minarg(void) {
  M mat;
  if (GET_MATRIX(YAP_ARG1, &mat)<0) {
    /* Error */
    return false;
  }
    intptr_t i, off = 0, sz = mat.sz;
  switch (mat.type) {
  case 'f': {
      double max = mat.data[0];
      for (i = 1; i < sz; i++) {
          if (mat.data[i] < max) {
              max = mat.data[i];
              off = i;
          }
      }
  }
  case 'i': {
          YAP_Int max = mat.ls[0];
          for (i = 1; i < sz; i++) {
              if (mat.ls[i] < max) {
                  max = mat.ls[i];
                  off = i;
              }
          }
     }
  case 'b':
  case 't':
  default:
    return false;
  }
  YAP_Term t = 0, tf = YAP_ARG3;
  if (GET_INDEX(&mat, off, &t) &&
      YAP_Unify(tf,t))
    return true;
  return false;
 }


static YAP_Bool matrix_max(void) {
  M mat;
  if (GET_MATRIX(YAP_ARG1, &mat)<0) {
    /* Error */
    return false;
  }
  switch (mat.type) {
  case 'f': {
      intptr_t i,  sz = mat.sz;
      double max = mat.data[0];
      for (i = 1; i < sz; i++) {
          if (mat.data[i] > max) {
              max = mat.data[i];
          }
      }
      return YAP_Unify(YAP_MkFloatTerm(max), YAP_ARG2);
  }
  case 'i': {
          intptr_t i, sz = mat.sz;
          YAP_Int max = mat.ls[0];
          for (i = 1; i < sz; i++) {
              if (mat.ls[i] > max) {
                  max = mat.ls[i];
              }
          }
          return YAP_Unify(YAP_MkIntTerm(max), YAP_ARG2);
      }
  case 'b':
  case 't':
  default:
    return false;
  }
}


static YAP_Bool matrix_maxarg(void) {
    M mat;
    if (GET_MATRIX(YAP_ARG1, &mat)<0) {
        /* Error */
        return false;
    }
    intptr_t i, off = 0, sz = mat.sz;
    switch (mat.type) {
        case 'f': {
            double max = mat.data[0];
            for (i = 1; i < sz; i++) {
                if (mat.data[i] > max) {
                    max = mat.data[i];
                    off = i;
                }
            }
	    return YAP_Unify(YAP_MkIntTerm(off), YAP_ARG2);
        }
        case 'i': {
            YAP_Int max = mat.ls[0];
            for (i = 1; i < sz; i++) {
                if (mat.ls[i] > max) {
                    max = mat.ls[i];
                    off = i;
                }
            }
	    return YAP_Unify(YAP_MkIntTerm(off), YAP_ARG2);
        }
        case 'b':
        case 't':
        default:
            return false;
    }
}


static YAP_Bool matrix_dims(void) {
    M *mat, m0;
    YAP_Term tf;
    mat = &m0;

   int rc = GET_MATRIX(YAP_ARG1, mat);
    if (rc<0) {
        /* Error */
        return FALSE;
    }
    tf = mk_int_list(mat->ndims, mat->dims);
    return YAP_Unify(YAP_ARG2, tf);
}


/** @pred matrix_to_list(+ _Matrix_,- _Elems_)



Unify  _Elems_ with the list including all the elements in  _Matrix_.


*/
static YAP_Bool matrix_to_list(void) {
  M mat;

  int rc = GET_MATRIX(YAP_ARG1, &mat);
    if (rc<0) {
        /* Error */
        return false;
    }
    YAP_Term t = YAP_TermNil();
switch (mat.type)
  {
  case 'i':
    {
    if (YAP_RequiresExtraStack(mat.sz*5+4096) < 0)
      return false;
    int i=mat.sz;
    while (i) {
      i--;
      t = YAP_MkPairTerm(YAP_MkIntTerm(mat.ls[i]),t);
    }
    }
    break;
      case 'f':
    {
    if (YAP_RequiresExtraStack(mat.sz*5+4096) < 0)
      return false;
    int i=mat.sz;
    while (i) {
      i--;
      t = YAP_MkPairTerm(YAP_MkFloatTerm(mat.ls[i]),t);
    }
    }
    break;
      case 'b':
    {
    if (YAP_RequiresExtraStack(mat.sz*2+4096) < 0)
      return false;
    int i=mat.sz;
    while (i) {
      i--;
      t = YAP_MkPairTerm((mat.bools[i] ? MTermTrue : MTermFalse),t);
    }
    }
    break;
      case 't':
    {
    if (YAP_RequiresExtraStack(mat.sz*2+4096) < 0)
      return false;
    int i=mat.sz;
    while (i) {
      i--;
      t = YAP_MkPairTerm(mat.terms[i],t);
    }
    }
    break;
  default:
    return false;
  }
  return YAP_Unify(YAP_ARG2, t);
}

static YAP_Bool matrix_set_base(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  mat[MAT_BASE] = YAP_IntOfTerm(YAP_ARG2);
  return TRUE;
}

static YAP_Bool matrix_dims3(void) {
  intptr_t *mat;
  YAP_Term tf, tof;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  tf = mk_int_list(mat[MAT_NDIMS], mat + MAT_DIMS);
  tof = mk_rep_int_list(mat[MAT_NDIMS], mat[MAT_BASE]);
  return YAP_Unify(YAP_ARG2, tf) && YAP_Unify(YAP_ARG3, tof);
}

/** @pred matrix_size(+ _Matrix_,- _NElems_)



Unify  _NElems_ with the number of elements for  _Matrix_.


*/
static YAP_Bool matrix_size(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(mat[MAT_SIZE]));
}

static YAP_Bool matrix_ndims(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(mat[MAT_NDIMS]));
}

static YAP_Bool matrix_type(void) {
  intptr_t *mat;
  YAP_Term tf;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* not an error, it may be called on a term matrix */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    tf = YAP_MkIntTerm(0);
  } else {
    tf = YAP_MkIntTerm(1);
  }
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_arg_to_offset(void) {
  M mat;
  intptr_t off;
  if (GET_MATRIX(YAP_ARG1, &mat)<0)
    return false;
  if (!GET_OFFSET(YAP_ARG2, &mat, &off))
      return false;
  return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(off));
}

static YAP_Bool matrix_offset_to_arg(void) {
  intptr_t indx[MAX_DIMS];
  M mat;
  YAP_Term off;
  if (GET_MATRIX(YAP_ARG1, &mat)<0)
    return false;
  if (!GET_INDEX(&mat, YAP_ARG2, &off))
      return false;
  return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(off));
  YAP_Term tf = mk_int_list2(mat.ndims, mat.base, indx);
  return YAP_Unify(YAP_ARG3, tf);
}


static YAP_Bool matrix_log_all(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    return FALSE;
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    intptr_t i;

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      data[i] = log(data[i]);
    }
  }
  return TRUE;
}

static YAP_Bool matrix_log_all2(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Term out;
    YAP_Int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    double *ndata;
    intptr_t i;
    intptr_t *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (intptr_t *)YAP_BlobOfTerm(out);
    ndata = matrix_double_data(nmat, mat[MAT_NDIMS]);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      ndata[i] = log((double)data[i]);
    }
    if (YAP_IsVarTerm(YAP_ARG2)) {
      return YAP_Unify(YAP_ARG2, out);
    }
  } else {
    YAP_Term out;
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]), *ndata;
    intptr_t i;
    intptr_t *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (intptr_t *)YAP_BlobOfTerm(out);
    ndata = matrix_double_data(nmat, mat[MAT_NDIMS]);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      ndata[i] = log(data[i]);
    }
    if (YAP_IsVarTerm(YAP_ARG2)) {
      return YAP_Unify(YAP_ARG2, out);
    }
  }
  return TRUE;
}

static YAP_Bool matrix_exp_all(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    return FALSE;
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    intptr_t i;

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      data[i] = exp(data[i]);
    }
  }
  return TRUE;
}

static YAP_Bool matrix_exp2_all(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    return FALSE;
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    intptr_t i;
    double max = data[0];

    for (i = 1; i < mat[MAT_SIZE]; i++) {
      if (data[i] > max)
        max = data[i];
    }

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      data[i] = exp(data[i] - max);
    }
  }
  return TRUE;
}

static YAP_Bool matrix_exp_all2(void) {
  intptr_t *mat;

  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Term out;
    YAP_Int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    double *ndata;
    intptr_t i;
    intptr_t *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (intptr_t *)YAP_BlobOfTerm(out);
    ndata = matrix_double_data(nmat, mat[MAT_NDIMS]);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      ndata[i] = exp((double)data[i]);
    }
    if (YAP_IsVarTerm(YAP_ARG2)) {
      return YAP_Unify(YAP_ARG2, out);
    }
  } else {
    YAP_Term out;
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]), *ndata;
    intptr_t i;
    intptr_t *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (intptr_t *)YAP_BlobOfTerm(out);
    ndata = matrix_double_data(nmat, mat[MAT_NDIMS]);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      ndata[i] = exp(data[i]);
    }
    if (YAP_IsVarTerm(YAP_ARG2)) {
      return YAP_Unify(YAP_ARG2, out);
    }
  }
  return TRUE;
}

/** @pred matrix_sum(+ _Matrix_,+ _Sum_)



Unify  _Sum_ with the sum of all elements in matrix   _Matrix_.


*/
static YAP_Bool matrix_sum(void) {
    M mat;
    YAP_Term tf;
    int i;
    if (GET_MATRIX(YAP_ARG1, &mat)>=0) {
        if (mat.type == 'i') {
            YAP_Int sum = 0;

            for (i = 0; i < mat.sz; i++) {
                sum += mat.ls[i];
            }
            tf = YAP_MkIntTerm(sum);
        } else {
            double *data = mat.data;
            intptr_t i;
            double sum = 0.0;
            // function KahanSum(input)
            double c = 0.0; // A running compensation for lost low-order bits.
            for (i = 0; i < mat.sz; i++) {
                double y = data[i] - c; // So far, so good: c is zero.
                double t =
                        sum +
                        y; // Alas, sum is big, y small, so low-order digits of y are lost.
                c = (t - sum) - y; // (t - sum) cancels the high-order part of y;
                // subtracting y recovers negative (low part of y)
                sum = t; // Algebraically, c should always be zero. Beware
                // overly-aggressive optimizing compilers!
            }
            tf = YAP_MkFloatTerm(sum);
        }
    }
  return YAP_Unify(YAP_ARG2, tf);
}

static void add_int_lines(int total, intptr_t nlines, YAP_Int *mat0,
                          YAP_Int *matf) {
  intptr_t ncols = total / nlines, i;
  for (i = 0; i < ncols; i++) {
    YAP_Int sum = 0;
    int j;

    for (j = i; j < total; j += ncols) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static void add_double_lines(int total, intptr_t nlines, double *mat0,
                             double *matf) {
  intptr_t ncols = total / nlines, i;
  for (i = 0; i < ncols; i++) {
    double sum = 0;
    int j;

    for (j = i; j < total; j += ncols) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static YAP_Bool matrix_agg_lines(void) {
  intptr_t *mat;
  YAP_Term tf;
  YAP_Term top = YAP_ARG2;
  op_type op;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* create a new array without first dimension */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;
    intptr_t dims = mat[MAT_NDIMS];
    intptr_t *nmat;

    tf = new_int_matrix(dims - 1, mat + (MAT_DIMS + 1), NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, dims);
    ndata = matrix_long_data(nmat, dims - 1);
    if (op == MAT_PLUS) {
      add_int_lines(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  } else {
    double *data, *ndata;
    intptr_t dims = mat[MAT_NDIMS];
    intptr_t *nmat;

    tf = new_float_matrix(dims - 1, mat + (MAT_DIMS + 1), NULL);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    if (tf == YAP_TermNil())
      return FALSE;
    data = matrix_double_data(mat, dims);
    ndata = matrix_double_data(nmat, dims - 1);
    if (op == MAT_PLUS) {
      add_double_lines(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG3, tf);
}

static void add_int_cols(int total, intptr_t nlines, YAP_Int *mat0,
                         YAP_Int *matf) {
  intptr_t ncols = total / nlines, i, j = 0;
  for (i = 0; i < nlines; i++) {
    YAP_Int sum = 0;
    int max = (i + 1) * ncols;

    for (; j < max; j++) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static void add_double_cols(int total, intptr_t nlines, double *mat0,
                            double *matf) {
  intptr_t ncols = total / nlines, i, j = 0;
  for (i = 0; i < nlines; i++) {
    double sum = 0;
    int max = (i + 1) * ncols;

    for (; j < max; j++) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static YAP_Bool matrix_agg_cols(void) {
  intptr_t *mat;
  YAP_Term tf;
  YAP_Term top = YAP_ARG2;
  op_type op;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* create a new array without first dimension */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;
    intptr_t dims = mat[MAT_NDIMS];
    intptr_t *nmat;

    tf = new_int_matrix(1, mat + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, dims);
    ndata = matrix_long_data(nmat, 1);
    if (op == MAT_PLUS) {
      add_int_cols(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  } else {
    double *data, *ndata;
    intptr_t dims = mat[MAT_NDIMS];
    intptr_t *nmat;

    tf = new_float_matrix(1, mat + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, dims);
    ndata = matrix_double_data(nmat, 1);
    if (op == MAT_PLUS) {
      add_double_cols(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG3, tf);
}

static void div_int_by_lines(int total, intptr_t nlines, YAP_Int *mat1,
                             YAP_Int *mat2, double *ndata) {
  intptr_t ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = ((double)mat1[i]) / mat2[i % ncols];
  }
}

static void div_int_by_dlines(int total, intptr_t nlines, YAP_Int *mat1,
                              double *mat2, double *ndata) {
  intptr_t ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] / mat2[i % ncols];
  }
}

static void div_float_long_by_lines(int total, intptr_t nlines, double *mat1,
                                    YAP_Int *mat2, double *ndata) {
  intptr_t ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] / mat2[i % ncols];
  }
}

static void div_float_by_lines(int total, intptr_t nlines, double *mat1,
                               double *mat2, double *ndata) {
  intptr_t ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] / mat2[i % ncols];
  }
}

/** @pred matrix_op_to_lines(+ _Matrix1_,+ _Lines_,+ _Op_,- _Result_)



 _Result_ is the result of applying  _Op_ to all elements of
 _Matrix1_, with the corresponding element in  _Lines_ as the
second argument. Currently, only division (`/`) is supported.


*/
static YAP_Bool matrix_op_to_lines(void) {
  intptr_t *mat1, *mat2;
  YAP_Term top = YAP_ARG3;
  op_type op;
  YAP_Term tf;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat1 = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat1) {
    /* Error */
    return FALSE;
  }
  mat2 = (intptr_t *)YAP_BlobOfTerm(YAP_ARG2);
  if (!mat2) {
    /* Error */
    return FALSE;
  }
  /* create a new array without first dimension */
  if (mat1[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data1;
    intptr_t dims = mat1[MAT_NDIMS];
    intptr_t *nmat;
    data1 = matrix_long_data(mat1, dims);

    if (mat2[MAT_TYPE] == INT_MATRIX) {
      YAP_Int *data2 = matrix_long_data(mat2, dims - 1);
      if (op == MAT_DIV) {
        double *ndata;

        tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = YAP_BlobOfTerm(tf);
        ndata = matrix_double_data(nmat, dims);
        div_int_by_lines(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2, ndata);
      } else {
        return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, dims - 1);
      if (op == MAT_DIV) {
        double *ndata;

        tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = YAP_BlobOfTerm(tf);
        ndata = matrix_double_data(nmat, dims);
        div_int_by_dlines(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2, ndata);
      } else {
        return FALSE;
      }
    } else {
      return FALSE;
    }
  } else {
    double *data1, *ndata;
    intptr_t dims = mat1[MAT_NDIMS];
    intptr_t *nmat;

    data1 = matrix_double_data(mat1, dims);
    tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
    nmat = YAP_BlobOfTerm(tf);
    if (tf == YAP_TermNil())
      return FALSE;
    ndata = matrix_double_data(nmat, dims);
    if (mat2[MAT_TYPE] == INT_MATRIX) {
      YAP_Int *data2 = matrix_long_data(mat2, dims - 1);
      if (op == MAT_DIV) {
        div_float_long_by_lines(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2,
                                ndata);
      } else {
        return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, dims - 1);
      if (op == MAT_DIV) {
        div_float_by_lines(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2, ndata);
      } else {
        return FALSE;
      }
    } else {
      return FALSE;
    }
  }
  return YAP_Unify(YAP_ARG4, tf);
}

/** @pred matrix_copy(+ _Matrix1_,+ _Matrix2_)

Replace the contents of _Matrix1_ by _Matrix2_. The two matrices must have the same type and size.

*/
static YAP_Bool matrix_copy(void) {
  M mat1, mat2;
  if (GET_MATRIX(YAP_ARG1, &mat1)<0 ||
  GET_MATRIX(YAP_ARG2, &mat2)<0) {
  return false;
  }
  if (mat1.sz != mat2.sz || mat1.type != mat2.type) {
    return false;
  }
  if (mat1.type ==  'i')
    memcpy(mat1.ls,mat2.ls,sizeof(YAP_Int));
  else
        memcpy(mat1.data,mat2.data,sizeof(double));
  return true;
}

static void matrix_long_add_data(YAP_Int *nmat, int siz, YAP_Int mat1[],
                                 YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] + mat2[i];
  }
}

static void matrix_long_double_add_data(double *nmat, int siz, YAP_Int mat1[],
                                        double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] + mat2[i];
  }
}

static void matrix_double_add_data(double *nmat, int siz, double mat1[],
                                   double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] + mat2[i];
  }
}

static void matrix_long_sub_data(YAP_Int *nmat, int siz, YAP_Int mat1[],
                                 YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] - mat2[i];
  }
}

static void matrix_long_double_sub_data(double *nmat, int siz, YAP_Int mat1[],
                                        double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] - mat2[i];
  }
}

static void matrix_long_double_rsub_data(double *nmat, int siz, double mat1[],
                                         YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat2[i] - mat1[i];
  }
}

static void matrix_double_sub_data(double *nmat, int siz, double mat1[],
                                   double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] - mat2[i];
  }
}

static void matrix_long_mult_data(YAP_Int *nmat, int siz, YAP_Int mat1[],
                                  YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] * mat2[i];
  }
}

static void matrix_long_double_mult_data(double *nmat, int siz, YAP_Int mat1[],
                                         double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] * mat2[i];
  }
}

static void matrix_double_mult_data(double *nmat, int siz, double mat1[],
                                    double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] * mat2[i];
  }
}

static void matrix_long_div_data(YAP_Int *nmat, int siz, YAP_Int mat1[],
                                 YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_div_data(double *nmat, int siz, YAP_Int mat1[],
                                        double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_div2_data(double *nmat, int siz, double mat1[],
                                         YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_double_div_data(double *nmat, int siz, double mat1[],
                                   double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_zdiv_data(YAP_Int *nmat, int siz, YAP_Int mat1[],
                                  YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0)
      nmat[i] = 0;
    else
      nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_zdiv_data(double *nmat, int siz, YAP_Int mat1[],
                                         double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0)
      nmat[i] = 0;
    else
      nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_zdiv2_data(double *nmat, int siz, double mat1[],
                                          YAP_Int mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0.0)
      nmat[i] = 0;
    else
      nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_double_zdiv_data(double *nmat, int siz, double mat1[],
                                    double mat2[]) {
  intptr_t i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0.0) {
      nmat[i] = 0.0;
    } else {
      nmat[i] = mat1[i] / mat2[i];
    }
  }
}

static YAP_Bool matrix_op(void) {
  M mat1, mat2, nmat;
  YAP_Term top = YAP_ARG3;
  op_type op;
  YAP_Term tf = YAP_ARG4;
  int create = true;
   if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  if (GET_MATRIX(YAP_ARG1, &mat1)<0 ||
  GET_MATRIX(YAP_ARG2, &mat2)<0) {
  return false;
  }
   if (tf == YAP_ARG1 || tf == YAP_ARG2) {
    create = false;
  }
   int mem= 0;
   if (create && (mem = YAP_RequiresExtraStack(mat1.sz*2+4096)) > 0) {
     GET_MATRIX(YAP_ARG1, &mat1);
     GET_MATRIX(YAP_ARG2, &mat2);
     } else if (mem<0) {
       return false;
     }
      if (create) {
	if (mat1.type == 'i' && mat2.type == 'i') {	    
	  tf = new_int_matrix(mat1.ndims, mat1.dims, NULL);
	  } else {
	    tf = new_float_matrix(mat1.ndims, mat1.dims, NULL); 
	}
	  if (tf == YAP_TermNil()) {
	    return FALSE;
	  }
     }else {
       tf=YAP_ARG1;
      }
	GET_MATRIX(tf, &nmat);
   if (mat1.type == 'i') {

    if (mat2.type == 'i') {
      switch (op) {
      case MAT_PLUS:
        matrix_long_add_data(nmat.ls, mat1.sz, mat1.ls, mat2.ls);
        break;
      case MAT_SUB:
        matrix_long_sub_data(nmat.ls, mat1.sz, mat1.ls, mat2.ls);
        break;
      case MAT_TIMES:
        matrix_long_mult_data(nmat.ls, mat1.sz, mat1.ls, mat2.ls);
        break;
      case MAT_DIV:
        matrix_long_div_data(nmat.ls, mat1.sz, mat1.ls, mat2.ls);
        break;
      case MAT_ZDIV:
        matrix_long_zdiv_data(nmat.ls, mat1.sz, mat1.ls, mat2.ls);
        break;
      default:
        return FALSE;
      }
    } else if (mat2.type == 'f') {
      switch (op) {
      case MAT_PLUS:
        matrix_long_double_add_data(nmat.data, nmat.sz, mat1.ls, mat2.data);
        break;
      case MAT_SUB:
      matrix_long_double_sub_data(nmat.data, nmat.sz, mat1.ls, mat2.data);
        break;
      case MAT_TIMES:
      matrix_long_double_mult_data(nmat.data, nmat.sz, mat1.ls, mat2.data);
        break;
      case MAT_DIV:
      matrix_long_double_div_data(nmat.data, nmat.sz, mat1.ls, mat2.data);
        break;
      case MAT_ZDIV:
        matrix_long_zdiv_data(nmat.ls, mat1.sz, mat1.ls, mat2.ls);
	break;
      default:
        return FALSE;
      }
    } else {
      return FALSE;
    }
  } else {
    if (mat2.type == 'i') {
      switch (op) {
      case MAT_PLUS:
        matrix_long_double_add_data(nmat.data, nmat.sz, mat2.ls, mat1.data);
        break;
      case MAT_SUB:
        matrix_long_double_rsub_data(nmat.data, nmat.sz, mat1.data, mat2.ls);
        break;
      case MAT_TIMES:
        matrix_long_double_mult_data(nmat.data, nmat.sz, mat2.ls, mat1.data  );
        break;
      case MAT_DIV:
        matrix_long_double_div2_data(nmat.data, nmat.sz, mat1.data, mat2.ls);
        break;
      case MAT_ZDIV:
        matrix_long_double_zdiv_data(nmat.data, nmat.sz, mat2.ls, mat1.data);
        break;
      default:
        return FALSE;
      }
    } else if (mat2.type == 'f') {
      switch (op) {
      case MAT_PLUS:
        matrix_double_add_data(nmat.data, nmat.sz, mat1.data, mat2.data);
        break;
      case MAT_SUB:
        matrix_double_sub_data(nmat.data, nmat.sz, mat1.data, mat2.data);
        break;
      case MAT_TIMES:
        matrix_double_mult_data(nmat.data, nmat.sz, mat1.data, mat2.data);
        break;
      case MAT_DIV:
        matrix_double_div_data(nmat.data, nmat.sz, mat1.data, mat2.data);
        break;
      case MAT_ZDIV:
        matrix_double_zdiv_data(nmat.data, nmat.sz, mat1.data, mat2.data);
        break;
      default:
        return FALSE;
      }
    } else {
      return FALSE;
    }
   }

  return YAP_Unify(YAP_ARG4, tf);
   }

static void add_int_by_cols(int total, intptr_t nlines, YAP_Int *mat1,
                            YAP_Int *mat2, YAP_Int *ndata) {
  intptr_t i, ncols = total / nlines;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] + mat2[i / ncols];
  }
}

static void add_int_by_dcols(int total, intptr_t nlines, YAP_Int *mat1,
                             double *mat2, double *ndata) {
  intptr_t i, ncols = total / nlines;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] + mat2[i / ncols];
  }
}

static void add_double_by_cols(int total, intptr_t nlines, double *mat1,
                               double *mat2, double *ndata) {
  intptr_t i;
  intptr_t ncols = total / nlines;

  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] + mat2[i / ncols];
  }
}

static YAP_Bool matrix_op_to_cols(void) {
  intptr_t *mat1, *mat2;
  YAP_Term top = YAP_ARG3;
  op_type op;
  YAP_Term tf;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat1 = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat1) {
    /* Error */
    return FALSE;
  }
  mat2 = (intptr_t *)YAP_BlobOfTerm(YAP_ARG2);
  if (!mat2) {
    /* Error */
    return FALSE;
  }
  if (mat1[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data1;
    intptr_t dims = mat1[MAT_NDIMS];
    intptr_t *nmat;
    data1 = matrix_long_data(mat1, dims);

    if (mat2[MAT_TYPE] == INT_MATRIX) {
      YAP_Int *data2 = matrix_long_data(mat2, 1);
      if (op == MAT_PLUS) {
        YAP_Int *ndata;

        tf = new_int_matrix(dims, mat1 + MAT_DIMS, NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = YAP_BlobOfTerm(tf);
        ndata = matrix_long_data(nmat, dims);
        add_int_by_cols(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2, ndata);
      } else {
        return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, 1);
      if (op == MAT_PLUS) {
        double *ndata;

        tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = YAP_BlobOfTerm(tf);
        ndata = matrix_double_data(nmat, dims);
        add_int_by_dcols(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2, ndata);
      } else {
        return FALSE;
      }
    } else {
      return FALSE;
    }
  } else {
    double *data1, *data2, *ndata;
    intptr_t dims = mat1[MAT_NDIMS];
    intptr_t *nmat;

    if (mat2[MAT_TYPE] != FLOAT_MATRIX)
      return FALSE;
    tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = YAP_BlobOfTerm(tf);
    data1 = matrix_double_data(mat1, dims);
    data2 = matrix_double_data(mat2, 1);
    ndata = matrix_double_data(nmat, dims);
    if (op == MAT_PLUS) {
      add_double_by_cols(mat1[MAT_SIZE], mat1[MAT_DIMS], data1, data2, ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG4, tf);
}

static YAP_Bool matrix_op_to_all(void) {
  intptr_t *mat;
  YAP_Term tf = 0;
  YAP_Term top = YAP_ARG2;
  op_type op;
  int create = FALSE;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (YAP_IsVarTerm(YAP_ARG4)) {
    create = TRUE;
  }
  /* create a new array with same dimensions */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data;
    intptr_t dims = mat[MAT_NDIMS];
    intptr_t *nmat;
    YAP_Term tnum = YAP_ARG3;

    if (YAP_IsIntTerm(tnum)) {
      YAP_Int num;
      YAP_Int *ndata;

      num = YAP_IntOfTerm(tnum);
      data = matrix_long_data(mat, dims);
      if (create) {
        tf = new_int_matrix(dims, mat + (MAT_DIMS), NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = (intptr_t *)YAP_BlobOfTerm(tf);
        ndata = matrix_long_data(nmat, dims);
      } else {
        nmat = mat;
        ndata = data;
      }
      if (op == MAT_PLUS) {
        intptr_t i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] + num;
        }
      } else if (op == MAT_TIMES) {
        intptr_t i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] * num;
        }
      } else {
        return FALSE;
      }
    } else if (YAP_IsFloatTerm(tnum)) {
      double num;
      double *ndata;

      num = YAP_FloatOfTerm(tnum);
      if (create) {
        tf = new_float_matrix(dims, mat + (MAT_DIMS), NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = (intptr_t *)YAP_BlobOfTerm(tf);
        ndata = matrix_double_data(nmat, dims);
      } else {
        return FALSE;
      }
      data = matrix_long_data(mat, dims);
      if (op == MAT_PLUS) {
        intptr_t i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] + num;
        }
      } else if (op == MAT_SUB) {
        intptr_t i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = num - data[i];
        }
      } else if (op == MAT_TIMES) {
        intptr_t i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] * num;
        }
      } else if (op == MAT_DIV) {
        intptr_t i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] / num;
        }
      }
    } else {
      return FALSE;
    }
  } else {
    double *data, *ndata;
    intptr_t dims = mat[MAT_NDIMS];
    intptr_t *nmat;
    YAP_Term tnum = YAP_ARG3;
    double num;

    if (YAP_IsFloatTerm(tnum)) {
      num = YAP_FloatOfTerm(tnum);
    } else if (!YAP_IntOfTerm(tnum)) {
      return FALSE;
    } else {
      if (!create)
        return FALSE;
      num = (double)YAP_IntOfTerm(tnum);
    }
    data = matrix_double_data(mat, dims);
    if (create) {
      tf = new_float_matrix(dims, mat + (MAT_DIMS), NULL);
      if (tf == YAP_TermNil())
        return FALSE;
      nmat = (intptr_t *)YAP_BlobOfTerm(tf);
      ndata = matrix_double_data(nmat, dims);
    } else {
      nmat = mat;
      ndata = data;
    }
    switch (op) {
    case MAT_PLUS: {
      intptr_t i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = data[i] + num;
      }
    } break;
    case MAT_SUB: {
      intptr_t i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = num - data[i];
      }
    } break;
    case MAT_TIMES: {
      intptr_t i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = data[i] * num;
      }
    } break;
    case MAT_DIV: {
      intptr_t i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = data[i] / num;
      }
    } break;
    default:
      return FALSE;
    }
  }
  if (create)
    return YAP_Unify(YAP_ARG4, tf);
  return YAP_Unify(YAP_ARG4, YAP_ARG1);
}

/* given a matrix M and a set of dims, build a new reordered matrix to follow
   the new order
*/
static YAP_Bool matrix_transpose(void) {
  intptr_t ndims, i, *dims, *dimsn;
  intptr_t conv[MAX_DIMS], indx[MAX_DIMS], nindx[MAX_DIMS];
  YAP_Term tconv, tf;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  if (!mat) {
    /* Error */
    return FALSE;
  }
  ndims = mat[MAT_NDIMS];
  if (mat[MAT_TYPE] == INT_MATRIX) {
    /* create a new matrix with the same size */
    tf = new_int_matrix(ndims, mat + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
  } else {
    /* create a new matrix with the same size */
    tf = new_float_matrix(ndims, mat + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
  }
  /* just in case there was an overflow */
  mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
  nmat = (intptr_t *)YAP_BlobOfTerm(tf);
  dims = mat + MAT_DIMS;
  dimsn = nmat + MAT_DIMS;
  /* we now have our target matrix, let us grab our conversion matrix */
  tconv = YAP_ARG2;
  for (i = 0; i < ndims; i++) {
    YAP_Term th;
    YAP_Int j;

    if (!YAP_IsPairTerm(tconv))
      return FALSE;
    th = YAP_HeadOfTerm(tconv);
    if (!YAP_IsIntTerm(th))
      return FALSE;
    conv[i] = j = YAP_IntOfTerm(th);
    dimsn[i] = dims[j];
    tconv = YAP_TailOfTerm(tconv);
  }
  /*
    we now got all the dimensions set up, so what we need to do
    next is to copy the elements to the new matrix.
  */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data = matrix_long_data(mat, ndims);
    /* create a new matrix with the same size */
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      YAP_Int x = data[i];
      int j;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0; j < ndims; j++) {
        nindx[j] = indx[conv[j]];
      }
      matrix_long_set(nmat, nindx, x);
    }
  } else {
    double *data = matrix_double_data(mat, ndims);
    /* create a new matrix with the same size */
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      double x = data[i];
      long j;

      matrix_get_index(mat, i, indx);
      for (j = 0; j < ndims; j++)
        nindx[j] = indx[conv[j]];
      matrix_float_set(nmat, nindx, x);
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, fold one of the dimensions of the
   matrix on one of the elements
*/
static YAP_Bool matrix_select(void) {
  intptr_t ndims, i, j, newdims, prdim, leftarg, *dims, indx[MAX_DIMS];
  intptr_t nindx[MAX_DIMS];
  YAP_Term tpdim, tdimarg, tf;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* we now have our target matrix, let us grab our conversion arguments */
  tpdim = YAP_ARG2;
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  if (!YAP_IsIntTerm(tpdim)) {
    return FALSE;
  }
  prdim = YAP_IntOfTerm(tpdim);
  tdimarg = YAP_ARG3;
  if (!YAP_IsIntTerm(tdimarg)) {
    return FALSE;
  }
  leftarg = YAP_IntOfTerm(tdimarg);
  for (i = 0, j = 0; i < ndims; i++) {
    if (i != prdim) {
      nindx[j] = dims[i];
      j++;
    }
  }
  newdims = ndims - 1;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(nmat, i, indx);
      for (j = 0, k = 0; j < newdims; j++, k++) {
        if (j == prdim) {
          nindx[k] = leftarg;
          k++;
        }
        nindx[k] = indx[j];
      }
      if (k == prdim) {
        nindx[k] = leftarg;
      }
      ndata[i] = data[matrix_get_offset(mat, nindx)];
    }
  } else {
    double *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_float_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, newdims);
    /* create a new matrix with the same size */
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(nmat, i, indx);
      for (j = 0, k = 0; j < newdims; j++, k++) {
        if (j == prdim) {
          nindx[k] = leftarg;
          k++;
        }
        nindx[k] = indx[j];
      }
      if (k == prdim) {
        nindx[k] = leftarg;
      }
      ndata[i] = data[matrix_get_offset(mat, nindx)];
    }
  }
  return YAP_Unify(YAP_ARG4, tf);
}

/* given a matrix M and a set of N-1 dims, get the first dimension
 */
static YAP_Bool matrix_column(void) {
  intptr_t size, i, ndims, newdims[1];
  intptr_t indx[MAX_DIMS];
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  YAP_Term tconv, tf;

  if (!mat) {
    /* Error */
    return FALSE;
  }
  ndims = mat[MAT_NDIMS];
  /* we now have our target matrix, let us grab our conversion arguments */
  tconv = YAP_ARG2;
  for (i = 1; i < ndims; i++) {
    YAP_Term th;

    if (!YAP_IsPairTerm(tconv))
      return FALSE;
    th = YAP_HeadOfTerm(tconv);
    if (!YAP_IsIntTerm(th))
      return FALSE;
    indx[i] = YAP_IntOfTerm(th);
    tconv = YAP_TailOfTerm(tconv);
  }
  if (tconv != YAP_TermNil())
    return FALSE;
  newdims[0] = size = mat[MAT_DIMS];
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(1, newdims, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, 1);
    /* create a new matrix with smaller size */
    for (i = 0; i < size; i++) {
      indx[0] = i;
      ndata[i] = data[matrix_get_offset(mat, indx)];
    }
  } else {
    double *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_float_matrix(1, newdims, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, 1);
    /* create a new matrix with smaller size */
    for (i = 0; i < size; i++) {
      indx[0] = i;
      ndata[i] = data[matrix_get_offset(mat, indx)];
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, sum out one of the dimensions
 */
static YAP_Bool matrix_sum_out(void) {
  intptr_t ndims, i, j, newdims, prdim;
  intptr_t indx[MAX_DIMS], nindx[MAX_DIMS];
  YAP_Term tpdim, tf;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* we now have our target matrix, let us grab our conversion arguments */
  tpdim = YAP_ARG2;
  ndims = mat[MAT_NDIMS];
  if (!YAP_IsIntTerm(tpdim)) {
    return FALSE;
  }
  prdim = YAP_IntOfTerm(tpdim);
  newdims = ndims - 1;
  for (i = 0, j = 0; i < ndims; i++) {
    if (i != prdim) {
      nindx[j] = (mat + MAT_DIMS)[i];
      j++;
    }
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++)
      ndata[i] = 0;
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0, k = 0; j < ndims; j++) {
        if (j != prdim) {
          nindx[k++] = indx[j];
        }
      }
      ndata[matrix_get_offset(nmat, nindx)] += data[i];
    }
  } else {
    double *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_float_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++)
      ndata[i] = 0.0;
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0, k = 0; j < ndims; j++) {
        if (j != prdim) {
          nindx[k++] = indx[j];
        }
      }
      ndata[matrix_get_offset(nmat, nindx)] += data[i];
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, sum out one of the dimensions
 */
static YAP_Bool matrix_sum_out_several(void) {
  intptr_t ndims, i, *dims, newdims;
  intptr_t indx[MAX_DIMS], nindx[MAX_DIMS], conv[MAX_DIMS];
  YAP_Term tf, tconv;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  if (!mat) {
    /* Error */
    return FALSE;
  }
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  /* we now have our target matrix, let us grab our conversion arguments */
  tconv = YAP_ARG2;
  for (i = 0, newdims = 0; i < ndims; i++) {
    YAP_Term th;

    if (!YAP_IsPairTerm(tconv))
      return FALSE;
    th = YAP_HeadOfTerm(tconv);
    if (!YAP_IsIntTerm(th))
      return FALSE;
    conv[i] = YAP_IntOfTerm(th);
    if (!conv[i]) {
      nindx[newdims++] = dims[i];
    }
    tconv = YAP_TailOfTerm(tconv);
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++)
      ndata[i] = 0;
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0, k = 0; j < ndims; j++) {
        if (!conv[j]) {
          nindx[k++] = indx[j];
        }
      }
      ndata[matrix_get_offset(nmat, nindx)] =
          log(exp(ndata[matrix_get_offset(nmat, nindx)]) + exp(data[i]));
    }
  } else {
    double *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_float_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++)
      ndata[i] = 0.0;
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0, k = 0; j < ndims; j++) {
        if (!conv[j]) {
          nindx[k++] = indx[j];
        }
      }
      ndata[matrix_get_offset(nmat, nindx)] =
          log(exp(ndata[matrix_get_offset(nmat, nindx)]) + exp(data[i]));
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, sum out one of the dimensions
 */
static YAP_Bool matrix_sum_out_logs(void) {
  intptr_t ndims, i, j, *dims, newdims, prdim;
  intptr_t nindx[MAX_DIMS];
  YAP_Term tpdim, tf;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* we now have our target matrix, let us grab our conversion arguments */
  tpdim = YAP_ARG2;
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  if (!YAP_IsIntTerm(tpdim)) {
    return FALSE;
  }
  prdim = YAP_IntOfTerm(tpdim);
  newdims = ndims - 1;
  for (i = 0, j = 0; i < ndims; i++) {
    if (i != prdim) {
      nindx[j] = dims[i];
      j++;
    }
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;
    int d = 1, j = 0, dd = 1;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, newdims);
    while (j < prdim) {
      d = d * dims[j];
      j++;
    }
    dd = d * dims[prdim];
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      int j = i % d + (i / dd) * d;
      ndata[j] = exp(data[i]);
    }
    for (; i < mat[MAT_SIZE]; i++) {
      int j = i % d + (i / dd) * d;
      ndata[j] += exp(data[i]);
    }
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      ndata[i] = log(ndata[i]);
    }
  } else {
    double *data, *ndata;
    int d = 1, j = 0, dd = 1;

    /* create a new matrix with the same size */
    tf = new_float_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, newdims);

    j = ndims - 1;
    while (j > prdim) {
      d = d * dims[j];
      j--;
    }
    dd = d * dims[prdim];
    memset(ndata, 0, sizeof(double) * nmat[MAT_SIZE]);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      YAP_Int k = i % d + (i / dd) * d;
      ndata[k] += exp(data[i]);
    }
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      ndata[i] = log(ndata[i]);
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, sum out one of the dimensions
 */
static YAP_Bool matrix_sum_out_logs_several(void) {
  intptr_t ndims, i, *dims, newdims;
  intptr_t indx[MAX_DIMS], nindx[MAX_DIMS], conv[MAX_DIMS];
  YAP_Term tf, tconv;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;

  if (!mat) {
    /* Error */
    return FALSE;
  }
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  /* we now have our target matrix, let us grab our conversion arguments */
  tconv = YAP_ARG2;
  for (i = 0, newdims = 0; i < ndims; i++) {
    YAP_Term th;

    if (!YAP_IsPairTerm(tconv))
      return FALSE;
    th = YAP_HeadOfTerm(tconv);
    if (!YAP_IsIntTerm(th))
      return FALSE;
    conv[i] = YAP_IntOfTerm(th);
    if (!conv[i]) {
      nindx[newdims++] = dims[i];
    }
    tconv = YAP_TailOfTerm(tconv);
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++)
      ndata[i] = 0;
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0, k = 0; j < ndims; j++) {
        if (!conv[j]) {
          nindx[k++] = indx[j];
        }
      }
      ndata[matrix_get_offset(nmat, nindx)] += exp(data[i]);
    }
  } else {
    double *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_float_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, newdims);
    /* create a new matrix with smaller size */
    for (i = 0; i < nmat[MAT_SIZE]; i++)
      ndata[i] = 0.0;
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      int j, k;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      for (j = 0, k = 0; j < ndims; j++) {
        if (!conv[j]) {
          nindx[k++] = indx[j];
        }
      }
      ndata[matrix_get_offset(nmat, nindx)] += exp(data[i]);
    }
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      ndata[i] = log(ndata[i]);
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, build a matrix to follow
   the new order
*/
static YAP_Bool matrix_expand(void) {
  intptr_t ndims, i, *dims, newdims = 0, olddims = 0;
  intptr_t new[MAX_DIMS], indx[MAX_DIMS], nindx[MAX_DIMS];
  YAP_Term tconv, tf;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* we now have our target matrix, let us grab our conversion matrix */
  tconv = YAP_ARG2;
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  for (i = 0; i < MAX_DIMS; i++) {
    YAP_Term th;
    YAP_Int j;

    if (!YAP_IsPairTerm(tconv)) {
      if (tconv != YAP_TermNil())
        return FALSE;
      break;
    }
    th = YAP_HeadOfTerm(tconv);
    if (!YAP_IsIntTerm(th))
      return FALSE;
    newdims++;
    j = YAP_IntOfTerm(th);
    if (j == 0) {
      new[i] = 0;
      nindx[i] = dims[olddims];
      olddims++;
    } else {
      new[i] = 1;
      nindx[i] = j;
    }
    tconv = YAP_TailOfTerm(tconv);
  }
  if (olddims != ndims)
    return FALSE;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, newdims);
    /* create a new matrix with the same size */
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      int j, k = 0;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(nmat, i, indx);
      for (j = 0; j < newdims; j++) {
        if (!new[j])
          nindx[k++] = indx[j];
      }
      ndata[i] = data[matrix_get_offset(mat, nindx)];
    }
  } else {
    double *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_float_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, newdims);
    /* create a new matrix with the same size */
    for (i = 0; i < newdims; i++)
      indx[i] = 0;
    for (i = 0; i < nmat[MAT_SIZE]; i++) {
      int j, k = 0;
      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      for (j = 0; j < newdims; j++) {
        if (!new[j])
          nindx[k++] = indx[j];
      }
      ndata[i] = data[matrix_get_offset(mat, nindx)];
      matrix_next_index(nmat + MAT_DIMS, newdims, indx);
    }
  }
  return YAP_Unify(YAP_ARG3, tf);
}

/* given a matrix M and a set of dims, build contract a matrix to follow
   the new order
*/
static YAP_Bool matrix_set_all_that_disagree(void) {
  intptr_t ndims, i, *dims;
  intptr_t indx[MAX_DIMS];
  YAP_Term tf;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  int dim = YAP_IntOfTerm(YAP_ARG2);
  int pos = YAP_IntOfTerm(YAP_ARG3);

  if (!mat) {
    /* Error */
    return FALSE;
  }
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data, *ndata, val;

    /* create a new matrix with the same size */
    tf = new_int_matrix(ndims, dims, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, ndims);
    ndata = matrix_long_data(nmat, ndims);
    if (!YAP_IsIntTerm(YAP_ARG4))
      return FALSE;
    val = YAP_IntOfTerm(YAP_ARG4);
    /* create a new matrix with the same size */
    for (i = 0; i < nmat[MAT_SIZE]; i++) {

      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      if (indx[dim] != pos)
        ndata[i] = val;
      else
        ndata[i] = data[i];
    }
  } else {
    double *data, *ndata, val;

    /* create a new matrix with the same size */
    tf = new_float_matrix(ndims, dims, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (intptr_t *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, ndims);
    ndata = matrix_double_data(nmat, ndims);
    if (YAP_IsFloatTerm(YAP_ARG4))
      val = YAP_FloatOfTerm(YAP_ARG4);
    else if (YAP_IsIntTerm(YAP_ARG4))
      val = YAP_IntOfTerm(YAP_ARG4);
    else
      return FALSE;
    /* create a new matrix with the same size */
    for (i = 0; i < nmat[MAT_SIZE]; i++) {

      /*
        not very efficient, we could try to take advantage of the fact
        that we usually only change an index at a time
      */
      matrix_get_index(mat, i, indx);
      if (indx[dim] != pos)
        ndata[i] = val;
      else
        ndata[i] = data[i];
    }
  }
  return YAP_Unify(YAP_ARG5, tf);
}

static YAP_Bool matrix_m(void) {
  intptr_t ndims, i, size;
  YAP_Term tm, *tp;
  intptr_t *mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);

  if (!mat) {
    return YAP_Unify(YAP_ARG1, YAP_ARG2);
  }
  ndims = mat[MAT_NDIMS];
  size = mat[MAT_SIZE];
  tm = YAP_MkNewApplTerm(MFunctorM, 5);
  tp = YAP_ArgsOfTerm(tm);
  tp[0] = mk_int_list(ndims, mat + MAT_DIMS);
  tp[1] = YAP_MkIntTerm(ndims);
  tp[2] = YAP_MkIntTerm(size);
  tp[3] = mk_rep_int_list(ndims, mat[MAT_BASE]);
  tp[4] = YAP_MkNewApplTerm(YAP_MkFunctor(MAtomC, size), size);
  tp = YAP_ArgsOfTerm(tp[3]);
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Int *data;

    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    data = matrix_long_data(mat, ndims);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      tp[i] = YAP_MkIntTerm(data[i]);
    }
  } else {
    double *data;

    /* in case the matrix moved */
    mat = (intptr_t *)YAP_BlobOfTerm(YAP_ARG1);
    data = matrix_double_data(mat, ndims);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      tp[i] = YAP_MkFloatTerm(data[i]);
    }
  }
  return YAP_Unify(YAP_ARG2, tm);
}


X_API void init_matrix(void);

X_API void init_matrix(void) {
    // useful constants
  MAtomC = YAP_LookupAtom("c");
  MFunctorM = YAP_MkFunctor(YAP_LookupAtom("$matrix"), 5);
  MFunctorFloats = YAP_MkFunctor(YAP_LookupAtom("floats"), 2);
  MTermTrue = YAP_MkAtomTerm(YAP_LookupAtom("true"));
  MTermFail = YAP_MkAtomTerm(YAP_LookupAtom("fail"));
  MTermFalse = YAP_MkAtomTerm(YAP_LookupAtom("false"));

  // new matrix
  YAP_UserCPredicate("new_ints_matrix", new_ints_matrix, 4);
  YAP_UserCPredicate("new_ints_matrix_set", new_ints_matrix_set, 4);
  YAP_UserCPredicate("new_floats_matrix", new_floats_matrix, 4);
  YAP_UserCPredicate("new_floats_matrix_set", new_floats_matrix_set, 4);

  // matrix op constant
  YAP_UserCPredicate("matrix_set_one", matrix_set_one, 3);
  YAP_UserCPredicate("matrix_set_all", matrix_set_all, 2);
  YAP_UserCPredicate("matrix_get_one", matrix_get_one, 3);
  //YAP_UserCPredicate("matrix_get_all", matrix_get_all, 3);
  YAP_UserCPredicate("matrix_add_to_all" , matrix_add_to_all, 2);
  YAP_UserCPredicate("matrix_inc", matrix_inc, 2);
  YAP_UserCPredicate("matrix_dec", matrix_dec, 2);
  YAP_UserCPredicate("matrix_inc", matrix_inc3, 3);
  YAP_UserCPredicate("matrix_dec", matrix_dec3, 3);
  // matrix aggregates
        YAP_UserCPredicate("matrix_max", matrix_max, 2);
        YAP_UserCPredicate("matrix_maxarg", matrix_maxarg, 2);
        YAP_UserCPredicate("matrix_min", matrix_min, 2);
        YAP_UserCPredicate("matrix_minarg", matrix_minarg, 2);
	YAP_UserCPredicate("matrix_to_list", matrix_to_list, 2);
  YAP_UserCPredicate("matrix_set_base", matrix_set_base, 2);
  YAP_UserCPredicate("matrix_dims", matrix_dims, 2);
  YAP_UserCPredicate("matrix_dims", matrix_dims3, 3);
  YAP_UserCPredicate("matrix_ndims", matrix_ndims, 2);
  YAP_UserCPredicate("matrix_size", matrix_size, 2);
  YAP_UserCPredicate("matrix_type_as_number", matrix_type, 2);
  YAP_UserCPredicate("matrix_arg_to_offset", matrix_arg_to_offset, 3);
  YAP_UserCPredicate("matrix_offset_to_arg", matrix_offset_to_arg, 3);
  YAP_UserCPredicate("matrix_sum", matrix_sum, 2);
  YAP_UserCPredicate("matrix_shuffle", matrix_transpose, 3);
  YAP_UserCPredicate("matrix_expand", matrix_expand, 3);
  YAP_UserCPredicate("matrix_select", matrix_select, 4);
  YAP_UserCPredicate("matrix_column", matrix_column, 3);
  YAP_UserCPredicate("matrix_to_logs", matrix_log_all, 1);
  YAP_UserCPredicate("matrix_to_exps", matrix_exp_all, 1);
  YAP_UserCPredicate("matrix_to_exps2", matrix_exp2_all, 1);
  YAP_UserCPredicate("matrix_to_logs", matrix_log_all2, 2);
  YAP_UserCPredicate("matrix_to_exps", matrix_exp_all2, 2);
  YAP_UserCPredicate("matrix_sum_out", matrix_sum_out, 3);
  YAP_UserCPredicate("matrix_sum_out_several", matrix_sum_out_several, 3);
  YAP_UserCPredicate("matrix_sum_logs_out", matrix_sum_out_logs, 3);
  YAP_UserCPredicate("matrix_sum_logs_out_several", matrix_sum_out_logs_several,
                     3);
  YAP_UserCPredicate("matrix_set_all_that_disagree",
                     matrix_set_all_that_disagree, 5);
  YAP_UserCPredicate("do_matrix_op", matrix_op, 4);
  YAP_UserCPredicate("do_matrix_agg_lines", matrix_agg_lines, 3);
  YAP_UserCPredicate("do_matrix_agg_cols", matrix_agg_cols, 3);
  YAP_UserCPredicate("do_matrix_op_to_all", matrix_op_to_all, 4);
  YAP_UserCPredicate("do_matrix_op_to_lines", matrix_op_to_lines, 4);
  YAP_UserCPredicate("do_matrix_op_to_cols", matrix_op_to_cols, 4);
  YAP_UserCPredicate("matrix_m", matrix_m, 2);
  YAP_UserCPredicate("matrix_copy", matrix_copy, 2);
  YAP_UserCPredicate("is_matrix", is_matrix, 1);
}

#ifdef _WIN32

int WINAPI win_matrixs(HANDLE, DWORD, LPVOID);

int WINAPI win_matrixs(HANDLE hinst, DWORD reason, LPVOID reserved) {
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    break;
  case DLL_PROCESS_DETACH:
    break;
  case DLL_THREAD_ATTACH:
    break;
  case DLL_THREAD_DETACH:
    break;
  }
  return 1;
}
#endif

/**
* @}
*/
