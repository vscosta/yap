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

#include "YapConfig.h"
#include "YapInterface.h"
#include <math.h>
#if defined(__MINGW32__) || _MSC_VER
#include <windows.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif

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

/* maximal number of dimensions, 1024 should be enough */
#define MAX_DIMS 1024

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

YAP_Functor FunctorM;
YAP_Atom AtomC;

static long int *matrix_long_data(int *mat, int ndims) {
  return (long int *)(mat + (MAT_DIMS + ndims));
}

static double *matrix_double_data(int *mat, int ndims) {
  return (double *)(mat + (MAT_DIMS + ndims));
}

static unsigned int matrix_get_offset(int *mat, int *indx) {
  unsigned int i, pos = mat[MAT_SIZE], off = 0;

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

static void matrix_get_index(int *mat, unsigned int offset, int *indx) {
  unsigned int i, pos = mat[MAT_SIZE];

  /* find where we are */
  for (i = 0; i < mat[MAT_NDIMS]; i++) {
    pos /= mat[MAT_DIMS + i];
    indx[i] = offset / pos;
    offset = offset % pos;
  }
}

static void matrix_next_index(int *dims, int ndims, int *indx) {
  unsigned int i;

  /* find where we are */
  for (i = ndims; i > 0;) {
    i--;
    indx[i]++;
    if (indx[i] != dims[i])
      return;
    indx[i] = 0;
  }
}

static YAP_Term new_int_matrix(int ndims, int dims[], long int data[]) {
  unsigned int sz;
  unsigned int i, nelems = 1;
  YAP_Term blob;
  int *mat;
  long int *bdata;
  int idims[MAX_DIMS];

  /* in case we don't have enough room and need to shift the stack, we can't
     really afford to keep a pointer to the global */
  for (i = 0; i < ndims; i++) {
    idims[i] = dims[i];
    nelems *= dims[i];
  }
  sz = ((MAT_DIMS + 1) * sizeof(int) + ndims * sizeof(int) +
        nelems * sizeof(long int)) /
       sizeof(YAP_CELL);
  blob = YAP_MkBlobTerm(sz);
  if (blob == YAP_TermNil()) {
    return blob;
  }
  mat = (int *)YAP_BlobOfTerm(blob);
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
  return blob;
}

static YAP_Term new_float_matrix(int ndims, int dims[], double data[]) {
  unsigned int sz;
  unsigned int i, nelems = 1;
  YAP_Term blob;
  int *mat;
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

static YAP_Bool scan_dims(int ndims, YAP_Term tl, int dims[MAX_DIMS]) {
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

static YAP_Bool scan_dims_args(int ndims, YAP_Term tl, int dims[MAX_DIMS]) {
  int i;

  for (i = 0; i < ndims; i++) {
    YAP_Term th;
    int d;

    th = YAP_ArgOfTerm(2 + i, tl);
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
  }
  return TRUE;
}

static YAP_Bool cp_int_matrix(YAP_Term tl, YAP_Term matrix) {
  int *mat = (int *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  long int *j = matrix_long_data(mat, mat[MAT_NDIMS]);

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
  int *mat = (int *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
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

static YAP_Bool set_int_matrix(YAP_Term matrix, long int set) {
  int *mat = (int *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  long int *j = matrix_long_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    j[i] = set;
  }
  return TRUE;
}

static YAP_Bool set_float_matrix(YAP_Term matrix, double set) {
  int *mat = (int *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  double *j = matrix_double_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    j[i] = set;
  }
  return TRUE;
}

static YAP_Bool new_ints_matrix(void) {
  int ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out;
  int dims[MAX_DIMS];
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
  int ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out, tset = YAP_ARG3;
  int dims[MAX_DIMS];
  long int set;

  if (!YAP_IsIntTerm(tset)) {
    return FALSE;
  }
  set = YAP_IntOfTerm(tset);
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_int_matrix(ndims, dims, NULL);
  if (!set_int_matrix(out, set))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static YAP_Bool new_floats_matrix(void) {
  int ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out, data;
  int dims[MAX_DIMS];
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_float_matrix(ndims, dims, NULL);
  if (out == YAP_TermNil())
    return FALSE;
  data = YAP_ARG3;
  if (!YAP_IsVarTerm(data) && !cp_float_matrix(data, out))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static YAP_Bool new_floats_matrix_set(void) {
  int ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out, tset = YAP_ARG3;
  int dims[MAX_DIMS];
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

static YAP_Term float_matrix_to_list(int *mat) {
  double *data = matrix_double_data(mat, mat[MAT_NDIMS]);

  /* prepare for worst case with double taking two cells */
  if (YAP_RequiresExtraStack(6 * mat[MAT_SIZE])) {
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    data = matrix_double_data(mat, mat[MAT_NDIMS]);
  }
  return YAP_FloatsToList(data, mat[MAT_SIZE]);
}

static YAP_Term mk_int_list(int nelems, int *data) {
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

static YAP_Term mk_int_list2(int nelems, int base, int *data) {
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

static YAP_Term mk_rep_int_list(int nelems, int data) {
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

static YAP_Term mk_long_list(int nelems, long int *data) {
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

static YAP_Term long_matrix_to_list(int *mat) {
  long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);

  /* prepare for worst case with longs evrywhere (3cells + 1) */
  if (YAP_RequiresExtraStack(5 * mat[MAT_SIZE])) {
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    data = matrix_long_data(mat, mat[MAT_NDIMS]);
  }
  return mk_long_list(mat[MAT_SIZE], data);
}

static YAP_Term matrix_access(int *mat, int *indx) {
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE] == FLOAT_MATRIX)
    return YAP_MkFloatTerm((matrix_double_data(mat, mat[MAT_NDIMS]))[off]);
  else
    return YAP_MkIntTerm((matrix_long_data(mat, mat[MAT_NDIMS]))[off]);
}

static void matrix_float_set(int *mat, int *indx, double nval) {
  unsigned int off = 0;

  off = matrix_get_offset(mat, indx);
  (matrix_double_data(mat, mat[MAT_NDIMS]))[off] = nval;
}

static void matrix_long_set(int *mat, int *indx, long int nval) {
  unsigned int off = matrix_get_offset(mat, indx);
  (matrix_long_data(mat, mat[MAT_NDIMS]))[off] = nval;
}

static void matrix_float_set_all(int *mat, double nval) {
  int i;
  double *data = matrix_double_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < mat[MAT_SIZE]; i++)
    data[i] = nval;
}

static void matrix_long_set_all(int *mat, long int nval) {
  int i;
  long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);

  if (nval == 0) {
    memset((void *)data, 0, sizeof(long int) * mat[MAT_SIZE]);
  } else {
    for (i = 0; i < mat[MAT_SIZE]; i++)
      data[i] = nval;
  }
}

static void matrix_float_add(int *mat, int *indx, double nval) {
  unsigned int off;
  double *dat = matrix_double_data(mat, mat[MAT_NDIMS]);

  off = matrix_get_offset(mat, indx);
  dat[off] += nval;
}

static void matrix_long_add(int *mat, int *indx, long int nval) {
  long int *dat = matrix_long_data(mat, mat[MAT_NDIMS]);
  unsigned int off = matrix_get_offset(mat, indx);
  dat[off] += nval;
}

static void matrix_inc(int *mat, int *indx) {
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE] == FLOAT_MATRIX)
    (matrix_double_data(mat, mat[MAT_NDIMS])[off])++;
  else
    ((matrix_long_data(mat, mat[MAT_NDIMS]))[off])++;
}

static void matrix_dec(int *mat, int *indx) {
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE] == FLOAT_MATRIX)
    (matrix_double_data(mat, mat[MAT_NDIMS])[off])--;
  else
    ((matrix_long_data(mat, mat[MAT_NDIMS]))[off])--;
}

static YAP_Term matrix_inc2(int *mat, int *indx) {
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE] == FLOAT_MATRIX) {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    double d = data[off];
    d++;
    data[off] = d;
    return YAP_MkFloatTerm(d);
  } else {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    long int d = data[off];
    d++;
    data[off] = d;
    return YAP_MkIntTerm(d);
  }
}

static YAP_Term matrix_dec2(int *mat, int *indx) {
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE] == FLOAT_MATRIX) {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    double d = data[off];
    d--;
    data[off] = d;
    return YAP_MkFloatTerm(d);
  } else {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    long int d = data[off];
    d--;
    data[off] = d;
    return YAP_MkIntTerm(d);
  }
}

static YAP_Bool matrix_set(void) {
  int dims[MAX_DIMS], *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  tf = YAP_ARG3;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    if (YAP_IsIntTerm(tf)) {
      matrix_long_set(mat, dims, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_long_set(mat, dims, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  } else {
    if (YAP_IsIntTerm(tf)) {
      matrix_float_set(mat, dims, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_float_set(mat, dims, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  }
  return TRUE;
}

static YAP_Bool matrix_set2(void) {
  int dims[MAX_DIMS], *mat;
  YAP_Term tf, t = YAP_ARG1;

  mat = (int *)YAP_BlobOfTerm(YAP_ArgOfTerm(1, t));
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims_args(mat[MAT_NDIMS], t, dims)) {
    /* Error */
    return FALSE;
  }
  tf = YAP_ARG2;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    if (YAP_IsIntTerm(tf)) {
      matrix_long_set(mat, dims, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_long_set(mat, dims, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  } else {
    if (YAP_IsIntTerm(tf)) {
      matrix_float_set(mat, dims, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_float_set(mat, dims, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  }
  return TRUE;
}

static YAP_Bool matrix_set_all(void) {
  int *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  tf = YAP_ARG2;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    if (YAP_IsIntTerm(tf)) {
      matrix_long_set_all(mat, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_long_set_all(mat, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  } else {
    if (YAP_IsIntTerm(tf)) {
      matrix_float_set_all(mat, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_float_set_all(mat, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  }
  return TRUE;
}

static YAP_Bool matrix_add(void) {
  int dims[MAX_DIMS], *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  tf = YAP_ARG3;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    if (YAP_IsIntTerm(tf)) {
      matrix_long_add(mat, dims, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_long_add(mat, dims, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  } else {
    if (YAP_IsIntTerm(tf)) {
      matrix_float_add(mat, dims, YAP_IntOfTerm(tf));
    } else if (YAP_IsFloatTerm(tf)) {
      matrix_float_add(mat, dims, YAP_FloatOfTerm(tf));
    } else {
      /* Error */
      return FALSE;
    }
  }
  return TRUE;
}

static YAP_Bool do_matrix_access(void) {
  int dims[MAX_DIMS], *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  tf = matrix_access(mat, dims);
  return YAP_Unify(tf, YAP_ARG3);
}

static YAP_Bool do_matrix_access2(void) {
  int dims[MAX_DIMS], *mat;
  YAP_Term tf, t = YAP_ARG1;

  mat = (int *)YAP_BlobOfTerm(YAP_ArgOfTerm(1, t));
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims_args(mat[MAT_NDIMS], t, dims)) {
    /* Error */
    return FALSE;
  }
  tf = matrix_access(mat, dims);
  return YAP_Unify(tf, YAP_ARG2);
}

static YAP_Bool do_matrix_inc(void) {
  int dims[MAX_DIMS], *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  matrix_inc(mat, dims);
  return TRUE;
}

static YAP_Bool do_matrix_dec(void) {
  int dims[MAX_DIMS], *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  matrix_dec(mat, dims);
  return TRUE;
}

static YAP_Bool do_matrix_inc2(void) {
  int dims[MAX_DIMS], *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(matrix_inc2(mat, dims), YAP_ARG3);
}

static YAP_Bool do_matrix_dec2(void) {
  int dims[MAX_DIMS], *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, dims)) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(matrix_dec2(mat, dims), YAP_ARG3);
}

static YAP_Bool matrix_to_list(void) {
  int *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX)
    tf = long_matrix_to_list(mat);
  else
    tf = float_matrix_to_list(mat);
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_set_base(void) {
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  mat[MAT_BASE] = YAP_IntOfTerm(YAP_ARG2);
  return TRUE;
}

static YAP_Bool matrix_dims(void) {
  int *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  tf = mk_int_list(mat[MAT_NDIMS], mat + MAT_DIMS);
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_dims3(void) {
  int *mat;
  YAP_Term tf, tof;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  tf = mk_int_list(mat[MAT_NDIMS], mat + MAT_DIMS);
  tof = mk_rep_int_list(mat[MAT_NDIMS], mat[MAT_BASE]);
  return YAP_Unify(YAP_ARG2, tf) && YAP_Unify(YAP_ARG3, tof);
}

static YAP_Bool matrix_size(void) {
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(mat[MAT_SIZE]));
}

static YAP_Bool matrix_ndims(void) {
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(mat[MAT_NDIMS]));
}

static YAP_Bool matrix_type(void) {
  int *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
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
  int indx[MAX_DIMS], *mat;
  unsigned int off;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!scan_dims(mat[MAT_NDIMS], YAP_ARG2, indx)) {
    /* Error */
    return FALSE;
  }
  off = matrix_get_offset(mat, indx);

  return YAP_Unify(YAP_ARG3, YAP_MkIntTerm(off));
}

static YAP_Bool matrix_offset_to_arg(void) {
  int indx[MAX_DIMS], *mat;
  unsigned int off;
  YAP_Term ti, tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (!YAP_IsIntTerm(ti = YAP_ARG2)) {
    /* Error */
    return FALSE;
  }
  off = YAP_IntOfTerm(ti);
  matrix_get_index(mat, off, indx);
  tf = mk_int_list2(mat[MAT_NDIMS], mat[MAT_BASE], indx);
  return YAP_Unify(YAP_ARG3, tf);
}

static unsigned int scan_max_long(int sz, long int *data) {
  int i, off = 0;
  long int max = data[0];
  for (i = 1; i < sz; i++) {
    if (data[i] > max) {
      off = i;
      max = data[i];
    }
  }
  return off;
}

static unsigned int scan_max_float(int sz, double *data) {
  int i, off = 0;
  double max = data[0];
  for (i = 1; i < sz; i++) {
    if (data[i] > max) {
      max = data[i];
      off = i;
    }
  }
  return off;
}

static unsigned int scan_min_long(int sz, long int *data) {
  int i, off = 0;
  long int max = data[0];
  for (i = 1; i < sz; i++) {
    if (data[i] < max) {
      max = data[i];
      off = i;
    }
  }
  return off;
}

static unsigned int scan_min_float(int sz, double *data) {
  int i, off = 0;
  double max = data[0];
  for (i = 1; i < sz; i++) {
    if (data[i] < max) {
      max = data[i];
      off = i;
    }
  }
  return off;
}

static YAP_Bool matrix_max(void) {
  int *mat;
  unsigned int off;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    off = scan_max_long(mat[MAT_SIZE], data);
    tf = YAP_MkIntTerm(data[off]);
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    off = scan_max_float(mat[MAT_SIZE], data);
    tf = YAP_MkFloatTerm(data[off]);
  }
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_maxarg(void) {
  int indx[MAX_DIMS], *mat;
  unsigned int off;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    off = scan_max_long(mat[MAT_SIZE], data);
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    off = scan_max_float(mat[MAT_SIZE], data);
  }
  matrix_get_index(mat, off, indx);
  tf = mk_int_list(mat[MAT_NDIMS], indx);
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_min(void) {
  int *mat;
  unsigned int off;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    off = scan_min_long(mat[MAT_SIZE], data);
    tf = YAP_MkIntTerm(data[off]);
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    off = scan_min_float(mat[MAT_SIZE], data);
    tf = YAP_MkFloatTerm(data[off]);
  }
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_log_all(void) {
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    return FALSE;
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    int i;

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      data[i] = log(data[i]);
    }
  }
  return TRUE;
}

static YAP_Bool matrix_log_all2(void) {
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Term out;
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    double *ndata;
    int i;
    int *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (int *)YAP_BlobOfTerm(out);
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
    int i;
    int *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (int *)YAP_BlobOfTerm(out);
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
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    return FALSE;
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    int i;

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      data[i] = exp(data[i]);
    }
  }
  return TRUE;
}

static YAP_Bool matrix_exp2_all(void) {
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    return FALSE;
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    int i;
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
  int *mat;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    YAP_Term out;
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    double *ndata;
    int i;
    int *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (int *)YAP_BlobOfTerm(out);
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
    int i;
    int *nmat;

    if (!YAP_IsVarTerm(YAP_ARG2)) {
      out = YAP_ARG2;
    } else {
      out = new_float_matrix(mat[MAT_NDIMS], mat + MAT_DIMS, NULL);
      if (out == YAP_TermNil())
        return FALSE;
    }
    nmat = (int *)YAP_BlobOfTerm(out);
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

static YAP_Bool matrix_minarg(void) {
  int indx[MAX_DIMS], *mat;
  unsigned int off;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    off = scan_min_long(mat[MAT_SIZE], data);
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    off = scan_min_float(mat[MAT_SIZE], data);
  }
  matrix_get_index(mat, off, indx);
  tf = mk_int_list(mat[MAT_NDIMS], indx);
  return YAP_Unify(YAP_ARG2, tf);
}

static YAP_Bool matrix_sum(void) {
  int *mat;
  YAP_Term tf;

  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);
    int i;
    long int sum = 0;

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      sum += data[i];
    }
    tf = YAP_MkIntTerm(sum);
  } else {
    double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
    int i;
    double sum = 0.0;
    // function KahanSum(input)
    double c = 0.0; // A running compensation for lost low-order bits.
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      double y = data[i] - c; // So far, so good: c is zero.
      double t =
          sum +
          y; // Alas, sum is big, y small, so low-order digits of y are lost.
      c = (t - sum) - y; // (t - sum) cancels the high-order part of y;
                         // subtracting y recovers negative (low part of y)
      sum = t;           // Algebraically, c should always be zero. Beware
                         // overly-aggressive optimizing compilers!
    }
    tf = YAP_MkFloatTerm(sum);
  }
  return YAP_Unify(YAP_ARG2, tf);
}

static void add_int_lines(int total, int nlines, long int *mat0,
                          long int *matf) {
  int ncols = total / nlines, i;
  for (i = 0; i < ncols; i++) {
    long int sum = 0;
    int j;

    for (j = i; j < total; j += ncols) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static void add_double_lines(int total, int nlines, double *mat0,
                             double *matf) {
  int ncols = total / nlines, i;
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
  int *mat;
  YAP_Term tf;
  YAP_Term top = YAP_ARG2;
  op_type op;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* create a new array without first dimension */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;

    tf = new_int_matrix(dims - 1, mat + (MAT_DIMS + 1), NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, dims);
    ndata = matrix_long_data(nmat, dims - 1);
    if (op == MAT_PLUS) {
      add_int_lines(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  } else {
    double *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;

    tf = new_float_matrix(dims - 1, mat + (MAT_DIMS + 1), NULL);
    nmat = (int *)YAP_BlobOfTerm(tf);
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

static void add_int_cols(int total, int nlines, long int *mat0,
                         long int *matf) {
  int ncols = total / nlines, i, j = 0;
  for (i = 0; i < nlines; i++) {
    long int sum = 0;
    int max = (i + 1) * ncols;

    for (; j < max; j++) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static void add_double_cols(int total, int nlines, double *mat0, double *matf) {
  int ncols = total / nlines, i, j = 0;
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
  int *mat;
  YAP_Term tf;
  YAP_Term top = YAP_ARG2;
  op_type op;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  /* create a new array without first dimension */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;

    tf = new_int_matrix(1, mat + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, dims);
    ndata = matrix_long_data(nmat, 1);
    if (op == MAT_PLUS) {
      add_int_cols(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  } else {
    double *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;

    tf = new_float_matrix(1, mat + MAT_DIMS, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, dims);
    ndata = matrix_double_data(nmat, 1);
    if (op == MAT_PLUS) {
      add_double_cols(mat[MAT_SIZE], mat[MAT_DIMS], data, ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG3, tf);
}

static void div_int_by_lines(int total, int nlines, long int *mat1,
                             long int *mat2, double *ndata) {
  int ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = ((double)mat1[i]) / mat2[i % ncols];
  }
}

static void div_int_by_dlines(int total, int nlines, long int *mat1,
                              double *mat2, double *ndata) {
  int ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] / mat2[i % ncols];
  }
}

static void div_float_long_by_lines(int total, int nlines, double *mat1,
                                    long int *mat2, double *ndata) {
  int ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] / mat2[i % ncols];
  }
}

static void div_float_by_lines(int total, int nlines, double *mat1,
                               double *mat2, double *ndata) {
  int ncols = total / nlines, i;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] / mat2[i % ncols];
  }
}

static YAP_Bool matrix_op_to_lines(void) {
  int *mat1, *mat2;
  YAP_Term top = YAP_ARG3;
  op_type op;
  YAP_Term tf;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat1) {
    /* Error */
    return FALSE;
  }
  mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
  if (!mat2) {
    /* Error */
    return FALSE;
  }
  /* create a new array without first dimension */
  if (mat1[MAT_TYPE] == INT_MATRIX) {
    long int *data1;
    int dims = mat1[MAT_NDIMS];
    int *nmat;
    data1 = matrix_long_data(mat1, dims);

    if (mat2[MAT_TYPE] == INT_MATRIX) {
      long int *data2 = matrix_long_data(mat2, dims - 1);
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
    int dims = mat1[MAT_NDIMS];
    int *nmat;

    data1 = matrix_double_data(mat1, dims);
    tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
    nmat = YAP_BlobOfTerm(tf);
    if (tf == YAP_TermNil())
      return FALSE;
    ndata = matrix_double_data(nmat, dims);
    if (mat2[MAT_TYPE] == INT_MATRIX) {
      long int *data2 = matrix_long_data(mat2, dims - 1);
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

static void matrix_long_add_data(long int *nmat, int siz, long int mat1[],
                                 long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] + mat2[i];
  }
}

static void matrix_long_double_add_data(double *nmat, int siz, long int mat1[],
                                        double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] + mat2[i];
  }
}

static void matrix_double_add_data(double *nmat, int siz, double mat1[],
                                   double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] + mat2[i];
  }
}

static void matrix_long_sub_data(long int *nmat, int siz, long int mat1[],
                                 long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] - mat2[i];
  }
}

static void matrix_long_double_sub_data(double *nmat, int siz, long int mat1[],
                                        double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] - mat2[i];
  }
}

static void matrix_long_double_rsub_data(double *nmat, int siz, double mat1[],
                                         long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat2[i] - mat1[i];
  }
}

static void matrix_double_sub_data(double *nmat, int siz, double mat1[],
                                   double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] - mat2[i];
  }
}

static void matrix_long_mult_data(long int *nmat, int siz, long int mat1[],
                                  long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] * mat2[i];
  }
}

static void matrix_long_double_mult_data(double *nmat, int siz, long int mat1[],
                                         double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] * mat2[i];
  }
}

static void matrix_double_mult_data(double *nmat, int siz, double mat1[],
                                    double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] * mat2[i];
  }
}

static void matrix_long_div_data(long int *nmat, int siz, long int mat1[],
                                 long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_div_data(double *nmat, int siz, long int mat1[],
                                        double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_div2_data(double *nmat, int siz, double mat1[],
                                         long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_double_div_data(double *nmat, int siz, double mat1[],
                                   double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_zdiv_data(long int *nmat, int siz, long int mat1[],
                                  long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0)
      nmat[i] = 0;
    else
      nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_zdiv_data(double *nmat, int siz, long int mat1[],
                                         double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0)
      nmat[i] = 0;
    else
      nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_long_double_zdiv2_data(double *nmat, int siz, double mat1[],
                                          long int mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0.0)
      nmat[i] = 0;
    else
      nmat[i] = mat1[i] / mat2[i];
  }
}

static void matrix_double_zdiv_data(double *nmat, int siz, double mat1[],
                                    double mat2[]) {
  int i;

  for (i = 0; i < siz; i++) {
    if (mat1[i] == 0.0) {
      nmat[i] = 0.0;
    } else {
      nmat[i] = mat1[i] / mat2[i];
    }
  }
}

static YAP_Bool matrix_op(void) {
  int *mat1, *mat2;
  YAP_Term top = YAP_ARG3;
  op_type op;
  YAP_Term tf = YAP_ARG4;
  int create = TRUE;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat1) {
    /* Error */
    return FALSE;
  }
  mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
  if (!mat2) {
    /* Error */
    return FALSE;
  }
  if (tf == YAP_ARG1 || tf == YAP_ARG2) {
    create = FALSE;
  }
  if (mat1[MAT_TYPE] == INT_MATRIX) {
    long int *data1;
    int dims = mat1[MAT_NDIMS];
    int *nmat;
    data1 = matrix_long_data(mat1, dims);

    if (mat2[MAT_TYPE] == INT_MATRIX) {
      long int *data2;
      long int *ndata;

      if (create)
        tf = new_int_matrix(dims, mat1 + MAT_DIMS, NULL);
      if (tf == YAP_TermNil()) {
        return FALSE;
      } else {
        /* there may have been an overflow */
        mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
        data1 = matrix_long_data(mat1, dims);
        mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
        data2 = matrix_long_data(mat2, dims);
      }
      nmat = YAP_BlobOfTerm(tf);
      ndata = matrix_long_data(nmat, dims);
      switch (op) {
      case MAT_PLUS:
        matrix_long_add_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_SUB:
        matrix_long_sub_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_TIMES:
        matrix_long_mult_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_DIV:
        matrix_long_div_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_ZDIV:
        matrix_long_zdiv_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      default:
        return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2;
      double *ndata;

      if (create)
        tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
      if (tf == YAP_TermNil()) {
        return FALSE;
      } else {
        /* there may have been an overflow */
        mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
        data1 = matrix_long_data(mat1, dims);
        mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
        data2 = matrix_double_data(mat2, dims);
      }
      nmat = YAP_BlobOfTerm(tf);
      ndata = matrix_double_data(nmat, dims);
      switch (op) {
      case MAT_PLUS:
        matrix_long_double_add_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_SUB:
        matrix_long_double_sub_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_TIMES:
        matrix_long_double_mult_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_DIV:
        matrix_long_double_div_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_ZDIV:
        matrix_long_double_zdiv_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      default:
        return FALSE;
      }
    } else {
      return FALSE;
    }
  } else {
    double *data1;
    int dims = mat1[MAT_NDIMS];
    int *nmat;
    data1 = matrix_double_data(mat1, dims);

    if (mat2[MAT_TYPE] == INT_MATRIX) {
      long int *data2;
      double *ndata;

      if (create)
        tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
      if (tf == YAP_TermNil()) {
        return FALSE;
      } else {
        /* there may have been an overflow */
        mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
        data1 = matrix_double_data(mat1, dims);
        mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
        data2 = matrix_long_data(mat2, dims);
      }
      nmat = YAP_BlobOfTerm(tf);
      ndata = matrix_double_data(nmat, dims);
      switch (op) {
      case MAT_PLUS:
        matrix_long_double_add_data(ndata, mat1[MAT_SIZE], data2, data1);
        break;
      case MAT_SUB:
        matrix_long_double_rsub_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_TIMES:
        matrix_long_double_mult_data(ndata, mat1[MAT_SIZE], data2, data1);
        break;
      case MAT_DIV:
        matrix_long_double_div2_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_ZDIV:
        matrix_long_double_zdiv2_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      default:
        return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2;
      double *ndata;

      if (create)
        tf = new_float_matrix(dims, mat1 + MAT_DIMS, NULL);
      if (tf == YAP_TermNil()) {
        return FALSE;
      } else {
        /* there may have been an overflow */
        mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
        data1 = matrix_double_data(mat1, dims);
        mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
        data2 = matrix_double_data(mat2, dims);
      }
      nmat = YAP_BlobOfTerm(tf);
      ndata = matrix_double_data(nmat, dims);
      switch (op) {
      case MAT_PLUS:
        matrix_double_add_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_SUB:
        matrix_double_sub_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_TIMES:
        matrix_double_mult_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_DIV:
        matrix_double_div_data(ndata, mat1[MAT_SIZE], data1, data2);
        break;
      case MAT_ZDIV:
        matrix_double_zdiv_data(ndata, mat1[MAT_SIZE], data1, data2);
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

static void add_int_by_cols(int total, int nlines, long int *mat1,
                            long int *mat2, long int *ndata) {
  int i, ncols = total / nlines;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] + mat2[i / ncols];
  }
}

static void add_int_by_dcols(int total, int nlines, long int *mat1,
                             double *mat2, double *ndata) {
  int i, ncols = total / nlines;
  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] + mat2[i / ncols];
  }
}

static void add_double_by_cols(int total, int nlines, double *mat1,
                               double *mat2, double *ndata) {
  int i;
  int ncols = total / nlines;

  for (i = 0; i < total; i++) {
    ndata[i] = mat1[i] + mat2[i / ncols];
  }
}

static YAP_Bool matrix_op_to_cols(void) {
  int *mat1, *mat2;
  YAP_Term top = YAP_ARG3;
  op_type op;
  YAP_Term tf;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat1 = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat1) {
    /* Error */
    return FALSE;
  }
  mat2 = (int *)YAP_BlobOfTerm(YAP_ARG2);
  if (!mat2) {
    /* Error */
    return FALSE;
  }
  if (mat1[MAT_TYPE] == INT_MATRIX) {
    long int *data1;
    int dims = mat1[MAT_NDIMS];
    int *nmat;
    data1 = matrix_long_data(mat1, dims);

    if (mat2[MAT_TYPE] == INT_MATRIX) {
      long int *data2 = matrix_long_data(mat2, 1);
      if (op == MAT_PLUS) {
        long int *ndata;

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
    int dims = mat1[MAT_NDIMS];
    int *nmat;

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
  int *mat;
  YAP_Term tf = 0;
  YAP_Term top = YAP_ARG2;
  op_type op;
  int create = FALSE;

  if (!YAP_IsIntTerm(top)) {
    return FALSE;
  }
  op = YAP_IntOfTerm(top);
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (YAP_IsVarTerm(YAP_ARG4)) {
    create = TRUE;
  }
  /* create a new array with same dimensions */
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data;
    int dims = mat[MAT_NDIMS];
    int *nmat;
    YAP_Term tnum = YAP_ARG3;

    if (YAP_IsIntTerm(tnum)) {
      long int num;
      long int *ndata;

      num = YAP_IntOfTerm(tnum);
      data = matrix_long_data(mat, dims);
      if (create) {
        tf = new_int_matrix(dims, mat + (MAT_DIMS), NULL);
        if (tf == YAP_TermNil())
          return FALSE;
        nmat = (int *)YAP_BlobOfTerm(tf);
        ndata = matrix_long_data(nmat, dims);
      } else {
        nmat = mat;
        ndata = data;
      }
      if (op == MAT_PLUS) {
        int i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] + num;
        }
      } else if (op == MAT_TIMES) {
        int i;

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
        nmat = (int *)YAP_BlobOfTerm(tf);
        ndata = matrix_double_data(nmat, dims);
      } else {
        return FALSE;
      }
      data = matrix_long_data(mat, dims);
      if (op == MAT_PLUS) {
        int i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] + num;
        }
      } else if (op == MAT_SUB) {
        int i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = num - data[i];
        }
      } else if (op == MAT_TIMES) {
        int i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] * num;
        }
      } else if (op == MAT_DIV) {
        int i;

        for (i = 0; i < mat[MAT_SIZE]; i++) {
          ndata[i] = data[i] / num;
        }
      }
    } else {
      return FALSE;
    }
  } else {
    double *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;
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
      nmat = (int *)YAP_BlobOfTerm(tf);
      ndata = matrix_double_data(nmat, dims);
    } else {
      nmat = mat;
      ndata = data;
    }
    switch (op) {
    case MAT_PLUS: {
      int i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = data[i] + num;
      }
    } break;
    case MAT_SUB: {
      int i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = num - data[i];
      }
    } break;
    case MAT_TIMES: {
      int i;

      for (i = 0; i < mat[MAT_SIZE]; i++) {
        ndata[i] = data[i] * num;
      }
    } break;
    case MAT_DIV: {
      int i;

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
  int ndims, i, *dims, *dimsn;
  int conv[MAX_DIMS], indx[MAX_DIMS], nindx[MAX_DIMS];
  YAP_Term tconv, tf;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  nmat = (int *)YAP_BlobOfTerm(tf);
  dims = mat + MAT_DIMS;
  dimsn = nmat + MAT_DIMS;
  /* we now have our target matrix, let us grab our conversion matrix */
  tconv = YAP_ARG2;
  for (i = 0; i < ndims; i++) {
    YAP_Term th;
    long int j;

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
    long int *data = matrix_long_data(mat, ndims);
    /* create a new matrix with the same size */
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      long int x = data[i];
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
  int ndims, i, j, newdims, prdim, leftarg, *dims, indx[MAX_DIMS];
  int nindx[MAX_DIMS];
  YAP_Term tpdim, tdimarg, tf;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
    long int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int size, i, ndims, newdims[1];
  int indx[MAX_DIMS];
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
    long int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(1, newdims, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, j, newdims, prdim;
  int indx[MAX_DIMS], nindx[MAX_DIMS];
  YAP_Term tpdim, tf;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
    long int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, *dims, newdims;
  int indx[MAX_DIMS], nindx[MAX_DIMS], conv[MAX_DIMS];
  YAP_Term tf, tconv;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
    long int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, j, *dims, newdims, prdim;
  int nindx[MAX_DIMS];
  YAP_Term tpdim, tf;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
    long int *data, *ndata;
    int d = 1, j = 0, dd = 1;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, *dims, newdims;
  int indx[MAX_DIMS], nindx[MAX_DIMS], conv[MAX_DIMS];
  YAP_Term tf, tconv;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;

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
    long int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, *dims, newdims = 0, olddims = 0;
  int new[MAX_DIMS], indx[MAX_DIMS], nindx[MAX_DIMS];
  YAP_Term tconv, tf;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
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
    long int j;

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
    long int *data, *ndata;

    /* create a new matrix with the same size */
    tf = new_int_matrix(newdims, nindx, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, *dims;
  int indx[MAX_DIMS];
  YAP_Term tf;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1), *nmat;
  int dim = YAP_IntOfTerm(YAP_ARG2);
  int pos = YAP_IntOfTerm(YAP_ARG3);

  if (!mat) {
    /* Error */
    return FALSE;
  }
  ndims = mat[MAT_NDIMS];
  dims = mat + MAT_DIMS;
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data, *ndata, val;

    /* create a new matrix with the same size */
    tf = new_int_matrix(ndims, dims, NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    nmat = (int *)YAP_BlobOfTerm(tf);
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
  int ndims, i, size;
  YAP_Term tm, *tp;
  int *mat = (int *)YAP_BlobOfTerm(YAP_ARG1);

  if (!mat) {
    return YAP_Unify(YAP_ARG1, YAP_ARG2);
  }
  ndims = mat[MAT_NDIMS];
  size = mat[MAT_SIZE];
  tm = YAP_MkNewApplTerm(FunctorM, 5);
  tp = YAP_ArgsOfTerm(tm);
  tp[0] = mk_int_list(ndims, mat + MAT_DIMS);
  tp[1] = YAP_MkIntTerm(ndims);
  tp[2] = YAP_MkIntTerm(size);
  tp[3] = mk_rep_int_list(ndims, mat[MAT_BASE]);
  tp[4] = YAP_MkNewApplTerm(YAP_MkFunctor(AtomC, size), size);
  tp = YAP_ArgsOfTerm(tp[3]);
  if (mat[MAT_TYPE] == INT_MATRIX) {
    long int *data;

    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    data = matrix_long_data(mat, ndims);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      tp[i] = YAP_MkIntTerm(data[i]);
    }
  } else {
    double *data;

    /* in case the matrix moved */
    mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
    data = matrix_double_data(mat, ndims);
    for (i = 0; i < mat[MAT_SIZE]; i++) {
      tp[i] = YAP_MkFloatTerm(data[i]);
    }
  }
  return YAP_Unify(YAP_ARG2, tm);
}

static YAP_Bool is_matrix(void) {
  YAP_Term t = YAP_ARG1;
  int *mat = (int *)YAP_BlobOfTerm(t);

  if (!mat) {
    if (!YAP_IsApplTerm(t))
      return FALSE;
    return YAP_FunctorOfTerm(t) == FunctorM;
  }
  return TRUE;
}

static YAP_Bool get_float_from_address(void) {
  YAP_Float *fp = (YAP_Float *)YAP_IntOfTerm(YAP_ARG1);
  YAP_Int off = YAP_IntOfTerm(YAP_ARG2);

  return YAP_Unify(YAP_ARG3, YAP_MkFloatTerm(fp[off]));
}

static YAP_Bool set_float_from_address(void) {
  YAP_Float *fp = (YAP_Float *)YAP_IntOfTerm(YAP_ARG1);
  YAP_Int off = YAP_IntOfTerm(YAP_ARG2);
  YAP_Float f = YAP_FloatOfTerm(YAP_ARG3);

  fp[off] = f;

  return true;
}


static YAP_Bool address_to_list(void) {
    YAP_Term t = YAP_TermNil();
    int i;
    YAP_Float *fp = (YAP_Float *)YAP_IntOfTerm(YAP_ARG1);
    YAP_Int sz = YAP_IntOfTerm(YAP_ARG2);
    for (i = 0; i< sz; i++)
        t = YAP_MkPairTerm(YAP_MkFloatTerm(fp[sz-i-1]),t);
    return YAP_Unify(YAP_ARG3, t);
}


static YAP_Bool address_to_sum(void) {
    YAP_Term t = YAP_TermNil();
    int i;
    YAP_Float *data = (YAP_Float *)YAP_IntOfTerm(YAP_ARG1);
    YAP_Int sz = YAP_IntOfTerm(YAP_ARG2);
    double sum = 0.0;
    // function KahanSum(input)
    double c = 0.0; // A running compensation for lost low-order bits.
    for (i = 0; i < sz; i++) {
        double y = data[i] - c; // So far, so good: c is zero.
        double t =
                sum +
                y; // Alas, sum is big, y small, so low-order digits of y are lost.
        c = (t - sum) - y; // (t - sum) cancels the high-order part of y;
        // subtracting y recovers negative (low part of y)
        sum = t;           // Algebraically, c should always be zero. Beware
        // overly-aggressive optimizing compilers!
    }
    t = YAP_MkFloatTerm(sum);
    return YAP_Unify(YAP_ARG3, t);
}



X_API void init_matrix(void);

X_API void init_matrix(void) {
  AtomC = YAP_LookupAtom("c");
  FunctorM = YAP_MkFunctor(YAP_LookupAtom("$matrix"), 5);

  YAP_UserCPredicate("new_ints_matrix", new_ints_matrix, 4);
  YAP_UserCPredicate("new_ints_matrix_set", new_ints_matrix_set, 4);
  YAP_UserCPredicate("new_floats_matrix", new_floats_matrix, 4);
  YAP_UserCPredicate("new_floats_matrix_set", new_floats_matrix_set, 4);
  YAP_UserCPredicate("matrixn_set", matrix_set, 3);
  YAP_UserCPredicate("matrix_set", matrix_set2, 2);
  YAP_UserCPredicate("matrix_set_all", matrix_set_all, 2);
  YAP_UserCPredicate("matrix_add", matrix_add, 3);
  YAP_UserCPredicate("matrixn_get", do_matrix_access, 3);
  YAP_UserCPredicate("matrixn_get", do_matrix_access2, 2);
  YAP_UserCPredicate("matrix_inc", do_matrix_inc, 2);
  YAP_UserCPredicate("matrix_dec", do_matrix_dec, 2);
  YAP_UserCPredicate("matrix_inc", do_matrix_inc2, 3);
  YAP_UserCPredicate("matrix_dec", do_matrix_dec2, 3);
  YAP_UserCPredicate("matrixn_to_list", matrix_to_list, 2);
  YAP_UserCPredicate("matrixn_set_base", matrix_set_base, 2);
  YAP_UserCPredicate("matrixn_dims", matrix_dims, 2);
  YAP_UserCPredicate("matrixn_dims", matrix_dims3, 3);
  YAP_UserCPredicate("matrixn_ndims", matrix_ndims, 2);
  YAP_UserCPredicate("matrixn_size", matrix_size, 2);
  YAP_UserCPredicate("matrix_type_as_number", matrix_type, 2);
  YAP_UserCPredicate("matrixn_arg_to_offset", matrix_arg_to_offset, 3);
  YAP_UserCPredicate("matrixn_offset_to_arg", matrix_offset_to_arg, 3);
  YAP_UserCPredicate("matrixn_max", matrix_max, 2);
  YAP_UserCPredicate("matrixn_maxarg", matrix_maxarg, 2);
  YAP_UserCPredicate("matrixn_min", matrix_min, 2);
  YAP_UserCPredicate("matrixn_minarg", matrix_minarg, 2);
  YAP_UserCPredicate("matrix_sum", matrix_sum, 2);
  YAP_UserCPredicate("matrix_shuffle", matrix_transpose, 3);
  YAP_UserCPredicate("matrix_expand", matrix_expand, 3);
  YAP_UserCPredicate("matrix_select", matrix_select, 4);
  YAP_UserCPredicate("matrix_column", matrix_column, 3);
  YAP_UserCPredicate("matrix_to_logs", matrix_log_all, 1);
  YAP_UserCPredicate("matrix_to_exps", matrix_exp_all, 1);
  YAP_UserCPredicate("matrix_to_exps2", matrix_exp2_all, 1);
  YAP_UserCPredicate("matrixn_to_logs", matrix_log_all2, 2);
  YAP_UserCPredicate("matrixn_to_exps", matrix_exp_all2, 2);
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
  YAP_UserCPredicate("matrix", is_matrix, 1);
  YAP_UserCPredicate("get_float_from_address", get_float_from_address, 3);
    YAP_UserCPredicate("set_float_from_address", set_float_from_address, 3);
    YAP_UserCPredicate("address_to_list", address_to_list, 3);
    YAP_UserCPredicate("address_to_sum", address_to_sum, 3);
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
