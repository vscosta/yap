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
* File:		matrix.c						 *
* Last rev:								 *
* mods:									 *
* comments:	numerical arrays		                         *
*									 *
*************************************************************************/

#include "config.h"
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

typedef enum {
  INT_MATRIX,
  FLOAT_MATRIX
} mat_data_type;

typedef enum {
  MAT_TYPE=0,
  MAT_NDIMS=1,
  MAT_SIZE=2,
  MAT_ALIGN=3,
  MAT_DIMS=4,
} mat_type;

typedef enum {
  MAT_PLUS=0,
  MAT_SUB=1,
  MAT_TIMES=2,
  MAT_DIV=3,
  MAT_IDIV=4
} op_type;

static long int *
matrix_long_data(int *mat, int ndims)
{
  return (long int *)(mat+(MAT_DIMS+ndims));
}

static double *
matrix_double_data(int *mat, int ndims)
{
  return (double *)(mat+(MAT_DIMS+ndims));
}

static unsigned int
matrix_get_offset(int *mat, int* indx)
{
  unsigned int i, pos = mat[MAT_SIZE], off = 0;

  /* find where we are */
  for (i = 0; i < mat[MAT_NDIMS]; i++) {
    pos /= mat[MAT_DIMS+i];
    if (indx[i] >= mat[MAT_DIMS+i]) {
      return off;
    }
    off += pos*indx[i];
  }
  return off;
}

static void
matrix_get_index(int *mat, unsigned int offset, int* indx)
{
  unsigned int i, pos = mat[MAT_SIZE];

  /* find where we are */
  for (i = 0; i < mat[MAT_NDIMS]; i++) {
    pos /= mat[MAT_DIMS+i];
    indx[i] = offset / pos;
    offset = offset % pos;
  }
}

static YAP_Term
new_int_matrix(int ndims, int dims[], long int data[])
{
  unsigned int sz;
  unsigned int i, nelems=1;
  YAP_Term blob;
  int *mat;
  long int *bdata;

  for (i=0;i< ndims;i++) {
    nelems *= dims[i];
  }
  sz = ((MAT_DIMS+1)*sizeof(int)+ndims*sizeof(int)+nelems*sizeof(long int))/sizeof(YAP_CELL);
  blob = YAP_MkBlobTerm(sz);
  if (blob == YAP_TermNil())
    return FALSE;
  mat = (int *)YAP_BlobOfTerm(blob);
  mat[MAT_TYPE] = INT_MATRIX;
  mat[MAT_NDIMS] = ndims;
  mat[MAT_SIZE] = nelems;
  for (i=0;i< ndims;i++) {
    mat[MAT_DIMS+i] = dims[i];
  }
  bdata = matrix_long_data(mat,ndims);
  if (data)
    memcpy((void *)bdata,(void *)data,sizeof(double)*nelems);
  return blob;
}

static YAP_Term
new_float_matrix(int ndims, int dims[], double data[])
{
  unsigned int sz;
  unsigned int i, nelems=1;
  YAP_Term blob;
  int *mat;
  double *bdata;

  for (i=0;i< ndims;i++) {
    nelems *= dims[i];
  }
  sz = ((MAT_DIMS+1)*sizeof(int)+ndims*sizeof(int)+(nelems+1)*sizeof(double))/sizeof(YAP_CELL);
  blob = YAP_MkBlobTerm(sz);
  if (blob == YAP_TermNil())
    return FALSE;
  mat = YAP_BlobOfTerm(blob);
  mat[MAT_TYPE] = FLOAT_MATRIX;
  mat[MAT_NDIMS] = ndims;
  mat[MAT_SIZE] = nelems;
  for (i=0;i< ndims;i++) {
    mat[MAT_DIMS+i] = dims[i];
  }
  bdata = matrix_double_data(mat,ndims);
  if (data)
    memcpy((void *)bdata,(void *)data,sizeof(double)*nelems);
  return blob;
}

static int
scan_dims(int ndims, YAP_Term tl, int dims[MAX_DIMS])
{
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

static int
cp_int_matrix(YAP_Term tl,YAP_Term matrix)
{
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

static int
cp_float_matrix(YAP_Term tl,YAP_Term matrix)
{
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
    if (!YAP_IsFloatTerm(th)) {
      /* ERROR */
      return FALSE;
    }
    d = YAP_FloatOfTerm(th);
    j[i] = d;
    tl = YAP_TailOfTerm(tl);
  }
  if (tl != YAP_TermNil()) {
    /* ERROR */
    return FALSE;
  }
  return TRUE;
}


static int
set_int_matrix(YAP_Term matrix,long int set)
{
  int *mat = (int *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  long int *j = matrix_long_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    j[i] = set;
  }
  return TRUE;
}

static int
set_float_matrix(YAP_Term matrix,double set)
{
  int *mat = (int *)YAP_BlobOfTerm(matrix);
  int i, nelems = mat[MAT_SIZE];
  double *j = matrix_double_data(mat, mat[MAT_NDIMS]);

  for (i = 0; i < nelems; i++) {
    j[i] = set;
  }
  return TRUE;
}

static int
new_ints_matrix(void)
{
  int ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out;
  int dims[MAX_DIMS];
  
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_int_matrix(ndims, dims, NULL);
  if (!cp_int_matrix(YAP_ARG3,out))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static int
new_ints_matrix_set(void)
{
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
  if (!set_int_matrix(out,set))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static int
new_floats_matrix(void)
{
  int ndims = YAP_IntOfTerm(YAP_ARG1);
  YAP_Term tl = YAP_ARG2, out;
  int dims[MAX_DIMS];
  
  if (!scan_dims(ndims, tl, dims))
    return FALSE;
  out = new_float_matrix(ndims, dims, NULL);
  if (!cp_float_matrix(YAP_ARG3,out))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static int
new_floats_matrix_set(void)
{
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
  if (!set_float_matrix(out,set))
    return FALSE;
  return YAP_Unify(YAP_ARG4, out);
}

static YAP_Term
float_matrix_to_list(int *mat) {
  double *data = matrix_double_data(mat, mat[MAT_NDIMS]);
  int i = 0;
  YAP_Term tf = YAP_TermNil();

  for (i = mat[MAT_SIZE]-1; i>= 0; i--) {
    tf = YAP_MkPairTerm(YAP_MkFloatTerm(data[i]),tf);
    if (tf == YAP_TermNil()) {
      /* error */
      return YAP_TermNil();
    }
  }
  return tf;
}

static YAP_Term
mk_int_list(int nelems, int *data)
{
  YAP_Term tn = YAP_TermNil();
  YAP_Term tf = tn;
  int i = 0;

  for (i = nelems-1; i>= 0; i--) {
    tf = YAP_MkPairTerm(YAP_MkIntTerm(data[i]),tf);
    if (tf == tn) {
      /* error */
      return tn;
    }
  }
  return tf;
}

static YAP_Term
mk_long_list(int nelems, long int *data)
{
  YAP_Term tn = YAP_TermNil();
  YAP_Term tf = tn;
  int i = 0;

  for (i = nelems-1; i>= 0; i--) {
    tf = YAP_MkPairTerm(YAP_MkIntTerm(data[i]),tf);
    if (tf == tn) {
      /* error */
      return tn;
    }
  }
  return tf;
}


static YAP_Term
long_matrix_to_list(int *mat) {
  long int *data = matrix_long_data(mat, mat[MAT_NDIMS]);

  return mk_long_list(mat[MAT_SIZE], data);
}

static YAP_Term
matrix_access(int *mat, int *indx)
{
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE]==FLOAT_MATRIX)
    return YAP_MkFloatTerm((matrix_double_data(mat,mat[MAT_NDIMS]))[off]);
  else
    return YAP_MkIntTerm((matrix_long_data(mat,mat[MAT_NDIMS]))[off]);
}

static void
matrix_float_set(int *mat, int *indx, double nval)
{
  unsigned int off = 0;

  off = matrix_get_offset(mat, indx);
  (matrix_double_data(mat,mat[MAT_NDIMS]))[off] = nval;
}

static void
matrix_long_set(int *mat, int *indx, long int nval)
{
  unsigned int off = matrix_get_offset(mat, indx);
  (matrix_long_data(mat,mat[MAT_NDIMS]))[off] = nval;
}

static void
matrix_float_set_all(int *mat, double nval)
{
  int i;
  double *data = matrix_double_data(mat,mat[MAT_NDIMS]);

  for (i = 0; i< mat[MAT_SIZE]; i++)
    data[i] = nval;
}

static void
matrix_long_set_all(int *mat, long int nval)
{
  int i;
  long int *data = matrix_long_data(mat,mat[MAT_NDIMS]);

  for (i = 0; i< mat[MAT_SIZE]; i++)
    data[i] = nval;
}

static void
matrix_float_add(int *mat, int *indx, double nval)
{
  unsigned int off;
  double *dat = matrix_double_data(mat,mat[MAT_NDIMS]);

  off = matrix_get_offset(mat, indx);
  dat[off] += nval;
}

static void
matrix_long_add(int *mat, int *indx, long int nval)
{
  long int *dat = matrix_long_data(mat,mat[MAT_NDIMS]);
  unsigned int off = matrix_get_offset(mat, indx);
  dat[off] += nval;
}

static void
matrix_inc(int *mat, int *indx)
{
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE]==FLOAT_MATRIX)
    (matrix_double_data(mat,mat[MAT_NDIMS])[off])++;
  else
    ((matrix_long_data(mat,mat[MAT_NDIMS]))[off])++;
}

static void
matrix_dec(int *mat, int *indx)
{
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE]==FLOAT_MATRIX)
    (matrix_double_data(mat,mat[MAT_NDIMS])[off])--;
  else
    ((matrix_long_data(mat,mat[MAT_NDIMS]))[off])--;
}

static YAP_Term
matrix_inc2(int *mat, int *indx)
{
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE]==FLOAT_MATRIX) {
    double *data  = matrix_double_data(mat,mat[MAT_NDIMS]);
    double d = data[off];
    d++;
    data[off] = d;
    return YAP_MkFloatTerm(d);
  } else {
    long int *data  = matrix_long_data(mat,mat[MAT_NDIMS]);
    long int d = data[off];
    d++;
    data[off] = d;
    return YAP_MkIntTerm(d);
  }
}

static YAP_Term
matrix_dec2(int *mat, int *indx)
{
  unsigned int off = matrix_get_offset(mat, indx);
  if (mat[MAT_TYPE]==FLOAT_MATRIX) {
    double *data  = matrix_double_data(mat,mat[MAT_NDIMS]);
    double d = data[off];
    d--;
    data[off] = d;
    return YAP_MkFloatTerm(d);
  } else {
    long int *data  = matrix_long_data(mat,mat[MAT_NDIMS]);
    long int d = data[off];
    d--;
    data[off] = d;
    return YAP_MkIntTerm(d);
  }
}


static int
matrix_set(void)
{
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

static int
matrix_set_all(void)
{
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

static int
matrix_add(void)
{
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

static int
do_matrix_access(void)
{
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

static int
do_matrix_inc(void)
{
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

static int
do_matrix_dec(void)
{
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

static int
do_matrix_inc2(void)
{
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
  return
    YAP_Unify(matrix_inc2(mat, dims), YAP_ARG3);
}

static int
do_matrix_dec2(void)
{
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
  return
    YAP_Unify(matrix_dec2(mat, dims), YAP_ARG3);
}

static int
matrix_to_list(void)
{
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

static int
matrix_dims(void)
{
  int *mat;
  YAP_Term tf;
  
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  tf = mk_int_list(mat[MAT_NDIMS],mat+MAT_DIMS);
  return YAP_Unify(YAP_ARG2, tf);
}

static int
matrix_size(void)
{
  int *mat;
  
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(mat[MAT_SIZE]));
}

static int
matrix_ndims(void)
{
  int *mat;
  
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(mat[MAT_NDIMS]));
}

static int
matrix_type(void)
{
  int *mat;
  YAP_Term tf;
  
  mat = (int *)YAP_BlobOfTerm(YAP_ARG1);
  if (!mat) {
    /* Error */
    return FALSE;
  }
  if (mat[MAT_TYPE] == INT_MATRIX) {
    tf = YAP_MkIntTerm(0);
  } else {
    tf = YAP_MkIntTerm(1);
  }
  return YAP_Unify(YAP_ARG2, tf);
}

static int
matrix_arg_to_offset(void)
{
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

static int
matrix_offset_to_arg(void)
{
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
  tf = mk_int_list(mat[MAT_NDIMS], indx);
  return YAP_Unify(YAP_ARG3, tf);
}

static unsigned int
scan_max_long(int sz, long int *data)
{
  int i, off=0;
  long int max= data[0];
  for (i=1; i<sz; i++) {
    if (data[i]>max) {
      off=i;
      max = data[i];
    }
  }
  return off;
}

static unsigned int
scan_max_float(int sz, double *data)
{
  int i, off=0;
  double max= data[0];
  for (i=1; i<sz; i++) {
    if (data[i]>max) {
      max = data[i];
      off=i;
    }
  }
  return off;
}

static unsigned int
scan_min_long(int sz, long int *data)
{
  int i, off=0;
  long int max= data[0];
  for (i=1; i<sz; i++) {
    if (data[i]<max) {
      max = data[i];
      off=i;
    }
  }
  return off;
}

static unsigned int
scan_min_float(int sz, double *data)
{
  int i, off=0;
  double max= data[0];
  for (i=1; i<sz; i++) {
    if (data[i]<max) {
      max = data[i];
      off=i;
    }
  }
  return off;
}

static int
matrix_max(void)
{
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

static int
matrix_maxarg(void)
{
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

static int
matrix_min(void)
{
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

static int
matrix_minarg(void)
{
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

static int
matrix_sum(void)
{
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

    for (i = 0; i < mat[MAT_SIZE]; i++) {
      sum += data[i];
    }
    tf = YAP_MkFloatTerm(sum);
  }
  return YAP_Unify(YAP_ARG2, tf);
}

static void
add_int_lines(int total,int nlines,long int *mat0,long int *matf)
{
  int ncols = total/nlines, i;
  for (i=0;i<ncols;i++) {
    long int sum = 0;
    int j;

    for (j=i;j<total;j+=ncols) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static void
add_double_lines(int total,int nlines,double *mat0,double *matf)
{
  int ncols = total/nlines, i;
  for (i=0;i<ncols;i++) {
    double sum = 0;
    int j;

    for (j=i;j<total;j+=ncols) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static int
matrix_agg_lines(void)
{
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

    tf = new_int_matrix(dims-1,mat+(MAT_DIMS+1),NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, dims);
    ndata = matrix_long_data(nmat, dims-1);
    if (op == MAT_PLUS) {
      add_int_lines(mat[MAT_SIZE],mat[MAT_DIMS],data,ndata);
    } else
      return FALSE;
  } else {
    double *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;

    tf = new_float_matrix(dims-1,mat+(MAT_DIMS+1),NULL);
    nmat = (int *)YAP_BlobOfTerm(tf);
    if (tf == YAP_TermNil())
      return FALSE;
    data = matrix_double_data(mat, dims);
    ndata = matrix_double_data(nmat, dims-1);
    if (op == MAT_PLUS) {
      add_double_lines(mat[MAT_SIZE],mat[MAT_DIMS],data,ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG3,tf);
}

static void
add_int_cols(int total,int nlines,long int *mat0,long int *matf)
{
  int ncols = total/nlines, i, j = 0;
  for (i=0;i<nlines;i++) {
    long int sum = 0;
    int max = (i+1)*ncols;

    for (;j<max;j++) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static void
add_double_cols(int total,int nlines,double *mat0,double *matf)
{
  int ncols = total/nlines, i, j = 0;
  for (i=0;i<nlines;i++) {
    double sum = 0;
    int max = (i+1)*ncols;

    for (;j<max;j++) {
      sum += mat0[j];
    }
    matf[i] = sum;
  }
}

static int
matrix_agg_cols(void)
{
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

    tf = new_int_matrix(1,mat+MAT_DIMS,NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_long_data(mat, dims);
    ndata = matrix_long_data(nmat, 1);
    if (op == MAT_PLUS) {
      add_int_cols(mat[MAT_SIZE],mat[MAT_DIMS],data,ndata);
    } else
      return FALSE;
  } else {
    double *data, *ndata;
    int dims = mat[MAT_NDIMS];
    int *nmat;

    tf = new_float_matrix(1,mat+MAT_DIMS,NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, dims);
    ndata = matrix_double_data(nmat, 1);
    if (op == MAT_PLUS) {
      add_double_cols(mat[MAT_SIZE],mat[MAT_DIMS],data,ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG3,tf);
}

static void
div_int_by_lines(int total,int nlines,long int *mat1,long int *mat2,double *ndata)
{
  int ncols = total/nlines, i;
  for (i=0;i<total;i++) {
    ndata[i] = ((double)mat1[i])/mat2[i%ncols];
  }
}

static void
div_int_by_dlines(int total,int nlines,long int *mat1,double *mat2,double *ndata)
{
  int ncols = total/nlines, i;
  for (i=0;i<total;i++) {
    ndata[i] = mat1[i]/mat2[i%ncols];
  }
}

static void
div_float_long_by_lines(int total,int nlines,double *mat1,long int *mat2,double *ndata)
{
  int ncols = total/nlines, i;
  for (i=0;i<total;i++) {
    ndata[i] = mat1[i]/mat2[i%ncols];
  }
}

static void
div_float_by_lines(int total,int nlines,double *mat1,double *mat2,double *ndata)
{
  int ncols = total/nlines, i;
  for (i=0;i<total;i++) {
    ndata[i] = mat1[i]/mat2[i%ncols];
  }
}

static int
matrix_op_to_lines(void)
{
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
      long int *data2 = matrix_long_data(mat2, dims-1);
      if (op == MAT_DIV) {
	double *ndata;

	tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_double_data(nmat, dims);
	div_int_by_lines(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
      } else {
	return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, dims-1);
      if (op == MAT_DIV) {
	double *ndata;

	tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_double_data(nmat, dims);
	div_int_by_dlines(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
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
    tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL); 
    nmat = YAP_BlobOfTerm(tf);
    if (tf == YAP_TermNil())
      return FALSE;
    ndata = matrix_double_data(nmat, dims);
    if (mat2[MAT_TYPE] == INT_MATRIX) {
      long int *data2 = matrix_long_data(mat2, dims-1);
      if (op == MAT_DIV) {
	div_float_long_by_lines(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
      } else {
	return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, dims-1);
      if (op == MAT_DIV) {
	div_float_by_lines(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  }
  return YAP_Unify(YAP_ARG4,tf);
}


static void
matrix_long_add_data(long int *nmat, int siz, long int mat1[], long int mat2[])
{
  int i;

  for (i=0; i< siz; i++) {
    nmat[i] = mat1[i]+mat2[i];
  }
}

static void
matrix_long_double_add_data(double *nmat, int siz, long int mat1[], double mat2[])
{
  int i;

  for (i=0; i< siz; i++) {
    nmat[i] = mat1[i]+mat2[i];
  }
}

static void
matrix_double_add_data(double *nmat, int siz, double mat1[], double mat2[])
{
  int i;

  for (i=0; i< siz; i++) {
    nmat[i] = mat1[i]+mat2[i];
  }
}

static int
matrix_op(void)
{
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

	tf = new_int_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_long_data(nmat, dims);
	matrix_long_add_data(ndata, mat1[MAT_SIZE], data1, data2);
      } else {
	return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, 1);
      if (op == MAT_PLUS) {
	double *ndata;

	tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_double_data(nmat, dims);
	matrix_long_double_add_data(ndata, mat1[MAT_SIZE], data1, data2);
      } else {
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
      long int *data2 = matrix_long_data(mat2, 1);
      if (op == MAT_PLUS) {
	double *ndata;

	tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_double_data(nmat, dims);
	matrix_long_double_add_data(ndata, mat1[MAT_SIZE], data2, data1);
      } else {
	return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, 1);
      if (op == MAT_PLUS) {
	double *ndata;

	tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_double_data(nmat, dims);
	matrix_double_add_data(ndata, mat1[MAT_SIZE], data1, data2);
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  }
  return YAP_Unify(YAP_ARG4,tf);
}

static void
add_int_by_cols(int total,int nlines,long int *mat1,long int *mat2,long int *ndata)
{
  int i, ncols = total/nlines;
  for (i=0;i<total;i++) {
    ndata[i] = mat1[i] + mat2[i/ncols];
  }
}

static void
add_int_by_dcols(int total,int nlines,long int *mat1,double *mat2,double *ndata)
{
  int i, ncols = total/nlines;
  for (i=0;i<total;i++) {
    ndata[i] = mat1[i] + mat2[i/ncols];
  }
}

static void
add_double_by_cols(int total,int nlines,double *mat1,double *mat2,double *ndata)
{
  int i;
  int ncols = total/nlines;

  for (i=0;i<total;i++) {
    ndata[i] = mat1[i] + mat2[i/ncols];
  }
}

static int
matrix_op_to_cols(void)
{
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

	tf = new_int_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_long_data(nmat, dims);
	add_int_by_cols(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
      } else {
	return FALSE;
      }
    } else if (mat2[MAT_TYPE] == FLOAT_MATRIX) {
      double *data2 = matrix_double_data(mat2, 1);
      if (op == MAT_PLUS) {
	double *ndata;

	tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
	if (tf == YAP_TermNil())
	  return FALSE;
	nmat = YAP_BlobOfTerm(tf);
	ndata = matrix_double_data(nmat, dims);
	add_int_by_dcols(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
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
    tf = new_float_matrix(dims,mat1+MAT_DIMS,NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = YAP_BlobOfTerm(tf);
    data1 = matrix_double_data(mat1, dims);
    data2 = matrix_double_data(mat2, 1);
    ndata = matrix_double_data(nmat, dims);
    if (op == MAT_PLUS) {
      add_double_by_cols(mat1[MAT_SIZE],mat1[MAT_DIMS],data1,data2,ndata);
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG4,tf);
}

static int
matrix_op_to_all(void)
{
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
    long int *data;
    int dims = mat[MAT_NDIMS];
    int *nmat;
    YAP_Term tnum = YAP_ARG3;

    if (YAP_IsIntTerm(tnum)) {
      long int num;
      long int *ndata;
      
      num = YAP_IntOfTerm(tnum);
      tf = new_int_matrix(dims,mat+(MAT_DIMS),NULL);
      if (tf == YAP_TermNil())
	return FALSE;
      nmat = (int *)YAP_BlobOfTerm(tf);
      data = matrix_long_data(mat, dims);
      ndata = matrix_long_data(nmat, dims);
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
      tf = new_float_matrix(dims,mat+(MAT_DIMS),NULL);
      if (tf == YAP_TermNil())
	return FALSE;
      nmat = (int *)YAP_BlobOfTerm(tf);
      data = matrix_long_data(mat, dims);
      ndata = matrix_double_data(nmat, dims);
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
      num = (double)YAP_IntOfTerm(tnum);
    }
    tf = new_float_matrix(dims,mat+(MAT_DIMS),NULL);
    if (tf == YAP_TermNil())
      return FALSE;
    nmat = (int *)YAP_BlobOfTerm(tf);
    data = matrix_double_data(mat, dims);
    ndata = matrix_double_data(nmat, dims);
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
    } else if (op == MAT_DIV) {
      int i;
      
      for (i = 0; i < mat[MAT_SIZE]; i++) {
	ndata[i] = data[i] / num;
      }
    } else
      return FALSE;
  }
  return YAP_Unify(YAP_ARG4,tf);
}

void PROTO(init_matrix, (void));

void
init_matrix(void)
{
  YAP_UserCPredicate("new_ints_matrix", new_ints_matrix, 4);
  YAP_UserCPredicate("new_ints_matrix_set", new_ints_matrix_set, 4);
  YAP_UserCPredicate("new_floats_matrix", new_floats_matrix, 4);
  YAP_UserCPredicate("new_floats_matrix_set", new_floats_matrix_set, 4);
  YAP_UserCPredicate("matrix_set", matrix_set, 3);
  YAP_UserCPredicate("matrix_set_all", matrix_set_all, 2);
  YAP_UserCPredicate("matrix_add", matrix_add, 3);
  YAP_UserCPredicate("matrix_get", do_matrix_access, 3);
  YAP_UserCPredicate("matrix_inc", do_matrix_inc, 2);
  YAP_UserCPredicate("matrix_dec", do_matrix_dec, 2);
  YAP_UserCPredicate("matrix_inc", do_matrix_inc2, 3);
  YAP_UserCPredicate("matrix_dec", do_matrix_dec2, 3);
  YAP_UserCPredicate("matrix_to_list", matrix_to_list, 2);
  YAP_UserCPredicate("matrix_dims", matrix_dims, 2);
  YAP_UserCPredicate("matrix_ndims", matrix_ndims, 2);
  YAP_UserCPredicate("matrix_size", matrix_size, 2);
  YAP_UserCPredicate("matrix_type_as_number", matrix_type, 2);
  YAP_UserCPredicate("matrix_arg_to_offset", matrix_arg_to_offset, 3);
  YAP_UserCPredicate("matrix_offset_to_arg", matrix_offset_to_arg, 3);
  YAP_UserCPredicate("matrix_max", matrix_max, 2);
  YAP_UserCPredicate("matrix_maxarg", matrix_maxarg, 2);
  YAP_UserCPredicate("matrix_min", matrix_min, 2);
  YAP_UserCPredicate("matrix_minarg", matrix_minarg, 2);
  YAP_UserCPredicate("matrix_sum", matrix_sum, 2);
  YAP_UserCPredicate("matrix_sum_to_all", matrix_sum, 2);
  YAP_UserCPredicate("do_matrix_op", matrix_op, 4);
  YAP_UserCPredicate("do_matrix_agg_lines", matrix_agg_lines, 3);
  YAP_UserCPredicate("do_matrix_agg_cols", matrix_agg_cols, 3);
  YAP_UserCPredicate("do_matrix_op_to_all", matrix_op_to_all, 4);
  YAP_UserCPredicate("do_matrix_op_to_lines", matrix_op_to_lines, 4);
  YAP_UserCPredicate("do_matrix_op_to_cols", matrix_op_to_cols, 4);
}

#ifdef _WIN32

int WINAPI PROTO(win_matrixs, (HANDLE, DWORD, LPVOID));

int WINAPI win_matrixs(HANDLE hinst, DWORD reason, LPVOID reserved)
{
  switch (reason) 
    {
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
