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
* File:		random.c						 *
* Last rev:								 *
* mods:									 *
* comments:	regular expression interpreter                           *
*									 *
*************************************************************************/

#include "config.h"
#include "YapInterface.h"
#include <math.h>
#include <string.h>
#if defined(__MINGW32__) || _MSC_VER
#include <windows.h>
#endif

#include <mex.h>
#include <engine.h>
#include <matrix.h>

#define MAT_ACCESS(I,J,ROWS,COLS) ((I)+((J)*(ROWS)))

#define BUFSIZE 512
#define OBUFSIZE 2048

void init_matlab(void);

static YAP_Functor MatlabAddress;
static Engine *Meng = NULL;

static mxArray *
matlab_getvar(YAP_Term t)
{
  return engGetVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(t)));
}

static YAP_Term
address2term(mxArray *mat)
{
  YAP_Term t[1];

  t[0] = YAP_MkIntTerm((YAP_Int)mat);
  return YAP_MkApplTerm(MatlabAddress,1,t);
}

static int
cp_back(YAP_Term vart, mxArray *mat)
{
  if (!YAP_IsAtomTerm(vart)) {
    return TRUE;
  }
  /* save back to matlab */
  return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(vart)), mat);
}

static int
p_startmatlab(void)
{
  char opts[BUFSIZE];
  const char *ptr;
  YAP_Term topts = YAP_ARG1;

  if (Meng)
    return TRUE;
  if (YAP_IsAtomTerm(topts))
    ptr = YAP_AtomName(YAP_AtomOfTerm(topts));
  else
    {
      if (!YAP_StringToBuffer(topts,opts, BUFSIZE))
	return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(-1));
      ptr = opts;
    }
  if (!strlen(ptr)) {
    if (!(Meng = engOpen("\0"))) {
      return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(-2));
    }
  } else {
    if (!(Meng = engOpen(ptr))) {
      return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(-2));
    }
  }
  engOutputBuffer(Meng, NULL, 0);
  return TRUE;
}

static int
p_matlabon(void)
{
  return Meng != NULL;
}

static int
p_closematlab(void)
{
  Engine *eng = Meng;

  Meng = NULL;
  if (Meng)
    return engClose(eng);
  else
    return FALSE;
}

static int
p_evalstring2(void)
{
  char com[BUFSIZE];
  YAP_Term tcom = YAP_ARG1;
  const char *comd;

  if (YAP_IsAtomTerm(tcom))
    comd = YAP_AtomName(YAP_AtomOfTerm(tcom));
  else {
    if (!YAP_StringToBuffer(tcom, com, BUFSIZE))
      return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(-1));
    comd = com;
  }
  return !engEvalString(Meng, comd);
}

static int
p_evalstring3(void)
{
  int out;
  YAP_Term tcom = YAP_ARG1;
  const char *comd;
  char com[BUFSIZE];
  char buf[OBUFSIZE];

  buf[0] = '\0';
  if (YAP_IsAtomTerm(tcom))
    comd = YAP_AtomName(YAP_AtomOfTerm(tcom));
  else {
    if (!YAP_StringToBuffer(tcom, com, BUFSIZE))
      return YAP_Unify(YAP_ARG2, YAP_MkIntTerm(-1));
    comd = com;
  }
  engOutputBuffer(Meng, buf, OBUFSIZE);
  out = !engEvalString(Meng, comd);
  engOutputBuffer(Meng, NULL, 0);
  return YAP_Unify(YAP_ARG2, YAP_BufferToString(buf));
}

static int
p_create_cell_vector(void)
{
  mwSize dims[1];
  mxArray *mat;

  dims[0] = YAP_IntOfTerm(YAP_ARG1);
  if (!(mat = mxCreateCellArray(1, dims)))
    return FALSE;
  if (YAP_IsAtomTerm(YAP_ARG2)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG2)), mat);
  }
  return YAP_Unify(YAP_ARG2,address2term(mat));
}

static int
p_create_cell_array(void)
{
  int rows, cols;
  mxArray *mat;

  rows = YAP_IntOfTerm(YAP_ARG1);
  cols = YAP_IntOfTerm(YAP_ARG2);
  if (!(mat = mxCreateCellMatrix(rows, cols)))
    return FALSE;
  if (YAP_IsAtomTerm(YAP_ARG3)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3)), mat);
  }
  return YAP_Unify(YAP_ARG3,address2term(mat));
}

static int
p_create_double_vector(void)
{
  mwSize dims[1];
  mxArray *mat;

  dims[0] = YAP_IntOfTerm(YAP_ARG1);
  if (!(mat = mxCreateNumericArray(1, dims, mxDOUBLE_CLASS, mxREAL)))
    return FALSE;
  if (YAP_IsAtomTerm(YAP_ARG2)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG2)), mat);
  }
  return YAP_Unify(YAP_ARG2,address2term(mat));
}

static int
p_create_double_array(void)
{
  int rows, cols;
  mxArray *mat;

  rows = YAP_IntOfTerm(YAP_ARG1);
  cols = YAP_IntOfTerm(YAP_ARG2);
  if (!(mat = mxCreateDoubleMatrix(rows, cols, mxREAL)))
    return FALSE;
  if (YAP_IsAtomTerm(YAP_ARG3)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3)), mat);
  }
  return YAP_Unify(YAP_ARG3,address2term(mat));
}

static int
p_create_double_array3(void)
{
  mwSize dims[3];
  mxArray *mat;

  dims[0] = YAP_IntOfTerm(YAP_ARG1);
  dims[1] = YAP_IntOfTerm(YAP_ARG2);
  dims[2] = YAP_IntOfTerm(YAP_ARG3);
  if (!(mat = mxCreateNumericArray(3, dims, mxDOUBLE_CLASS, mxREAL)))
    return FALSE;
  if (YAP_IsAtomTerm(YAP_ARG3)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG4)), mat);
  }
  return YAP_Unify(YAP_ARG3,address2term(mat));
}

static int
p_set_int_array(void)
{
  int rows, cols, i = 0, j = 0;
  YAP_Int *input;
  mxArray *mat;
  YAP_Term tl = YAP_ARG4;

  mat = matlab_getvar(YAP_ARG1);
  rows = YAP_IntOfTerm(YAP_ARG2);
  cols = YAP_IntOfTerm(YAP_ARG3);
  input = (YAP_Int *)mxGetPr(mat);
  /* copy ints to matrix. */
  for (i = 0; i < rows*cols; i++) {
    YAP_Term th;

    if (!YAP_IsPairTerm(tl)) {
      return FALSE;
    }
    th = YAP_HeadOfTerm(tl);
    if (!YAP_IsIntTerm(th)) {
      /* ERROR */
      return FALSE;
    }
    input[MAT_ACCESS(i++,j,rows,cols)] = YAP_IntOfTerm(th);
    if (i == rows) {
      i = 0;
      j++;
    }
    tl = YAP_TailOfTerm(tl);
  }
  if (YAP_IsAtomTerm(YAP_ARG4)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG4)), mat);
  }
  return YAP_Unify(YAP_ARG4,address2term(mat));
}

static int
p_set_float_array(void)
{
  int rows, cols, i = 0, j = 0;
  double *input;
  mxArray *mat;
  YAP_Term tl = YAP_ARG3;

  rows = YAP_IntOfTerm(YAP_ARG1);
  cols = YAP_IntOfTerm(YAP_ARG2);
  if (!(mat = mxCreateDoubleMatrix(rows, cols, mxREAL)))
    return FALSE;
  input = mxGetPr(mat);
  /* copy ints to matrix. */
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      YAP_Term th;

      if (!YAP_IsPairTerm(tl)) {
	return FALSE;
      }
      th = YAP_HeadOfTerm(tl);
      if (YAP_IsIntTerm(th)) {
	input[MAT_ACCESS(i,j,rows,cols)] = YAP_IntOfTerm(th);
      } else if (YAP_IsFloatTerm(th)) {
	input[MAT_ACCESS(i,j,rows,cols)] = YAP_FloatOfTerm(th);
      } else {
	/* ERROR */
	return FALSE;
      }
      tl = YAP_TailOfTerm(tl);
    }
  }
  if (YAP_IsAtomTerm(YAP_ARG4)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG4)), mat);
  }
  return YAP_Unify(YAP_ARG4,address2term(mat));
}

static int
p_set_float_vector(void)
{
  mwSize len[1];
  int i = 0;
  double *input;
  mxArray *mat;
  YAP_Term tl = YAP_ARG2;

  len[0] = YAP_IntOfTerm(YAP_ARG1);
  if (!(mat = mxCreateNumericArray(1,len, mxDOUBLE_CLASS, mxREAL)))
    return FALSE;
  input = mxGetPr(mat);
  /* copy ints to matrix. */
  for (i = 0; i < len[0]; i++) {
    YAP_Term th;

    if (!YAP_IsPairTerm(tl)) {
      return FALSE;
    }
    th = YAP_HeadOfTerm(tl);
    if (YAP_IsIntTerm(th)) {
      input[i] = YAP_IntOfTerm(th);
    } else if (YAP_IsFloatTerm(th)) {
      input[i] = YAP_FloatOfTerm(th);
    } else {
      /* ERROR */
      return FALSE;
    }
    tl = YAP_TailOfTerm(tl);
  }
  if (YAP_IsAtomTerm(YAP_ARG3)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG3)), mat);
  }
  return YAP_Unify(YAP_ARG3,address2term(mat));
}

static int
p_set_int(void)
{
  int rows, cols, i, j;
  YAP_Int *input;
  mxArray *mat;

  mat = matlab_getvar(YAP_ARG1);
  i =  YAP_IntOfTerm(YAP_ARG2);
  j =  YAP_IntOfTerm(YAP_ARG3);
  input = (YAP_Int *)mxGetPr(mat);
  rows = mxGetM(mat);
  cols = mxGetN(mat);
  input[MAT_ACCESS(i-1,j-1,rows,cols)] = YAP_IntOfTerm(YAP_ARG4);
  return TRUE;
}

static int
p_set_float(void)
{
  int rows, cols, i, j;
  double *input;
  mxArray *mat;
  YAP_Term t = YAP_ARG4;

  mat = matlab_getvar(YAP_ARG1);
  i =  YAP_IntOfTerm(YAP_ARG2);
  j =  YAP_IntOfTerm(YAP_ARG3);
  input = mxGetPr(mat);
  rows = mxGetM(mat);
  cols = mxGetN(mat);
  if (YAP_IsIntTerm(t))
    input[MAT_ACCESS(i-1,j-1,rows,cols)] = YAP_IntOfTerm(t);
  else
    input[MAT_ACCESS(i-1,j-1,rows,cols)] = YAP_FloatOfTerm(t);
  return TRUE;
}

/* this has to be done carefully because we all need to transpose the matrix */
static YAP_Term
cp_ints32(int ndims, int *dims, INT32_T *input, int factor, int base, YAP_Term t)
{
  int i;

  if (ndims == 1) 
    for (i=dims[0]; i>0; i--) {
      t = YAP_MkPairTerm(YAP_MkIntTerm(input[base+factor*(i-1)]),t);
    }
  else 
    for (i=dims[0]; i>0; i--) {
      t = cp_ints32(ndims-1, dims+1, input, factor*dims[0], base+factor*(i-1),t);
    }
  return t;
}

static YAP_Term
cp_ints64(int ndims, int *dims, INT64_T *input, int factor, int base, YAP_Term t)
{
  int i;

  if (ndims == 1) 
    for (i=dims[0]; i>0; i--) {
      t = YAP_MkPairTerm(YAP_MkIntTerm(input[base+factor*(i-1)]),t);
    }
  else 
    for (i=dims[0]; i>0; i--) {
      t = cp_ints64(ndims-1, dims+1, input, factor*dims[0], base+factor*(i-1),t);
    }
  return t;
}

static YAP_Term
cp_cells(int ndims, int *dims, mxArray *mat, int factor, int base, YAP_Term t)
{
  int i;

  if (ndims == 1) 
    for (i=dims[0]; i>0; i--) {
      t = YAP_MkPairTerm(YAP_MkIntTerm((YAP_Int)mxGetCell(mat,base+factor*(i-1))),t);
    }
  else 
    for (i=dims[0]; i>0; i--) {
      t = cp_cells(ndims-1, dims+1, mat, factor*dims[0], base+factor*(i-1),t);
    }
  return t;
}

/* this has to be done carefully because we all need to transpose the matrix */
static YAP_Term
cp_floats(int ndims, int *dims, double *input, int factor, int base, YAP_Term t)
{
  int i;

  if (ndims == 1) 
    for (i=dims[0]; i>0; i--) {
      t = YAP_MkPairTerm(YAP_MkFloatTerm(input[base+factor*(i-1)]),t);
    }
  else 
    for (i=dims[0]; i>0; i--) {
      t = cp_floats(ndims-1, dims+1, input, factor*dims[0], base+factor*(i-1),t);
    }
  return t;
}


static mxArray*
get_array(YAP_Term ti)
{
  if (YAP_IsIntTerm(ti)) {
    return mxCreateDoubleScalar(YAP_IntOfTerm(ti));
  } else if (YAP_IsFloatTerm(ti)) {
    return mxCreateDoubleScalar(YAP_FloatOfTerm(ti));
  } else if (YAP_IsAtomTerm(ti)) {
    return matlab_getvar(ti);    
  } else if (YAP_IsPairTerm(ti)) {
    YAP_Term tv = YAP_HeadOfTerm(ti);
    YAP_Term tf = YAP_TailOfTerm(ti);
    const mxArray *mout;

    if (!YAP_IsAtomTerm(tv)) {
      char s[BUFSIZE];
      if (!YAP_StringToBuffer(ti, s, BUFSIZE))
	return FALSE;
      return mxCreateString(s);
    }
    mout = matlab_getvar(tv);
    if (!mout)
      return FALSE;
    if (YAP_IsIntTerm(tf)) {
      return mxGetFieldByNumber(mout, 0, YAP_IntOfTerm(tf));
    } else if (YAP_IsAtomTerm(tf))  {
      const char *s=YAP_AtomName(YAP_AtomOfTerm(tf));
      return mxGetField(mout, 0, s);
    } else {
      return NULL;
    }
  } else {
    return (mxArray *)YAP_IntOfTerm(YAP_ArgOfTerm(1,ti));
  }
}

static int
p_get_variable(void)
{
  YAP_Term t;
  mxArray *mat;
  const mwSize *dims;
  int ndims;

  mat = get_array(YAP_ARG1);
  if (!mat)
    return FALSE;
  dims = mxGetDimensions(mat);
  ndims = mxGetNumberOfDimensions(mat);
  if (mxIsInt32(mat)) {
    INT32_T *input = (INT32_T *)mxGetPr(mat);
    t = cp_ints32(ndims, (int *)dims, input, 1, 0, YAP_TermNil());
  } else if (mxIsInt64(mat)) {
    INT64_T *input = (INT64_T *)mxGetPr(mat);
    t = cp_ints64(ndims, (int *)dims, input, 1, 0, YAP_TermNil());
  } else if (mxIsInt32(mat) || mxIsInt64(mat) || mxIsCell(mat)) {
    t = cp_cells(ndims, (int *)dims, mat, 1, 0, YAP_TermNil());
  } else if (mxIsDouble(mat)) {
    double *input = mxGetPr(mat);
    t = cp_floats(ndims, (int *)dims, input, 1, 0, YAP_TermNil());
  } else {
    return FALSE;
  }
  return YAP_Unify(YAP_ARG2, t);
}


static int
item1(YAP_Term tvar, YAP_Term titem, int off)
{
  mxArray *mat;
  mat = get_array(tvar);
  if (!mat)
    return FALSE;
  if (mxIsInt32(mat)) {
    INT32_T *input = (INT32_T *)mxGetPr(mat);
    if (YAP_IsIntTerm(titem)) {
      input[off] = YAP_IntOfTerm(titem);
    } else if (YAP_IsFloatTerm(titem)) {
      input[off] = YAP_FloatOfTerm(titem);
    } else if (YAP_IsVarTerm(titem)) {
      return YAP_Unify(titem, YAP_MkIntTerm(input[off]));
    } else
      return FALSE;
  } else if (mxIsInt64(mat)) {
    INT64_T *input = (INT64_T *)mxGetPr(mat);
    if (YAP_IsIntTerm(titem)) {
      input[off] = YAP_IntOfTerm(titem);
    } else if (YAP_IsFloatTerm(titem)) {
      input[off] = YAP_FloatOfTerm(titem);
    } else if (YAP_IsVarTerm(titem)) {
      return YAP_Unify(titem, YAP_MkIntTerm(input[off]));
    } else
      return FALSE;
  } else if (mxIsCell(mat)) {
    if (YAP_IsVarTerm(titem)) {
      return YAP_Unify(titem, YAP_MkIntTerm((YAP_Int)mxGetCell(mat,off)));
    } else {
      mxArray *mat2 = get_array(titem);
      mxSetCell(mat,off, mat2);
    }
  } else if (mxIsDouble(mat)) {
    double *input = mxGetPr(mat);
    if (YAP_IsFloatTerm(titem)) {
      input[off] = YAP_FloatOfTerm(titem);
    } else if (YAP_IsIntTerm(titem)) {
      input[off] = YAP_IntOfTerm(titem);
    } else {
      return YAP_Unify(titem, YAP_MkFloatTerm(input[off]));
    }
  } else
    return FALSE;
  return cp_back(tvar, mat);
}

static int
p_item(void)
{
  YAP_Term titem;
  int off = YAP_IntOfTerm(YAP_ARG2);

  titem = YAP_ARG3;
  return item1(YAP_ARG1,titem,off);
}

static int
p_item_1(void)
{
  YAP_Term titem;
  int off = YAP_IntOfTerm(YAP_ARG2)-1;

  titem = YAP_ARG3;
  return item1(YAP_ARG1,titem,off);
}


static int
item2(YAP_Term tvar, YAP_Term titem, int offx, int offy)
{
  mxArray *mat;
  int rows;
  int cols;
  int off;

  mat = get_array(tvar);
  rows = mxGetM(mat);
  cols = mxGetN(mat);
  off = MAT_ACCESS(offx,offy,rows,cols);
  if (!mat)
    return FALSE;
  if (mxIsInt32(mat)) {
    INT32_T *input = (INT32_T *)mxGetPr(mat);
    if (YAP_IsIntTerm(titem)) {
      input[off] = YAP_IntOfTerm(titem);
    } else if (YAP_IsFloatTerm(titem)) {
      input[off] = YAP_FloatOfTerm(titem);
    } else if (YAP_IsVarTerm(titem)) {
      return YAP_Unify(titem, YAP_MkIntTerm(input[off]));
    } else
      return FALSE;
  } else if (mxIsInt64(mat)) {
    INT64_T *input = (INT64_T *)mxGetPr(mat);
    if (YAP_IsIntTerm(titem)) {
      input[off] = YAP_IntOfTerm(titem);
    } else if (YAP_IsFloatTerm(titem)) {
      input[off] = YAP_FloatOfTerm(titem);
    } else if (YAP_IsVarTerm(titem)) {
      return YAP_Unify(titem, YAP_MkIntTerm(input[off]));
    } else
      return FALSE;
  } else if (mxIsCell(mat)) {
    if (YAP_IsVarTerm(titem)) {
      return YAP_Unify(titem, YAP_MkIntTerm((YAP_Int)mxGetCell(mat,off)));
    } else {
      mxArray *mat2 = get_array(titem);
      mxSetCell(mat,off, mat2);
    }
  } else if (mxIsDouble(mat)) {
    double *input = mxGetPr(mat);
    if (YAP_IsFloatTerm(titem)) {
      input[off] = YAP_FloatOfTerm(titem);
    } else if (YAP_IsIntTerm(titem)) {
      input[off] = YAP_IntOfTerm(titem);
    } else {
      return YAP_Unify(titem, YAP_MkFloatTerm(input[off]));
    }
  } else
    return FALSE;
  return cp_back(tvar, mat);
}

static int
p_item2(void)
{
  YAP_Term titem;
  int x = YAP_IntOfTerm(YAP_ARG2);
  int y = YAP_IntOfTerm(YAP_ARG3);

  titem = YAP_ARG4;
  return item2(YAP_ARG1,titem,x,y);
}

static int
p_item2_1(void)
{
  YAP_Term titem;
  int offx = YAP_IntOfTerm(YAP_ARG2)-1;
  int offy = YAP_IntOfTerm(YAP_ARG3)-1;

  titem = YAP_ARG4;
  return item2(YAP_ARG1,titem,offx,offy);
}

static int
p_call_matlab(void)
{
  YAP_Term tlength = YAP_ARG2,
    tl = YAP_ARG3,
    tname = YAP_ARG1,
    tolength = YAP_ARG4,
    tout = YAP_ARG5;
  int i = 0;
  mxArray *inps[50], *outs[50];
  const char *name;
  int olength = YAP_IntOfTerm(tolength);

  if (!YAP_IsAtomTerm(tname))
    return FALSE;
  name = YAP_AtomName(YAP_AtomOfTerm(tname));
  if (!YAP_IsIntTerm(tlength))
    return FALSE;
  while (YAP_IsPairTerm(tl)) {
    inps[i] = get_array(YAP_HeadOfTerm(tl));
    i++;
    tl = YAP_TailOfTerm(tl);
  }
  if (mexCallMATLAB(olength, outs, i, inps, name))
    return FALSE;
  /* output arguments */
  if (YAP_IsPairTerm(tout)) {
    for (i=0; i<olength; i++) {
      YAP_Term ti = YAP_HeadOfTerm(tout);
      if (YAP_IsAtomTerm(ti)) {
	return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(ti)), outs[i]);
      } else {
	return YAP_Unify(ti,address2term(outs[i]));
      }
    }    
  } else {
    YAP_Term to = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
    for (i=olength; i>0; i--) {
      to = YAP_MkPairTerm(address2term(outs[i-1]),to);
    }    
  }
  return TRUE;
}

static int
p_create_cell_matrix_and_copy1(void)
{
  int rows, cols;
  mxArray *mat;
  YAP_Term tl = YAP_ARG3;

  rows = YAP_IntOfTerm(YAP_ARG1);
  cols = YAP_IntOfTerm(YAP_ARG2);
  if (!(mat = mxCreateCellMatrix(rows, cols)))
    return FALSE;
    while (YAP_IsPairTerm(tl)) {
    YAP_Term th = YAP_HeadOfTerm(tl);
    int off = MAT_ACCESS(YAP_IntOfTerm(YAP_ArgOfTerm(1,th))-1,
			 YAP_IntOfTerm(YAP_ArgOfTerm(2,th))-1,
			 rows,cols);
    mxArray *mat2 = get_array(YAP_ArgOfTerm(3,th));
    mxSetCell(mat,off, mat2);
    tl = YAP_TailOfTerm(tl);
  }
  if (YAP_IsAtomTerm(YAP_ARG4)) {
    return !engPutVariable(Meng, YAP_AtomName(YAP_AtomOfTerm(YAP_ARG4)), mat);
  }
  return YAP_Unify(YAP_ARG4,address2term(mat));
}

void
init_matlab(void)
{
  MatlabAddress = YAP_MkFunctor(YAP_LookupAtom("MATLAB"),1);
  YAP_UserCPredicate("start_matlab", p_startmatlab, 1);
  YAP_UserCPredicate("close_matlab", p_closematlab, 0);
  YAP_UserCPredicate("matlab_on", p_matlabon, 0);
  YAP_UserCPredicate("matlab_eval_string", p_evalstring2, 1);
  YAP_UserCPredicate("matlab_eval_string", p_evalstring3, 2);
  YAP_UserCPredicate("matlab_cells", p_create_cell_vector, 2);
  YAP_UserCPredicate("matlab_cells", p_create_cell_array, 3);
  YAP_UserCPredicate("matlab_initialized_cells", p_create_cell_matrix_and_copy1, 4);
  YAP_UserCPredicate("matlab_zeros", p_create_double_vector, 2);
  YAP_UserCPredicate("matlab_zeros", p_create_double_array, 3);
  YAP_UserCPredicate("matlab_zeros", p_create_double_array3, 4);
  YAP_UserCPredicate("matlab_int_array", p_set_int_array, 4);
  YAP_UserCPredicate("matlab_vector", p_set_float_vector, 3);
  YAP_UserCPredicate("matlab_matrix", p_set_float_array, 4);
  YAP_UserCPredicate("matlab_set_int", p_set_int, 4);
  YAP_UserCPredicate("matlab_set", p_set_float, 4);
  YAP_UserCPredicate("matlab_get_variable", p_get_variable, 2);
  YAP_UserCPredicate("matlab_item", p_item, 3);
  YAP_UserCPredicate("matlab_item", p_item2, 4);
  YAP_UserCPredicate("matlab_item1", p_item_1, 3);
  YAP_UserCPredicate("matlab_item1", p_item2_1, 4);
  YAP_UserCPredicate("matlab_call_matlab", p_call_matlab, 5);
}

#ifdef _WIN32

int WINAPI win_matlab(HANDLE, DWORD, LPVOID);

int WINAPI win_matlab(HANDLE hinst, DWORD reason, LPVOID reserved)
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

