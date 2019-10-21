/**
 * @file   real.c
 * @date   Sat May 19 13:44:04 2018
 * 
 * @brief  Prolog  to R interface
 * 
 * 
 */

/**
 * @defgroup realI Interface Prolog to R
 * @ brief How to call R from YAP
 * @ingroup realm
 * @{
 */
#define CSTACK_DEFNS
#include "Rconfig.h"

#include <SWI-Prolog.h>
#undef ERROR
#if HAVE_R_EMBEDDED_H
#include <Rembedded.h>
#endif
#if HAVE_R_INTERFACE_H
#include <Rinterface.h>
#define R_SIGNAL_HANDLERS 1
#endif
#include <R.h>

#include <Rdefines.h>
#include <assert.h>
#include <string.h>
#include <R_ext/Parse.h>

#include "real.h"

static atom_t ATOM_break;
static atom_t ATOM_false;
static atom_t ATOM_function;
static atom_t ATOM_i;
static atom_t ATOM_next;
static atom_t ATOM_true;

static functor_t FUNCTOR_at2;
static functor_t FUNCTOR_boolop1;
static functor_t FUNCTOR_brackets1;
static functor_t FUNCTOR_dollar1;
static functor_t FUNCTOR_dollar2;
static functor_t FUNCTOR_dot1;
static functor_t FUNCTOR_equal2;
static functor_t FUNCTOR_hat2;
static functor_t FUNCTOR_i1;
static functor_t FUNCTOR_if2;
static functor_t FUNCTOR_iff2;
static functor_t FUNCTOR_iff3;
static functor_t FUNCTOR_in2;
static functor_t FUNCTOR_inner2;
static functor_t FUNCTOR_for3;
static functor_t FUNCTOR_minus1;
static functor_t FUNCTOR_minus2;
static functor_t FUNCTOR_outer2;
static functor_t FUNCTOR_plus1;
static functor_t FUNCTOR_plus2;
static functor_t FUNCTOR_quote1;
static functor_t FUNCTOR_repeat1;
static functor_t FUNCTOR_square_brackets2;
static functor_t FUNCTOR_tilde1;
static functor_t FUNCTOR_tilde2;
static functor_t FUNCTOR_while2;

X_API install_t install_real(void);

static bool REAL_Error__(int line, const char *function, const char *s,
                         term_t t) {
  term_t except = PL_new_term_ref();

  PL_unify_term(except, PL_FUNCTOR_CHARS, "real_error", 2, PL_CHARS, s, PL_TERM,
                t, PL_CHARS, function, PL_INT, line);

  return PL_raise_exception(except);
}

#define Sdprintf(S, A1) fprintf(stderr, S, A1)

static size_t pos_dims(size_t R_index[], size_t ndims, size_t dims[]) {
  int i, index = 0;
  for (i = ndims - 1; i >= 0; i--) {
    index = index * dims[i] + R_index[i] - 1;
  }
  return index;
}

static void inc_dims(size_t R_index[], size_t ndims, size_t dims[]) {
  int i;
  for (i = ndims - 1; i >= 0; i--) {
    if (++R_index[i] <= dims[i])
      return;
    R_index[i] = 1;
  }
}

static size_t sexp_rank(SEXP sexp) {
  /* Return the number of dimensions for the buffer
   * (e.g., a vector will return 1, a matrix 2, ...)
   */
  /* Copied from rpy2 */
  SEXP dim = getAttrib(sexp, R_DimSymbol);
  if (dim == R_NilValue)
    return 1;
  return GET_LENGTH(dim);
}

/* Copied, with slight mods from rpy2 */
static int sexp_shape(SEXP sexp, size_t nd, size_t *shape) {
  /* Set 'shape', containing the size of each dimension (see sexp_rank).  */
  int i;
  SEXP dim = getAttrib(sexp, R_DimSymbol);
  if (dim == R_NilValue)
    shape[0] = LENGTH(sexp);
  else
    for (i = 0; i < nd; i++) {
      shape[i] = INTEGER(dim)[i];
    }
  return TRUE;
}

/* get the list element named str, or return NULL */

static SEXP getListElement(SEXP list, const char *str) {
  SEXP elmt = R_NilValue, names;
  int i;
  if (list == R_NilValue)
    return R_NilValue;
  names = getAttrib(list, R_NamesSymbol);
  for (i = 0; i < length(list); i++)
    if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  return elmt;
}

static int setListElement(term_t t, SEXP s_str, SEXP sexp) {
  int i, hadError, nprotect = 0;
  size_t shape;
  SEXP names, name_R, call_R, p, list;
  const char *str;

  if (TYPEOF(s_str) == SYMSXP) {
    s_str = PRINTNAME(s_str);
  }
  if (TYPEOF(s_str) == STRSXP) {
    if (sexp_rank(s_str) > 1) {
      Ureturn FALSE;
    }
    sexp_shape(s_str, 1, &shape);
    if (shape != 1) {
      Ureturn FALSE;
    }
    str = CHAR(CHARACTER_DATA(s_str)[0]);
  } else {
    Ureturn FALSE;
  }
  PROTECT_AND_COUNT(list = term_to_sexp(t, TRUE));
  if (list == R_NilValue) {
    Ureturn FALSE;
  }
  names = getAttrib(list, R_NamesSymbol);
  for (i = 0; i < length(list); i++) {
    if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      SET_ELEMENT(list, i, sexp);
      Ureturn TRUE;
    }
  }
  // new attribute,
  // we need to work with the identifier
  PROTECT_AND_COUNT(list = term_to_sexp(t, FALSE));
  PROTECT_AND_COUNT(name_R = allocVector(STRSXP, 1));
  SET_STRING_ELT(name_R, 0, mkCharCE(str, CE_UTF8));
  PROTECT_AND_COUNT(call_R = lang3(R_DollarSymbol, list, name_R));
  p = lang3(install("<-"), call_R, sexp);
  (void)protected_tryEval(p, R_GlobalEnv, &hadError);
  Ureturn TRUE;
}

static int complex_term(term_t head, double *valxP, double *valyP) {
  term_t val1 = PL_new_term_ref();
  atom_t name;
  size_t arity;

  if (PL_is_functor(head, FUNCTOR_plus2) && PL_get_arg(2, head, val1) &&
      ((PL_is_functor(val1, FUNCTOR_i1) && PL_get_arg(1, val1, val1) &&
        PL_get_float(val1, valyP)) ||
       (PL_get_name_arity(val1, &name, &arity) && name == ATOM_i &&
        arity == 0 && (*valyP = 1, TRUE))) &&
      PL_get_arg(1, head, head) && PL_get_float(head, valxP))
    return TRUE;
  if (PL_is_functor(head, FUNCTOR_minus2) && PL_get_arg(2, head, val1) &&
      ((PL_is_functor(val1, FUNCTOR_i1) && PL_get_arg(1, val1, val1) &&
        PL_get_float(val1, valyP) && (*valyP = -*valyP, TRUE)) ||
       (PL_get_name_arity(val1, &name, &arity) && name == ATOM_i &&
        arity == 0 && (*valyP = -1, TRUE))) &&
      PL_get_arg(1, head, head) && PL_get_float(head, valxP))
    return 1;
  return 0;
}

static int REAL_term_type(term_t t, int context) {
  int objtype = PL_term_type(t), rc;
  term_t tmp = PL_copy_term_ref(t);
  functor_t f;

  switch (objtype) {
  case PL_VARIABLE:
    return PL_R_VARIABLE;
  case PL_INTEGER:
    return PL_R_INTEGER;
  case PL_FLOAT:
    return PL_R_FLOAT;
  case PL_STRING:
    return PL_R_CHARS;
  case PL_ATOM:
#ifdef PL_NIL
  case PL_NIL:
#endif
  {
    int got_v = 0;
    int bool_vP = 0;
    atom_t tmp_atom;

    if ((got_v = PL_get_bool(t, &bool_vP)))
      return PL_R_BOOL;

    if (!PL_get_atom(t, &tmp_atom))
      REAL_Error("type atom", t);

    if (tmp_atom == ATOM_true || tmp_atom == ATOM_false)
      return PL_R_BOOL;
    if (tmp_atom == ATOM_break)
      return PL_R_BREAK;
    if (tmp_atom == ATOM_next)
      return PL_R_NEXT;
    else if (context & PL_R_VECTOR)
      return PL_R_CHARS;
    else
      return PL_R_SYMBOL;
  } break;
  case PL_TERM:
#ifdef PL_LIST_PAIR
  case PL_LIST_PAIR:
#endif
  {
    term_t tail = PL_new_term_ref();
    size_t len;
    atom_t a;
    size_t arity;

    if (PL_LIST == PL_skip_list(t, tail, &len)) {
      if (!PL_get_list(t, tmp, t)) {
        return FALSE;
      }
      int rc = PL_R_VECTOR | REAL_term_type(tmp, context | PL_R_VECTOR);
      return rc;
    } else if (len > 0) {
      // must be a dot term
      return PL_R_DOT;
    }
    if (!PL_get_functor(t, &f))
      return FALSE;
    if ((context & PL_R_VECTOR) && f == FUNCTOR_equal2) {
      return PL_R_NAME | PL_R_VECTOR;
    }
    if (!(context & PL_R_VECTOR) && f == FUNCTOR_dollar2) {
      if (!PL_get_arg(2, t, tmp))
        return FALSE;
      return PL_R_LISTEL;
    }
    if (!(context & PL_R_VECTOR) && f == FUNCTOR_at2) {
      if (!PL_get_arg(2, t, tmp))
        return FALSE;
      return PL_R_SLOT;
    }
    if (!(context & PL_R_VECTOR) && f == FUNCTOR_square_brackets2) {
      return PL_R_SUBSET;
    }
    {
      double x, y;
      if (complex_term(t, &x, &y))
        return PL_R_COMPLEX;
    }
    if (f == FUNCTOR_tilde2) {
      return PL_R_FORMULA;
    }
    if (f == FUNCTOR_tilde1) {
      return PL_R_RFORMULA;
    }
    if (f == FUNCTOR_plus1) {
      if (!PL_get_arg(1, t, tmp))
        return FALSE;
      rc = REAL_term_type(tmp, context);
      if (rc == PL_R_CHARS || rc == PL_R_SYMBOL)
        return PL_R_PLUS;
      return PL_R_CALL;
    }
    if (f == FUNCTOR_dot1) {
      if (!PL_get_arg(1, t, tmp))
        return FALSE;
      rc = REAL_term_type(tmp, context);
      if (rc == PL_R_DOT || rc == PL_R_SYMBOL)
        return PL_R_DOT;
    }
    if (f == FUNCTOR_brackets1) {
      if (!PL_get_arg(1, t, tmp))
        return FALSE;
      return PL_R_CALL;
    }
    if (f == FUNCTOR_equal2) {
      return PL_R_EQUAL;
    }
    if (f == FUNCTOR_minus1 || f == FUNCTOR_dollar1) {
      if (!PL_get_arg(1, t, tmp))
        return FALSE;
      rc = REAL_term_type(tmp, context);
      if (rc == PL_R_CHARS || rc == PL_R_SYMBOL)
        return PL_R_PSYMBOL;
      return PL_R_CALL;
    }
    if (f == FUNCTOR_quote1)
      return PL_R_QUOTE;

    if (f == FUNCTOR_if2 && PL_get_arg(1, t, tmp) &&
        PL_get_name_arity(tmp, &a, &arity) && a == ATOM_function)
      return PL_R_DEFUN;

    if (f == FUNCTOR_iff2)
      return PL_R_IF;

    if (f == FUNCTOR_in2)
      return PL_R_IN;

    if (f == FUNCTOR_iff3)
      return PL_R_IF_ELSE;

    if (f == FUNCTOR_while2)
      return PL_R_WHILE;

    if (f == FUNCTOR_repeat1)
      return PL_R_REPEAT;

    if (f == FUNCTOR_boolop1) {

      if (!PL_get_arg(1, t, tmp))
        return REAL_Error("argument access", t);

      if (!PL_get_atom(tmp, &a))
        return REAL_Error("type atom", t);

      if (a == ATOM_true || a == ATOM_false)
        return PL_R_BOOL;
    }
    return PL_R_CALL;
  } break;
  default:
    return FALSE;
  }
}

static int merge_dots(term_t t) {
  char so[1025], *ns = so;
  int loop = TRUE, first = TRUE;
  size_t arity;
  term_t tmp = PL_new_term_ref();
  atom_t name;

  so[0] = '\0';
  while (loop) {
    if (PL_get_list(t, tmp, t))
      loop = TRUE;
    else if ((PL_is_functor(t, FUNCTOR_dot1) && PL_get_arg(1, t, tmp)) ||
             (tmp = t, TRUE))
      loop = FALSE;
    if (!first || !loop) {
      strncat(so, ".", 1024);
    }
    if (first) {
      first = FALSE;
    }

    if (PL_get_chars(tmp, &ns,
                     CVT_ATOM | CVT_STRING | BUF_DISCARDABLE | REP_UTF8)) {
      ns += strlen(ns);
      if (!loop) {
        atom_t at = PL_new_atom(so);
        return PL_put_atom(t, at);
      }
    } else if (!loop && PL_is_functor(t, FUNCTOR_brackets1) &&
               PL_get_arg(1, t, tmp) &&
               PL_get_chars(tmp, &ns, CVT_ATOM | CVT_STRING | BUF_DISCARDABLE |
                                          REP_UTF8)) {
      strncat(so, ns, 1024 - strlen(so) - 1);
      return PL_put_atom_chars(tmp, so) &&
             PL_cons_functor(t, FUNCTOR_brackets1, tmp);
    } else if (!loop && PL_get_name_arity(tmp, &name, &arity) &&
               (ns = PL_atom_chars(name))) {
      strncat(so, ns, 1024 - strlen(so) - 1);
      term_t a = PL_new_term_refs(arity);
      int i;
      for (i = 0; i < arity; i++)
        if (!PL_get_arg(i + 1, tmp, a + i))
          return FALSE;
      return PL_cons_functor_v(t, PL_new_functor(PL_new_atom(so), arity), a);
    } else
      return FALSE;
  }
  return FALSE;
}

// put t in ans[index]; and stores elements of type objtype
int term_to_S_el(term_t t, int objtype, size_t index, SEXP ans) {
  switch (objtype) {
  case PL_R_CHARS:
  case PL_R_PLUS: {
    char *s = NULL;

    if (PL_get_chars(t, &s, CVT_ATOM | CVT_STRING | CVT_LIST | BUF_DISCARDABLE |
                                REP_UTF8)) {
      CHARACTER_DATA(ans)[index] = mkCharCE(s, CE_UTF8);
      return TRUE;
    } else {
      if (PL_get_arg(1, t, t) &&
          PL_get_chars(t, &s, CVT_ATOM | CVT_STRING | CVT_LIST |
                                  BUF_DISCARDABLE | REP_UTF8)) {
        CHARACTER_DATA(ans)[index] = mkCharCE(s, CE_UTF8);
        return TRUE;
      }
    }
  }
    return FALSE;

  case PL_R_INTEGER: {
    int64_t val;

    if (PL_get_int64(t, &val)) {
      INTEGER_DATA(ans)[index] = val;
    } else {
      return FALSE;
    }
  } break;
  case PL_R_FLOAT: {
    double val;
    int64_t ival;

    if (PL_get_float(t, &val)) {
      NUMERIC_DATA(ans)[index] = val;
      return TRUE;
    } else if (PL_get_int64(t, &ival)) {
      NUMERIC_DATA(ans)[index] = ival;
      return TRUE;
    } else
      return FALSE;

  } break;
  case PL_R_ATBOOL:
  case PL_R_BOOL: {
    int val;

    if (PL_get_bool(t, &val)) {
      LOGICAL_DATA(ans)[index] = val;
      return TRUE;
    } else {
      if (PL_get_arg(1, t, t) && PL_get_bool(t, &val)) {
        LOGICAL_DATA(ans)[index] = val;
        return TRUE;
      }
      return FALSE;
    }
  } break;

  case PL_R_COMPLEX: {
    double valx, valy, val;
    int64_t ival;

    if (complex_term(t, &valx, &valy)) {
      COMPLEX_DATA(ans)[index].r = valx;
      COMPLEX_DATA(ans)[index].i = valy;
      return TRUE;
    } else if (PL_get_float(t, &val)) {
      COMPLEX_DATA(ans)[index].r = val;
      COMPLEX_DATA(ans)[index].i = 0.0;
      return TRUE;
    } else if (PL_get_int64(t, &ival)) {
      COMPLEX_DATA(ans)[index].r = ival;
      COMPLEX_DATA(ans)[index].i = 0.0;
      return TRUE;
    } else {        /* FIXME: Destroy ans */
      return FALSE; /* type error */
    }
  }

  break;

  default:
    {
      SEXP s = term_to_sexp(t, false);
      SET_ELEMENT(ans, index, s);
    }
  }
  return TRUE;
}

// put t in ans[index]; and stores elements of type objtype
static int sexp_to_S_el(SEXP sin, size_t index, SEXP ans) {
  switch (TYPEOF(ans)) {
  case STRSXP: {
    if (TYPEOF(sin) != STRSXP)
      return FALSE;
    CHARACTER_DATA(ans)[index] = CHARACTER_DATA(sin)[0];
  } break;
  case INTSXP: {
    if (TYPEOF(sin) != INTSXP)
      return FALSE;
    INTEGER_DATA(ans)[index] = INTEGER_DATA(sin)[0];
  } break;

  case REALSXP: {
    if (TYPEOF(sin) == INTSXP)
      NUMERIC_DATA(ans)[index] = INTEGER_DATA(sin)[0];
    else if (TYPEOF(sin) == REALSXP)
      NUMERIC_DATA(ans)[index] = NUMERIC_DATA(sin)[0];
    else
      return FALSE;
  } break;

  case LGLSXP: {
    if (TYPEOF(sin) == LGLSXP)
      LOGICAL_DATA(ans)[index] = LOGICAL_DATA(sin)[0];
    else
      return FALSE;
    break;
  }

  case CPLXSXP: {
    if (TYPEOF(sin) == CPLXSXP) {
      COMPLEX_DATA(ans)[index] = COMPLEX_DATA(sin)[0];
    } else if (TYPEOF(sin) == INTSXP) {
      COMPLEX_DATA(ans)[index].r = INTEGER_DATA(sin)[0];
      COMPLEX_DATA(ans)[index].i = 0;
    } else if (TYPEOF(sin) == REALSXP) {
      COMPLEX_DATA(ans)[index].r = NUMERIC_DATA(sin)[0];
      COMPLEX_DATA(ans)[index].i = 0;
    } else
      return FALSE;
  } break;

  case VECSXP:
  default:
    {
    SEXPTYPE type = TYPEOF(sin);
    switch (type) {
    case CPLXSXP:
    case INTSXP:
    case REALSXP:
      VECTOR_DATA(ans)[index] = Rf_coerceVector(sin, type);
      break;
    case VECSXP:
      VECTOR_DATA(ans)[index] = VECTOR_DATA(sin)[0];
      break;
    default:
      return FALSE;
    }
  } break;
    
  }
  return 1;
}

static int set_listEl_to_sexp(term_t t, SEXP sexp) {
  term_t tslot = PL_new_term_ref();
  SEXP s;
  int nprotect = 0;

  if (!PL_get_arg(2, t, tslot))
    return FALSE;
  if (PL_is_pair(tslot) || PL_is_functor(tslot, FUNCTOR_dot1)) {
    if (!merge_dots(tslot))
      return FALSE;
  }
  s = term_to_sexp(tslot, FALSE);
  if (!PL_get_arg(1, t, t))
    Ureturn FALSE;

  // we now have s with the slot, and tmp_R with the object. Let us roll..
  return setListElement(t, s, sexp);
}

static SEXP list_to_sexp(term_t t0, int objtype) {
  term_t 
    tmp = PL_copy_term_ref(t0),
    tail = PL_new_term_ref();
  
  term_t t = PL_copy_term_ref(t0);
  size_t dims[256];
  term_t stack[256];
  size_t R_index[256];
  size_t ndims = 0, len, spos = 0;
  int nprotect = 0, i, sobjtype = objtype;
  SEXP ans;

  // cheking the depth of the list
  tmp = PL_copy_term_ref(t);
  while (PL_is_pair(tmp)) {
    size_t len;
    if (PL_LIST != PL_skip_list(tmp, tail, &len)) {
      Ureturn R_NilValue;
    }
    if (!PL_get_list(tmp, tmp, tail)) {
      Ureturn R_NilValue;
    }
    dims[ndims] = len;
    ndims++;
  }
  for (i = 0, len = 1; i < ndims; i++) {
    len *= dims[i];
  }
    /***
   * 
   */
  if (PL_get_list(t,tmp,tail) &&
       PL_is_functor(tmp, FUNCTOR_equal2)) {
    SEXP names;
    int nprotect = 0;

    PROTECT_AND_COUNT(ans = NEW_LIST(len));
    PROTECT_AND_COUNT(names = allocVector(STRSXP, len));

    for (i = 0; i<len; i++) {
      if ( PL_get_list(t, tmp, t)&&
	   PL_is_functor(tmp, FUNCTOR_equal2)) {
        char *nm = NULL;
        SEXP sexp;

	if (PL_get_arg(1, tmp, tail) && PL_get_arg(2, tmp, tmp) &&
            (PL_is_pair(tail) || PL_is_functor(tail, FUNCTOR_dot1)) &&
            merge_dots(tail) &&
            PL_get_chars(tail, &nm,
                         CVT_ATOM | CVT_STRING | BUF_MALLOC | REP_UTF8)) {
          sexp = term_to_sexp(tmp, FALSE);
          SET_STRING_ELT(names, i, mkCharCE(nm, CE_UTF8));
          SET_ELEMENT(ans, i, sexp);
	  if (nm)
	  PL_free(nm);
        } else if ((PL_is_atom(tail) || PL_is_string(tail)) &&
                   PL_get_chars(tail, &nm, CVT_ATOM | CVT_STRING | BUF_MALLOC |
                                               REP_UTF8)) {
          sexp = term_to_sexp(tmp, FALSE);
          SET_STRING_ELT(names, i, mkCharCE(nm, CE_UTF8));
          SET_ELEMENT(ans, i, sexp);
    if (nm)
      PL_free(nm);
    }
          /* also check cases like java.parameters */
      } else { /* */
        REAL_Error("type list", tmp);
        Ureturn ans;
       }
     }
    SET_NAMES(ans, names);
    Ureturn ans;
  } 
  switch (sobjtype& ~PL_R_VECTOR) {
  case PL_R_INTEGER:
    PROTECT_AND_COUNT(ans = NEW_INTEGER(len));
    break;
  case PL_R_FLOAT:
    PROTECT_AND_COUNT(ans = NEW_NUMERIC(len));
    break;
  case PL_R_CHARS:
  case PL_R_PLUS:
    PROTECT_AND_COUNT(ans = NEW_CHARACTER(len));
    break;
  case PL_R_COMPLEX:
    PROTECT_AND_COUNT(ans = NEW_COMPLEX(len));
    break;
  case PL_R_ATBOOL:
  case PL_R_BOOL:
    PROTECT_AND_COUNT(ans = NEW_LOGICAL(len));
    break;
  default:
   PROTECT_AND_COUNT(ans = NEW_LIST(len));
  }

  // take care of dims
  SEXP sdims = NEW_INTEGER(ndims);
  for (i = 0; i < ndims; i++) {
    INTEGER_DATA(sdims)[i] = dims[i];
    R_index[i] = 1; // use R notation
  }
  setAttrib(ans, R_DimSymbol, sdims);

  stack[0] = PL_copy_term_ref(t);
  term_t l = stack[0];
  for (i = 1; i <= ndims; i++)
    stack[i] = PL_new_term_ref();
  while (TRUE) {
    if (PL_is_pair(l)) {
      PL_get_list(l, stack[spos + 1], l);
      l = stack[spos + 1];
      spos++;
    } else if (PL_is_list(l)) {
      if (spos == 0)
        break;
      l = stack[spos - 1];
      spos--;
    } else {
      if (!term_to_S_el(l, objtype & ~PL_R_VECTOR,
                        pos_dims(R_index, ndims, dims), ans)) {
        if ((objtype & PL_R_INTEGER) && PL_is_float(l)) {
          Ureturn list_to_sexp(t, PL_R_FLOAT | PL_R_VECTOR);
        }
        Ureturn R_NilValue;
      }
      inc_dims(R_index, ndims, dims);
      l = stack[spos - 1];
      spos--;
    }
  }
  Ureturn ans;
}

static int slot_to_sexp(term_t t, SEXP *ansP) {
  term_t tslot = PL_new_term_ref();
  char *s = NULL;
  SEXP tmp_R, name_R;
  int nprotect = 0;

  if (!PL_get_arg(2, t, tslot))
    return FALSE;
  if (PL_is_pair(tslot) || PL_is_functor(tslot, FUNCTOR_dot1)) {
    if (!merge_dots(tslot))
      return FALSE;
  }
  if (!PL_get_chars(tslot, &s, CVT_ATOM | BUF_MALLOC | REP_UTF8)) {
    return FALSE;
  }
  if (!PL_get_arg(1, t, t))
    return FALSE;

  PROTECT_AND_COUNT(tmp_R = term_to_sexp(t, TRUE));
  // we now have s with the slot, and tmp_R with the object. Let us roll..

  PROTECT_AND_COUNT(name_R = install(s));
  if (!R_has_slot(tmp_R, name_R)) {
    return FALSE;
  }

  *ansP = GET_SLOT(tmp_R, name_R);
  if (!*ansP)
    return FALSE;
  return TRUE;
}

static int set_slot_to_sexp(term_t t, SEXP sexp) {
  term_t tslot = PL_new_term_ref();
  char *s = NULL;
  SEXP tmp_R, name_R;
  int nprotect = 0;

  if (!PL_get_arg(2, t, tslot))
    return FALSE;
  if (PL_is_pair(tslot) || PL_is_functor(tslot, FUNCTOR_dot1)) {
    if (!merge_dots(tslot))
      return FALSE;
  }
  if (!PL_get_chars(tslot, &s, CVT_ATOM | BUF_MALLOC | REP_UTF8)) {
    return FALSE;
  }
  if (!PL_get_arg(1, t, t))
    return FALSE;

  PROTECT_AND_COUNT(tmp_R = term_to_sexp(t, TRUE));

  // we now have s with the slot, and tmp_R with the object. Let us roll..

  PROTECT_AND_COUNT(name_R = install(s));
  //  if (! R_has_slot(tmp_R, name_R)) {
  //    return FALSE;
  //}

  SET_SLOT(tmp_R, name_R, sexp);
  Ureturn TRUE;
}

static int listEl_to_sexp(term_t t, SEXP *ansP) {
  term_t tslot = PL_new_term_ref();
  char *s = NULL;
  SEXP tmp_R;
  int nprotect = 0;

  if (!PL_get_arg(2, t, tslot))
    return FALSE;
  if (PL_is_pair(tslot) || PL_is_functor(tslot, FUNCTOR_dot1)) {
    if (!merge_dots(tslot))
      return FALSE;
  }
  if (!PL_get_chars(tslot, &s, CVT_ATOM | BUF_MALLOC | REP_UTF8)) {
    return FALSE;
  }
  if (!PL_get_arg(1, t, t))
    return FALSE;

  PROTECT_AND_COUNT(tmp_R = term_to_sexp(t, TRUE));
  // we now have s with the slot, and tmp_R with the object. Let us roll..

  *ansP = getListElement(tmp_R, s);
  if (*ansP == R_NilValue)
    Ureturn FALSE;
  Ureturn TRUE;
}

static SEXP pl_to_func(term_t t, bool eval) {
  atom_t name;
  size_t arity;
  term_t a1 = PL_new_term_ref(), a;
  int i, ierror;
  SEXP c_R, call_R, res_R;
  char *sf = NULL;
  int nprotect = 0;

  if (!PL_get_name_arity(t, &name, &arity)) {
    Ureturn FALSE;
  }
  if (!(sf = PL_atom_chars(name))) {
    Ureturn FALSE;
  }
  if (!strcmp(sf, "()")) {
    if (!PL_get_arg(1, t, a1) ||
        !PL_get_chars(a1, &sf, CVT_ATOM | BUF_MALLOC | REP_UTF8)) {
      Ureturn FALSE;
    }
    arity = 0;
  }

  // first evaluate arguments left to right
  a = PL_new_term_ref(), a1 = PL_new_term_ref();
  PROTECT_AND_COUNT(call_R = allocList(arity + 1));
  c_R = call_R;
  c_R = CDR(c_R);
  for (i = 0; i < arity; i++) {
    if (!PL_get_arg(i + 1, t, a)) {
      REAL_Error("argument access", t);
      { Ureturn R_NilValue; }
    }
    if (PL_is_functor(a, FUNCTOR_equal2)) {
      char *s = NULL;
      if (!PL_get_arg(1, a, a1)) {
        Ureturn FALSE;
      }
      if (PL_is_pair(a1) || PL_is_functor(a1, FUNCTOR_dot1)) {
        if (!merge_dots(a1)) {
          Ureturn FALSE;
        }
      }
      if (!PL_get_chars(a1, &s,
                        CVT_ATOM | CVT_STRING | BUF_MALLOC | REP_UTF8)) {
        Ureturn FALSE;
      }
      if (!PL_get_arg(2, a, a)) {
        Ureturn FALSE;
      }

      SETCAR(c_R, term_to_sexp(a, FALSE));
      SET_TAG(c_R, install(s));
      PL_free(s);
    } else {
      SETCAR(c_R, term_to_sexp(a, FALSE));
    }
    c_R = CDR(c_R);
  }

  // now we can evaluate the function
  if (arity == 1) {
    SEXP mu;
    PROTECT_AND_COUNT(mu = getAttrib(CADR(call_R), install(sf)));
    if (!(mu == R_UnboundValue || mu == R_NilValue)) {
      // PL_free( sf );
      { Ureturn mu; }
    }
  }
  c_R = call_R;
  // PROTECT_AND_COUNT( fn_R = myFindFun(install(sf), R_GlobalEnv) );
  SET_TYPEOF(c_R, LANGSXP);
  SETCAR(c_R, install(sf));
  // PL_free( sf );
  if (eval) {
    PROTECT_AND_COUNT(res_R = protected_tryEval(call_R, R_GlobalEnv, &ierror));
    if (res_R == NULL)
      res_R = call_R;
    { Ureturn res_R; }
  }
  Ureturn call_R;
}

static int pl_to_body(term_t t, SEXP *ansP) {
  term_t tmp = PL_copy_term_ref(t), tail = PL_copy_term_ref(t);
  size_t i, len;
  SEXP body_R;
  int nprotect = 0;

  if (PL_LIST == PL_skip_list(tmp, tail, &len)) {
    SEXP ans, stmp;

    PROTECT_AND_COUNT(ans = stmp = allocList(len));
    for (i = 0; i < len; i++) {
      if (!PL_get_list(t, tmp, t)) {
        Ureturn FALSE;
      }
      PROTECT_AND_COUNT(body_R = term_to_sexp(t, FALSE));
      SETCAR(stmp, body_R);
      stmp = CDR(stmp);
    }
    *ansP = ans;
  } else {
    PROTECT_AND_COUNT(*ansP = term_to_sexp(t, FALSE));
    if (Rf_isNull(*ansP)) {
      Ureturn FALSE;
    }
  }
  Ureturn TRUE;
}

static int pl_to_defun(term_t t, SEXP *ansP) {
  atom_t name;
  size_t arity;
  term_t a = PL_new_term_ref(), body = PL_new_term_ref();
  int i;
  SEXP clo_R, c_R, call_R, body_R;
  int nprotect = 0;

  if (!PL_get_arg(1, t, a)) {
    Ureturn FALSE;
  }
  if (!PL_get_name_arity(a, &name, &arity)) {
    Ureturn FALSE;
  }
  if (!PL_get_arg(2, t, body)) {
    Ureturn FALSE;
  }

  PROTECT_AND_COUNT(clo_R = allocSExp(CLOSXP));
  if (!clo_R) {
    Ureturn FALSE;
  }
  PROTECT_AND_COUNT(c_R = call_R = allocList(arity));
  SET_TYPEOF(c_R, LANGSXP);
  for (i = 0; i < arity; i++) {
    SEXP tmp_R;

    if (!PL_get_arg(i + 1, a, t)) {
      Ureturn REAL_Error("argument access", t);
    }
    PROTECT_AND_COUNT(tmp_R = term_to_sexp(t, FALSE));
    if (Rf_isNull(tmp_R)) {
      Ureturn FALSE;
    }
    SETCAR(c_R, tmp_R);
    SET_TAG(c_R, CreateTag(tmp_R));
    c_R = CDR(c_R);
  }
  SET_FORMALS(clo_R, call_R);
  SET_CLOENV(clo_R, R_GlobalEnv);
  if (!pl_to_body(body, &body_R)) {
    Ureturn FALSE;
  }
  SET_BODY(clo_R, body_R);
  *ansP = clo_R;
  Ureturn TRUE;
}

static int old_list_to_sexp(term_t t, SEXP c_R, int n, bool eval) {
  int i;
  term_t a = PL_new_term_ref();
  SEXP head_R;
  int nprotect = 0;

  for (i = 0; i < n; i++) {
    if (PL_get_list(t, a, t)) {
      if (PL_is_variable(a)) {
        SETCAR(c_R, R_MissingArg);
      } else {
        PROTECT_AND_COUNT(head_R = term_to_sexp(a, eval));
        SETCAR(c_R, head_R);
      }
      c_R = CDR(c_R);
    } else {
      Ureturn FALSE;
    }
  }
  Ureturn TRUE;
}

static SEXP subset_to_sexp(term_t t, bool eval) {
  term_t a = PL_new_term_ref(), b = PL_new_term_ref();
  SEXP lhs_R, call_R, res_R, sin, c_R;
  int nprotect = 0;
  int ierror;
  size_t len;

  // get lh side
  if (!PL_get_arg(2, t, a)) {
    REAL_Error("argument access", t);
    Ureturn R_NilValue;
  }
  PROTECT_AND_COUNT(lhs_R = term_to_sexp(a, eval));
  if (Rf_isNull(lhs_R)) {
    Ureturn R_NilValue;
  }
  // get index
  if (!PL_get_arg(1, t, a)) {
    REAL_Error("argument access", t);
    Ureturn R_NilValue;
  }
  if (PL_get_list(a, t, b) && PL_is_pair(t) &&
      PL_get_nil(b)) { /* [[ operator */
    sin = R_Bracket2Symbol;
    a = t;
  } else {
    sin = R_BracketSymbol; // [ operator
  }
  if (PL_skip_list(a, b, &len) != PL_LIST) {
    Ureturn R_NilValue;
  }
  PROTECT_AND_COUNT(call_R = allocList(len + 2));
  c_R = call_R;
  SETCAR(c_R, sin);
  SET_TYPEOF(c_R, LANGSXP);
  c_R = CDR(c_R);
  SETCAR(c_R, lhs_R);
  c_R = CDR(c_R);
  if (!old_list_to_sexp(a, c_R, len, FALSE)) {
    Ureturn R_NilValue;
  }
  SEXP ans;
  if (eval) {
    PROTECT_AND_COUNT(res_R = protected_tryEval(call_R, R_GlobalEnv, &ierror));

    if (ierror) {
      Ureturn call_R;
    }
    ans = res_R;
  } else {
    ans = call_R;
  }
  Ureturn ans;
}

static int set_subset_eval(SEXP symbol, term_t a, SEXP lhs_R, SEXP sexp) {
  int hadError;
  SEXP p, call_R, index_R, c_R, sin;
  term_t f, b;
  int nprotect = 0;
  size_t len;

  f = PL_new_term_ref();
  b = PL_new_term_ref();
  if (PL_get_list(a, b, f) && PL_is_pair(b) &&
      PL_get_nil(f)) { /* [[ operator ]] */
    sin = R_Bracket2Symbol;
    a = b;
  } else {
    sin = R_BracketSymbol; // [ operator
  }
  if (PL_skip_list(a, b, &len) != PL_LIST) {
    Ureturn FALSE;
  }
  PROTECT_AND_COUNT(c_R = index_R = allocList(len + 1));
  SETCAR(c_R, sin);
  SET_TYPEOF(c_R, LANGSXP);
  c_R = CDR(c_R);
  if (!old_list_to_sexp(a, c_R, len, TRUE)) {
    { Ureturn 0; }
  }
  PROTECT_AND_COUNT(call_R = LCONS(symbol, CONS(lhs_R, index_R)));
  SET_TYPEOF(call_R, LANGSXP);
  PROTECT_AND_COUNT(p = lang3(install("<-"), call_R, sexp));
  (void)protected_tryEval(p, R_GlobalEnv, &hadError);
  { Ureturn hadError; }
}

static int set_subset_to_sexp(term_t t, SEXP sexp) {
  term_t a = PL_new_term_ref();
  SEXP lhs_R;
  int i = 0;
  size_t dims[256], indexi[256], ndims, index;
  int nprotect = 0;

  if (!PL_get_arg(1, t, a))
    return REAL_Error("argument access", t);

  if (!PL_get_arg(2, t, t))
    return REAL_Error("argument access", t);

  term_t t0 = PL_copy_term_ref(t);

  term_t a0 = PL_copy_term_ref(a);
  while (PL_get_list(a, t, a)) {
    int64_t j;
    if (!PL_get_int64(t, &j)) {
      PROTECT_AND_COUNT(lhs_R = term_to_sexp(t0, FALSE));
      return set_subset_eval(R_BracketSymbol, a0, lhs_R, sexp);
    }
    indexi[i] = j;
    i++;
  }

  PROTECT_AND_COUNT(lhs_R = term_to_sexp(t0, TRUE));

  ndims = sexp_rank(lhs_R);
  sexp_shape(lhs_R, ndims, dims);
  if (i != ndims)
    Ureturn FALSE;
  index = pos_dims(indexi, ndims, dims);
  Ureturn sexp_to_S_el(sexp, index, lhs_R);
}

static int pl_to_unary(const char *s, term_t t, SEXP *ansP) {
  int nprotect = 0;
  if (!PL_get_arg(1, t, t)) {
    Ureturn FALSE;
  }
  PROTECT_AND_COUNT(*ansP = term_to_sexp(t, FALSE));
  PROTECT_AND_COUNT(*ansP = lang2(install(s), *ansP));
  Ureturn TRUE;
}

static int pl_to_binary(const char *s, term_t t, term_t tmp, SEXP *ansP) {
  int nprotect = 0;
  SEXP sexp;

  if (!PL_get_arg(2, t, tmp)) {
    return FALSE;
  }
  if (!PL_get_arg(1, t, t)) {
    return FALSE;
  }
  PROTECT_AND_COUNT(*ansP = term_to_sexp(t, FALSE));
  PROTECT_AND_COUNT(sexp = term_to_sexp(tmp, FALSE));
  PROTECT_AND_COUNT(*ansP = lang3(install(s), *ansP, sexp));
  Ureturn TRUE;
}

/**
 * term_to_sexp: convert a Prolog term to an R sexp
 *
 * @param t the Prolog term
 * @param ansP a pointer to the result SEXP
 * @param eval whether to evaluate functions, eg, whether  `2+3` should
 *   be converted to `closure(+,[[2],[3]))` or to `5`.
 *
 * @return whether it succeeds or fails.
 */
SEXP term_to_sexp(term_t t, bool eval) {
  int nprotect = 0;
  SEXP ans = R_NilValue;
  int objtype;
  term_t tmp = PL_copy_term_ref(t), i0 = tmp;
  t = PL_copy_term_ref(t);
  int rc;

  objtype = REAL_term_type(tmp, 0);

  if (objtype & PL_R_VECTOR) {
    PROTECT_AND_COUNT(ans = list_to_sexp(t, objtype));
    rc = (ans != R_NilValue);
  } else
    switch (objtype) {
    /// free variable is translated to an argument that can take
    /// any value, eg:
    ///   `[_,2]`  corresponds to `[,2]` in R selectors
    ///   `X ~ _`  corresponds tp `X ~ .` in R formulas
    case PL_R_VARIABLE:
      ans = R_MissingArg;
      rc = true;
      break;

    /// +'Atom' or "string" to R 'string' or  CHARACTER object
    ///
    /// real suggest using "..." notation for strings,
    /// but `string` will work as well.
    ///
    /// @deprecated +atom is an hack, and should be avoided
    case PL_R_PLUS:
    case PL_R_CHARS:
      PROTECT_AND_COUNT(ans = NEW_CHARACTER(1));
      rc = term_to_S_el(t, PL_R_CHARS, 0, ans);
      break;

    /// Prolog -atom or -"symbol" matches to R symbol
    ///
    /// @deprecated not needed any longer
    case PL_R_PSYMBOL:
      rc = PL_get_arg(1, t, t);

    /// Prolog atom matches to R symbol
    ///
    /// atoms can be evaluated
    case PL_R_SYMBOL: {
      char *s = NULL;

      if ((rc = PL_get_chars(t, &s, CVT_ATOM | CVT_STRING | BUF_DISCARDABLE |
                                        REP_UTF8))) {
        if (eval) {
          PROTECT_AND_COUNT(ans = findVar(Rf_install(s), R_GlobalEnv));
        } else {
          PROTECT_AND_COUNT(ans = Rf_install(s)); // NEW_CHARACTER(1));
          //		if ( ! term_to_S_el( t, PL_R_CHARS, 0, ans) )
          // Ureturn 0;
        }
        if (ans == R_UnboundValue) {
          rc = false;
        }
      }
    } break;

    /// YAP supports . as an infix operator, so a.b can be converted into R's
    /// 'a.b'
    ///
    case PL_R_DOT:
      rc = merge_dots(t);
      PROTECT_AND_COUNT(ans = term_to_sexp(t, eval));
      break;

    /// integer basic type
    case PL_R_INTEGER:
      PROTECT_AND_COUNT(ans = NEW_INTEGER(1));
      rc = term_to_S_el(t, PL_R_INTEGER, 0, ans);
      break;

    /// float basic type
    case PL_R_FLOAT:
      PROTECT_AND_COUNT(ans = NEW_NUMERIC(1));
      rc = term_to_S_el(t, PL_R_FLOAT, 0, ans);
      break;

    /// boolean in real is true or 'TRUE', false or 'FALSE'
    case PL_R_BOOL:
      PROTECT_AND_COUNT(ans = NEW_LOGICAL(1));
      rc = term_to_S_el(t, PL_R_BOOL, 0, ans);
      break;

    /// X$E access a named attribute from a list (ie. an attribute)
    case PL_R_LISTEL: {
      rc = listEl_to_sexp(t, &ans);
    } break;
    /// O@S access a slot from an object
    case PL_R_SLOT: {
      rc = slot_to_sexp(t, &ans);
    } break;

    /// [...] selects a subset from a vector
    case PL_R_SUBSET: {
      ans = subset_to_sexp(t, eval);
      rc = (ans != R_NilValue && ans != R_UnboundValue);
    } break;

    /// = applied in code definition,
    ///
    /// currently never evaluated
    case PL_R_EQUAL: {
      tmp = PL_new_term_ref();
      rc = pl_to_binary("=", t, tmp, &ans);
    } break;

    /// function call or closure
    case PL_R_CALL: {
      PROTECT_AND_COUNT(ans = pl_to_func(t, eval));
      if (ans && !Rf_isNull(ans)) {
        rc = true;
      } else {
        rc = false;
      }
    }

    break;

    /// fuction definition (yes, you can write R code as a Prolog term)
    case PL_R_DEFUN: {
      rc = pl_to_defun(t, &ans);
    } break;

    /// (X -> Y)

    case PL_R_IF: {
      term_t tcond = PL_new_term_ref();
      SEXP cond, expr;

      if ((rc = PL_get_arg(1, t, tcond))) {
        PROTECT_AND_COUNT(cond = term_to_sexp(tcond, FALSE));
      }
      if (rc && PL_get_arg(2, t, t) && pl_to_body(t, &expr)) {
        PROTECT_AND_COUNT(ans = LCONS(cond, expr));
      }
    } break;

    /// if(Then, Else)

    case PL_R_IF_ELSE: {
      term_t tcond = PL_new_term_ref();
      SEXP cond, sthen, selse;
      if ((rc = PL_get_arg(1, t, tcond))) {
        PROTECT_AND_COUNT(cond = term_to_sexp(tcond, FALSE));
        if (PL_get_arg(2, t, tcond) && pl_to_body(tcond, &sthen) &&
            PL_get_arg(3, t, t) && pl_to_body(t, &selse)) {
          PROTECT_AND_COUNT(ans = lang4(install("if"), cond, sthen, selse));
        }
      }
      break;

    /// in(Cond, Expr)
    case PL_R_IN: {
      term_t tcond = PL_new_term_ref();
      SEXP cond, expr;

      if ((rc = PL_get_arg(1, t, tcond))) {

        PROTECT_AND_COUNT(cond = term_to_sexp(tcond, FALSE));
        if ((rc = PL_get_arg(2, t, t))) {
          PROTECT_AND_COUNT(expr = term_to_sexp(t, FALSE));
          PROTECT_AND_COUNT(ans = lang3(install("in"), cond, expr));
        }
      }
      break;

    /// while(Cond, Expr)
    case PL_R_WHILE: {
      term_t tcond = PL_new_term_ref();
      SEXP cond, expr;
      PROTECT_AND_COUNT(cond = term_to_sexp(tcond, FALSE));
      if ((rc = PL_get_arg(2, t, t))) {
        PROTECT_AND_COUNT(expr = term_to_sexp(t, FALSE));
        PROTECT_AND_COUNT(ans = lang3(install("while"), cond, expr));
      }
    }
    }
    } break;

    /// reepeat( Expr)
    case PL_R_REPEAT: {
      SEXP expr;

      if ((rc = PL_get_arg(1, t, t) && pl_to_body(t, &expr))) {
        PROTECT_AND_COUNT(ans = lang2(install("repeat"), expr));
      }
    } break;

    /// break
    case PL_R_BREAK: {
      PROTECT_AND_COUNT(ans = lang1(install("break")));
    }
      rc = true;
      break;

    /// next
    case PL_R_NEXT: {
      PROTECT_AND_COUNT(ans = lang1(install("next")));
    }
      rc = true;
      break;

    // binary formula X ~ _
    case PL_R_FORMULA: {
      if ((rc = PL_get_arg(2, t, tmp))) {
        if (PL_is_variable(tmp)) {
          if ((rc = PL_get_arg(1, t, t))) {
            PROTECT_AND_COUNT(ans = lang3(install("~"), *&ans, install(".")));
          }
        } else {
          rc = pl_to_binary("~", t, tmp, &ans);
        }
      }
    } break;

    // unary formula ~ _
    case PL_R_RFORMULA:
      if ((rc = PL_get_arg(1, t, tmp))) {
        if (PL_is_variable(tmp)) {
          PROTECT_AND_COUNT(ans = term_to_sexp(t, FALSE));
          PROTECT_AND_COUNT(ans = lang2(install("~"), install(".")));
        }
      } else {
        rc = pl_to_unary("~", tmp, &ans);
      }
      break;

    case PL_R_QUOTE: {
      rc = PL_get_arg(1, t, t);
      PROTECT_AND_COUNT(ans = term_to_sexp(t, TRUE));
    } break;

    case PL_R_OUTER:
      rc = pl_to_binary("%o%", t, tmp, &ans);
      break;

    case PL_R_INNER: {
      rc = pl_to_binary("%i%", t, tmp, &ans);
    } break;

    default:
      assert(0);
      rc = false;
    }

  PL_reset_term_refs(i0);
  Ureturn ans;
}

//
// Prolog to SEXP
//
static int bind_sexp(term_t t, SEXP sexp) {
  int nprotect = 0;
  int objtype;

  objtype = REAL_term_type(t, 0);

  if (objtype & PL_R_VECTOR) {
    return FALSE;
  }

  switch (objtype) {
  case PL_R_VARIABLE:
    break;
  case PL_R_BOOL: {
    int b;
    size_t n;
    return sexp_rank(sexp) == 1 && sexp_shape(sexp, 0, &n) && n == 1 &&
           TYPEOF(sexp) == LGLSXP && PL_get_bool(t, &b) &&
           b == LOGICAL(sexp)[0];
  }

  case PL_R_FLOAT: {
    double dbl;
    size_t n;
    return sexp_rank(sexp) == 1 && sexp_shape(sexp, 0, &n) && n == 1 &&
           TYPEOF(sexp) == REALSXP && PL_get_float(t, &dbl) &&
           dbl == REAL(sexp)[0];
  }

  case PL_R_INTEGER: {
    size_t n;
    int64_t i;
    return sexp_rank(sexp) == 1 && sexp_shape(sexp, 0, &n) && n == 1 &&
           TYPEOF(sexp) == INTSXP && PL_get_int64(t, &i) &&
           i == INTEGER(sexp)[0];
  }

  case PL_R_COMPLEX:
  case PL_R_PLUS:
  case PL_R_CHARS:
    return FALSE;
  case PL_R_CALL: {
    // look only for attributes
    size_t arity;
    atom_t name;
    SEXP tmp_R;
    const char *s;
    if (!PL_get_name_arity(t, &name, &arity) || arity != 1) {
      return FALSE;
    }
    if (!(s = PL_atom_chars(name))) {
      return FALSE;
    }
    if (!PL_get_arg(1, t, t)) {
      return FALSE;
    }
    PROTECT_AND_COUNT(tmp_R = term_to_sexp(t, TRUE));
    if (Rf_isNull(tmp_R)) {
      Ureturn FALSE;
    }
    // these two are tricky...
    if (sexp_rank(tmp_R) == 1) {
      if (!strcmp(s, "rownames")) {
        SEXP dimnames, ans;
        PROTECT_AND_COUNT(dimnames = allocVector(VECSXP, 1));
        if (!Rf_isNull(sexp)) {
          size_t i, n = Rf_length(sexp);
          PROTECT_AND_COUNT(ans = allocVector(STRSXP, n));
          for (i = 0; i < n; i++)
            SET_STRING_ELT(ans, i, STRING_ELT(sexp, i));
          SET_VECTOR_ELT(dimnames, 0, ans);
        }
        dimnamesgets(tmp_R, dimnames);
      }
    }
    if (!strcmp(s, "colnames")) {
      SEXP dimnames, old, ans;
      PROTECT_AND_COUNT(dimnames = allocVector(VECSXP, 2));
      PROTECT_AND_COUNT(old =
                            Rf_GetRowNames(getAttrib(tmp_R, R_DimNamesSymbol)));
      SET_VECTOR_ELT(dimnames, 0, old);
      if (!isNull(sexp)) {
        size_t i, n = Rf_length(sexp);
        PROTECT_AND_COUNT(ans = allocVector(STRSXP, n));
        for (i = 0; i < n; i++)
          SET_STRING_ELT(ans, i, STRING_ELT(sexp, i));
        SET_VECTOR_ELT(dimnames, 1, ans);
      }
      dimnamesgets(tmp_R, dimnames);
      Ureturn true;
    } else if (!strcmp(s, "rownames")) {
      SEXP dimnames, old, ans;
      PROTECT_AND_COUNT(dimnames = allocVector(VECSXP, 2));
      PROTECT_AND_COUNT(old =
                            Rf_GetColNames(getAttrib(tmp_R, R_DimNamesSymbol)));
      SET_VECTOR_ELT(dimnames, 1, old);
      if (!Rf_isNull(sexp)) {
        size_t i, n = Rf_length(sexp);
        PROTECT_AND_COUNT(ans = allocVector(STRSXP, n));
        for (i = 0; i < n; i++)
          SET_STRING_ELT(ans, i, STRING_ELT(sexp, i));
        SET_VECTOR_ELT(dimnames, 0, ans);
        dimnamesgets(tmp_R, dimnames);
      }
      Ureturn true;
    }
    // we don't really care about it,

    // there is an atribute
    setAttrib(tmp_R, install(s), sexp);

    return true;
  }

  case PL_R_PSYMBOL:
    if (!PL_get_arg(1, t, t)) {
      return FALSE;
    }
    break;
  case PL_R_SYMBOL: {
    char *s = NULL;

    if (PL_get_chars(t, &s,
                     CVT_ATOM | CVT_STRING | BUF_DISCARDABLE | REP_UTF8)) {
      defineVar(Rf_install(s), sexp, R_GlobalEnv);
    }
    break;
  }

    return 0;

  case PL_R_LISTEL:
    return set_listEl_to_sexp(t, sexp);

  case PL_R_SLOT:
    return set_slot_to_sexp(t, sexp);

  case PL_R_DOT:
    if (!merge_dots(t))
      return FALSE;
    return bind_sexp(t, sexp);

  case PL_R_SUBSET:
    return set_subset_to_sexp(t, sexp);

  default:
    assert(0);
  }

  { Ureturn TRUE; }
}

/*
  static foreign_t
  pl_rtest1(term_t t)
  { SEXP sexp;

  if ( ( , &sexp) )
  { PrintValue(sexp);

  return TRUE;
  }

  return FALSE;
  }


  static foreign_t
  pl_rtest2(term_t t = term_to_sexp(t, term_t out) )
  { SEXP sexp;

  if ( ( , &sexp) )
  { term_t tmp = PL_new_term_ref();

  if ( sexp_to_pl(tmp = term_to_sexp(t, sexp) ) )
  return PL_unify(out, tmp);
  }

  return FALSE;
  }

  PL_register_foreign("rtest",		  1, pl_rtest1,	       0);
  PL_register_foreign("rtest",		  2, pl_rtest2,	       0);
*/

/*******************************
 *	   SEXP --> Prolog	*
 *******************************/
bool  sexp_to_pl(term_t t, SEXP s) {
  int rank = sexp_rank(s);
  size_t shape[256];

  if (rank > 2)
    return REAL_Error("multi-dimensional arrays unsupported", t);

  sexp_shape(s, rank, shape);

  switch (rank) {
  case 1: {
    int i;

    switch (TYPEOF(s)) {
    case NILSXP:
      PL_put_nil(t);
      return TRUE;
    case SYMSXP:
      /* FIXME: take it as as an atom */
      s = PRINTNAME(s);
      if (TYPEOF(s) == STRSXP) {
        size_t shape;

        if (sexp_rank(s) > 1)
          return FALSE;
        sexp_shape(s, 1, &shape);
        if (shape != 1)
          return FALSE;
        return PL_unify_chars(t, PL_ATOM | REP_UTF8, -1,
                              CHAR(CHARACTER_DATA(s)[0]));
      }
      return FALSE;
    case REALSXP: {
      term_t head = PL_new_term_ref();
      term_t tail = PL_new_term_ref();

      PL_put_nil(tail);
      for (i = shape[0] - 1; i >= 0; i--) {
        if (!PL_put_float(head, NUMERIC_DATA(s)[i]) ||
            !PL_cons_list(tail, head, tail))
          return FALSE;
      }
      PL_put_term(t, tail);
      break;
    }
    case INTSXP: {
      term_t head = PL_new_term_ref();
      term_t tail = PL_new_term_ref();

      PL_put_nil(tail);
      for (i = shape[0] - 1; i >= 0; i--) {
        if (!PL_put_int64(head, INTEGER_DATA(s)[i]) ||
            !PL_cons_list(tail, head, tail))
          return FALSE;
      }
      PL_put_term(t, tail);
      break;
    }
    case LGLSXP: {
      term_t head = PL_new_term_ref();
      term_t tail = PL_new_term_ref();

      PL_put_nil(tail);
      for (i = shape[0] - 1; i >= 0; i--) {
        if (!PL_put_variable(head) || /* TBD: All PL_put_bool() */
            !PL_unify_bool(head, LOGICAL_DATA(s)[i]) ||
            !PL_cons_list(tail, head, tail))
          return FALSE;
      }
      PL_put_term(t, tail);
      break;
    }
    case CPLXSXP: {
      term_t headr = PL_new_term_ref();
      term_t headi = PL_new_term_ref();
      term_t tail = PL_new_term_ref();

      PL_put_nil(tail);
      for (i = shape[0] - 1; i >= 0; i--) {
        if (COMPLEX_DATA(s)[i].i >= 0) {
          if (!PL_put_float(headr, COMPLEX_DATA(s)[i].r) ||
              !PL_put_float(headi, COMPLEX_DATA(s)[i].i) ||
              !PL_cons_functor(headi, FUNCTOR_i1, headi) ||
              !PL_cons_functor(headr, FUNCTOR_plus2, headr, headi) ||
              !PL_cons_list(tail, headr, tail))
            return FALSE;
        } else if (!PL_put_float(headr, COMPLEX_DATA(s)[i].r) ||
                   !PL_put_float(headi, -COMPLEX_DATA(s)[i].i) ||
                   !PL_cons_functor(headi, FUNCTOR_i1, headi) ||
                   !PL_cons_functor(headr, FUNCTOR_minus2, headr, headi) ||
                   !PL_cons_list(tail, headr, tail))
          return FALSE;
      }
      PL_put_term(t, tail);
      break;
    }
    case VECSXP: {
      SEXP names = GET_NAMES(s);
      term_t av = PL_new_term_refs(2);
      term_t head = PL_new_term_ref();
      term_t tail = PL_new_term_ref();

      PL_put_nil(tail);
      for (i = LENGTH(s) - 1; i >= 0; i--) {
        SEXP elem = VECTOR_ELT(s, i);

        if (names == R_NilValue || STRING_ELT(names, i) == R_NilValue) {
          //		    PL_unify(av+0,av+1);
          if (!sexp_to_pl(av, elem) ||
              //	!PL_cons_functor_v(head, FUNCTOR_equal2, av) ||
              !PL_cons_list(tail, av, tail))
            return FALSE;
        } else if (!PL_put_atom_chars(av + 0, CHAR(STRING_ELT(names, i))) ||
                   !sexp_to_pl(av + 1, elem) ||
                   !PL_cons_functor_v(head, FUNCTOR_equal2, av) ||
                   !PL_cons_list(tail, head, tail))
          return FALSE;
      }
      PL_put_term(t, tail);
      break;
    }
    case STRSXP: {
      term_t tail = PL_new_term_ref();

      PL_put_nil(tail);
      for (i = shape[0] - 1; i >= 0; i--) {
        const char *chars = CHAR(CHARACTER_DATA(s)[i]);
        term_t head = PL_new_term_ref();
        // use string to communicate with outside world
        if (!PL_unify_chars(head, PL_STRING | REP_UTF8, -1, chars) ||
            !PL_cons_list(tail, head, tail))
          return FALSE;
      }
      PL_put_term(t, tail);
      break;
    }
    default: {
      char buf[256];
      snprintf(buf, 255, "Unsupported r-type, with id: %d \n", TYPEOF(s));
      return REAL_Error(buf, t);
    }
    }
    if (shape[0] == 1) {
      if (!PL_get_arg(1, t, t)) /* Just return the head */
        REAL_Error("argument access", t);
    }
    break;
  }
  case 2: {
    SEXP adims = getAttrib(s, R_DimSymbol);
    int nrows = INTEGER(adims)[0];
    int ncols = INTEGER(adims)[1];
    term_t tail = PL_new_term_ref();
    term_t nest_tail = PL_new_term_ref();
    term_t nest_head = PL_new_term_ref();
    int i, j, c;

    PL_put_nil(tail);

    for (i = (nrows - 1); i > -1; i--) {
      PL_put_nil(nest_tail);
      for (j = (ncols - 1); j > -1; j--) {
        c = (j * nrows) + i;
        // { size_t index = col_i*len + row_i;

        switch (TYPEOF(s)) {
        case REALSXP:
          if (!PL_put_float(nest_head, NUMERIC_DATA(s)[c]))
            return FALSE;
          break;
        case INTSXP:
          if (!PL_put_int64(nest_head, INTEGER_DATA(s)[c]))
            return FALSE;
          break;
        case STRSXP:
          nest_head = PL_new_term_ref();
          if (!PL_unify_chars(nest_head, PL_STRING | REP_UTF8, -1,
                              CHAR(CHARACTER_DATA(s)[c])))
            return FALSE;
          break;
        case LGLSXP:
          if (!PL_put_variable(nest_head) ||
              !PL_unify_bool(nest_head, LOGICAL_DATA(s)[c]))
            return FALSE;
          break;
        }
        if (!PL_cons_list(nest_tail, nest_head, nest_tail))
          return FALSE;
      }
      if (!PL_cons_list(tail, nest_tail, tail))
        return FALSE;
    }

    PL_put_term(t, tail);
    break;
  }
  default:
    assert(0);
  }

  return TRUE;
}

/*******************************
 *	      START/END		*
 *******************************/

static foreign_t init_R(void) {
  // int argc = 2;

//  Rf_endEmbeddedR(0);

#if R_SIGNAL_HANDLERS
  R_SignalHandlers = 0;
#endif
    char *argv[]= {"yap", "--gui=none","--silent","--vanilla"};
    Rf_initEmbeddedR(sizeof(argv)/sizeof(argv[0]), argv);

    #ifndef WIN32
  R_CStackLimit = -1;
#endif
  return TRUE;
}

static foreign_t stop_R(void) {
  Rf_endEmbeddedR(0);
  R_dot_Last();
  R_RunExitFinalizers();
  R_gc();

  return TRUE;
}

/*******************************
 *	 EXECUTE COMMAND	*
 *******************************/

static SEXP process_expression(const char *expression) {
  SEXP e, tmp, val;
  int hadError;
  ParseStatus status;
  int nprotect = 0;

  //  PROTECT_AND_COUNT(tmp = mkString(expression));
  PROTECT_AND_COUNT(tmp = ScalarString(mkCharCE(expression, CE_UTF8)));
  PROTECT_AND_COUNT(e = R_ParseVector(tmp, 1, &status, R_NilValue));
  if (status != PARSE_OK) {
    Sdprintf("Error: %d, in parsing R expression.\n", status);
    /* do not continue with protected_tryEval() */
    /* PL_unify_term(except, PL_FUNCTOR_CHARS, "r_expression_syntax_error", 2,
     * PL_CHARS, expression, PL_R_INTEGER, status ); */
    /*FIXME: return the expression too (as atom) */
    /* PL_FUNCTOR_CHARS, "r_expression_syntax_error", 2, PL_CHARS, "atom",
     * PL_TERM, to; */
    /* return PL_raise_exception(except); */
    Ureturn NULL;
  }

  /* FIXME: Check status (nicos: it seems to be always 1 though? */
  PROTECT_AND_COUNT(
      val = protected_tryEval(VECTOR_ELT(e, 0), R_GlobalEnv, &hadError));
  if (!hadError) {
    Ureturn val;
  }
  { Ureturn NULL; }
}

static foreign_t send_R_command(term_t cmd) {
  char *s = NULL;
  term_t except = PL_new_term_ref();

  if (PL_get_chars(cmd, &s, CVT_ALL | REP_UTF8 | BUF_MALLOC)) {
    if (process_expression(s)) {
      PL_free(s);
      return TRUE;
    }
    PL_free(s);
    if (PL_unify_term(except, PL_FUNCTOR_CHARS, "real_error", 1, PL_CHARS,
                      "correspondence"))
      return PL_raise_exception(except);
    return FALSE;
  }
  Sdprintf("Error in PL_get_chars for %s\n", s); /* FIXME: Exception */
  return FALSE;
}

// fast copy of a Prolog vector to R
static foreign_t send_c_vector(term_t tvec, term_t tout) {
  char *s = NULL;
  size_t arity, i;
  atom_t name;
  term_t targ = PL_new_term_ref();
  SEXP rho = R_GlobalEnv, ans;
  int nprotect = 0;

  if (!PL_get_name_arity(tvec, &name, &arity) || arity <= 0) {
    return FALSE;
  }
  if (!PL_get_atom_chars(tout, &s)) {
    return FALSE;
  }
  _PL_get_arg(1, tvec, targ);
  if (PL_is_number(targ)) {
    int ints = TRUE;

    for (i = 0; i < arity; i++) {
      _PL_get_arg(i + 1, tvec, targ);
      if (!PL_is_integer(targ)) {
        ints = FALSE;
        if (!PL_is_float(targ)) {
          Ureturn FALSE;
        }
      }
    }
    if (ints) {
      int *vec;

      PROTECT_AND_COUNT(ans = allocVector(INTSXP, arity));
      if (!ans)
        return FALSE;
      vec = INTEGER(ans);
      for (i = 0; i < arity; i++) {
        int64_t j;
        _PL_get_arg(i + 1, tvec, targ);
        if (!PL_get_int64(targ, &j)) {
          Ureturn FALSE;
        }
        vec[i] = j;
      }
    } else {
      double *vec;

      PROTECT_AND_COUNT(ans = allocVector(REALSXP, arity));
      if (!ans) {
        Ureturn FALSE;
      }
      vec = REAL(ans);
      for (i = 0; i < arity; i++) {
        _PL_get_arg(i + 1, tvec, targ);
        if (!PL_get_float(targ, vec + i)) {
          int64_t j;
          if (!PL_get_int64(targ, &j)) {
            Ureturn FALSE;
          }
          vec[i] = j;
        }
      }
    }
  } else if (PL_is_atom(targ) || PL_is_string(targ)) {

    PROTECT_AND_COUNT(ans = allocVector(STRSXP, arity));
    if (!ans) {
      Ureturn FALSE;
    }
    for (i = 0; i < arity; i++) {
      char *str = NULL;

      _PL_get_arg(i + 1, tvec, targ);
      if (PL_get_chars(targ, &str, CVT_ALL | BUF_DISCARDABLE | REP_UTF8)) {
        SET_STRING_ELT(ans, i, mkCharCE(str, CE_UTF8));
      } else {
        Ureturn FALSE;
      }
    }
  } else {
    Ureturn FALSE;
  }
  defineVar(install(s), ans, rho);
  Ureturn TRUE;
}

static foreign_t rexpr_to_pl_term(term_t in, term_t out) {
  char *s = NULL;

  if (PL_get_chars(in, &s, CVT_ALL | BUF_MALLOC | REP_UTF8)) {
    SEXP sexp;

    if ((sexp = process_expression(s))) {
      term_t tmp = PL_new_term_ref();

      PL_free(s);
      if (sexp_to_pl(tmp, sexp))
        return PL_unify(out, tmp);

      return FALSE;
    } else { /* FIXME: Throw exception */
      PL_free(s);
    }
  }

  return FALSE;
}

static foreign_t robj_to_pl_term(term_t name, term_t out) {
  char *plname = NULL;
  int nprotect = 0;

  if (PL_get_chars(name, &plname, CVT_ALL | BUF_DISCARDABLE | REP_UTF8)) {
    SEXP s;
    term_t tmp = PL_new_term_ref();
    int rc;

    PROTECT_AND_COUNT(s = findVar(install(plname), R_GlobalEnv));
    if (s == R_UnboundValue || TYPEOF(s) == SYMSXP) {
      Ureturn REAL_Error("r_variable", name);
    }

    rc = sexp_to_pl(tmp, s);

    if (rc) {
      Ureturn PL_unify(out, tmp);
    }
  }

  { Ureturn FALSE; }
}

static foreign_t set_R_variable(term_t rvar, term_t value) {
  char *vname = NULL;
  SEXP sexp;
  int nprotect = 0;
  bool rc = false;

  if (PL_get_chars(rvar, &vname, CVT_ALL | BUF_MALLOC | REP_UTF8)) {
    PROTECT_AND_COUNT(sexp = (term_to_sexp(value, TRUE)));
    if (!Rf_isNull(sexp))
      defineVar(Rf_install(vname), sexp, R_GlobalEnv);
    rc = true;
  }
  if (vname)
    PL_free(vname);
  Ureturn rc;
}

static foreign_t execute_R_1(term_t value) {
  SEXP sexp;
  foreign_t rc = FALSE;
  int nprotect = 0;
  int hadError;

  PROTECT_AND_COUNT(R_GlobalEnv);
  PROTECT_AND_COUNT(sexp = term_to_sexp(value, TRUE));
  rc = !Rf_isNull(sexp);
  if (rc) {
    PROTECT_AND_COUNT(sexp = protected_tryEval(sexp, R_GlobalEnv, &hadError));
    if (hadError) {
      Ureturn false;
    }
  }
  Ureturn rc;
}

static foreign_t execute_R(term_t rvar, term_t value) {
  SEXP sexp;
  foreign_t rc = FALSE;
  term_t t1 = PL_new_term_ref();
  int nprotect = 0;
  PROTECT_AND_COUNT(sexp = term_to_sexp(value, true));
  // PROTECT_AND_COUNT( sexp = protected_tryEval(sexp, R_GlobalEnv, &hadError)
  // );
  if (sexp == R_UnboundValue || Rf_isNull(sexp)) {
    PL_reset_term_refs(t1);
    Ureturn false;
  } else {
    int hadError = false;
    sexp = protected_tryEval(sexp, R_GlobalEnv, &hadError);
    if (hadError) {
      PL_reset_term_refs(t1);
      Ureturn false;
    }
  }
  if (PL_is_ground(rvar)) {
    rc = bind_sexp(rvar, sexp);
  } else {
    if (!sexp_to_pl(t1, sexp))
      rc = FALSE;
    else
      rc = PL_unify(rvar, t1);
  }
  PL_reset_term_refs(t1);
  Ureturn rc;
}

static foreign_t is_R_variable(term_t t) {
  SEXP name, o;
  char *s = NULL;
  int nprotect = 0;

  /* is this variable defined in R?.  */
  if (PL_get_chars(t, &s, CVT_ATOM | CVT_STRING | BUF_DISCARDABLE | REP_UTF8)) {
    PROTECT_AND_COUNT(name = NEW_CHARACTER(1));
    CHARACTER_DATA(name)[0] = mkCharCE(s, CE_UTF8);
  } else {
    Ureturn FALSE;
  }

  PROTECT_AND_COUNT(
      o = findVar(install(CHAR(STRING_ELT(name, 0))), R_GlobalEnv));
  Ureturn o != R_UnboundValue;
}

#ifndef ATOM_dot
#define ATOM_dot PL_new_atom(".")
#endif

X_API install_t
install_real(void) { /* FUNCTOR_dot2 = PL_new_functor(PL_new_atom("."), 2); */
  static bool initialised = false;
  
  if (initialised)
    return;
  initialised = true;
  ATOM_break = PL_new_atom("break");
  ATOM_false = PL_new_atom("false");
  ATOM_function = PL_new_atom("function");
  ATOM_i = PL_new_atom("i");
  ATOM_next = PL_new_atom("next");
  ATOM_true = PL_new_atom("true");

  FUNCTOR_at2 = PL_new_functor(PL_new_atom("@"), 2);
  FUNCTOR_boolop1 = PL_new_functor(PL_new_atom("@"), 1);
  FUNCTOR_brackets1 = PL_new_functor(PL_new_atom("()"), 1);
  FUNCTOR_dollar1 = PL_new_functor(PL_new_atom("$"), 1);
  FUNCTOR_dollar2 = PL_new_functor(PL_new_atom("$"), 2);
  FUNCTOR_dot1 = PL_new_functor(ATOM_dot, 1);
  FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_hat2 = PL_new_functor(PL_new_atom("^"), 2);
  FUNCTOR_i1 = PL_new_functor(ATOM_i, 1);
  FUNCTOR_if2 = PL_new_functor(PL_new_atom("->"), 2);
  FUNCTOR_iff2 = PL_new_functor(PL_new_atom("if"), 2);
  FUNCTOR_iff3 = PL_new_functor(PL_new_atom("if"), 3);
  FUNCTOR_in2 = PL_new_functor(PL_new_atom("in"), 2);
  FUNCTOR_inner2 = PL_new_functor(PL_new_atom("@*@"), 2);
  FUNCTOR_for3 = PL_new_functor(PL_new_atom("for"), 3);
  FUNCTOR_minus1 = PL_new_functor(PL_new_atom("-"), 1);
  FUNCTOR_minus2 = PL_new_functor(PL_new_atom("-"), 2);
  FUNCTOR_outer2 = PL_new_functor(PL_new_atom("@^@"), 2);
  FUNCTOR_plus1 = PL_new_functor(PL_new_atom("+"), 1);
  FUNCTOR_plus2 = PL_new_functor(PL_new_atom("+"), 2);
  FUNCTOR_quote1 = PL_new_functor(PL_new_atom("quote"), 1);
  FUNCTOR_repeat1 = PL_new_functor(PL_new_atom("repeat"), 1);
  FUNCTOR_square_brackets2 = PL_new_functor(PL_new_atom("[]"), 2);
  FUNCTOR_tilde1 = PL_new_functor(PL_new_atom("~"), 1);
  FUNCTOR_tilde2 = PL_new_functor(PL_new_atom("~"), 2);
  FUNCTOR_while2 = PL_new_functor(PL_new_atom("while"), 2);

  PL_register_foreign("init_R", 0, init_R, 0);
  PL_register_foreign("stop_R", 0, stop_R, 0);
  PL_register_foreign("send_R_command", 1, send_R_command, 0);
  PL_register_foreign("send_c_vector", 2, send_c_vector, 0);
  PL_register_foreign("rexpr_to_pl_term", 2, rexpr_to_pl_term, 0);
  PL_register_foreign("robj_to_pl_term", 2, robj_to_pl_term, 0);
  PL_register_foreign("set_R_variable", 2, set_R_variable, 0);
  PL_register_foreign("execute_R", 2, execute_R, 0);
  PL_register_foreign("execute_R", 1, execute_R_1, 0);
  PL_register_foreign("is_R_variable", 1, is_R_variable, 0);
}

/// @}

