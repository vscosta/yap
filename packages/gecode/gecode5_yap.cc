// -*- c++ -*-
//=============================================================================
// Copyright (C) 2011 by Denys Duchier
//
// This program is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//=============================================================================

#include "gecode5-common.icc"
#include <iostream>
using namespace std;
using namespace generic_gecode;
using namespace Gecode;

extern "C"
{
void gecode_init(void);

#include "config.h"
}

namespace generic_gecode
{
#ifndef HAVE_DYNARRAY
  template <typename T> struct DynArray
  {
    T* _array;
    DynArray(int n): _array(new T[n]) {}
    ~DynArray() { delete[] _array; }
    T& operator[](int i) { return _array[i]; }
  };
#define DYNARRAY(T,A,N) DynArray<T> A(N)
#else
#define DYNARRAY(T,A,N) T A[N]
#endif

#ifndef HAVE_DYNARRAY
  struct SpecArray
  {
    int (*_array)[2];
    SpecArray(int n): _array((int (*)[2]) new int[n*2]) {}
    ~SpecArray() { delete[] _array; }
    int& operator()(int i,int j) { return _array[i][j]; }
  };
#define SPECARRAY(A,N) SpecArray A(N)
#define SPECARRAYELEM(A,I,J) A(I,J)
#define SPECARRAYDEREF(A) A._array
#else
#define SPECARRAY(A,N) int A[N][2]
#define SPECARRAYELEM(A,I,J) A[I][J]
#define SPECARRAYDEREF(A) A
#endif
}

extern "C"
{
#include "YapInterface.h"

  static YAP_opaque_tag_t gecode_space_tag;
  static YAP_opaque_handler_t gecode_space_handler;

  static YAP_Bool gecode_space_fail_handler(YAP_Term t)
  {
    delete *(GenericSpace **)YAP_OpaqueObjectFromTerm(YAP_HeadOfTerm(t));
    return TRUE;
  }

  static YAP_Bool
  gecode_space_write_handler
  (FILE  *stream, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<space %p>", p);
    return TRUE;
  }

  static YAP_Term gecode_term_from_space(GenericSpace* s)
  {
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_space_tag, sizeof(GenericSpace*));
    GenericSpace** ptr =
      (GenericSpace**) YAP_OpaqueObjectFromTerm(term);
    *ptr = s;
    return term;
  }

  static YAP_Bool gecode_new_space(void)
  {
    YAP_Term term = gecode_term_from_space(new GenericSpace);
    return YAP_Unify(YAP_ARG1, term);
  }


#ifndef DISJUNCTOR
  static inline GenericSpace*
  gecode_Space_from_term(YAP_Term t)
  {
    return * (GenericSpace**) YAP_OpaqueObjectFromTerm(t);
  }
#else
  struct YapDisjunctor
  {
    GenericSpace* home;
    Disjunctor disj;
    YapDisjunctor(GenericSpace* home_)
      : home(home_), disj(*home_) {}
  };

  static YAP_opaque_tag_t gecode_disjunctor_tag;
  static YAP_opaque_handler_t gecode_disjunctor_handler;
  static YAP_opaque_tag_t gecode_disjunctor_clause_tag;
  static YAP_opaque_handler_t gecode_disjunctor_clause_handler;

  static inline Disjunctor&
  gecode_Disjunctor_from_term(YAP_Term t)
  {
    return ((YapDisjunctor*) YAP_OpaqueObjectFromTerm(t))->disj;
  }

  static inline YapDisjunctor&
  gecode_YapDisjunctor_from_term(YAP_Term t)
  {
    return * (YapDisjunctor*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline Clause&
  gecode_Clause_from_term(YAP_Term t)
  {
    return * (Clause*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline GenericSpace*
  gecode_Space_from_term(YAP_Term t)
  {
    if (YAP_IsOpaqueObjectTerm(t, gecode_disjunctor_clause_tag))
      {
	Clause& c = gecode_Clause_from_term(t);
	return & (GenericSpace&) c;
      }
    return * (GenericSpace**) YAP_OpaqueObjectFromTerm(t);
  }
#endif

  static inline FloatAssign&
  gecode_FloatAssign_from_term(YAP_Term t)
  {
    return * (FloatAssign*) YAP_OpaqueObjectFromTerm(t);
  }

static inline BoolAssign&
  gecode_BoolAssign_from_term(YAP_Term t)
  {
    return * (BoolAssign*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline IntAssign&
  gecode_IntAssign_from_term(YAP_Term t)
  {
    return * (IntAssign*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline SetAssign&
  gecode_SetAssign_from_term(YAP_Term t)
  {
    return * (SetAssign*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline TupleSet&
  gecode_TupleSet_from_term(YAP_Term t)
  {
    return *(TupleSet *) YAP_OpaqueObjectFromTerm(t);
  }

  static inline DFA&
  gecode_DFA_from_term(YAP_Term t)
  {
    return *(DFA *) YAP_OpaqueObjectFromTerm(t);
  }

  static inline Rnd&
  gecode_Rnd_from_term(YAP_Term t)
  {
    return *(Rnd *) YAP_OpaqueObjectFromTerm(t);
  }

  static inline std::function<void(Space&home)>&
  gecode_std_function_from_term(YAP_Term t)
  {
    return *(std::function<void(Space&home)> *) YAP_OpaqueObjectFromTerm(t);
  }

  static inline FloatNum
  gecode_FloatNum_from_term(YAP_Term t)
  {
    return  (FloatNum) YAP_FloatOfTerm(t);
  }

  static YAP_Term gecode_SET_VAR_NONE;
  static YAP_Term gecode_SET_VAR_DEGREE_MIN;
  static YAP_Term gecode_SET_VAR_DEGREE_MAX;
  static YAP_Term gecode_SET_VAR_MIN_MIN;
  static YAP_Term gecode_SET_VAR_MIN_MAX;
  static YAP_Term gecode_SET_VAR_MAX_MIN;
  static YAP_Term gecode_SET_VAR_MAX_MAX;
  static YAP_Term gecode_SET_VAR_SIZE_MIN;
  static YAP_Term gecode_SET_VAR_SIZE_MAX;
  static YAP_Term gecode_SET_VAR_DEGREE_SIZE_MIN;
  static YAP_Term gecode_SET_VAR_DEGREE_SIZE_MAX;

  static inline SetVarBranch
  gecode_SetVarBranch_from_term(YAP_Term t)
  {

    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_SET_VAR_SIZE_MIN)
	return SET_VAR_SIZE_MIN();
      if ( t == gecode_SET_VAR_SIZE_MAX)
	return SET_VAR_SIZE_MAX();
      if ( t == gecode_SET_VAR_NONE)
	return SET_VAR_NONE();
      if ( t == gecode_SET_VAR_DEGREE_MIN)
	return SET_VAR_DEGREE_MIN();
      if ( t == gecode_SET_VAR_DEGREE_MAX)
	return SET_VAR_DEGREE_MAX();
      if ( t == gecode_SET_VAR_MIN_MIN)
	return SET_VAR_MIN_MIN();
      if ( t == gecode_SET_VAR_MIN_MAX)
	return SET_VAR_MIN_MAX();
      if ( t == gecode_SET_VAR_MAX_MIN)
	return SET_VAR_MAX_MIN();
      if ( t == gecode_SET_VAR_MAX_MAX)
	return SET_VAR_MAX_MAX();
      if ( t == gecode_SET_VAR_DEGREE_SIZE_MIN)
	return SET_VAR_DEGREE_SIZE_MIN();
      if ( t == gecode_SET_VAR_DEGREE_SIZE_MAX)
	return SET_VAR_DEGREE_SIZE_MAX();
    }
    cerr << "unsupported SET VAR" << endl; exit(1);
  }

  static YAP_Term gecode_SET_VAL_MIN_INC;
  static YAP_Term gecode_SET_VAL_MIN_EXC;
  static YAP_Term gecode_SET_VAL_MED_INC;
  static YAP_Term gecode_SET_VAL_MED_EXC;
  static YAP_Term gecode_SET_VAL_MAX_INC;
  static YAP_Term gecode_SET_VAL_MAX_EXC;

  static inline SetValBranch
  gecode_SetValBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_SET_VAL_MIN_INC)
	return SET_VAL_MIN_INC();
      if ( t == gecode_SET_VAL_MIN_EXC)
	return SET_VAL_MIN_EXC();
      if ( t == gecode_SET_VAL_MED_INC)
	return SET_VAL_MED_INC();
      if ( t == gecode_SET_VAL_MED_EXC)
	return SET_VAL_MED_EXC();
      if ( t == gecode_SET_VAL_MAX_INC)
	return SET_VAL_MAX_INC();
      if ( t == gecode_SET_VAL_MAX_EXC)
	return SET_VAL_MAX_EXC();
    }
    cerr << "unsupported INT VAL" << endl; exit(1);
  }

  static inline SetBranchFilter&
  gecode_SetBranchFilter_from_term(YAP_Term t)
  {
    return * (SetBranchFilter*) YAP_OpaqueObjectFromTerm(t);
  }

  static YAP_Term gecode_INT_VAR_NONE;
  static YAP_Term gecode_INT_VAR_DEGREE_MIN;
  static YAP_Term gecode_INT_VAR_DEGREE_MAX;
  static YAP_Term gecode_INT_VAR_MIN_MIN;
  static YAP_Term gecode_INT_VAR_MIN_MAX;
  static YAP_Term gecode_INT_VAR_MAX_MIN;
  static YAP_Term gecode_INT_VAR_MAX_MAX;
  static YAP_Term gecode_INT_VAR_SIZE_MIN;
  static YAP_Term gecode_INT_VAR_SIZE_MAX;
  static YAP_Term gecode_INT_VAR_DEGREE_SIZE_MIN;
  static YAP_Term gecode_INT_VAR_DEGREE_SIZE_MAX;
  static YAP_Term gecode_INT_VAR_REGRET_MIN_MIN;
  static YAP_Term gecode_INT_VAR_REGRET_MIN_MAX;
  static YAP_Term gecode_INT_VAR_REGRET_MAX_MIN;
  static YAP_Term gecode_INT_VAR_REGRET_MAX_MAX;

  static inline IntVarBranch
  gecode_IntVarBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_INT_VAR_SIZE_MIN)
	return INT_VAR_SIZE_MIN();
      if ( t == gecode_INT_VAR_SIZE_MAX)
	return INT_VAR_SIZE_MAX();
      if ( t == gecode_INT_VAR_NONE)
	return INT_VAR_NONE();
      if ( t == gecode_INT_VAR_DEGREE_MIN)
	return INT_VAR_DEGREE_MIN();
      if ( t == gecode_INT_VAR_DEGREE_MAX)
	return INT_VAR_DEGREE_MAX();
      if ( t == gecode_INT_VAR_MIN_MIN)
	return INT_VAR_MIN_MIN();
      if ( t == gecode_INT_VAR_MIN_MAX)
	return INT_VAR_MIN_MAX();
      if ( t == gecode_INT_VAR_MAX_MIN)
	return INT_VAR_MAX_MIN();
      if ( t == gecode_INT_VAR_MAX_MAX)
	return INT_VAR_MAX_MAX();
      if ( t == gecode_INT_VAR_DEGREE_SIZE_MIN)
	return INT_VAR_DEGREE_SIZE_MIN();
      if ( t == gecode_INT_VAR_DEGREE_SIZE_MAX)
	return INT_VAR_DEGREE_SIZE_MAX();
      if ( t == gecode_INT_VAR_REGRET_MIN_MIN)
	return INT_VAR_REGRET_MIN_MIN();
      if ( t == gecode_INT_VAR_REGRET_MIN_MAX)
	return INT_VAR_REGRET_MIN_MAX();
      if ( t == gecode_INT_VAR_REGRET_MAX_MIN)
	return INT_VAR_REGRET_MAX_MIN();
      if ( t == gecode_INT_VAR_REGRET_MAX_MAX)
	return INT_VAR_REGRET_MAX_MAX();
    }
    cerr << "unsupported INT VAR" << endl; exit(1);
  }

static YAP_Term gecode_BOOL_VAR_NONE;
static YAP_Term gecode_BOOL_VAR_RND;
  // static YAP_Term gecode_BOOL_VAR_MERIT_MIN;
  // static YAP_Term gecode_BOOL_VAR_MERIT_MAX;
  static YAP_Term gecode_BOOL_VAR_DEGREE_MIN;
  static YAP_Term gecode_BOOL_VAR_DEGREE_MAX;
  static YAP_Term gecode_BOOL_VAR_MAX_MIN;
  static YAP_Term gecode_BOOL_VAR_MAX_MAX;
  static YAP_Term gecode_BOOL_VAR_AFC_MIN;
  static YAP_Term gecode_BOOL_VAR_AFC_MAX;
  static YAP_Term gecode_BOOL_VAR_ACTION_MIN;
  static YAP_Term gecode_BOOL_VAR_ACTION_MAX;
  static YAP_Term gecode_BOOL_VAR_CHB_MIN;
  static YAP_Term gecode_BOOL_VAR_CHB_MAX;

  static inline BoolVarBranch
  gecode_BoolVarBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_BOOL_VAR_NONE)
	return BOOL_VAR_NONE();
       if ( t == gecode_BOOL_VAR_RND)
	return BOOL_VAR_RND(Rnd());
  //     if ( t == gecode_BOOL_VAR_MERIT_MIN)
	// return BOOL_VAR_MERIT_MIN();
  //     if ( t == gecode_BOOL_VAR_MERIT_MAX)
	// return BOOL_VAR_MERIT_MAX();
      if ( t == gecode_BOOL_VAR_DEGREE_MIN)
	return BOOL_VAR_DEGREE_MIN();
      if ( t == gecode_BOOL_VAR_DEGREE_MAX)
	return BOOL_VAR_DEGREE_MAX();
      if ( t == gecode_BOOL_VAR_AFC_MIN)
	return BOOL_VAR_AFC_MIN();
      if ( t == gecode_BOOL_VAR_AFC_MAX)
	return BOOL_VAR_AFC_MAX();
      if ( t == gecode_BOOL_VAR_ACTION_MIN)
	return BOOL_VAR_ACTION_MIN();
      if ( t == gecode_BOOL_VAR_ACTION_MAX)
	return BOOL_VAR_ACTION_MAX();
      if ( t == gecode_BOOL_VAR_CHB_MIN)
	return BOOL_VAR_CHB_MIN();
      if ( t == gecode_BOOL_VAR_CHB_MAX)
	return BOOL_VAR_CHB_MAX();
    }
    cerr << "unsupported INT VAR" << endl; exit(1);
  }

  static YAP_Term gecode_FLOAT_VAR_NONE;
  static YAP_Term gecode_FLOAT_VAR_DEGREE_MIN;
  static YAP_Term gecode_FLOAT_VAR_DEGREE_MAX;
  static YAP_Term gecode_FLOAT_VAR_MIN_MIN;
  static YAP_Term gecode_FLOAT_VAR_MIN_MAX;
  static YAP_Term gecode_FLOAT_VAR_MAX_MIN;
  static YAP_Term gecode_FLOAT_VAR_MAX_MAX;
  static YAP_Term gecode_FLOAT_VAR_SIZE_MIN;
  static YAP_Term gecode_FLOAT_VAR_SIZE_MAX;
  static YAP_Term gecode_FLOAT_VAR_DEGREE_SIZE_MAX;
  static YAP_Term gecode_FLOAT_VAR_DEGREE_SIZE_MIN;

  static inline FloatVarBranch
  gecode_FloatVarBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_FLOAT_VAR_SIZE_MIN)
	return FLOAT_VAR_SIZE_MIN();
      if ( t == gecode_FLOAT_VAR_SIZE_MAX)
	return FLOAT_VAR_SIZE_MAX();
      if ( t == gecode_FLOAT_VAR_NONE)
	return FLOAT_VAR_NONE();
      if ( t == gecode_FLOAT_VAR_DEGREE_MIN)
	return FLOAT_VAR_DEGREE_MIN();
      if ( t == gecode_FLOAT_VAR_DEGREE_MAX)
	return FLOAT_VAR_DEGREE_MAX();
      if ( t == gecode_FLOAT_VAR_MIN_MIN)
	return FLOAT_VAR_MIN_MIN();
      if ( t == gecode_FLOAT_VAR_MIN_MAX)
	return FLOAT_VAR_MIN_MAX();
      if ( t == gecode_FLOAT_VAR_MAX_MIN)
	return FLOAT_VAR_MAX_MIN();
      if ( t == gecode_FLOAT_VAR_MAX_MAX)
	return FLOAT_VAR_MAX_MAX();
      if ( t == gecode_FLOAT_VAR_DEGREE_SIZE_MIN)
	return FLOAT_VAR_DEGREE_SIZE_MIN();
      if ( t == gecode_FLOAT_VAR_DEGREE_SIZE_MAX)
	return FLOAT_VAR_DEGREE_SIZE_MAX();
    }
    cerr << "unsupported FLOAT VAR" << endl; exit(1);
  }

  static YAP_Term gecode_INT_VAL_MIN;
  static YAP_Term gecode_INT_VAL_MED;
  static YAP_Term gecode_INT_VAL_MAX;
  static YAP_Term gecode_INT_VAL_SPLIT_MIN;
  static YAP_Term gecode_INT_VAL_SPLIT_MAX;
  static YAP_Term gecode_INT_VAL_RANGE_MIN;
  static YAP_Term gecode_INT_VAL_RANGE_MAX;
  static YAP_Term gecode_INT_VALUES_MIN;
  static YAP_Term gecode_INT_VALUES_MAX;

  static inline IntValBranch
  gecode_IntValBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_INT_VAL_MIN)
	return INT_VAL_MIN();
      if ( t == gecode_INT_VAL_MED)
	return INT_VAL_MED();
      if ( t == gecode_INT_VAL_MAX)
	return INT_VAL_MAX();
      if ( t == gecode_INT_VAL_SPLIT_MIN)
	return INT_VAL_SPLIT_MIN();
      if ( t == gecode_INT_VAL_SPLIT_MAX)
	return INT_VAL_SPLIT_MAX();
      if ( t == gecode_INT_VAL_RANGE_MIN)
	return INT_VAL_RANGE_MIN();
      if ( t == gecode_INT_VAL_RANGE_MAX)
	return INT_VAL_RANGE_MAX();
      if ( t == gecode_INT_VALUES_MIN)
	return INT_VALUES_MIN();
      if ( t == gecode_INT_VALUES_MAX)
	return INT_VALUES_MAX();
    }
    cerr << "unsupported INT VAL" << endl; exit(1);
  }

  static YAP_Term gecode_BOOL_VAL_MIN;
  static YAP_Term gecode_BOOL_VAL_MAX;
  static YAP_Term gecode_BOOL_VAL_RND;

  static inline BoolValBranch
  gecode_BoolValBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_BOOL_VAL_MIN)
	return BOOL_VAL_MIN();
    if ( t == gecode_BOOL_VAL_MAX)
  return BOOL_VAL_MAX();
  if ( t == gecode_BOOL_VAL_RND)
return BOOL_VAL_RND(Rnd());
    }
    cerr << "unsupported BOOL VAL" << endl; exit(1);
  }
  //
  // static inline BoolVal&
  // gecode_BoolVal_from_term(YAP_Term t)
  // {
  //   return * (BoolVal*) YAP_OpaqueObjectFromTerm(t);
  // }
  //

  static YAP_Term gecode_FLOAT_VAL_SPLIT_MIN;
  static YAP_Term gecode_FLOAT_VAL_SPLIT_MAX;

  static inline FloatValBranch
  gecode_FloatValBranch_from_term(YAP_Term t)
  {
    if (YAP_IsAtomTerm(t)) {
      if ( t == gecode_FLOAT_VAL_SPLIT_MIN)
	return FLOAT_VAL_SPLIT_MIN();
      if ( t == gecode_FLOAT_VAL_SPLIT_MAX)
	return FLOAT_VAL_SPLIT_MAX();
    }
    cerr << "unsupported FLOAT VAL" << endl; exit(1);
  }

  static inline FloatVal&
  gecode_FloatVal_from_term(YAP_Term t)
  {
    return * (FloatVal*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline Symmetries&
  gecode_Symmetries_from_term(YAP_Term t)
  {
    return * (Symmetries*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline IntBranchFilter&
  gecode_IntBranchFilter_from_term(YAP_Term t)
  {
    return * (IntBranchFilter*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline BoolBranchFilter&
  gecode_BoolBranchFilter_from_term(YAP_Term t)
  {
    return * (BoolBranchFilter*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline FloatBranchFilter&
  gecode_FloatBranchFilter_from_term(YAP_Term t)
  {
    return * (FloatBranchFilter*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline SetVarValPrint&
  gecode_SetVarValPrint_from_term(YAP_Term t)
  {
    return * (SetVarValPrint*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline IntVarValPrint&
  gecode_IntVarValPrint_from_term(YAP_Term t)
  {
    return * (IntVarValPrint*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline BoolVarValPrint&
  gecode_BoolVarValPrint_from_term(YAP_Term t)
  {
    return * (BoolVarValPrint*) YAP_OpaqueObjectFromTerm(t);
  }

  static inline FloatVarValPrint&
  gecode_FloatVarValPrint_from_term(YAP_Term t)
  {
    return * (FloatVarValPrint*) YAP_OpaqueObjectFromTerm(t);
  }

  static YAP_opaque_tag_t gecode_engine_tag;
  static YAP_opaque_handler_t gecode_engine_handler;

  static RestartMode gecode_RestartMode_from_term(YAP_Term t);

  static YAP_Bool gecode_new_engine(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    bool restart = YAP_IntOfTerm(YAP_ArgOfTerm(1, arg3));
    double threads = YAP_FloatOfTerm(YAP_ArgOfTerm(2, arg3));
    unsigned int c_d = YAP_IntOfTerm(YAP_ArgOfTerm(3, arg3));
    unsigned int a_d = YAP_IntOfTerm(YAP_ArgOfTerm(4, arg3));
    unsigned int nogoods_limit = YAP_IntOfTerm(YAP_ArgOfTerm(6, arg3));
    bool clone = ( YAP_IntOfTerm(YAP_ArgOfTerm(7, arg3)) == 0 ?  FALSE : TRUE );
    RestartMode md;
    YAP_Term t = YAP_ArgOfTerm(5, arg3);
    if (YAP_IsAtomTerm(t)) {
      md = gecode_RestartMode_from_term(t);
    } else if (YAP_IsApplTerm(t)) {
      md = gecode_RestartMode_from_term(YAP_MkAtomTerm(YAP_NameOfFunctor(YAP_FunctorOfTerm(t))));
    } else {
      cerr << "bad engine cutoff option" << endl; exit(1);
    }
    Search::Cutoff* cutoff;
    YAP_Term t_s, t_b;
    switch (md) {
    case RM_CONSTANT:
      YAP_Term t_a;
      if (YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)) == 1 &&
	  YAP_IsIntTerm(t_a = YAP_ArgOfTerm(1,t))) {
	unsigned long int a = YAP_IntOfTerm(t_a);
	cutoff = Search::Cutoff::constant(a);
      } else {
	cerr << "bad parameter for constant" << endl; exit(1);
      }
      break;
    case RM_GEOMETRIC:
      if (YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)) == 2 &&
	  YAP_IsIntTerm(t_s = YAP_ArgOfTerm(1,t)) &&
	  YAP_IsIntTerm(t_b = YAP_ArgOfTerm(2,t))) {
	unsigned long int s = YAP_IntOfTerm(t_s);
	unsigned long int b = YAP_IntOfTerm(t_b);
	cutoff = Search::Cutoff::geometric(s,b);
      } else {
	cerr << "bad parameter for geometric" << endl; exit(1);
      }
      break;
    case RM_LUBY:
      if (YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)) == 1 &&
	  YAP_IsIntTerm(t_s = YAP_ArgOfTerm(1,t))) {
	unsigned long int s = YAP_IntOfTerm(t_s);
	cutoff = Search::Cutoff::luby(s);
      } else {
	cerr << "bad parameter for luby" << endl; exit(1);
      }
      break;
    case RM_LINEAR:
      if (YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)) == 1 &&
	  YAP_IsIntTerm(t_s = YAP_ArgOfTerm(1,t))) {
	unsigned long int s = YAP_IntOfTerm(t_s);
	cutoff = Search::Cutoff::linear(s);
      } else {
	cerr << "bad parameter for linear" << endl; exit(1);
      }
      break;
    default:
      cutoff = NULL;
    }
    Search::Options opt;
    opt.threads = threads;
    opt.c_d = c_d;
    opt.a_d = a_d;
    opt.cutoff = cutoff;
    opt.nogoods_limit = nogoods_limit;
    opt.clone = clone;
    opt.stop = NULL;
    GenericSpace* space = gecode_Space_from_term(arg1);
    GenericEngine* engine = space->new_engine(restart,opt);
    YAP_Term y_engine =
      YAP_NewOpaqueObject(gecode_engine_tag, sizeof(GenericEngine*));
    GenericEngine** ptr =
      (GenericEngine**) YAP_OpaqueObjectFromTerm(y_engine);
    *ptr = engine;
    return YAP_Unify(arg2, y_engine);
  }

  static inline GenericEngine*
  gecode_engine_from_term(YAP_Term t)
  {
    return * (GenericEngine**) YAP_OpaqueObjectFromTerm(t);
  }

  static YAP_Bool gecode_engine_fail_handler(YAP_Term t)
  {
    delete *(GenericEngine**)YAP_OpaqueObjectFromTerm(YAP_HeadOfTerm(t));
    return TRUE;
  }

  static YAP_Bool
  gecode_engine_write_handler
  (FILE *stream, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<engine %p>", p);
    return TRUE;
  }

  static YAP_Bool gecode_engine_search(void)
  {
    GenericEngine* engine = gecode_engine_from_term(YAP_ARG1);
    GenericSpace* space = engine->next();
    if (space)
      {
	YAP_Term term = gecode_term_from_space(space);
	return YAP_Unify(YAP_ARG2, term);
      }
    else YAP_cut_fail();
  }

#ifdef DISJUNCTOR
  static  YAP_Bool gecode_new_disjunctor(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_disjunctor_tag, sizeof(YapDisjunctor));
    new (YAP_OpaqueObjectFromTerm(term)) YapDisjunctor(space);
    return YAP_Unify(result, term);
  }

  static YAP_Bool
  gecode_disjunctor_write_handler
  (FILE *stream, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<disjunctor %p>", p);
    return TRUE;
  }

  static YAP_Bool gecode_new_clause(void)
  {
    YAP_Term result = YAP_ARG1;
    YapDisjunctor& disj = gecode_YapDisjunctor_from_term(YAP_ARG2);
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_disjunctor_clause_tag, sizeof(Clause));
    new (YAP_OpaqueObjectFromTerm(term)) Clause(*disj.home,disj.disj);
    return YAP_Unify(result, term);
  }

  static YAP_Bool
  gecode_clause_write_handler
  (FILE *stream_, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<clause %p>", p);
    return TRUE;
  }

  static IntVar gecode_IntVar_from_term(GenericSpace*,YAP_Term);
  static BoolVar gecode_BoolVar_from_term(GenericSpace*,YAP_Term);
  static FloatVar gecode_FloatVar_from_term(GenericSpace*,YAP_Term);
  static SetVar gecode_SetVar_from_term(GenericSpace*,YAP_Term);

  static YAP_Bool gecode_clause_intvar_forward(void)
  {
    Clause& clause = gecode_Clause_from_term(YAP_ARG1);
    GenericSpace* outer = clause.generic_parent();
    GenericSpace* inner = clause.generic_space();
    IntVar outer_var = gecode_IntVar_from_term(outer, YAP_ARG2);
    IntVar inner_var = gecode_IntVar_from_term(inner, YAP_ARG3);
    clause.forward(outer_var,inner_var);
    return TRUE;
  }

  static YAP_Bool gecode_clause_boolvar_forward(void)
  {
    Clause& clause = gecode_Clause_from_term(YAP_ARG1);
    GenericSpace* outer = clause.generic_parent();
    GenericSpace* inner = clause.generic_space();
    BoolVar outer_var = gecode_BoolVar_from_term(outer, YAP_ARG2);
    BoolVar inner_var = gecode_BoolVar_from_term(inner, YAP_ARG3);
    clause.forward(outer_var,inner_var);
    return TRUE;
  }

  static YAP_Bool gecode_clause_setvar_forward(void)
  {
    Clause& clause = gecode_Clause_from_term(YAP_ARG1);
    GenericSpace* outer = clause.generic_parent();
    GenericSpace* inner = clause.generic_space();
    SetVar outer_var = gecode_SetVar_from_term(outer, YAP_ARG2);
    SetVar inner_var = gecode_SetVar_from_term(inner, YAP_ARG3);
    clause.forward(outer_var,inner_var);
    return TRUE;
  }
#endif

  static YAP_Bool gecode_new_intvar_from_bounds(void)
  {
    YAP_Term ivar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int lo = YAP_IntOfTerm(YAP_ARG3);
    int hi = YAP_IntOfTerm(YAP_ARG4);
    int i = space->new_ivar(lo, hi);
    YAP_Term y_i = YAP_MkIntTerm(i);
    return YAP_Unify(ivar, y_i);
  }

  static YAP_Bool gecode_new_floatvar_from_bounds(void)
  {
    YAP_Term ivar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    double lo = YAP_FloatOfTerm(YAP_ARG3);
    double hi = YAP_FloatOfTerm(YAP_ARG4);
    int i = space->new_fvar(lo, hi);
    YAP_Term y_i = YAP_MkIntTerm(i);
    return YAP_Unify(ivar, y_i);
  }

  static int
  gecode_list_length(YAP_Term l)
  {
    int n = 0;
    while (YAP_IsPairTerm(l))
      {
	n += 1;
	l = YAP_TailOfTerm(l);
      }
    return n;
  }

  static IntSet
  gecode_IntSet_from_term(YAP_Term specs)
  {
    int n = gecode_list_length(specs);
    SPECARRAY(r,n);
    int i = 0;
    while (YAP_IsPairTerm(specs))
      {
	YAP_Term head = YAP_HeadOfTerm(specs);
	specs = YAP_TailOfTerm(specs);
	SPECARRAYELEM(r,i,0) = YAP_IntOfTerm(YAP_ArgOfTerm(1, head));
	SPECARRAYELEM(r,i,1) = YAP_IntOfTerm(YAP_ArgOfTerm(2, head));
	i += 1;
      }
    return IntSet(SPECARRAYDEREF(r), n);
  }

  static YAP_Bool gecode_new_intvar_from_intset(void)
  {
    YAP_Term ivar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    YAP_Term specs = YAP_ARG3;
    IntSet iset = gecode_IntSet_from_term(specs);
    int i = space->new_ivar(iset);
    return YAP_Unify(ivar, YAP_MkIntTerm(i));
  }

  static YAP_Bool gecode_new_boolvar(void)
  {
    YAP_Term bvar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int i = space->new_bvar();
    return YAP_Unify(bvar, YAP_MkIntTerm(i));
  }

  static YAP_Bool gecode_new_setvar_1(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    int LubMin = YAP_IntOfTerm(YAP_ARG5);
    int LubMax = YAP_IntOfTerm(YAP_ARG6);
    int CardMin= YAP_IntOfTerm(YAP_ARG7);
    int CardMax= YAP_IntOfTerm(YAP_ARG8);
    int idx = space->new_svar(GlbMin,GlbMax,LubMin,LubMax,CardMin,CardMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_2(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    int LubMin = YAP_IntOfTerm(YAP_ARG5);
    int LubMax = YAP_IntOfTerm(YAP_ARG6);
    int CardMin= YAP_IntOfTerm(YAP_ARG7);
    int idx = space->new_svar(GlbMin,GlbMax,LubMin,LubMax,CardMin);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_3(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    int LubMin = YAP_IntOfTerm(YAP_ARG5);
    int LubMax = YAP_IntOfTerm(YAP_ARG6);
    int idx = space->new_svar(GlbMin,GlbMax,LubMin,LubMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_4(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    int LubMin = YAP_IntOfTerm(YAP_ARG4);
    int LubMax = YAP_IntOfTerm(YAP_ARG5);
    int CardMin = YAP_IntOfTerm(YAP_ARG6);
    int CardMax = YAP_IntOfTerm(YAP_ARG7);
    int idx = space->new_svar(Glb,LubMin,LubMax,CardMin,CardMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_5(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    int LubMin = YAP_IntOfTerm(YAP_ARG4);
    int LubMax = YAP_IntOfTerm(YAP_ARG5);
    int CardMin = YAP_IntOfTerm(YAP_ARG6);
    int idx = space->new_svar(Glb,LubMin,LubMax,CardMin);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_6(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    int LubMin = YAP_IntOfTerm(YAP_ARG4);
    int LubMax = YAP_IntOfTerm(YAP_ARG5);
    int idx = space->new_svar(Glb,LubMin,LubMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_7(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG5);
    int CardMin = YAP_IntOfTerm(YAP_ARG6);
    int CardMax = YAP_IntOfTerm(YAP_ARG7);
    int idx = space->new_svar(GlbMin,GlbMax,Lub,CardMin,CardMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_8(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG5);
    int CardMin = YAP_IntOfTerm(YAP_ARG6);
    int idx = space->new_svar(GlbMin,GlbMax,Lub,CardMin);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_9(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG5);
    int idx = space->new_svar(GlbMin,GlbMax,Lub);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_10(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG4);
    int CardMin = YAP_IntOfTerm(YAP_ARG5);
    int CardMax = YAP_IntOfTerm(YAP_ARG6);
    int idx = space->new_svar(Glb,Lub,CardMin,CardMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_11(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG4);
    int CardMin = YAP_IntOfTerm(YAP_ARG5);
    int idx = space->new_svar(Glb,Lub,CardMin);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_new_setvar_12(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG4);
    int idx = space->new_svar(Glb,Lub);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static YAP_Bool gecode_space_minimize(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    space->minimize(i);
    return TRUE;
  }

  static YAP_Bool gecode_space_maximize(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    space->maximize(i);
    return TRUE;
  }

  static YAP_Bool gecode_space_minimize_ratio(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    int j = YAP_IntOfTerm(YAP_ARG3);
    space->minimize(i,j);
    return TRUE;
  }

  static YAP_Bool gecode_space_maximize_ratio(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    int j = YAP_IntOfTerm(YAP_ARG3);
    space->maximize(i,j);
    return TRUE;
  }

  static IntVar
  gecode_IntVar_from_term(GenericSpace* space, YAP_Term x)
  {
    int i = YAP_IntOfTerm(x);
    return space->get_ivar(i);
  }

  static BoolVar
  gecode_BoolVar_from_term(GenericSpace* space, YAP_Term x)
  {
    int i = YAP_IntOfTerm(x);
    return space->get_bvar(i);
  }

  static SetVar
  gecode_SetVar_from_term(GenericSpace* space, YAP_Term x)
  {
    int i = YAP_IntOfTerm(x);
    return space->get_svar(i);
  }

  static IntVarArgs
  gecode_IntVarArgs_from_term(GenericSpace* space, YAP_Term l)
  {
    int n = gecode_list_length(l);
    IntVarArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	int idx = YAP_IntOfTerm(YAP_HeadOfTerm(l));
	v[i] = space->get_ivar(idx);
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static BoolVarArgs
  gecode_BoolVarArgs_from_term(GenericSpace* space, YAP_Term l)
  {
    int n = gecode_list_length(l);
    BoolVarArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	int idx = YAP_IntOfTerm(YAP_HeadOfTerm(l));
	v[i] = space->get_bvar(idx);
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static FloatVar
  gecode_FloatVar_from_term(GenericSpace* space, YAP_Term x)
  {
    int i = YAP_IntOfTerm(x);
    return space->get_fvar(i);
  }

  static FloatVarArgs
  gecode_FloatVarArgs_from_term(GenericSpace* space, YAP_Term l)
  {
    int n = gecode_list_length(l);
    FloatVarArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	int idx = YAP_IntOfTerm(YAP_HeadOfTerm(l));
	v[i] = space->get_fvar(idx);
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static FloatValArgs
  gecode_FloatValArgs_from_term(YAP_Term l)
  {
    int n = gecode_list_length(l);
    FloatValArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	YAP_Term t = YAP_HeadOfTerm(l);
	v[i] = * (FloatVal*) YAP_OpaqueObjectFromTerm(t);
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static SetVarArgs
  gecode_SetVarArgs_from_term(GenericSpace* space, YAP_Term l)
  {
    int n = gecode_list_length(l);
    SetVarArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	int idx = YAP_IntOfTerm(YAP_HeadOfTerm(l));
	v[i] = space->get_svar(idx);
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static IntArgs
  gecode_IntArgs_from_term(YAP_Term l)
  {
    int n = gecode_list_length(l);
    IntArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	int idx = YAP_IntOfTerm(YAP_HeadOfTerm(l));
	v[i] = idx;
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static IntSetArgs
  gecode_IntSetArgs_from_term(YAP_Term l)
  {
    int n = gecode_list_length(l);
    IntSetArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	IntSet s = gecode_IntSet_from_term(YAP_HeadOfTerm(l));
	v[i] = s;
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static TaskType gecode_TaskType_from_term(YAP_Term);

  static TaskTypeArgs
  gecode_TaskTypeArgs_from_term(YAP_Term l)
  {
    int n = gecode_list_length(l);
    TaskTypeArgs v(n);
    int i = 0;
    while (YAP_IsPairTerm(l))
      {
	TaskType tt = gecode_TaskType_from_term(YAP_HeadOfTerm(l));
	v[i] = tt;
	l = YAP_TailOfTerm(l);
	i += 1;
      }
    return v;
  }

  static YAP_Term gecode_TRUE;
  static YAP_Term gecode_FALSE;

  static bool
  gecode_bool_from_term(YAP_Term X)
  {
    if (X==gecode_TRUE || YAP_MkIntTerm(1)) return true;
    if (X==gecode_FALSE || YAP_MkIntTerm(0)) return false;
    cerr << "this should never happen" << endl; exit(1);
  }

  static YAP_Bool gecode_space_use_keep_index(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    GenericSpace* space = gecode_Space_from_term(arg1);
    return YAP_Unify(arg2,(space->use_keep_index()
			   ?gecode_TRUE:gecode_FALSE));
  }

  static YAP_Bool gecode_intvar_keep(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    GenericSpace* space = gecode_Space_from_term(arg1);
    int idx = YAP_IntOfTerm(arg2);
    int kidx = space->keep_ivar(idx);
    return YAP_Unify(arg3,YAP_MkIntTerm(kidx));
  }

  static YAP_Bool gecode_boolvar_keep(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    GenericSpace* space = gecode_Space_from_term(arg1);
    int idx = YAP_IntOfTerm(arg2);
    int kidx = space->keep_bvar(idx);
    return YAP_Unify(arg3,YAP_MkIntTerm(kidx));
  }

  static YAP_Bool gecode_setvar_keep(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    GenericSpace* space = gecode_Space_from_term(arg1);
    int idx = YAP_IntOfTerm(arg2);
    int kidx = space->keep_svar(idx);
    return YAP_Unify(arg3,YAP_MkIntTerm(kidx));
  }

  // INFO ON INTVARS
  static YAP_Bool gecode_intvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static YAP_Bool gecode_intvar_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.min()));
  }

  static YAP_Bool gecode_intvar_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.max()));
  }

  static YAP_Bool gecode_intvar_med(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.med()));
  }

  static YAP_Bool gecode_intvar_val(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.val()));
  }

  static YAP_Bool gecode_intvar_size(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.size()));
  }

  static YAP_Bool gecode_intvar_width(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.width()));
  }

  static YAP_Bool gecode_intvar_regret_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_min()));
  }

  static YAP_Bool gecode_intvar_regret_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_max()));
  }

  static YAP_Functor gecode_COMMA2;

  static YAP_Bool gecode_intvar_ranges(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    int n = 0;
    { IntVarRanges it(x); while (it()) { ++n; ++it; } }
    DYNARRAY(int,min,n);
    DYNARRAY(int,max,n);
    { IntVarRanges it(x); int i=0;
      while (it()) { min[i]=it.min(); max[i]=it.max(); ++it; ++i; } }
    YAP_Term lst = YAP_TermNil();
    for (;n--;)
      {
	YAP_Term args[2];
	args[0] = YAP_MkIntTerm(min[n]);
	args[1] = YAP_MkIntTerm(max[n]);
	YAP_Term range = YAP_MkApplTerm(gecode_COMMA2,2,args);
	lst = YAP_MkPairTerm(range,lst);
      }
    return YAP_Unify(result,lst);
  }

  static YAP_Bool gecode_intvar_values(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    int n = x.size();
    DYNARRAY(int,a,n);
    { IntVarValues it(x); int i=0;
      while (it()) { a[i]=it.val(); ++it; ++i; } }
    YAP_Term lst = YAP_TermNil();
    for (;n--;)
      {
	lst = YAP_MkPairTerm(YAP_MkIntTerm(a[n]),lst);
      }
    return YAP_Unify(result,lst);
  }

  // INFO ON BOOLVARS
  static YAP_Bool gecode_boolvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static YAP_Bool gecode_boolvar_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.min()));
  }

  static YAP_Bool gecode_boolvar_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.max()));
  }

  static YAP_Bool gecode_boolvar_med(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.med()));
  }

  static YAP_Bool gecode_boolvar_val(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.val()));
  }

  static YAP_Bool gecode_boolvar_size(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.size()));
  }

  static YAP_Bool gecode_boolvar_width(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.width()));
  }

  static YAP_Bool gecode_boolvar_regret_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_min()));
  }

  static YAP_Bool gecode_boolvar_regret_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_max()));
  }

  // INFO ON SETVARS
  static YAP_Bool gecode_setvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static YAP_Bool gecode_setvar_glbSize(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.glbSize()));
  }

  static YAP_Bool gecode_setvar_lubSize(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.lubSize()));
  }

  static YAP_Bool gecode_setvar_unknownSize(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.unknownSize()));
  }

  static YAP_Bool gecode_setvar_cardMin(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.cardMin()));
  }

  static YAP_Bool gecode_setvar_cardMax(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.cardMax()));
  }

  static YAP_Bool gecode_setvar_lubMin(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.lubMin()));
  }

  static YAP_Bool gecode_setvar_lubMax(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.lubMax()));
  }

  static YAP_Bool gecode_setvar_glbMin(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.glbMin()));
  }

  static YAP_Bool gecode_setvar_glbMax(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.glbMax()));
  }

  static YAP_Bool gecode_setvar_glb_ranges(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    int n = 0;
    { SetVarGlbRanges it(x); while (it()) { ++n; ++it; } }
    DYNARRAY(int,min,n);
    DYNARRAY(int,max,n);
    { SetVarGlbRanges it(x); int i=0;
      while (it()) { min[i]=it.min(); max[i]=it.max(); ++it; ++i; } }
    YAP_Term lst = YAP_TermNil();
    for (;n--;)
      {
	YAP_Term args[2];
	args[0] = YAP_MkIntTerm(min[n]);
	args[1] = YAP_MkIntTerm(max[n]);
	YAP_Term range = YAP_MkApplTerm(gecode_COMMA2,2,args);
	lst = YAP_MkPairTerm(range,lst);
      }
    return YAP_Unify(result,lst);
  }

  static YAP_Bool gecode_setvar_lub_ranges(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    int n = 0;
    { SetVarLubRanges it(x); while (it()) { ++n; ++it; } }
    DYNARRAY(int,min,n);
    DYNARRAY(int,max,n);
    { SetVarLubRanges it(x); int i=0;
      while (it()) { min[i]=it.min(); max[i]=it.max(); ++it; ++i; } }
    YAP_Term lst = YAP_TermNil();
    for (;n--;)
      {
	YAP_Term args[2];
	args[0] = YAP_MkIntTerm(min[n]);
	args[1] = YAP_MkIntTerm(max[n]);
	YAP_Term range = YAP_MkApplTerm(gecode_COMMA2,2,args);
	lst = YAP_MkPairTerm(range,lst);
      }
    return YAP_Unify(result,lst);
  }

  static YAP_Bool gecode_setvar_unknown_ranges(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    int n = 0;
    { SetVarUnknownRanges it(x); while (it()) { ++n; ++it; } }
    DYNARRAY(int,min,n);
    DYNARRAY(int,max,n);
    { SetVarUnknownRanges it(x); int i=0;
      while (it()) { min[i]=it.min(); max[i]=it.max(); ++it; ++i; } }
    YAP_Term lst = YAP_TermNil();
    for (;n--;)
      {
	YAP_Term args[2];
	args[0] = YAP_MkIntTerm(min[n]);
	args[1] = YAP_MkIntTerm(max[n]);
	YAP_Term range = YAP_MkApplTerm(gecode_COMMA2,2,args);
	lst = YAP_MkPairTerm(range,lst);
      }
    return YAP_Unify(result,lst);
  }

  static YAP_Bool gecode_setvar_glb_values(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    DYNARRAY(YAP_Term,elems,x.glbSize());
    SetVarGlbValues it(x);
    int n = 0;
    while (it()) { elems[n] = YAP_MkIntTerm(it.val()); ++it; ++n; }
    YAP_Term lst = YAP_TermNil();
    for (;n--;) lst = YAP_MkPairTerm(elems[n],lst);
    return YAP_Unify(result,lst);
  }

  static YAP_Bool gecode_setvar_lub_values(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    DYNARRAY(YAP_Term,elems,x.lubSize());
    SetVarLubValues it(x);
    int n = 0;
    while (it()) { elems[n] = YAP_MkIntTerm(it.val()); ++it; ++n; }
    YAP_Term lst = YAP_TermNil();
    for (;n--;) lst = YAP_MkPairTerm(elems[n],lst);
    return YAP_Unify(result,lst);
  }

  static YAP_Bool gecode_setvar_unknown_values(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    DYNARRAY(YAP_Term,elems,x.unknownSize());
    SetVarUnknownValues it(x);
    int n = 0;
    while (it()) { elems[n] = YAP_MkIntTerm(it.val()); ++it; ++n; }
    YAP_Term lst = YAP_TermNil();
    for (;n--;) lst = YAP_MkPairTerm(elems[n],lst);
    return YAP_Unify(result,lst);
  }

  // INFO ON FLOATVARS
  static YAP_Bool gecode_floatvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    FloatVar x = gecode_FloatVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static YAP_Bool gecode_floatvar_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    FloatVar x = gecode_FloatVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkFloatTerm(x.min()));
  }

  static YAP_Bool gecode_floatvar_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    FloatVar x = gecode_FloatVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkFloatTerm(x.max()));
  }

  static YAP_Bool gecode_floatvar_med(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    FloatVar x = gecode_FloatVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkFloatTerm(x.med()));
  }

  static YAP_Bool gecode_floatvar_size(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    FloatVar x = gecode_FloatVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkFloatTerm(x.size()));
  }

  static inline Reify
  gecode_Reify_from_term(YAP_Term t)
  {
    return * (Reify*) YAP_OpaqueObjectFromTerm(t);
  }

  #define gecode_int_from_term YAP_IntOfTerm

  #define gecode_double_from_term YAP_FloatOfTerm

#include "gecode_yap_cc_forward_auto_generated.icc"
#include "gecode_yap_cc_impl_auto_generated.icc"

  static YAP_opaque_tag_t gecode_reify_tag;
  static YAP_opaque_handler_t gecode_reify_handler;

  static YAP_Bool
  gecode_reify_write_handler
  (FILE *stream, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<reify %p>", p);
    return TRUE;
  }

  static YAP_Term gecode_term_from_reify(Reify r)
  {
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_reify_tag, sizeof(Reify));
    Reify *ptr =
      (Reify*) YAP_OpaqueObjectFromTerm(term);
    *ptr = r;
    return term;
  }

  static YAP_Bool gecode_new_reify(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    BoolVar b = gecode_BoolVar_from_term(space, YAP_ARG2);
    ReifyMode flag = gecode_ReifyMode_from_term(YAP_ARG3);
    Reify r = Reify(b,flag);

    YAP_Term term = gecode_term_from_reify(r);
    return YAP_Unify(YAP_ARG4, term);
  }

  static YAP_opaque_tag_t gecode_tupleset_tag;
  static YAP_opaque_handler_t gecode_tupleset_handler;

  static YAP_Bool gecode_tupleset_fail_handler(YAP_Term p)
  {
    return TRUE;
  }

  static YAP_Bool
  gecode_tupleset_write_handler
  (FILE *stream, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<tupleset %p>", p);
    return TRUE;
  }

  static YAP_Bool gecode_new_tupleset(void)
  {
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_tupleset_tag, sizeof(TupleSet));
    TupleSet *ts = new (YAP_OpaqueObjectFromTerm(term)) TupleSet;
    YAP_Term t = YAP_ARG1;
    while (YAP_IsPairTerm( t ) ) {
      YAP_Term l = YAP_HeadOfTerm(t);
      int n = gecode_list_length(l);
      int i = 0;
      IntArgs is(n);
      while (YAP_IsPairTerm( l ) ) {
	YAP_Term ll = YAP_HeadOfTerm(l);
	if (!YAP_IsIntTerm(ll)) {
	    cerr << "non-integer on tuple set" << endl; exit(1);
	}
	is[i++] = YAP_IntOfTerm(ll);
	l = YAP_TailOfTerm(l);
      }
      ts->add(is);
      if (l != YAP_TermNil()) {
	  cerr << "non-list on tuple set" << endl; exit(1);
      }
      t = YAP_TailOfTerm(t);
    }
    if (t != YAP_TermNil()) {
      cerr << "non-list on tuple set" << endl; exit(1);
    }
    ts->finalize();
    return YAP_Unify(YAP_ARG2, term);
  }

  static YAP_opaque_tag_t gecode_dfa_tag;
  static YAP_opaque_handler_t gecode_dfa_handler;

  static YAP_Bool gecode_dfa_fail_handler(YAP_Term p)
  {
    return TRUE;
  }

  static YAP_Bool
  gecode_dfa_write_handler
  (FILE *stream, YAP_opaque_tag_t type, void *p, int flags)
  {
    fprintf(stream,"<dfa %p>", p);
    return TRUE;
  }

  static YAP_Bool gecode_new_dfa(void)
  {
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_dfa_tag, sizeof(DFA));
    //    DFA ts = new (YAP_OpaqueObjectFromTerm(term)) DFA;
    YAP_Term t2 = YAP_ARG2;
    int s0 = YAP_IntOfTerm(t2);
    YAP_Term t3 = YAP_ARG3;
    int n = gecode_list_length(t3), i=0;
    DFA::Transition t[1024];
    if (n > 1024) cerr<< "DFA too long" << endl;
    while (YAP_IsPairTerm( t3 ) ) {
      YAP_Term tt = YAP_HeadOfTerm(t3);
      int is, sy, os;
      is = YAP_IntOfTerm(YAP_ArgOfTerm(1,tt));
      sy = YAP_IntOfTerm(YAP_ArgOfTerm(2,tt));
      os = YAP_IntOfTerm(YAP_ArgOfTerm(3,tt));
      t[i++] = DFA::Transition(is, sy, os);
      t3 = YAP_TailOfTerm(t3);
    }
    if (t3 != YAP_TermNil()) {
      cerr << "non-list on DFA" << endl; exit(1);
    }
    YAP_Term t4 = YAP_ARG4;
    n = gecode_list_length(t4)+1;
    i=0;
    int s[n];
    s[n-1] = -1;
    while (YAP_IsPairTerm( t4 ) ) {
      YAP_Term tt = YAP_HeadOfTerm(t4);
      s[i++] = YAP_IntOfTerm(tt);
      t4 = YAP_TailOfTerm(t4);
    }
    if (t4 != YAP_TermNil()) {
      cerr << "non-list on DFA" << endl; exit(1);
    }
    new (YAP_OpaqueObjectFromTerm(term)) DFA(s0, t, s);
    return YAP_Unify(YAP_ARG1, term);
  }

  void gecode_init(void)
  {
    { YAP_Atom X= YAP_LookupAtom("true");
      gecode_TRUE = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("false");
      gecode_FALSE = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom(",");
      YAP_AtomGetHold(X);
      gecode_COMMA2 = YAP_MkFunctor(X,2); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_NONE");
      gecode_INT_VAR_NONE = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_DEGREE_MIN");
      gecode_INT_VAR_DEGREE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_DEGREE_MAX");
      gecode_INT_VAR_DEGREE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_MIN_MIN");
      gecode_INT_VAR_MIN_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_MIN_MAX");
      gecode_INT_VAR_MIN_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_MAX_MIN");
      gecode_INT_VAR_MAX_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_MAX_MAX");
      gecode_INT_VAR_MAX_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_SIZE_MIN");
      gecode_INT_VAR_SIZE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_SIZE_MAX");
      gecode_INT_VAR_SIZE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_DEGREE_SIZE_MIN");
      gecode_INT_VAR_DEGREE_SIZE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_DEGREE_SIZE_MAX");
      gecode_INT_VAR_DEGREE_SIZE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_REGRET_MIN_MIN");
      gecode_INT_VAR_REGRET_MIN_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_REGRET_MIN_MAX");
      gecode_INT_VAR_REGRET_MIN_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_REGRET_MAX_MIN");
      gecode_INT_VAR_REGRET_MAX_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAR_REGRET_MAX_MAX");
      gecode_INT_VAR_REGRET_MAX_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_MIN");
      gecode_INT_VAL_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_MED");
      gecode_INT_VAL_MED = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_MAX");
      gecode_INT_VAL_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_SPLIT_MIN");
      gecode_INT_VAL_SPLIT_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_SPLIT_MAX");
      gecode_INT_VAL_SPLIT_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_RANGE_MIN");
      gecode_INT_VAL_RANGE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VAL_RANGE_MAX");
      gecode_INT_VAL_RANGE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VALUES_MIN");
      gecode_INT_VALUES_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("INT_VALUES_MAX");
      gecode_INT_VALUES_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
      { YAP_Atom X= YAP_LookupAtom("BOOL_VAL_MIN");
        gecode_BOOL_VAL_MIN = YAP_MkAtomTerm(X);
        YAP_AtomGetHold(X); }
        { YAP_Atom X= YAP_LookupAtom("BOOL_VAL_RND");
          gecode_BOOL_VAL_RND = YAP_MkAtomTerm(X);
          YAP_AtomGetHold(X); }
      { YAP_Atom X= YAP_LookupAtom("SET_VAR_NONE");
      gecode_SET_VAR_NONE = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_DEGREE_MIN");
      gecode_SET_VAR_DEGREE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_DEGREE_MAX");
      gecode_SET_VAR_DEGREE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_MIN_MIN");
      gecode_SET_VAR_MIN_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_MIN_MAX");
      gecode_SET_VAR_MIN_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_MAX_MIN");
      gecode_SET_VAR_MAX_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_MAX_MAX");
      gecode_SET_VAR_MAX_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_SIZE_MIN");
      gecode_SET_VAR_SIZE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_SIZE_MAX");
      gecode_SET_VAR_SIZE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_DEGREE_SIZE_MIN");
      gecode_SET_VAR_DEGREE_SIZE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAR_DEGREE_SIZE_MAX");
      gecode_SET_VAR_DEGREE_SIZE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAL_MIN_INC");
      gecode_SET_VAL_MIN_INC = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAL_MED_INC");
      gecode_SET_VAL_MED_INC = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAL_MAX_INC");
      gecode_SET_VAL_MAX_INC = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAL_MIN_EXC");
      gecode_SET_VAL_MIN_EXC = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAL_MED_EXC");
      gecode_SET_VAL_MED_EXC = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("SET_VAL_MAX_EXC");
      gecode_SET_VAL_MAX_EXC = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
          { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_NONE");
      gecode_BOOL_VAR_NONE = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
          { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_RND");
      gecode_BOOL_VAR_RND = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    //    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_MERIT_MIN");
    //   gecode_BOOL_VAR_MERIT_MIN = YAP_MkAtomTerm(X);
    //   YAP_AtomGetHold(X); }
    // { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_MERIT_MAX");
    //   gecode_BOOL_VAR_MERIT_MAX = YAP_MkAtomTerm(X);
    //  YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_DEGREE_MIN");
      gecode_BOOL_VAR_DEGREE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_DEGREE_MAX");
      gecode_BOOL_VAR_DEGREE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
      { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_AFC_MIN");
      gecode_BOOL_VAR_AFC_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_AFC_MAX");
      gecode_BOOL_VAR_AFC_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_ACTION_MIN");
      gecode_BOOL_VAR_ACTION_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_ACTION_MAX");
      gecode_BOOL_VAR_MAX_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_CHB_MIN");
      gecode_BOOL_VAR_CHB_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("BOOL_VAR_CHB_MAX");
      gecode_BOOL_VAR_CHB_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }


    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_NONE");
      gecode_FLOAT_VAR_NONE = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_DEGREE_MIN");
      gecode_FLOAT_VAR_DEGREE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_DEGREE_MAX");
      gecode_FLOAT_VAR_DEGREE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_MIN_MIN");
      gecode_FLOAT_VAR_MIN_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_MIN_MAX");
      gecode_FLOAT_VAR_MIN_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_MAX_MIN");
      gecode_FLOAT_VAR_MAX_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_MAX_MAX");
      gecode_FLOAT_VAR_MAX_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_SIZE_MIN");
      gecode_FLOAT_VAR_SIZE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_SIZE_MAX");
      gecode_FLOAT_VAR_SIZE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_DEGREE_SIZE_MIN");
      gecode_FLOAT_VAR_DEGREE_SIZE_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAR_DEGREE_SIZE_MAX");
      gecode_FLOAT_VAR_DEGREE_SIZE_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAL_SPLIT_MIN");
      gecode_FLOAT_VAL_SPLIT_MIN = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
    { YAP_Atom X= YAP_LookupAtom("FLOAT_VAL_SPLIT_MAX");
      gecode_FLOAT_VAL_SPLIT_MAX = YAP_MkAtomTerm(X);
      YAP_AtomGetHold(X); }
#include "gecode_yap_cc_init_auto_generated.icc"
    // opaque spaces
    gecode_space_handler.fail_handler = gecode_space_fail_handler;
    gecode_space_handler.write_handler = gecode_space_write_handler;
    gecode_space_tag = YAP_NewOpaqueType(&gecode_space_handler);
    YAP_UserCPredicate("gecode_new_space", gecode_new_space, 1);
    // opaque engines
    gecode_engine_handler.fail_handler = gecode_engine_fail_handler;
    gecode_engine_handler.write_handler = gecode_engine_write_handler;
    gecode_engine_tag = YAP_NewOpaqueType(&gecode_engine_handler);
    YAP_UserCPredicate("gecode_new_engine", gecode_new_engine, 3);
    // opaque reifications
    gecode_reify_handler.fail_handler = NULL;
    gecode_reify_handler.write_handler = gecode_reify_write_handler;
    gecode_reify_tag = YAP_NewOpaqueType(&gecode_reify_handler);
    YAP_UserCPredicate("gecode_new_reify", gecode_new_reify, 4);
    // Opaque TupleSet
    gecode_tupleset_handler.fail_handler = gecode_tupleset_fail_handler;
    gecode_tupleset_handler.write_handler = gecode_tupleset_write_handler;
    gecode_tupleset_tag = YAP_NewOpaqueType(&gecode_tupleset_handler);
    YAP_UserCPredicate("gecode_new_tupleset", gecode_new_tupleset, 2);
    // Opaque DFA
    gecode_dfa_handler.fail_handler = gecode_dfa_fail_handler;
    gecode_dfa_handler.write_handler = gecode_dfa_write_handler;
    gecode_dfa_tag = YAP_NewOpaqueType(&gecode_dfa_handler);
    YAP_UserCPredicate("gecode_new_dfa", gecode_new_dfa, 4);
#ifdef DISJUNCTOR
    // opaque disjunctors and clauses
    gecode_disjunctor_handler.write_handler = gecode_disjunctor_write_handler;
    gecode_disjunctor_tag = YAP_NewOpaqueType(&gecode_disjunctor_handler);
    gecode_disjunctor_clause_handler.write_handler = gecode_clause_write_handler;
    gecode_disjunctor_clause_tag =
      YAP_NewOpaqueType(&gecode_disjunctor_clause_handler);
    YAP_UserCPredicate("gecode_new_disjunctor", gecode_new_disjunctor, 2);
    YAP_UserCPredicate("gecode_new_clause", gecode_new_clause, 2);
    YAP_UserCPredicate("gecode_clause_intvar_forward", gecode_clause_intvar_forward, 3);
    YAP_UserCPredicate("gecode_clause_boolvar_forward", gecode_clause_boolvar_forward, 3);
    YAP_UserCPredicate("gecode_clause_setvar_forward", gecode_clause_setvar_forward, 3);
#endif
    // backtracking search using an engine
    YAP_UserBackCutCPredicate("gecode_engine_search",
			      gecode_engine_search, gecode_engine_search,
			      NULL, 2, 0);
    // creating variables
    YAP_UserCPredicate("gecode_new_intvar_from_bounds",
		       gecode_new_intvar_from_bounds, 4);
    YAP_UserCPredicate("gecode_new_intvar_from_intset",
		       gecode_new_intvar_from_intset, 3);
    YAP_UserCPredicate("gecode_new_floatvar_from_bounds",
		       gecode_new_floatvar_from_bounds, 4);
    YAP_UserCPredicate("gecode_new_boolvar", gecode_new_boolvar, 2);
    YAP_UserCPredicate("gecode_new_setvar_1", gecode_new_setvar_1, 8);
    YAP_UserCPredicate("gecode_new_setvar_2", gecode_new_setvar_2, 7);
    YAP_UserCPredicate("gecode_new_setvar_3", gecode_new_setvar_3, 6);
    YAP_UserCPredicate("gecode_new_setvar_4", gecode_new_setvar_4, 7);
    YAP_UserCPredicate("gecode_new_setvar_5", gecode_new_setvar_5, 6);
    YAP_UserCPredicate("gecode_new_setvar_6", gecode_new_setvar_6, 5);
    YAP_UserCPredicate("gecode_new_setvar_7", gecode_new_setvar_7, 7);
    YAP_UserCPredicate("gecode_new_setvar_8", gecode_new_setvar_8, 6);
    YAP_UserCPredicate("gecode_new_setvar_9", gecode_new_setvar_9, 5);
    YAP_UserCPredicate("gecode_new_setvar_10", gecode_new_setvar_10, 6);
    YAP_UserCPredicate("gecode_new_setvar_11", gecode_new_setvar_11, 5);
    YAP_UserCPredicate("gecode_new_setvar_12", gecode_new_setvar_12, 4);
    YAP_UserCPredicate("gecode_space_minimize", gecode_space_minimize, 2);
    YAP_UserCPredicate("gecode_space_maximize", gecode_space_maximize, 2);
    YAP_UserCPredicate("gecode_space_minimize_ratio", gecode_space_minimize_ratio, 3);
    YAP_UserCPredicate("gecode_space_maximize_ratio", gecode_space_maximize_ratio, 3);
    // INFO ON INTVARS
    YAP_UserCPredicate("gecode_intvar_assigned", gecode_intvar_assigned, 2);
    YAP_UserCPredicate("gecode_intvar_min", gecode_intvar_min, 3);
    YAP_UserCPredicate("gecode_intvar_max", gecode_intvar_max, 3);
    YAP_UserCPredicate("gecode_intvar_med", gecode_intvar_med, 3);
    YAP_UserCPredicate("gecode_intvar_val", gecode_intvar_val, 3);
    YAP_UserCPredicate("gecode_intvar_size", gecode_intvar_size, 3);
    YAP_UserCPredicate("gecode_intvar_width", gecode_intvar_width, 3);
    YAP_UserCPredicate("gecode_intvar_regret_min", gecode_intvar_regret_min, 3);
    YAP_UserCPredicate("gecode_intvar_regret_max", gecode_intvar_regret_max, 3);
    YAP_UserCPredicate("gecode_intvar_ranges", gecode_intvar_ranges, 3);
    YAP_UserCPredicate("gecode_intvar_values", gecode_intvar_values, 3);
    // INFO ON BOOLVARS
    YAP_UserCPredicate("gecode_boolvar_assigned", gecode_boolvar_assigned, 2);
    YAP_UserCPredicate("gecode_boolvar_min", gecode_boolvar_min, 3);
    YAP_UserCPredicate("gecode_boolvar_max", gecode_boolvar_max, 3);
    YAP_UserCPredicate("gecode_boolvar_med", gecode_boolvar_med, 3);
    YAP_UserCPredicate("gecode_boolvar_val", gecode_boolvar_val, 3);
    YAP_UserCPredicate("gecode_boolvar_size", gecode_boolvar_size, 3);
    YAP_UserCPredicate("gecode_boolvar_width", gecode_boolvar_width, 3);
    YAP_UserCPredicate("gecode_boolvar_regret_min", gecode_boolvar_regret_min, 3);
    YAP_UserCPredicate("gecode_boolvar_regret_max", gecode_boolvar_regret_max, 3);
    // INFO ON SETVARS
    YAP_UserCPredicate("gecode_setvar_assigned", gecode_setvar_assigned, 2);
    YAP_UserCPredicate("gecode_setvar_glbSize", gecode_setvar_glbSize, 3);
    YAP_UserCPredicate("gecode_setvar_lubSize", gecode_setvar_lubSize, 3);
    YAP_UserCPredicate("gecode_setvar_unknownSize", gecode_setvar_unknownSize, 3);
    YAP_UserCPredicate("gecode_setvar_cardMin", gecode_setvar_cardMin, 3);
    YAP_UserCPredicate("gecode_setvar_cardMax", gecode_setvar_cardMax, 3);
    YAP_UserCPredicate("gecode_setvar_lubMin", gecode_setvar_lubMin, 3);
    YAP_UserCPredicate("gecode_setvar_lubMax", gecode_setvar_lubMax, 3);
    YAP_UserCPredicate("gecode_setvar_glbMin", gecode_setvar_glbMin, 3);
    YAP_UserCPredicate("gecode_setvar_glbMax", gecode_setvar_glbMax, 3);
    YAP_UserCPredicate("gecode_setvar_glb_ranges", gecode_setvar_glb_ranges, 3);
    YAP_UserCPredicate("gecode_setvar_lub_ranges", gecode_setvar_lub_ranges, 3);
    YAP_UserCPredicate("gecode_setvar_unknown_ranges", gecode_setvar_unknown_ranges, 3);
    YAP_UserCPredicate("gecode_setvar_glb_values", gecode_setvar_glb_values, 3);
    YAP_UserCPredicate("gecode_setvar_lub_values", gecode_setvar_lub_values, 3);
    YAP_UserCPredicate("gecode_setvar_unknown_values", gecode_setvar_unknown_values, 3);
    YAP_UserCPredicate("gecode_space_use_keep_index", gecode_space_use_keep_index, 2);
    YAP_UserCPredicate("gecode_intvar_keep", gecode_intvar_keep, 3);
    YAP_UserCPredicate("gecode_boolvar_keep", gecode_boolvar_keep, 3);
    YAP_UserCPredicate("gecode_setvar_keep", gecode_setvar_keep, 3);
    // INFO ON FLOATVARS
    YAP_UserCPredicate("gecode_floatvar_assigned", gecode_floatvar_assigned, 2);
    YAP_UserCPredicate("gecode_floatvar_min", gecode_floatvar_min, 3);
    YAP_UserCPredicate("gecode_floatvar_max", gecode_floatvar_max, 3);
    YAP_UserCPredicate("gecode_floatvar_med", gecode_floatvar_med, 3);
    YAP_UserCPredicate("gecode_floatvar_size", gecode_floatvar_size, 3);
  }
}
