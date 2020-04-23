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

#include "gecode3-common.icc"
#include <iostream>
using namespace std;
using namespace generic_gecode;
using namespace Gecode;

extern "C"
{
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
#include "SWI-Stream.h"
#include "YapInterface.h"

  static YAP_opaque_tag_t gecode_space_tag;
  static YAP_opaque_handler_t gecode_space_handler;

  static int gecode_space_fail_handler(void* p)
  {
    delete *(GenericSpace**)p;
    return TRUE;
  }

  static int
  gecode_space_write_handler
  (FILE *stream, YAP_opaque_tag_t type, void *p, int flags)
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

  static int gecode_new_space(void)
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

  static YAP_opaque_tag_t gecode_engine_tag;
  static YAP_opaque_handler_t gecode_engine_handler;

  static int gecode_new_engine(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    bool restart = YAP_IntOfTerm(YAP_ArgOfTerm(1, arg3));
    double threads = YAP_FloatOfTerm(YAP_ArgOfTerm(2, arg3));
    unsigned int c_d = YAP_IntOfTerm(YAP_ArgOfTerm(3, arg3));
    unsigned int a_d = YAP_IntOfTerm(YAP_ArgOfTerm(4, arg3));
    Search::Options opt;
    opt.threads = threads;
    opt.c_d = c_d;
    opt.a_d = a_d;
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

  static int gecode_engine_fail_handler(void* p)
  {
    delete *(GenericEngine**)p;
    return TRUE;
  }

  static int
  gecode_engine_write_handler
  (void *stream_, YAP_opaque_tag_t type, void *p, int flags)
  {
    IOSTREAM* stream = (IOSTREAM*) stream_;
    Sfprintf(stream,"<engine %p>", p);
    return TRUE;
  }

  static int gecode_engine_search(void)
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
  static int gecode_new_disjunctor(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_disjunctor_tag, sizeof(YapDisjunctor));
    new (YAP_OpaqueObjectFromTerm(term)) YapDisjunctor(space);
    return YAP_Unify(result, term);
  }

  static int
  gecode_disjunctor_write_handler
  (void *stream_, YAP_opaque_tag_t type, void *p, int flags)
  {
    IOSTREAM* stream = (IOSTREAM*) stream_;
    Sfprintf(stream,"<disjunctor %p>", p);
    return TRUE;
  }

  static int gecode_new_clause(void)
  {
    YAP_Term result = YAP_ARG1;
    YapDisjunctor& disj = gecode_YapDisjunctor_from_term(YAP_ARG2);
    YAP_Term term =
      YAP_NewOpaqueObject(gecode_disjunctor_clause_tag, sizeof(Clause));
    new (YAP_OpaqueObjectFromTerm(term)) Clause(*disj.home,disj.disj);
    return YAP_Unify(result, term);
  }

  static int
  gecode_clause_write_handler
  (void *stream_, YAP_opaque_tag_t type, void *p, int flags)
  {
    IOSTREAM* stream = (IOSTREAM*) stream_;
    Sfprintf(stream,"<clause %p>", p);
    return TRUE;
  }

  static IntVar gecode_IntVar_from_term(GenericSpace*,YAP_Term);
  static BoolVar gecode_BoolVar_from_term(GenericSpace*,YAP_Term);
  static SetVar gecode_SetVar_from_term(GenericSpace*,YAP_Term);

  static int gecode_clause_intvar_forward(void)
  {
    Clause& clause = gecode_Clause_from_term(YAP_ARG1);
    GenericSpace* outer = clause.generic_parent();
    GenericSpace* inner = clause.generic_space();
    IntVar outer_var = gecode_IntVar_from_term(outer, YAP_ARG2);
    IntVar inner_var = gecode_IntVar_from_term(inner, YAP_ARG3);
    clause.forward(outer_var,inner_var);
    return TRUE;
  }

  static int gecode_clause_boolvar_forward(void)
  {
    Clause& clause = gecode_Clause_from_term(YAP_ARG1);
    GenericSpace* outer = clause.generic_parent();
    GenericSpace* inner = clause.generic_space();
    BoolVar outer_var = gecode_BoolVar_from_term(outer, YAP_ARG2);
    BoolVar inner_var = gecode_BoolVar_from_term(inner, YAP_ARG3);
    clause.forward(outer_var,inner_var);
    return TRUE;
  }

  static int gecode_clause_setvar_forward(void)
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

  static int gecode_new_intvar_from_bounds(void)
  {
    YAP_Term ivar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int lo = YAP_IntOfTerm(YAP_ARG3);
    int hi = YAP_IntOfTerm(YAP_ARG4);
    int i = space->new_ivar(lo, hi);
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

  static int gecode_new_intvar_from_intset(void)
  {
    YAP_Term ivar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    YAP_Term specs = YAP_ARG3;
    IntSet iset = gecode_IntSet_from_term(specs);
    int i = space->new_ivar(iset);
    return YAP_Unify(ivar, YAP_MkIntTerm(i));
  }

  static int gecode_new_boolvar(void)
  {
    YAP_Term bvar = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int i = space->new_bvar();
    return YAP_Unify(bvar, YAP_MkIntTerm(i));
  }

  static int gecode_new_setvar_1(void)
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

  static int gecode_new_setvar_2(void)
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

  static int gecode_new_setvar_3(void)
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

  static int gecode_new_setvar_4(void)
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

  static int gecode_new_setvar_5(void)
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

  static int gecode_new_setvar_6(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    int LubMin = YAP_IntOfTerm(YAP_ARG4);
    int LubMax = YAP_IntOfTerm(YAP_ARG5);
    int idx = space->new_svar(Glb,LubMin,LubMax);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static int gecode_new_setvar_7(void)
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

  static int gecode_new_setvar_8(void)
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

  static int gecode_new_setvar_9(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    int GlbMin = YAP_IntOfTerm(YAP_ARG3);
    int GlbMax = YAP_IntOfTerm(YAP_ARG4);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG5);
    int idx = space->new_svar(GlbMin,GlbMax,Lub);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static int gecode_new_setvar_10(void)
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

  static int gecode_new_setvar_11(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG4);
    int CardMin = YAP_IntOfTerm(YAP_ARG5);
    int idx = space->new_svar(Glb,Lub,CardMin);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static int gecode_new_setvar_12(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntSet Glb = gecode_IntSet_from_term(YAP_ARG3);
    IntSet Lub = gecode_IntSet_from_term(YAP_ARG4);
    int idx = space->new_svar(Glb,Lub);
    return YAP_Unify(result, YAP_MkIntTerm(idx));
  }

  static int gecode_space_minimize(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    space->minimize(i);
    return TRUE;
  }

  static int gecode_space_maximize(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    space->maximize(i);
    return TRUE;
  }

  static int gecode_space_minimize_ratio(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    int i = YAP_IntOfTerm(YAP_ARG2);
    int j = YAP_IntOfTerm(YAP_ARG3);
    space->minimize(i,j);
    return TRUE;
  }

  static int gecode_space_maximize_ratio(void)
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
    if (X==gecode_TRUE) return true;
    if (X==gecode_FALSE) return false;
    cerr << "this should never happen" << endl; exit(1);
  }

  static int gecode_space_use_keep_index(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    GenericSpace* space = gecode_Space_from_term(arg1);
    return YAP_Unify(arg2,(space->use_keep_index()
			   ?gecode_TRUE:gecode_FALSE));
  }

  static int gecode_intvar_keep(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    GenericSpace* space = gecode_Space_from_term(arg1);
    int idx = YAP_IntOfTerm(arg2);
    int kidx = space->keep_ivar(idx);
    return YAP_Unify(arg3,YAP_MkIntTerm(kidx));
  }

  static int gecode_boolvar_keep(void)
  {
    YAP_Term arg1 = YAP_ARG1;
    YAP_Term arg2 = YAP_ARG2;
    YAP_Term arg3 = YAP_ARG3;
    GenericSpace* space = gecode_Space_from_term(arg1);
    int idx = YAP_IntOfTerm(arg2);
    int kidx = space->keep_bvar(idx);
    return YAP_Unify(arg3,YAP_MkIntTerm(kidx));
  }

  static int gecode_setvar_keep(void)
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
  static int gecode_intvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static int gecode_intvar_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.min()));
  }

  static int gecode_intvar_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.max()));
  }

  static int gecode_intvar_med(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.med()));
  }

  static int gecode_intvar_val(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.val()));
  }

  static int gecode_intvar_size(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.size()));
  }

  static int gecode_intvar_width(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.width()));
  }

  static int gecode_intvar_regret_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_min()));
  }

  static int gecode_intvar_regret_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    IntVar x = gecode_IntVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_max()));
  }

  static YAP_Functor gecode_COMMA2;

  static int gecode_intvar_ranges(void)
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

  static int gecode_intvar_values(void)
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
  static int gecode_boolvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static int gecode_boolvar_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.min()));
  }

  static int gecode_boolvar_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.max()));
  }

  static int gecode_boolvar_med(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.med()));
  }

  static int gecode_boolvar_val(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.val()));
  }

  static int gecode_boolvar_size(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.size()));
  }

  static int gecode_boolvar_width(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.width()));
  }

  static int gecode_boolvar_regret_min(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_min()));
  }

  static int gecode_boolvar_regret_max(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    BoolVar x = gecode_BoolVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.regret_max()));
  }

  // INFO ON SETVARS
  static int gecode_setvar_assigned(void)
  {
    GenericSpace* space = gecode_Space_from_term(YAP_ARG1);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG2);
    return (x.assigned()) ? TRUE : FALSE;
  }

  static int gecode_setvar_glbSize(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.glbSize()));
  }

  static int gecode_setvar_lubSize(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.lubSize()));
  }

  static int gecode_setvar_unknownSize(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.unknownSize()));
  }

  static int gecode_setvar_cardMin(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.cardMin()));
  }

  static int gecode_setvar_cardMax(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.cardMax()));
  }

  static int gecode_setvar_lubMin(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.lubMin()));
  }

  static int gecode_setvar_lubMax(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.lubMax()));
  }

  static int gecode_setvar_glbMin(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.glbMin()));
  }

  static int gecode_setvar_glbMax(void)
  {
    YAP_Term result = YAP_ARG1;
    GenericSpace* space = gecode_Space_from_term(YAP_ARG2);
    SetVar x = gecode_SetVar_from_term(space, YAP_ARG3);
    return YAP_Unify(result, YAP_MkIntTerm(x.glbMax()));
  }

  static int gecode_setvar_glb_ranges(void)
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

  static int gecode_setvar_lub_ranges(void)
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

  static int gecode_setvar_unknown_ranges(void)
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

  static int gecode_setvar_glb_values(void)
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

  static int gecode_setvar_lub_values(void)
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

  static int gecode_setvar_unknown_values(void)
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

#define gecode_int_from_term YAP_IntOfTerm

#include "gecode_yap_cc_forward_auto_generated.icc"
#include "gecode_yap_cc_impl_auto_generated.icc"

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
  }
}
