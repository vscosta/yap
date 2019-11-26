
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
* File:		userpreds.c						 *
* Last rev:								 *
* mods:									 *
* comments:	an entry for user defined predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file is an entry for user defined C-predicates.
 *
 * There are two sorts of C-Predicates: deterministic - which should be defined
 * in the function InitUserCPreds().
 *
 * backtrackable - they include a start and a continuation function, the first
 * one called by the first invocation, the last one called after a fail. This
 * can be seen as: pred :- init ; repeat, cont. These predicates should be
 * defined in the function InitUserBacks()
 *
 * These two functions are called after any "restore" operation.
 *
 * The function InitUserExtensions() is called once, when starting the execution
 * of the program, and should be used to initialize any user-defined
 * extensions (like the execution environment or interfaces to other
 * programs).
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#if EUROTRA
#include "yapio.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif

/* You should include here the prototypes for all static functions */

#ifdef EUROTRA
static int p_clean(void);
static int p_namelength(void);
static int p_getpid(void);
static int p_exit(void);
static int p_incrcounter(void);
static int p_setcounter(void);
static int p_trapsignal(void);
static int subsumes(Term, Term);
static int p_subsumes(void);
static int p_grab_tokens(void);
#endif
#ifdef MACYAP
static typedef int (*SignalProc)();
static SignalProc skel_signal(int, SignalProc);
static int chdir(char *);
#endif

#ifdef SFUNC
static int p_softfunctor(void);
#endif /* SFUNC */

#ifdef USERPREDS
/* These are some examples of user-defined functions */

/*
 * unify(A,B) --> unification with occurs-check it uses the functions
 * full_unification  and occurs_in
 *
 * occurs_check(V,S) :- var(S), !, S \== V. occurs_check(V,S) :- primitive(S),
 * !. occurs_check(V,[H|T]) :- !, occurs_check(V,H), occurs_check(V,T).
 * occurs_check(V,St) :- functor(T,_,N), occurs_check_struct(N,V,St).
 *
 * occurs_check_struct(1,V,T) :- !, arg(1,T,A), occurs_check(V,A).
 * occurs_check_struct(N,V,T) :- N1 is N-1, occurs_check_structure(N1,V,T),
 * arg(N,T,A), occurs_check(V,A).
 *
 * unify(X,Y) :- var(X), var(Y), !, X = Y. unify(X,Y) :- var(X), !,
 * occurs_check(X,Y), X = Y. unify(X,Y) :- var(Y), !, occurs_check(Y,X), X =
 * Y. unify([H0|T0],[H1|T1]) :- !, unify(H0,H1), unify(T0,T1). unify(X,Y) :-
 * functor(X,A,N), functor(Y,A,N), unify_structs(N,X,Y).
 *
 * unify_structs(1,X,Y) :- !, arg(1,X,A), arg(1,Y,B), unify(A,B).
 * unify_structs(N,Y,Z) :- N1 is N-1, unify_structs(N1,X,Y), arg(N,X,A),
 * arg(N,Y,B), unify(A,B).
 */

/* occurs-in --> checks if the variable V occurs in term S */

static int occurs_check(V, T) Term V, T;
{
  /* V and S are always derefed */
  if (IsVarTerm(T)) {
    return (V != T);
  } else if (IsPrimitiveTerm(T)) {
    return (TRUE);
  } else if (IsPairTerm(T)) {
    return (occurs_check(V, HeadOfTerm(T)) && occurs_check(V, TailOfTerm(T)));
  } else if (IsApplTerm(T)) {
    unsigned int i;
    unsigned int arity = ArityOfFunctor(FunctorOfTerm(T));

    for (i = 1; i <= arity; ++i)
      if (!occurs_check(V, ArgOfTerm(i, T)))
        return (FALSE);
    return (TRUE);
  }
  return (FALSE);
}

/*
  If you worry about coroutining the routine must receive the
  arguments before dereferencing, otherwise unify() won't be
  to wake possible bound variables
*/
static int full_unification(T1, T2) Term T1, T2;
{
  Term t1 = Deref(T1);
  Term t2 = Deref(T2);
  if (IsVarTerm(t1)) {/* Testing for variables should be done first */
    if (IsVarTerm(t2) || IsPrimitiveTerm(t2))
      return (Yap_unify(T1, t2));
    if (occurs_check(t1, t2))
      return (Yap_unify(T1, t2));
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    if (occurs_check(t2, t1))
      return (Yap_unify(T2, t1));
    return (FALSE);
  }
  if (IsPrimitiveTerm(t1)) {
    if (IsFloatTerm(t1))
      return (IsFloatTerm(t2) && FloatOfTerm(t1) == FloatOfTerm(t2));
    else if (IsRefTerm(t1))
      return (IsRefTerm(t2) && RefOfTerm(t1) == RefOfTerm(t2));
    if (IsLongIntTerm(t1))
      return (IsLongIntTerm(t2) && LongIntOfTerm(t1) == LongIntOfTerm(t2));
    else
      return (t1 == t2);
  }
  if (IsPairTerm(t1)) {
    if (!IsPairTerm(t2))
      return (FALSE);
    return (full_unification(HeadOfTermCell(t1), HeadOfTermCell(t2)) &&
            full_unification(TailOfTermCell(t1), TailOfTermCell(t2)));
  }
  if (IsApplTerm(t1)) {
    unsigned int i, arity;
    if (!IsApplTerm(t2))
      return (FALSE);
    if (FunctorOfTerm(t1) != FunctorOfTerm(t2))
      return (FALSE);
    arity = ArityOfFunctor(FunctorOfTerm(t1));
    for (i = 1; i <= arity; ++i)
      if (!full_unification(ArgOfTermCell(i, t1), ArgOfTerm(i, t2)))
        return (FALSE);
    return (TRUE);
  }
#ifdef lint
  return (FALSE);
#endif
}

static int p_occurs_check() { /* occurs_check(?,?)	 */
  return (occurs_check(Deref(ARG1), Deref(DARG2)));
}

/* Out of date, use unify_with_occurs_check instead*/
static int p_unify() { /* unify(?,?)		 */
  /* routines that perform unification must receive the original arguments */
  return (full_unification(ARG1, ARG2));
}

/*
 * One example of a counter using the atom value functions counter(Atom,M,N)
 *
 * If the second argument is uninstantiated, then it will be unified with the
 * current value of the counter, otherwyse the counter will be set to its
 * value. The third argument then be unified with the next integer, which
 * will become the current counter value.
 */
static int p_counter() { /* counter(+Atom,?Number,?Next) */
  Term TCount, TNext, T1, T2;
  Atom a;
  /* Int -> an YAP integer */
  Int val;
  T1 = Deref(ARG1);
  ARG2 = Deref(ARG2);

  /* No need to deref ARG3, we don't want to know what's in there */
  if (IsVarTerm(T1) || !IsAtomTerm(T1))
    return (FALSE);
  a = AtomOfTerm(T1);
  if (IsVarTerm(T2)) {
    TCount = Yap_GetValue(a);
    if (!IsIntTerm(TCount))
      return (FALSE);
    Yap_unify_constant(ARG2, TCount); /* always succeeds */
    val = IntOfTerm(TCount);
  } else {
    if (!IsIntTerm(T2))
      return (FALSE);
    val = IntOfTerm(T2);
  }
  val++;
  /* The atom will now take the incremented value */
  Yap_PutValue(a, TNext = MkIntTerm(val));
  return (Yap_unify_constant(ARG3, TNext));
}

/*
 * Concatenate an instantiated list to another list, and unify with third
 * argument
 */

/*
 * In order to be more efficient, iconcat instead of unifying the terms in
 * the old structure with the ones in the new one just copies them. This is a
 * dangerous behaviour, though acceptable in this case, and you should try to
 * avoid it whenever possible
 */
#ifdef COMMENT
static int p_iconcat() { /* iconcat(+L1,+L2,-L) */
  Term Tkeep[1025];      /* Will do it just for lists less
                                  * than 1024 elements long */
  register Term *Tkp = Tkeep;
  register Term L0, L1;
  Term T2;

  L0 = Deref(ARG1);
  *Tkp++ = Unsigned(0);
  L1 = TermNil;
  while (L0 != L1) {
    /*
     * Usually you should test if L1 a var, if (!IsPairTerm(L0))
     * return(FALSE);
     */
    *Tkp++ = HeadOfTerm(L0);
    L0 = TailOfTerm(L0);
  }
  L1 = Deref(ARG2);
  while (L0 = *--Tkp)
    L1 = MkPairTerm(L0, L1);
  T2 = L1;
  return (Yap_unify(T2, ARG3));
}
#endif /* COMMENT */

static int p_iconcat() { /* iconcat(+L1,+L2,-L) */
  register Term *Tkp = H, *tp;
  register Term L0, L1;
  Term T2;

  L0 = Deref(ARG1);
  L1 = TermNil;
  while (L0 != L1) {
    /* if (!IsPairTerm(L0)) return(FALSE); */
    tp = Tkp;
    *tp = AbsPair(++Tkp);
    *Tkp++ = HeadOfTerm(L0);
    L0 = TailOfTerm(L0);
  }
  *Tkp++ = Deref(ARG2);
  T2 = *H;
  H = Tkp;
  return (Yap_unify(T2, ARG3));
}

#endif /* USERPREDS */

#ifdef EUROTRA

static int p_clean() /* predicate clean for ets 			 */
                     /*
                      * clean(FB,CFB) :- FB =.. [fb|L],!, clean1(L,CL), CFB =.. [fb|CL].
                      * clean(FB,CFB) :- var(FB).
                      *
                      * clean1([],[]) :- !. clean1([H|T],[CH|CT]) :- H==$u,!, clean1(T,CT).
                      * clean1([H|T],[H|CT]) :- clean1(T,CT).
                      */
{
  unsigned int arity, i;
  Term t, Args[255];
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1))
    return (TRUE);
  if (!(IsApplTerm(t1) && NameOfFunctor(FunctorOfTerm(t1)) == AtomFB))
    return (FALSE);
  arity = ArityOfFunctor(FunctorOfTerm(t1));
#ifdef SFUNC
  if (arity == SFArity) {
    CELL *pt = H, *ntp = ArgsOfSFTerm(t1);
    Term tn = AbsAppl(H);
    *pt++ = FunctorOfTerm(t1);
    RESET_VARIABLE(pt);
    pt++;
    while (*pt++ = *ntp++)
      if ((*pt++ = *ntp++) == MkAtomTerm(AtomDollarUndef))
        pt -= 2;
    H = pt;
    return (Yap_unify(tn, ARG2));
  }
#endif
  for (i = 1; i <= arity; ++i) {
    if ((t = ArgOfTerm(i, t1)) == TermDollarU)
      t = MkVarTerm();
    Args[i - 1] = t;
  }
  t = Yap_MkApplTerm(FunctorOfTerm(t1), arity, Args);
  return (Yap_unify(ARG2, t));
}

static Term *subs_table;
static int subs_entries;
#define SUBS_TABLE_SIZE 500

static int subsumes(T1, T2) Term T1, T2;
{
  int i;

  if (IsVarTerm(T1)) {
    if (!IsVarTerm(T2))
      return (FALSE);
    if (T1 == T2)
      return (TRUE);
    for (i = 0; i < subs_entries; ++i)
      if (subs_table[i] == T2)
        return (FALSE);
    if (T2 < T1) {/* T1 gets instantiated with T2 */
      Yap_unify(T1, T2);
      for (i = 0; i < subs_entries; ++i)
        if (subs_table[i] == T1) {
          subs_table[i] = T2;
          return (TRUE);
        }
      subs_table[subs_entries++] = T2;
      return (TRUE);
    }
    /* T2 gets instantiated with T1 */
    Yap_unify(T1, T2);
    for (i = 0; i < subs_entries; ++i)
      if (subs_table[i] == T1)
        return (TRUE);
    subs_table[subs_entries++] = T1;
    return (TRUE);
  }
  if (IsVarTerm(T2)) {
    for (i = 0; i < subs_entries; ++i)
      if (subs_table[i] == T2)
        return (FALSE);
    return (Yap_unify(T1, T2));
  }
  if (IsPrimitiveTerm(T1)) {
    if (IsFloatTerm(T1))
      return (IsFloatTerm(T2) && FloatOfTerm(T1) == FloatOfTerm(T2));
    else if (IsRefTerm(T1))
      return (IsRefTerm(T2) && RefOfTerm(T1) == RefOfTerm(T2));
    else if (IsLongIntTerm(T1))
      return (IsLongIntTerm(T2) && LongIntOfTerm(T1) == LongIntOfTerm(T2));
    else
      return (T1 == T2);
  }
  if (IsPairTerm(T1)) {
    if (!IsPairTerm(T2))
      return (FALSE);
    return (subsumes(HeadOfTerm(T1), HeadOfTerm(T2)) &&
            subsumes(TailOfTerm(T1), TailOfTerm(T2)));
  }
  if (IsApplTerm(T1)) {
    int arity;
    if (!IsApplTerm(T2))
      return (FALSE);
    if (FunctorOfTerm(T1) != FunctorOfTerm(T2))
      return (FALSE);
    arity = ArityOfFunctor(FunctorOfTerm(T1));
#ifdef SFUNC
    if (arity == SFArity) {
      CELL *a1a = ArgsOfSFTerm(T1), *a2a = ArgsOfSFTerm(T2);
      CELL *a1p = a1a - 1, *a2p = a2a - 1;
      CELL *pt = H;
      int flags = 0;
      Term t1, t2;
      *pt++ = FunctorOfTerm(T1);
      RESET_VARIABLE(pt);
      pt++;
      while (1) {
        if (*a2a < *a1a || *a1a == 0) {
          if (*a2a) {
            *pt++ = *a2a++;
            t2 = Derefa(a2a);
            ++a2a;
            if (!IsVarTerm(t2))
              return (FALSE);
            for (i = 0; i < subs_entries; ++i)
              if (subs_table[i] == t2)
                return (FALSE);
            subs_table[subs_entries++] = t2;
            *pt++ = t2;
            flags |= 1;
          } else {                 /* T2 is finished */
            if ((flags & 1) == 0) {/* containned in first */
              *a2p = Unsigned(a1p - 1);
              if (a2p < HB)
                *TR++ = Unsigned(a2p);
              return (TRUE);
            }
            while ((*pt++ = *a1a++))
              ;
            *a1p = Unsigned(H);
            if (a1p < HB)
              *TR++ = Unsigned(a1p);
            *a2p = Unsigned(H);
            if (a2p < HB)
              *TR++ = Unsigned(a2p);
            H = pt;
            return (TRUE);
          }
        } else if (*a2a > *a1a || *a2a == 0) {
          *pt++ = *a1a++;
          t1 = Derefa(a1a);
          ++a1a;
          if (IsVarTerm(t1)) {
            for (i = 0; i < subs_entries; ++i)
              if (subs_table[i] == t1)
                break;
            if (i >= subs_entries)
              subs_table[subs_entries++] = t1;
          }
          *pt++ = t1;
          flags |= 2;
        } else if (*a1a == *a2a) {
          *pt++ = *a1a++;
          ++a2a;
          t1 = Derefa(a1a);
          ++a1a;
          t2 = Derefa(a2a);
          ++a2a;
          *pt++ = t1;
          if (!subsumes(t1, t2))
            return (FALSE);
        }
      }
    }
#endif
    for (i = 1; i <= arity; ++i)
      if (!subsumes(ArgOfTerm(i, T1), ArgOfTerm(i, T2)))
        return (FALSE);
    return (TRUE);
  }
  return (FALSE);
}

static int p_subsumes() {
  Term work_space[SUBS_TABLE_SIZE];
  subs_table = work_space;
  subs_entries = 0;
  return (subsumes(Deref(ARG1), Deref(ARG2)));
}

static int p_namelength() {
  register Term t = Deref(ARG1);
  Term tf;

  if (IsVarTerm(t)) {
    return (FALSE);
  }
  if (IsAtomTerm(t)) {
    Term tf = MkIntTerm(strlen(RepAtom(AtomOfTerm(t))->StrOfAE));
    return (Yap_unify_constant(ARG2, tf));
  } else if (IsIntTerm(t)) {
    register int i = 1, k = IntOfTerm(t);
    if (k < 0)
      ++i, k = -k;
    while (k > 10)
      ++i, k /= 10;
    tf = MkIntTerm(i);
    return (Yap_unify_constant(ARG2, tf));
  } else
    return (FALSE);
}

static int p_getpid() {
#ifndef MPW
  Term t = MkIntTerm(getpid());
#else
  Term t = MkIntTerm(1);
#endif
  return (Yap_unify_constant(ARG1, t));
}

static int p_exit() {
  register Term t = Deref(ARG1);
  if (IsVarTerm(t) || !IsIntTerm(t))
    return (FALSE);
  Yap_exit((int)IntOfTerm(t));
  return (FALSE);
}

static int current_pos;

static int p_incrcounter() {
  register Term t = Deref(ARG1);
  if (IsVarTerm(t) || !IsIntTerm(t))
    return (FALSE);
  current_pos += IntOfTerm(t);
  return (TRUE);
}

static int p_setcounter() {
  register Term t = Deref(ARG1);
  if (IsVarTerm(t) || !IsIntTerm(t)) {
    return (Yap_unify_constant(ARG1, MkIntTerm(current_pos)));
  } else {
    current_pos = IntOfTerm(t);
    return (TRUE);
  }
}

#include <signal.h>
#ifdef MACYAP
#define signal(A, B) skel_signal(A, B)
#endif

#ifndef EOF
#define EOF -1
#endif

static int p_trapsignal(void) {
#ifndef MPW
  signal(SIGINT, SIG_IGN);
#endif
  return (TRUE);
}

#define varstarter(ch) ((ch >= 'A' && ch <= 'Z') || ch == '_')
#define idstarter(ch) (ch >= 'a' && ch <= 'z')
#define idchar(ch)                                                             \
  ((ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') ||                     \
   (ch >= 'a' && ch <= 'z') || ch == '_')

static int p_grab_tokens() {
  Term *p = ASP - 20, *p0, t;
  Functor IdFunctor, VarFunctor;
  char ch, IdChars[256], *chp;

  IdFunctor = FunctorId;
  VarFunctor = FunctorDollarVar;
  p0 = p;
  ch = Yap_PlGetchar();
  while (1) {
    while (ch <= ' ' && ch != EOF)
      ch = Yap_PlGetchar();
    if (ch == '.' || ch == EOF)
      break;
    if (ch == '%') {
      while ((ch = Yap_PlGetchar()) != 10)
        ;
      ch = Yap_PlGetchar();
      continue;
    }
    if (ch == '\'') {
      chp = IdChars;
      while (1) {
        ch = Yap_PlGetchar();
        if (ch == '\'')
          break;
        *chp++ = ch;
      }
      *chp = 0;
      t = MkAtomTerm(Yap_LookupAtom(IdChars));
      *p-- = Yap_MkApplTerm(IdFunctor, 1, &t);
      ch = Yap_PlGetchar();
      continue;
    }
    if (varstarter(ch)) {
      chp = IdChars;
      *chp++ = ch;
      while (1) {
        ch = Yap_PlGetchar();
        if (!idchar(ch))
          break;
        *chp++ = ch;
      }
      *chp = 0;
      t = MkAtomTerm(Yap_LookupAtom(IdChars));
      *p-- = Yap_MkApplTerm(VarFunctor, 1, &t);
      continue;
    }
    if (idstarter(ch)) {
      chp = IdChars;
      *chp++ = ch;
      while (1) {
        ch = Yap_PlGetchar();
        if (!idchar(ch))
          break;
        *chp++ = ch;
      }
      *chp = 0;
      t = MkAtomTerm(Yap_LookupAtom(IdChars));
      *p-- = Yap_MkApplTerm(IdFunctor, 1, &t);
      continue;
    }
    IdChars[0] = ch;
    IdChars[1] = 0;
    *p-- = MkAtomTerm(Yap_LookupAtom(IdChars));
    ch = Yap_PlGetchar();
  }
  t = MkAtomTerm(AtomNil);
  while (p != p0) {
    t = MkPairTerm(*++p, t);
  }
  return (Yap_unify(ARG1, t));
}

#endif /* EUROTRA */

#ifdef SFUNC

static p_softfunctor() {
  Term nilvalue = 0;
  SFEntry *pe;
  Prop p0;
  Atom a;
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);

  if (IsAtomTerm(t2))
    nilvalue = t2;
  if (!IsAtomTerm(t1))
    return (FALSE);
  a = AtomOfTerm(t1);
  WRITE_LOCK(RepAtom(a)->ARWLock);
  if ((p0 = Yap_GetAProp(a, SFProperty)) == NIL) {
    pe = (SFEntry *)Yap_AllocAtomSpace(sizeof(*pe));
    pe->KindOfPE = SFProperty;
    AddPropToAtom(RepAtom(a), (PropEntry *)pe);
  } else
    pe = RepSFProp(p0);
  WRITE_UNLOCK(RepAtom(a)->ARWLock);
  pe->NilValue = nilvalue;
  return (TRUE);
}

#endif /* SFUNC */

#include <math.h>

/*
static Int
p_matching_distances(void)
{
  return(fabs(FloatOfTerm(Deref(ARG1))-FloatOfTerm(Deref(ARG2))) <=
FloatOfTerm(Deref(ARG3)));
}
*/

void Yap_InitUserCPreds(void) {
#ifdef XINTERFACE
  Yap_InitXPreds();
#endif
#ifdef EUROTRA
  Yap_InitCPred("clean", 2, p_clean, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("name_length", 2, p_namelength, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("get_pid", 1, p_getpid, SafePredFlag);
  Yap_InitCPred("exit", 1, p_exit, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("incr_counter", 1, p_incrcounter, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("set_counter", 1, p_setcounter, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("trap_signal", 0, p_trapsignal, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("mark2_grab_tokens", 1, p_grab_tokens,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("subsumes", 2, p_subsumes, SafePredFlag);
#endif
#ifdef SFUNC
  Yap_InitCPred("sparse_functor", 2, p_softfunctor, SafePredFlag);
#endif /* SFUNC */
  /*  Yap_InitCPred("match_distances", 3, p_matching_distances, SafePredFlag);
   */
  /* Yap_InitCPred("unify",2,p_unify,SafePredFlag); */
  /* Yap_InitCPred("occurs_check",2,p_occurs_check,SafePredFlag); */
  /* Yap_InitCPred("counter",3,p_counter,SafePredFlag); */
  /* Yap_InitCPred("iconcat",3,p_iconcat,SafePredFlag); */
}

void Yap_InitUserBacks(void) {}
