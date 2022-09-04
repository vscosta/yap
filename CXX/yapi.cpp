
extern "C" {


#include "inline-only.h"
#define _EXPORT_KERNEL 1
}

#include <vector>

#include "yapi.hh"



extern "C" {

#if __ANDROID__
#include "android/log.h"
#endif

#if YAP_PYTHON
#include "Python.h"
#endif

#include "YapBlobs.h"
#include "YapInterface.h"
#include "iopreds.h"
    
#include "YapInit.h"

X_API extern char *Yap_TermToBuffer(Term t, int flags);

X_API extern void YAP_UserCPredicate(const char *, YAP_UserCPred, arity_t arity);
X_API extern void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, arity_t,
                                      YAP_Term);
X_API extern void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred,
                                  YAP_Arity, YAP_Arity);

#include "heapgc.h"

#if YAP_PYTHON
X_API bool do_init_python(void);
#endif
}

static void YAPCatchError() {
    CACHE_REGS
  if (LOCAL_CommittedError != nullptr &&
       LOCAL_CommittedError->errorNo != YAP_NO_ERROR) {
    // Yap_PopTermFromDB(info->errorTerm);
    // throw  throw YAPError(  );
    Term es[2];
    es[0] = TermError;
    es[1] = Yap_MkErrorTerm(LOCAL_CommittedError);
    Functor f = Yap_MkFunctor(Yap_LookupAtom("print_message"), 2);
    YAP_RunGoalOnce(Yap_MkApplTerm(f, 2, es));
    // Yap_PopTermFromDB(info->errorTerm);
    // throw  throw YAPError( SOURCE(), );
  } else if (LOCAL_ActiveError != nullptr &&
             LOCAL_ActiveError->errorNo != YAP_NO_ERROR) {
    // Yap_PopTermFromDB(info->errorTerm);
    // throw  throw YAPError(  );
    Term es[2];
    es[0] = TermError;
    es[1] = Yap_MkErrorTerm(LOCAL_ActiveError);
    Functor f = Yap_MkFunctor(Yap_LookupAtom("print_message"), 2);
    YAP_RunGoalOnce(Yap_MkApplTerm(f, 2, es));
    // Yap_PopTermFromDB(info->errorTerm);
    // throw  throw YAPError( SOURCE(), );
  }
}

YAPPredicate::YAPPredicate(Term &t, Term &tmod, CELL *&ts, const char *pname) {
  Term t0 = t;
  ap = nullptr;
restart:
  if (IsVarTerm(t)) {
    throw YAPError(SOURCE(), INSTANTIATION_ERROR, t0, pname);
  } else if (IsAtomTerm(t)) {
    ap = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), tmod));
    ts = nullptr;
  } else if (IsIntegerTerm(t) && tmod == IDB_MODULE) {
    ts = nullptr;
    ap = Yap_FindLUIntKey(IntegerOfTerm(t));
  } else if (IsPairTerm(t)) {
    t = Yap_MkApplTerm(FunctorCsult, 1, &t);
    goto restart;
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    if (IsExtensionFunctor(fun)) {
      throw YAPError(SOURCE(), TYPE_ERROR_CALLABLE,
                     Yap_TermToIndicator(t, tmod), pname);
    }
    if (fun == FunctorModule) {
      tmod = ArgOfTerm(1, t);
      if (IsVarTerm(tmod)) {
        throw YAPError(SOURCE(), INSTANTIATION_ERROR, t0, pname);
      }
      if (!IsAtomTerm(tmod)) {
        throw YAPError(SOURCE(), TYPE_ERROR_ATOM, t0, pname);
      }
      t = ArgOfTerm(2, t);
      goto restart;
    }
    ap = RepPredProp(Yap_GetPredPropByFunc(fun, tmod));
    ts = RepAppl(t) + 1;
  } else {
    throw YAPError(SOURCE(), TYPE_ERROR_CALLABLE, t0, pname);
  }
}

Term YAPTerm::getArg(arity_t i) {
  BACKUP_MACHINE_REGS();
  Term tf = 0;
  Term t0 = gt();

  if (IsApplTerm(t0)) {
    if (i > ArityOfFunctor(FunctorOfTerm(t0)))
      throw YAPError(SOURCE(), DOMAIN_ERROR_OUT_OF_RANGE, t0, "t0.getArg()");
    tf = (ArgOfTerm(i, t0));
  } else if (IsPairTerm(t0)) {
    if (i == 1)
      tf = (HeadOfTerm(t0));
    else if (i == 2)
      tf = (TailOfTerm(t0));
    else
      throw YAPError(SOURCE(), DOMAIN_ERROR_OUT_OF_RANGE, t0, "t0.getArg()");
  } else {
    throw YAPError(SOURCE(), TYPE_ERROR_COMPOUND, t0, "t0.getArg()");
  }
  RECOVER_MACHINE_REGS();
  return tf;
}

YAPAtomTerm::YAPAtomTerm(char s[]) { // build string
  CACHE_REGS
  BACKUP_H();


  seq_tv_t inp, out;
    inp.enc = LOCAL_encoding;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOM;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    hdl = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(char *s, size_t len) { // build string
    CACHE_REGS
  BACKUP_H();


  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
    inp.enc = LOCAL_encoding;
    out.type = YAP_STRING_ATOM | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    hdl = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(wchar_t *s) : YAPTerm() { // build string
    CACHE_REGS
  BACKUP_H();


  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    hdl = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(wchar_t *s, size_t len) : YAPTerm() { // build string
    CACHE_REGS
      
  BACKUP_H();


  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    hdl = 0L;
  RECOVER_H();
}

YAPStringTerm::YAPStringTerm(std::string &s) { // build string

  CACHE_REGS
  
  BACKUP_H();


  Term ts = MkStringTerm(s.c_str());
  mk(ts);
  RECOVER_H();
}

YAPApplTerm::YAPApplTerm(YAPFunctor f, YAPTerm ts[]) {
  BACKUP_H();
  arity_t arity = ArityOfFunctor(f.f);
  Term o = Yap_MkNewApplTerm(f.f, arity);
  Term *tt = RepAppl(o) + 1;
  for (arity_t i = 0; i < arity; i++)
    tt[i] = ts[i].term();
  mk(o);
  RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const std::string f, std::vector<Term> ts) {
        CACHE_REGS

    BACKUP_H();
    arity_t arity = ts.size();
    Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
    Term o = AbsAppl(HR);
    Term *tt = HR;
    HR+=1+arity;
    *tt++=(CELL)ff;
    for (arity_t i = 0; i < arity; i++)
        tt[i] = ts[i];
    mk(o);
    RECOVER_H();
}




YAPApplTerm::YAPApplTerm(const std::string f, std::vector<YAPTerm> ts) {
        CACHE_REGS

    BACKUP_H();
    arity_t arity = ts.size();
    Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
    Term o = AbsAppl(HR);
    Term *tt = HR;
    HR+=1+arity;
    *tt++=(CELL)ff;
    for (arity_t i = 0; i < arity; i++)
        tt[i] = ts[i].term();
    mk(o);
    RECOVER_H();
}



YAPApplTerm::YAPApplTerm(YAPFunctor f) : YAPTerm() {
  BACKUP_H();
  arity_t arity = ArityOfFunctor(f.f);
  mk(Yap_MkNewApplTerm(f.f, arity));
  RECOVER_H();
}

Term &YAPTerm::operator[](arity_t i) {
  BACKUP_MACHINE_REGS();
  Term t0 = gt();
  Term *tf = nullptr;
  if (IsApplTerm(t0)) {
    // Functor f = FunctorOfTerm(t0);
    // if (IsExtensionFunctor(f))
    //  return 0;
    tf = RepAppl(t0) + (i + 1);
  } else if (IsPairTerm(t0)) {
    if (i == 0)
      tf = RepPair(t0);
    else if (i == 1)
      tf = RepPair(t0) + 1;
    RECOVER_MACHINE_REGS();
  } else {
    throw YAPError(SOURCE(), TYPE_ERROR_COMPOUND, t0, "");
  }
  RECOVER_MACHINE_REGS();
  return *tf;
}

Term &YAPListTerm::operator[](arity_t i) {
  BACKUP_MACHINE_REGS();
  Term t0 = gt();
  while (IsPairTerm(t0)) {
    if (i == 0) {
      return *RepPair(t0);
    } else {
      t0 = TailOfTerm(t0);
    }
  }
  return *RepPair(t0);
}

YAPPairTerm::YAPPairTerm(YAPTerm th, YAPTerm tl) {
  CACHE_REGS
  BACKUP_H();
  mk(MkPairTerm(th.term(), tl.term()));
  RECOVER_H();
}

YAPPairTerm::YAPPairTerm() {
  BACKUP_H();
  mk(TermNil);
  RECOVER_H();
}

std::vector<Term> YAPPairTerm::listToArray() {
  Term *tailp;
  Term t1 = gt();
  Int l = Yap_SkipList(&t1, &tailp);
  if (l < 0) {
    throw YAPError(SOURCE(), TYPE_ERROR_LIST, t1, nullptr);
  }
  std::vector<Term> o = *new std::vector<Term>(l);
  int i = 0;
  Term t = gt();
  while (t != TermNil) {
    o[i++] = HeadOfTerm(t);
    t = TailOfTerm(t);
  }
  return o;
}

std::vector<YAPTerm> YAPPairTerm::listToVector() {
  Term *tailp;
  Term t1 = gt();
  Int len = Yap_SkipList(&t1, &tailp);
  if (len < 0) {
    throw YAPError(SOURCE(), TYPE_ERROR_LIST, (t1), nullptr);
  }
  std::vector<YAPTerm> o = *new std::vector<YAPTerm>(len);
  int i = 0;
  Term l = gt();
  while (l != TermNil) {
    o[i++] = YAPTerm(HeadOfTerm(l));
    l = TailOfTerm(l);
  }
  return o;
}

YAP_tag_t YAPTerm::tag() {
  Term tt = gt();
  if (IsVarTerm(tt)) {
    CELL *pt = VarOfTerm(tt);
    if (IsUnboundVar(pt)) {
      CACHE_REGS
      if (IsAttVar(pt))
        return YAP_TAG_ATT;
      return YAP_TAG_UNBOUND;
    }
    return YAP_TAG_REF;
  }
  if (IsPairTerm(tt))
    return YAP_TAG_PAIR;
  if (IsAtomOrIntTerm(tt)) {
    if (IsAtomTerm(tt))
      return YAP_TAG_ATOM;
    return YAP_TAG_INT;
  } else {
    Functor f = FunctorOfTerm(tt);

    if (IsExtensionFunctor(f)) {
      if (f == FunctorDBRef) {
        return YAP_TAG_DBREF;
      }
      if (f == FunctorLongInt) {
        return YAP_TAG_LONG_INT;
      }
      if (f == FunctorBigInt) {
        big_blob_type bt = (big_blob_type)RepAppl(tt)[1];
        switch (bt) {
        case BIG_INT:
          return YAP_TAG_BIG_INT;
        case BIG_RATIONAL:
          return YAP_TAG_RATIONAL;
        default:
          return YAP_TAG_OPAQUE;
        }
      }
    }
    return YAP_TAG_APPL;
  }
}

Term YAPTerm::deepCopy() {
  yhandle_t tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return (tn);
}

Term YAPListTerm::cdr() {
  Term to = gt();
  if (IsPairTerm(to))
    return (TailOfTerm(to));
  else if (to == TermNil)
    return TermNil;
  /* error */
  throw YAPError(SOURCE(), TYPE_ERROR_LIST, to, "");
}

Term &YAPConjunctiveTerm::operator[](arity_t i) {
  BACKUP_MACHINE_REGS();
  Term t0 = gt();
  Term tf = 0;
  while (IsApplTerm(t0) && FunctorOfTerm(t0) == FunctorComma) {
    if (i == 0) {
      return *(RepAppl(t0)+1);
    } else {
      t0 = ArgOfTerm(2,t0);
      i--;
    }
  }
  RECOVER_MACHINE_REGS();
  return RepPair(tf)[i];
}

Term YAPListTerm::dup() {
  yhandle_t tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return tn;
}

const char *YAPQuery::text() {
  return YAPTerm(goal).text(); }

YAPIntegerTerm::YAPIntegerTerm(intptr_t i) {
  CACHE_REGS

    Term tn = MkIntegerTerm(i);
  mk(tn);
}

/*
YAPTerm *YAPTerm::vars()
{
  BACKUP_MACHINE_REGS();
  CACHE_REGS
  YAPPairTerm lv = YAPPairTerm(Yap_TermVariables(gt(), TermNil PASS_REGS));
  RECOVER_MACHINE_REGS();
  return lv;
}
 */

YAPTerm::YAPTerm(void *ptr) {
  CACHE_REGS
  mk(MkIntegerTerm((Int)ptr));
}

Term YAPListTerm::car() {
  Term to = gt();
  if (IsPairTerm(to))
    return (HeadOfTerm(to));
  else {
        throw YAPError(SOURCE(), TYPE_ERROR_LIST, to, "");
    return TermUnique;
  }
}

YAPListTerm::YAPListTerm(Term ts[], size_t n) {
        CACHE_REGS

  BACKUP_H();
  while (HR + n * 3 > ASP-1024) {
    RECOVER_H();
    if (!Yap_dogc( PASS_REGS1 )) {
      mk(TermNil);
    }
    BACKUP_H();
  }
  Term rc = AbsAppl(HR);
  CELL *ptr = HR;
  HR+=2*n;
  for (size_t i=0; i< n; i++) {
      ptr[0] = MkGlobal(ts[i]);
      ptr[1] = AbsPair(ptr+2);
      ptr += 2;
  }
  ptr[-1] = TermNil;
  mk(rc);
}

YAPListTerm::YAPListTerm(const std::vector<Term> ts) {
  CACHE_REGS
  BACKUP_H();
  size_t n=ts.size();
  if (n == 0)
    mk(TermNil);
  while (HR + n * 2 > ASP - 1024) {
    RECOVER_H();
    if (!Yap_dogc( PASS_REGS1 )) {
      mk(TermNil);
    }
    BACKUP_H();
  }
  Term a = AbsPair(HR);
  CELL *ptr = HR;
  HR += 2*n;
  for (arity_t i = 0; i < n; i++) {
    ptr[0] = MkGlobal(ts[i]);
    ptr[1] = AbsPair(ptr + 2);
    ptr += 2;
  }
  ptr[-1]=TermNil;
  mk(a);
  RECOVER_H();
}


Term YAPConjunctiveTerm::car() {
  Term to = gt();
   if (IsApplTerm(to)&& FunctorOfTerm(to)==FunctorComma)
     return (ArgOfTerm(1,to));
  else {
    throw YAPError(SOURCE(), TYPE_ERROR_LIST, to, "");
    return TermUnique;
  }
}

Term YAPConjunctiveTerm::cdr() {
  Term to = gt();
   if (IsApplTerm(to)&& FunctorOfTerm(to)==FunctorComma)
     return (ArgOfTerm(2,to));
  else {
    throw YAPError(SOURCE(), TYPE_ERROR_LIST, to, "");
    return TermUnique;
  }
}

YAPConjunctiveTerm::YAPConjunctiveTerm(const std::vector<Term> ts) {
  CACHE_REGS
  BACKUP_H();
  size_t n=ts.size();
  if (n == 0) {
    mk(TermNil);
    return;
  }
  if (n == 1) {
    mk(ts[0]);
    return;
  }
  while (HR + n * 3 > ASP - 1024) {
    RECOVER_H();
    if (!Yap_dogc( PASS_REGS1 )) {
      mk(TermNil);
    }
    BACKUP_H();
  }
  Term a = AbsAppl(HR);
  CELL *ptr = HR;
  HR += 3*(n-1);
  for (arity_t i = 0; i < n-1; i++) {
    ptr[0] = (CELL)FunctorComma;
    ptr[1] = MkGlobal(ts[i]);
    ptr[2] = AbsAppl(ptr + 3);
    ptr += 3;
  }
  ptr[-1]=ts[n-1];
  mk(a);
  RECOVER_H();
}

  
YAPConjunctiveTerm::YAPConjunctiveTerm(const Term ts[], size_t n) {
  CACHE_REGS
  BACKUP_H();
  if (n == 0) {
    mk(TermNil);
    return;
  }
  if (n == 1) {
    mk(ts[0]);
    return;
  }
  while (HR + n * 3 > ASP - 1024) {
    RECOVER_H();
    if (!Yap_dogc( PASS_REGS1 )) {
      mk(TermNil);
    }
    BACKUP_H();
  }
  Term a = AbsAppl(HR);
  CELL *ptr = HR;
  HR += 3*(n-1);
  for (arity_t i = 0; i < n-1; i++) {
    ptr[0] = (CELL)FunctorComma;
    ptr[1] = MkGlobal(ts[i]);
    ptr[2] = AbsAppl(ptr + 3);
    ptr += 3;
  }
  ptr[-1]=ts[n-1];
  mk(a);
  RECOVER_H();
}

  
const char *YAPAtom::getName(void) {
    CACHE_REGS
  return Yap_AtomToUTF8Text(a PASS_REGS); }

void YAPQuery::openQuery() {
  if (ap == NULL || ap->OpcodeOfPred == UNDEF_OPCODE) {
    ap = rewriteUndefQuery();
  }
  //  std::cerr <<  "open " <<  YAPTerm(goal).text() << "\n";
  q_open = true;
  setNext();
}

bool YAPEngine::call(YAPPredicate ap, YAPTerm ts[]) {
  CACHE_REGS
  if (ap.ap == NULL)
    return false;
  BACKUP_MACHINE_REGS();
  arity_t arity = ap.getArity();
  bool result;
  YAP_dogoalinfo q;

  q.CurSlot = Yap_StartSlots();
  q.p = P;

  q.cp = CP;
 q.b0 = LCL0-CellPtr(B);
  q.env0 = LCL0-ENV;
  for (arity_t i = 0; i < arity; i++)
    XREGS[i + 1] = ts[i].term();

  // allow Prolog style exceotion handling
  // don't forget, on success these bindings will still be there);
  result = YAP_EnterGoal(ap.ap, nullptr, &q);
  YAP_LeaveGoal(result, &q);

  YAPCatchError();

  RECOVER_MACHINE_REGS();
  return result;
}

bool YAPEngine::mgoal(Term t, Term tmod, bool release) {
#if YAP_PYTHON
  //  std::cerr << "mgoal(in) "  << YAPTerm(tmod).text() << ":" << YAPTerm(t).text() << "\n";
  // PyThreadState *_save;

  // std::cerr << "mgoal " << YAPTerm(t).text() << "\n";
  //  _save = PyEval_SaveThread();
#endif
  CACHE_REGS
  YAP_dogoalinfo q;
  BACKUP_MACHINE_REGS();

   Term *ts = nullptr;
  q.CurSlot = Yap_StartSlots();
  q.p = P;
  q.cp = CP;
 Int oenv = LCL0-ENV;
 Int oB = LCL0-CellPtr(B);
  PredEntry *ap = nullptr;
  if (IsStringTerm(tmod))
    tmod = MkAtomTerm(Yap_LookupAtom(StringOfTerm(tmod)));
  ap =  Yap_get_pred(t, tmod, "C++");
  if (ap == nullptr ||
      ap->OpcodeOfPred == UNDEF_OPCODE) {
    ap = rewriteUndefEngineQuery(ap, t, tmod);
  }
  if (false && ap->PredFlags & MetaPredFlag) {
    ts[0] = tmod;
    ts[1] = t;
    ARG1 = Yap_MkApplTerm(FunctorModule,2,ts);
    ap = PredCall;
  } else {
  if (IsApplTerm(t))
    ts = RepAppl(t) + 1;
  else if (IsPairTerm(t))
    ts = RepPair(t);
  /* legal ap */
  
  arity_t arity = ap->ArityOfPE;

  for (arity_t i = 0; i < arity; i++) {
    XREGS[i + 1] = ts[i];
  }
  }
  ts = nullptr;
  bool result;
  // allow Prolog style exception handling
  // don't forget, on success these guys may create slots
  //__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");
  Term ocmod = CurrentModule;
  Term osmod = LOCAL_SourceModule;
      CurrentModule = LOCAL_SourceModule = tmod;
  result = (bool)YAP_EnterGoal(ap, nullptr, &q);
  LOCAL_SourceModule = osmod;
  CurrentModule = ocmod;
      //  std::cerr << "mgoal "  << YAPTerm(tmod).text() << ":" << YAPTerm(t).text() << "\n
      YAP_LeaveGoal(result, &q);
  if (release)
    HR = B->cp_h;
 ENV = LCL0-oenv;
 B = (choiceptr)(LCL0-oB);
  //      PyEval_RestoreThread(_save);
  RECOVER_MACHINE_REGS();
  return result;
}
/**
 * called when a query must be terminated and its state fully recovered,
 */
void YAPEngine::release() {
      CACHE_REGS

  BACKUP_MACHINE_REGS();
  HR = B->cp_h;

  RECOVER_MACHINE_REGS();
}

Term YAPEngine::fun(Term t) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  YAP_dogoalinfo q;
  Term tmod = Yap_CurrentModule(), *ts = nullptr;
  PredEntry *ap;
  arity_t arity;
  Functor f;
  Atom name;
  q.CurSlot = Yap_StartSlots();
  q.p = P;
  q.cp = CP;

 Int oenv = LCL0-ENV;
 Int oB = LCL0-CellPtr(B);

  if (IsApplTerm(t)) {
    ts = RepAppl(t) + 1;
    f = (Functor)ts[-1];
    name = NameOfFunctor(f);
    arity = ArityOfFunctor(f);
    for (arity_t i = 0; i < arity; i++)
      HR[i + 1] = ts[i];
    arity++;
  } else if (IsAtomTerm(t)) {
    name = AtomOfTerm(t);
    f = nullptr;
    arity = 1;
  } else if (IsPairTerm(t)) {
    HR[1] = ts[0];
    HR[2] = ts[1];
    arity = 3;
    name = AtomDot;
    f = FunctorDot;
  } else {
    Yap_CloseSlots(q.CurSlot);
    throw YAPError(SOURCE(), TYPE_ERROR_CALLABLE, t, 0);
    return 0L;
  }
  Term ot = XREGS[arity + 1] = MkVarTerm();
  yhandle_t h = Yap_InitHandle(ot); 
  arity++;
  HR += arity;
  f = Yap_MkFunctor(name, arity);
  ap = (PredEntry *)(PredPropByFunc(f, tmod));
  if (ap == nullptr || ap->OpcodeOfPred == UNDEF_OPCODE) {
    Term g = (Yap_MkApplTerm(f, arity, ts));
    ap = rewriteUndefEngineQuery(ap, g, (ap->ModuleOfPred));
  }
  // make sure this is safe
  // allow Prolog style exception handling
  //__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");

  bool result = (bool)YAP_EnterGoal(ap, nullptr, &q);
    if (result)
      ot = Yap_GetFromHandle(h);
    else
      ot = TermNone;
    YAPCatchError();
  {
    
    YAP_LeaveGoal(result, &q);
 ENV = LCL0-oenv;
 
 B = (choiceptr)(LCL0-oB);
    //      PyEval_RestoreThread(_save);
    RECOVER_MACHINE_REGS();
    Yap_CloseSlots(q.CurSlot);
    return ot;
  }
  }

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm ts[])
    : YAPPredicate(f, mod) {
        CACHE_REGS


  /* ignore flags  for now */
  BACKUP_MACHINE_REGS();
  Term goal;

  if (ts) {

    goal = YAPApplTerm(f, ts).term();
    nts = RepAppl(goal) + 1;
    size_t arity = f.arity();
    for (arity_t i = 0; i < arity; i++)
      XREGS[i + 1] = nts[i];
  } else {
    goal = MkVarTerm();
  }
  openQuery();
  names = new YAPPairTerm(TermNil);
  RECOVER_MACHINE_REGS();
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, Term ts[])
        : YAPPredicate(f, mod) {
   CACHE_REGS
  /* ignore flags  for now */
  BACKUP_MACHINE_REGS();
  Term goal;

  if (ts) {
    size_t arity = f.arity();
    goal = Yap_MkApplTerm(Yap_MkFunctor(f.name().asAtom(),arity), arity, ts);
    nts = RepAppl(goal) + 1;
    for (arity_t i = 0; i < arity; i++)
      XREGS[i + 1] = ts[i];
  } else {
    goal = MkVarTerm();
  }
  openQuery();
  names = new YAPPairTerm(TermNil);
  RECOVER_MACHINE_REGS();
}

#if 0
YAPQuery::YAPQuery(YAPFunctor f, YAPTerm ts[]) : YAPPredicate(f) {
        CACHE_REGS

  /* ignore flags for now */
  BACKUP_MACHINE_REGS();
  if (ts) {
goal =  YAPApplTerm(f, nts);
  } else {
   goal = YAPVarTerm();
   nts = nullptr;
  }
  names = new YAPPairTerm( TermNil );
  openQuery(term(), nts);
  RECOVER_MACHINE_REGS();
}
#endif

YAPQuery::YAPQuery(YAPPredicate p, YAPTerm ts[]) : YAPPredicate(p.ap) {
        CACHE_REGS

  BACKUP_MACHINE_REGS();
  try {
    arity_t arity = p.ap->ArityOfPE;
    if (arity) {
      goal = YAPApplTerm(YAPFunctor(p.ap->FunctorOfPred), ts).term();
      for (arity_t i = 0; i < arity; i++)
        XREGS[i + 1] = ts[i].term();
      openQuery();
    } else {
      goal = MkAtomTerm((Atom)(p.ap->FunctorOfPred));
      openQuery();
    }
    names = new YAPPairTerm(TermNil);
  } catch (...) {
  }
  RECOVER_MACHINE_REGS();
}

bool YAPQuery::next() {
  CACHE_REGS
  bool result = false;
  //std::cerr <<  "next " <<  YAPTerm(goal).text() << "\n";
  q_h.CurSlot = Yap_StartSlots();
  q_h.p = P;
  q_h.cp = CP;

  sigjmp_buf buf, *oldp = (sigjmp_buf *)LOCAL_RestartEnv;
  e = nullptr;
  BACKUP_MACHINE_REGS();
  if (!q_open)
    return false;
  LOCAL_RestartEnv = &buf;
  // don't forget, on success these guys may create slots
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");

  if (q_state == 0) {
    // Yap_do_low_level_trace = 1;
    result = (bool)YAP_EnterGoal(ap, nullptr, &q_h);
  } else {
    LOCAL_AllowRestart = q_open;
    result = (bool)YAP_RetryGoal(&q_h);
  }
  q_state = 1;
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "out  %d", result);
  if (!result) {
    YAP_LeaveGoal(result, &q_h);
  Yap_CloseHandles(q_h.CurSlot);
    q_open = false;
  } else if(deterministic()) {
      YAP_LeaveGoal(result, &q_h);
    q_open = false;
    }
  
  YAPCatchError();
  RECOVER_MACHINE_REGS();
  LOCAL_RestartEnv = oldp;
  return result;
}

PredEntry *YAPQuery::rewriteUndefQuery() {
        CACHE_REGS

  Term ts[2];
  ts[0] = CurrentModule;
  ts[1] = goal;
   goal = Yap_MkApplTerm(FunctorModule, 2, ts);
  ARG1 = goal = Yap_SaveTerm(goal);
  return ap = PredCall;
}

PredEntry *YAPEngine::rewriteUndefEngineQuery(PredEntry *a, Term &tgoal,Term mod)
{ 
      CACHE_REGS

  Term  ts[2];
      ts[0]=mod;
      ts[1] = tgoal;
       tgoal = Yap_MkApplTerm(FunctorModule, 2, ts);
       tgoal = Yap_SaveTerm(Yap_MkApplTerm(FunctorCall, 1, &tgoal));
       LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
       return PredCall;

       // return YAPApplTerm(FunctorUndefinedQuery, ts);
}

void YAPQuery::cut() {
  BACKUP_MACHINE_REGS();
  if (!q_open || q_state == 0)
    return;
  YAP_LeaveGoal(true, &q_h);
  q_open = false;
  // LOCAL_execution = this;
  RECOVER_MACHINE_REGS();
}

bool YAPQuery::deterministic() {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (!q_open || q_state == 0)
    return false;
  choiceptr myB = (choiceptr)(LCL0 - q_h.b_entry);
  return (B >= myB);
  RECOVER_MACHINE_REGS();
}

YAPTerm YAPQuery::getTerm(yhandle_t t) {
  return YAPTerm(t); }

void YAPQuery::close() {
  CACHE_REGS

  RECOVER_MACHINE_REGS();
  Yap_ResetException(nullptr);
  /* need to implement backtracking here */
  if (q_open != true || q_state == 0) {
    RECOVER_MACHINE_REGS();
    return;
  }
  YAP_LeaveGoal(false, &q_h);
  q_open = 0;
  Yap_CloseHandles(q_h.CurSlot);
  // LOCAL_execution = this;
  RECOVER_MACHINE_REGS();
}

#if __ANDROID__

#include <jni.h>
#include <string.h>

JNIEnv *Yap_jenv;

extern JNIEXPORT jint JNICALL JNI_MySQLOnLoad(JavaVM *vm, void *reserved);

JNIEXPORT jint JNICALL JNI_MySQLOnLoad(JavaVM *vm, void *reserved) {
        CACHE_REGS

  JNIEnv *env;
  if (vm->GetEnv(reinterpret_cast<void **>(&env), JNI_VERSION_1_6) != JNI_OK) {
    return -1;
  }
  Yap_jenv = env;

  return JNI_VERSION_1_6;
}

char *Yap_AndroidBufp;

static size_t Yap_AndroidMax, Yap_AndroidSz;

extern void (*Yap_DisplayWithJava)(int c);

static YAPCallback *cb = new YAPCallback();

void Yap_displayWithJava(int c) {
        CACHE_REGS

  char *ptr = Yap_AndroidBufp;
  if (!ptr)
    ptr = Yap_AndroidBufp = (char *)malloc(Yap_AndroidSz);
  ptr[Yap_AndroidSz++] = c;
  if (Yap_AndroidMax - 1 == Yap_AndroidSz) {
    if (Yap_AndroidMax < 32 * 1024) {
      Yap_AndroidMax *= 2;
    } else {
      Yap_AndroidMax += 32 * 1024;
    }
    Yap_AndroidBufp = (char *)realloc(ptr, Yap_AndroidMax);
  }
  Yap_AndroidBufp[Yap_AndroidSz] = '\0';
  if (c == '\n') {
    Yap_AndroidBufp[Yap_AndroidSz] = '\0';
    cb->run(Yap_AndroidBufp);
    Yap_AndroidSz = 0;
  }
}

#endif

void YAPEngine::doInit(YAP_file_type_t BootMode, YAPEngineArgs *engineArgs) {
        CACHE_REGS

  if (GLOBAL_Initialised)
    return;
  GLOBAL_Initialised = true;
  YAP_Init(engineArgs);
// yerror = throw YAPError( SOURCE(), );
CurrentModule = LOCAL_SourceModule = TermUser;
#if YAP_PYTHON
  do_init_python();
#endif
  // std::string s = "initialize_prolog";
  // YAPPredicate p = YAPPredicate(MkAtomTerm(Yap_LookupAtom(s.c_str())));
  // YAPQuery initq = YAPQuery(YAPPredicate(p), nullptr);
  // if (initq.next()) {
  //   initq.cut();
  // }
  CurrentModule = TermUser;
  LOCAL_SourceModule = TermUser;
}

YAPEngine::YAPEngine(int argc, char *argv[],
                     YAPCallback *cb)
    : _callback(0) { // a single engine can be active
      CACHE_REGS

  
  YAP_file_type_t BootMode;
  engine_args = new YAPEngineArgs();
  BootMode = YAP_parse_yap_arguments(argc, argv, engine_args);
  // delYAPCallback()b
  // if (cb)
  //  setYAPCallback(cb);

  doInit(BootMode, engine_args);
  CurrentModule = LOCAL_SourceModule = TermUser;
}

YAPPredicate::YAPPredicate(YAPAtom at) {
  CACHE_REGS
  ap = RepPredProp(PredPropByAtom(at.a, Yap_CurrentModule()));
}

YAPPredicate::YAPPredicate(YAPAtom at, uintptr_t arity) {
  CACHE_REGS
  if (arity) {
    Functor f = Yap_MkFunctor(at.a, arity);
    ap = RepPredProp(PredPropByFunc(f, Yap_CurrentModule()));
  } else {
    ap = RepPredProp(PredPropByAtom(at.a, Yap_CurrentModule()));
  }
}

/// auxiliary routine to find a predicate in the current module.
PredEntry *YAPPredicate::getPred(Term &t, Term &m, CELL *&out) {
  t = Yap_StripModule(t, &m);

  if (IsVarTerm(t) || IsNumTerm(t)) {
    if (IsVarTerm(t))
      throw YAPError(SOURCE(), INSTANTIATION_ERROR, t, 0);
    else if (IsNumTerm(t))
      throw YAPError(SOURCE(), TYPE_ERROR_CALLABLE, t, 0);
  }
  if (IsAtomTerm(t)) {
    ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
    return ap;
  } else if (IsPairTerm(t)) {
    Term ts[2], *s = (out ? out : ts);
    Functor FunctorConsult = Yap_MkFunctor(Yap_LookupAtom("consult"), 1);
    s[1] = t;
    s[0] = m;
    t = Yap_MkApplTerm(FunctorModule, 2, s);
    t = Yap_MkApplTerm(FunctorConsult, 1, &t);
    if (!out)
      out = RepAppl(t) + 1;
  }
  Functor f = FunctorOfTerm(t);
  if (IsExtensionFunctor(f)) {
    throw YAPError(SOURCE(), TYPE_ERROR_CALLABLE, t, 0);
  } else {
    ap = RepPredProp(PredPropByFunc(f, m));
    if (out)
      memmove(out, (const CELL *)RepAppl(t) + 1, ap->ArityOfPE * sizeof(CELL));
    else
      out = RepAppl(t) + 1;
  }
  return ap;
}

bool YAPPrologPredicate::assertClause(YAPTerm cl, bool last, YAPTerm source) {
  CACHE_REGS

  RECOVER_MACHINE_REGS();
  Term tt = cl.gt();
  Term ntt = cl.gt();
  gc_entry_info_t info;
    Yap_track_cpred( _Ystop, P, 0,   &info);

  yamop *codeaddr =
    Yap_cclause(tt, ap->ArityOfPE, (last ? TermAssertz : TermAsserta),MkIntTerm(0), Yap_CurrentModule(), (void*)&info); /* vsc: give the number of arguments
                               to cclause in case there is overflow */
  if (LOCAL_ErrorMessage) {
    RECOVER_MACHINE_REGS();
    return false;
  }
  Term *tref = &ntt;
  if (Yap_addclause(ntt, codeaddr, (last ? TermAssertz : TermAsserta),
                    Yap_CurrentModule(), tref)) {
    RECOVER_MACHINE_REGS();
  }
  return tref;
}

bool YAPPrologPredicate::assertFact(YAPTerm *cl, bool last) {
  CACHE_REGS
  arity_t i;
  RECOVER_MACHINE_REGS();
      gc_entry_info_t info;
    Yap_track_cpred( _Ystop, P, 0,   &info);
Term tt = AbsAppl(HR);
  *HR++ = (CELL)(ap->FunctorOfPred);
  for (i = 0; i < ap->ArityOfPE; i++, cl++)
    *HR++ = cl->gt();
  yamop *codeaddr = Yap_cclause(tt, ap->ArityOfPE, (last ? TermAssertz : TermAsserta), MkIntTerm(0), CurrentModule,( void *) &info); /* vsc: give the number of arguments
                                        to cclause in case there is overflow */
  if (LOCAL_ErrorMessage) {
    RECOVER_MACHINE_REGS();
    return false;
  }
  Term *tref = &tt;
  if (Yap_addclause(tt, codeaddr, (last ? TermAssertz : TermAsserta),
                    Yap_CurrentModule(), tref)) {
    RECOVER_MACHINE_REGS();
  }
  return tref;
}

void *YAPPrologPredicate::retractClause(YAPTerm skeleton, bool all) {
  return 0;
}

std::string YAPError::text() {
  return "Error";
#if 0
std::stringstream s;
  s << "";
  if (info->errorNo == YAP_NO_ERROR)
    return 0;
      if (info->errorFunction) {
    s += info->errorFile;
    s += ":";
    sprintf(buf, "%ld", (long int)info->errorLine);
    s += buf;
    s += ":0 in C-code";
  }
  return s;
  if (info->prologPredLine) {
    s += "\n";
    s += info->prologPredFile;
    s += ":";
    s << info->prologPredLine;
    // YAPIntegerTerm(info->prologPredLine).text();
    s += ":0   ";
    s += info->prologPredModule;
    s += ":";
    s += (info->prologPredName);
    s += "/";
    s << info->prologPredArity;
  }
  s += " error ";
  if (info->classAsText == nullptr)
    info->classAsText = Yap_errorClassName(info->errorClass);
  if (info->classAsText != nullptr)
    s += info->classAsText;
  s += ".";
  if (info->errorAsText == nullptr)
    info->errorAsText = Yap_errorName(info->errorNo);
  if (info->errorAsText != nullptr)
    s += info->errorAsText;
  s += ".\n";
  //      printf("%s\n", s.c_str());
  return s.c_str();
#endif
}

void YAPEngine::reSet() {
     CACHE_REGS

  /* ignore flags  for now */
  if (B && B->cp_b && B->cp_ap != NOCODE)
    //    YAP_LeaveGoal(false, &q);
  LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
  if (LOCAL_CommittedError) {
    LOCAL_CommittedError->errorNo = YAP_NO_ERROR;
    free(LOCAL_CommittedError);
    LOCAL_CommittedError = NULL;
  }
  pop_text_stack(0);
  LOCAL_CurSlot = 0;
}

Term YAPEngine::top_level(std::string s) {
        CACHE_REGS

  /// parse string s and make term with var names
  /// available.
  Term tp;
  ARG1 = YAP_ReadBuffer(s.data(), &tp);
  ARG2 = tp;
  ARG3 = MkVarTerm();
  if (ARG1 == 0)
    throw YAPError(SOURCE(), SYNTAX_ERROR, ARG1, "in input query");
  YAPPredicate p = YAPPredicate(YAP_TopGoal());
  YAPQuery *Q = new YAPQuery(p, 0);
  Term ts[2];
  ts[0] = MkAddressTerm(Q);
  if (Q->next()) {
    ts[1] = ARG3;
  } else {
    ts[1] = TermNil;
  }
  return YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("t"), 2), 2, ts);
}

Term YAPEngine::next_answer(YAPQuery *&Q) {
      CACHE_REGS

  /// parse string s and make term with var names
  /// available.
  Term ts[2];
  ts[0] = MkAddressTerm(Q);
  if (Q->next()) {
    ts[1] = ARG3;
  } else {
    ts[1] = TermNil;
  }
  return YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("t"), 2), 2, ts);
}
