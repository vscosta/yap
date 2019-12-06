
extern "C" {


#include "inline-only.h"
#define _EXPORT_KERNEL 1
}

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

X_API char *Yap_TermToBuffer(Term t, int flags);

X_API void YAP_UserCPredicate(const char *, YAP_UserCPred, arity_t arity);
X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, arity_t,
                                      YAP_Term);
X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred,
                                  YAP_Arity, YAP_Arity);

#if YAP_PYTHON
X_API bool do_init_python(void);
#endif
}

static void YAPCatchError() {
  if (LOCAL_CommittedError != nullptr &&
       LOCAL_CommittedError->errorNo != YAP_NO_ERROR) {
    // Yap_PopTermFromDB(info->errorTerm);
    // throw  throw YAPError(  );
    Term es[2];
    es[0] = TermError;
    es[1] = MkErrorTerm(LOCAL_CommittedError);
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
    es[1] = MkErrorTerm(LOCAL_ActiveError);
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
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
    inp.enc = LOCAL_encoding;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOM;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    t = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(char *s, size_t len) { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
    inp.enc = LOCAL_encoding;
    out.type = YAP_STRING_ATOM | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    t = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(wchar_t *s) : YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    t = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(wchar_t *s, size_t len) : YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(MkAtomTerm(out.val.a));
  else
    t = 0L;
  RECOVER_H();
}

YAPStringTerm::YAPStringTerm(char *s) { // build string
  BACKUP_H();

  CACHE_REGS
  Term ts = MkStringTerm(s);
  mk(ts);
  RECOVER_H();
}

YAPStringTerm::YAPStringTerm(char *s, size_t len) { // build string
  BACKUP_H();

  CACHE_REGS

  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_STRING | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(out.val.t);
  else
    t = 0L;
  RECOVER_H();
}

YAPStringTerm::YAPStringTerm(wchar_t *s) : YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS

  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(out.val.t);
  else
    t = 0L;
  RECOVER_H();
}

YAPStringTerm::YAPStringTerm(wchar_t *s, size_t len)
    : YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS

  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING | YAP_STRING_NCHARS | YAP_STRING_TRUNC;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(out.val.t);
  else
    t = 0L;
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
    BACKUP_H();
    arity_t arity = ts.size();
    Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
    Term o = Yap_MkNewApplTerm(ff, arity);
    Term *tt = RepAppl(o) + 1;
    for (arity_t i = 0; i < arity; i++)
        tt[i] = ts[i];
    mk(o);
    RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const std::string f, std::vector<YAPTerm> ts) {
    BACKUP_H();
    arity_t arity = ts.size();
    Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
    Term o = Yap_MkNewApplTerm(ff, arity);
    Term *tt = RepAppl(o) + 1;
    for (arity_t i = 0; i < arity; i++)
        tt[i] = ts[i].term();
    mk(o);
    RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const std::string f, YAPTerm a1) {
  BACKUP_H();
  arity_t arity = 1;
  Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
  Term o = Yap_MkNewApplTerm(ff, arity);
  Term *tt = RepAppl(o) + 1;
  tt[0] = a1.term();
  mk(o);
    RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const std::string f, YAPTerm a1, YAPTerm a2) {
  BACKUP_H();
  arity_t arity = 2;
  Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
  Term o = Yap_MkNewApplTerm(ff, arity);
  Term *tt = RepAppl(o) + 1;
  tt[0] = a1.term();
  tt[1] = a2.term();
  mk(o);
    RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const std::string f, YAPTerm a1, YAPTerm a2, YAPTerm a3) {
  BACKUP_H();
  arity_t arity = 3;
  Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
  Term o = Yap_MkNewApplTerm(ff, arity);
  Term *tt = RepAppl(o) + 1;
  tt[0] = a1.term();
  tt[2] = a2.term();
  tt[3] = a3.term();
  mk(o);
    RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const std::string f, YAPTerm a1, YAPTerm a2, YAPTerm a3,  YAPTerm a4) {
  BACKUP_H();
  arity_t arity = 4;
  Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
  Term o = Yap_MkNewApplTerm(ff, arity);
  Term *tt = RepAppl(o) + 1;
  tt[0] = a1.term();
  tt[2] = a2.term();
  tt[3] = a3.term();
  tt[4] = a4.term();
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
  Term tf = 0;
  while (IsPairTerm(t0)) {
    if (i == 0) {

      tf = HeadOfTerm(t0);
      break;
    } else {
      t0 = TailOfTerm(t0);
      i--;
    }
  }
  RECOVER_MACHINE_REGS();
  return RepPair(tf)[i];
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
    throw YAPError(SOURCE(), TYPE_ERROR_LIST, (t), nullptr);
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
  Int l = Yap_SkipList(&t1, &tailp);
  if (l < 0) {
    throw YAPError(SOURCE(), TYPE_ERROR_LIST, (t), nullptr);
  }
  std::vector<YAPTerm> o = *new std::vector<YAPTerm>(l);
  int i = 0;
  Term t = gt();
  while (t != TermNil) {
    o[i++] = YAPTerm(HeadOfTerm(t));
    t = TailOfTerm(t);
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

Term YAPListTerm::dup() {
  yhandle_t tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return tn;
}

intptr_t YAPTerm::numberVars(intptr_t i0, bool skip_singletons) {
  BACKUP_MACHINE_REGS();

  intptr_t i = Yap_NumberVars(gt(), i0, skip_singletons, nullptr);

  RECOVER_MACHINE_REGS();
  return i;
}

const char *YAPQuery::text() { return YAPTerm(goal).text(); }

YAPIntegerTerm::YAPIntegerTerm(intptr_t i) {
  CACHE_REGS Term tn = MkIntegerTerm(i);
  mk(tn);
}

/*
YAPTerm *YAPTerm::vars()
{
  BACKUP_MACHINE_REGS();
  CACHE_REGS
  YAPPairTerm lv = YAPPairTerm(Yap_TermVariables(gt(), 0 PASS_REGS));
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

YAPListTerm::YAPListTerm(YAPTerm ts[], arity_t n) {
  CACHE_REGS
  BACKUP_H();
  if (n == 0)
    t = TermNil;
  while (HR + n * 2 > ASP - 1024) {
    RECOVER_H();
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      t = TermNil;
    }
    BACKUP_H();
  }
  t = AbsPair(HR);
  for (arity_t i = 0; i < n; i++) {
    HR[2 * i] = ts[i].gt();
    HR[2 * i + 1] = AbsPair(HR + (2 * i + 2));
  }
}

const char *YAPAtom::getName(void) { return Yap_AtomToUTF8Text(a); }

void YAPQuery::openQuery() {
  CACHE_REGS
  if (ap == NULL || ap->OpcodeOfPred == UNDEF_OPCODE) {
    ap = rewriteUndefQuery();
  }
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
  Term omod = CurrentModule;
  PredEntry *ap = nullptr;
  if (IsStringTerm(tmod))
    tmod = MkAtomTerm(Yap_LookupAtom(StringOfTerm(tmod)));
  ap =  Yap_get_pred(t, tmod, "C++");
  if (ap == nullptr ||
      ap->OpcodeOfPred == UNDEF_OPCODE) {
    ap = rewriteUndefEngineQuery(ap, t, tmod);
  }
  if (IsApplTerm(t))
    ts = RepAppl(t) + 1;
  else if (IsPairTerm(t))
    ts = RepPair(t);
  /* legal ap */
  arity_t arity = ap->ArityOfPE;

  for (arity_t i = 0; i < arity; i++) {
    XREGS[i + 1] = ts[i];
  }
  ts = nullptr;
  bool result;
  // allow Prolog style exception handling
  // don't forget, on success these guys may create slots
  //__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");

   result = (bool)YAP_EnterGoal(ap, nullptr, &q);
  //  std::cerr << "mgoal "  << YAPTerm(tmod).text() << ":" << YAPTerm(t).text() << "\n
  YAP_LeaveGoal(result, &q);
  if (release)
    HR = B->cp_h;
 ENV = LCL0-oenv;
 B = (choiceptr)(LCL0-oB);
  CurrentModule = LOCAL_SourceModule = omod;
  //      PyEval_RestoreThread(_save);
  RECOVER_MACHINE_REGS();
  return result;
}
/**
 * called when a query must be terminated and its state fully recovered,
 * @type {[type]}
 */
void YAPEngine::release() {

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
  // std::cerr <<  "next " <<  YAPTerm(goal).text() << "\n";
  q_h.CurSlot = Yap_StartSlots();
  q_h.p = P;
  q_h.cp = CP;

  sigjmp_buf buf, *oldp = LOCAL_RestartEnv;
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
  }
  YAPCatchError();
  RECOVER_MACHINE_REGS();
  LOCAL_RestartEnv = oldp;
  return result;
}

PredEntry *YAPQuery::rewriteUndefQuery() {
  ARG1 = goal = Yap_SaveTerm(Yap_MkApplTerm(FunctorCall, 1, &goal));
  return ap = PredCall;
}

PredEntry *YAPEngine::rewriteUndefEngineQuery(PredEntry *a, Term &tgoal,
                                              Term mod) {
  tgoal = Yap_MkApplTerm(FunctorCall, 1, &tgoal);
  LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
  return PredCall;

  // return YAPApplTerm(FunctorUndefinedQuery, ts);
}

void YAPQuery::cut() {
  CACHE_REGS

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

YAPTerm YAPQuery::getTerm(yhandle_t t) { return YAPTerm(t); }

void YAPQuery::close() {
  CACHE_REGS

  RECOVER_MACHINE_REGS();
  Yap_ResetException(worker_id);
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
  if (init_done)
    return;
  init_done = true;
    if (BootMode == YAP_FOUND_BOOT_ERROR) {
    std::cerr << "Exception received by  " << __func__ << "( "
              << "while booting"
              << ").\n Forwarded...\n\n";
    return;
  }
  YAP_Init(engineArgs);
/* Begin preprocessor code */
/* live */
// yerror = throw YAPError( SOURCE(), );
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

  YAP_file_type_t BootMode;
  engine_args = new YAPEngineArgs();
  BootMode = YAP_parse_yap_arguments(argc, argv, engine_args);
  // delYAPCallback()b
  // if (cb)
  //  setYAPCallback(cb);

  doInit(BootMode, engine_args);
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
  CACHE_REGS
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
  Term sourcet;
  Term ntt = cl.gt();
  if (source.initialized())
    sourcet = source.gt();
  else
    sourcet = TermZERO;
  yamop *codeaddr =
      Yap_cclause(tt, ap->ArityOfPE, Yap_CurrentModule(),
                  sourcet); /* vsc: give the number of arguments
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
  Term tt = AbsAppl(HR);
  *HR++ = (CELL)(ap->FunctorOfPred);
  for (i = 0; i < ap->ArityOfPE; i++, cl++)
    *HR++ = cl->gt();
  yamop *codeaddr = Yap_cclause(tt, ap->ArityOfPE, Yap_CurrentModule(),
                                tt); /* vsc: give the number of arguments
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
