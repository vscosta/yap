

#define _EXPORT_KERNEL 1

#include "yapi.hh"
extern "C" {

#if __ANDROID__
#include "android/log.h"
#endif

#include "YapInterface.h"
#include "blobs.h"

X_API char *Yap_TermToBuffer(Term t, encoding_t encodingp, int flags);

X_API void YAP_UserCPredicate(const char *, YAP_UserCPred, arity_t arity);
X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, arity_t,
                                      YAP_Term);
X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred,
                                  YAP_Arity, YAP_Arity);

#if YAP_PYTHON
X_API bool do_init_python(void);
#endif
}

YAPAtomTerm::YAPAtomTerm(char *s) { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
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
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_STRING;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(out.val.t);
  else
    t = 0L;
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

YAPApplTerm::YAPApplTerm(std::string f, std::vector<YAPTerm> ts) {
  BACKUP_H();
  arity_t arity = ts.size();
  Functor ff = Yap_MkFunctor(Yap_LookupAtom(f.c_str()), arity);
  Term o = Yap_MkNewApplTerm(ff, arity);
  Term *tt = RepAppl(o) + 1;
  for (arity_t i = 0; i < arity; i++)
    tt[i] = ts[i].term();
  mk(o);
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
  Term tf = 0;
  if (IsApplTerm(t0)) {
    // Functor f = FunctorOfTerm(t0);
    // if (IsExtensionFunctor(f))
    //  return 0;
    tf = RepAppl(t0)[(i + 1)];
  } else if (IsPairTerm(t0)) {
    if (i == 0)
      tf = HeadOfTerm(t0);
    else if (i == 1)
      tf = TailOfTerm(t0);
    RECOVER_MACHINE_REGS();
    tf = RepPair(tf)[i];
  }
  RECOVER_MACHINE_REGS();
  Yap_Error(TYPE_ERROR_COMPOUND, tf, "");
  throw YAPError();
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

Term YAPListTerm::dup() {
  yhandle_t tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return tn;
}

intptr_t YAPTerm::numberVars(intptr_t i0, bool skip_singletons) {
  BACKUP_MACHINE_REGS();

  intptr_t i = Yap_NumberVars(gt(), i0, skip_singletons);

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
    Yap_Error(TYPE_ERROR_LIST, to, "");
    return 0;
    throw YAPError();
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
  sigjmp_buf *oj = LOCAL_RestartEnv, buf;
  try {
    CACHE_REGS
    if (ap.ap == NULL)
      return false;
    BACKUP_MACHINE_REGS();
    arity_t arity = ap.getArity();
    bool result;
    YAP_dogoalinfo q;

    for (arity_t i = 0; i < arity; i++)
      XREGS[i + 1] = ts[i].term();

    if (ap.ap == nullptr || ap.ap->OpcodeOfPred == UNDEF_OPCODE) {
      Term g = YAP_MkApplTerm(ap.ap->FunctorOfPred, arity, XREGS + 1);
      ap = YAPPredicate(rewriteUndefEngineQuery(ap.ap, g, ap.ap->ModuleOfPred));
    }

    q.CurSlot = Yap_StartSlots();
    q.p = P;

    q.cp = CP;
    // allow Prolog style exceotion handling
    LOCAL_RestartEnv = &buf;
    if (sigsetjmp(*LOCAL_RestartEnv, false)) {
      return 0;
      throw YAPError();
    }
    // don't forget, on success these bindings will still be there);
    result = YAP_LeaveGoal(false, &q);
    Yap_CloseHandles(q.CurSlot);
    LOCAL_RestartEnv = oj;
    RECOVER_MACHINE_REGS();
    return result;
  } catch (YAPError e) {
    YAP_LeaveGoal(false, &q);
    Yap_CloseHandles(q.CurSlot);
    std::cerr << "Exception received by  "
              << YAPApplTerm(ap.functor(), ts).text() << ".\n Forwarded...\n\n";
    LOCAL_RestartEnv = oj;
    return 0;
    throw e;
  }
}

bool YAPEngine::mgoal(Term t, Term tmod) {
  sigjmp_buf buf, *oldp = LOCAL_RestartEnv;
  try {
    CACHE_REGS
    BACKUP_MACHINE_REGS();
    Term *ts = nullptr;
    PredEntry *ap = Yap_get_pred(t, tmod, "C++");

    if (ap == nullptr || ap->OpcodeOfPred == UNDEF_OPCODE) {
      ap = rewriteUndefEngineQuery(ap, t, tmod);
      if (ap == nullptr || ap->OpcodeOfPred == UNDEF_OPCODE)
        return false;
    } else {
      /* legal ap */
      arity_t arity = ap->ArityOfPE;

      if (arity) {
        if (IsApplTerm(t)) {
          ts = RepAppl(t) + 1;
        } else {
          ts = RepPair(t);
        }
        for (arity_t i = 0; i < arity; i++)
          XREGS[i + 1] = ts[i];
      } else if (IsAtomTerm(t)) {
        ts = nullptr;
      }
    }
    bool result;
    q.CurSlot = Yap_StartSlots();
    q.p = P;
    q.cp = CP;
    // allow Prolog style exceotion handling
    LOCAL_RestartEnv = &buf;
    if (sigsetjmp(*LOCAL_RestartEnv, false)) {
      return false;
      return 0;
      throw YAPError();
    }
    // don't forget, on success these guys may create slots
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");

    result = (bool)YAP_EnterGoal(ap, nullptr, &q);
    {
      YAP_LeaveGoal(false, &q);
      LOCAL_RestartEnv = oldp;
      RECOVER_MACHINE_REGS();
      return result;
    }
  } catch (YAPError e) {
    YAP_LeaveGoal(false, &q);
    Yap_CloseHandles(q.CurSlot);
    LOCAL_RestartEnv = oldp;
    return 0;
    throw e;
  }
}

void YAPEngine::release() {

  BACKUP_MACHINE_REGS();
  YAP_LeaveGoal(FALSE, &q);
  RECOVER_MACHINE_REGS();
}

Term YAPEngine::fun(Term t) {
  CACHE_REGS
  try {
    BACKUP_MACHINE_REGS();
    Term tmod = CurrentModule, *ts = nullptr;
    PredEntry *ap;
    arity_t arity;
    Functor f;
    Atom name;

    if (IsApplTerm(t)) {
      ts = RepAppl(t) + 1;
      f = (Functor)ts[-1];
      name = NameOfFunctor(f);
      arity = ArityOfFunctor(f);
      for (arity_t i = 0; i < arity; i++)
        XREGS[i + 1] = ts[i];
    } else if (IsAtomTerm(t)) {
      name = AtomOfTerm(t);
      f = nullptr;
      arity = 0;
    } else if (IsPairTerm(t)) {
      XREGS[1] = ts[0];
      XREGS[2] = ts[1];
      arity = 2;
      name = AtomDot;
      f = FunctorDot;
    } else {
      Yap_ThrowError(TYPE_ERROR_CALLABLE, t, 0);
      return 0L;
    }
    XREGS[arity + 1] = MkVarTerm();
    arity++;
    f = Yap_MkFunctor(name, arity);
    ap = (PredEntry *)(PredPropByFunc(f, tmod));
    if (ap == nullptr || ap->OpcodeOfPred == UNDEF_OPCODE) {
      Term g = (Yap_MkApplTerm(f, arity, ts));
      ap = rewriteUndefEngineQuery(ap, g, (ap->ModuleOfPred));
    }
    q.CurSlot = Yap_StartSlots();
    q.p = P;
    q.cp = CP;
    // make sure this is safe
    yhandle_t o = Yap_InitHandle(XREGS[arity]);
    // allow Prolog style exception handling
    sigjmp_buf buf, *oldp = LOCAL_RestartEnv;
    LOCAL_RestartEnv = &buf;
    if (sigsetjmp(*LOCAL_RestartEnv, false)) {
      return false;
      throw YAPError();
    }
    // don't forget, on success these guys may create slots
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");

    if (YAP_EnterGoal(ap, nullptr, &q) == 0) {
#if DEBUG
      fprintf(stderr, "function call failed:\n");
#endif
      LOCAL_RestartEnv = oldp;
      return 0;
    }
    DBTerm *pt = Yap_StoreTermInDB(Yap_GetFromSlot(o), arity);
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "out  %d", o);
    YAP_LeaveGoal(false, &q);
    Yap_CloseHandles(q.CurSlot);
    Term rc = Yap_PopTermFromDB(pt);
    LOCAL_RestartEnv = oldp;
    RECOVER_MACHINE_REGS();
    return rc;
  } catch (YAPError e) {
    YAP_LeaveGoal(false, &q);
    Yap_CloseHandles(q.CurSlot);
    std::cerr << "Exception received by  " << __func__ << "( "
              << YAPTerm(t).text() << ").\n Forwarded...";
    throw e;
  }
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm ts[])
    : YAPPredicate(f, mod) {

  /* ignore flags  for now */
  BACKUP_MACHINE_REGS();
  Term *nts;
  Term goal;

  if (ts) {

    goal = YAPApplTerm(f, ts).term();
    nts = RepAppl(goal) + 1;
    size_t arity = f.arity();
    for (int i = 0; i < arity; i++)
      XREGS[i + 1] = nts[i];
  } else {
    goal = MkVarTerm();
  }
  openQuery();
  names = YAPPairTerm(TermNil);
  RECOVER_MACHINE_REGS();
}

#if 0
YAPQuery::YAPQuery(YAPFunctor f, YAPTerm ts[]) : YAPPredicate(f) {
  /* ignore flags for now */
  BACKUP_MACHINE_REGS();
  CELL *nts;
  if (ts) {
  goal =  YAPApplTerm(f, nts);
  } else {
   goal = YAPVarTerm();
   nts = nullptr;
  }
  names = YAPPairTerm( TermNil );
  openQuery(term(), nts);
  RECOVER_MACHINE_REGS();
}
#endif

YAPQuery::YAPQuery(YAPTerm t) : YAPPredicate(t) {
  BACKUP_MACHINE_REGS();
  CELL *nts;
  Term tt = t.term();
  goal = *new YAPTerm(tt);
  if (IsPairTerm(tt)) {
    nts = RepPair(tt);
    tt = Yap_MkApplTerm(FunctorCsult, 1, nts);
  }
  if (IsApplTerm(tt)) {
    Functor f = FunctorOfTerm(tt);
    if (!IsExtensionFunctor(f)) {
      nts = nullptr;
      arity_t arity = ap->ArityOfPE;
      if (arity) {
        nts = RepAppl(tt) + 1;
        for (arity_t i = 0; i < arity; i++)
          XREGS[i + 1] = nts[i];
      }
    }
  }
  openQuery();
  names = YAPPairTerm(TermNil);
  RECOVER_MACHINE_REGS();
}

YAPQuery::YAPQuery(YAPPredicate p, YAPTerm ts[]) : YAPPredicate(p.ap) {
  BACKUP_MACHINE_REGS();
  arity_t arity = p.ap->ArityOfPE;
  if (arity) {
    goal = YAPApplTerm(YAPFunctor(p.ap->FunctorOfPred), ts).term();
    for (arity_t i = 0; i < arity; i++)
      XREGS[i + 1] = ts[i].term();
    openQuery();
  } else {
    goal = YAPAtomTerm((Atom)(p.ap->FunctorOfPred));
    openQuery();
  }
  names = TermNil;
  RECOVER_MACHINE_REGS();
}

bool YAPQuery::next() {
  CACHE_REGS
  bool result = false;
  sigjmp_buf buf, *oldp = LOCAL_RestartEnv;
  Term terr;
  try {
    BACKUP_MACHINE_REGS();
    if (!q_open)
      return false;
    LOCAL_RestartEnv = &buf;
    if (sigsetjmp(*LOCAL_RestartEnv, false)) {
      // throw YAPError();
      return false;
    }
    // don't forget, on success these guys may create slots
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");

    if (q_state == 0) {
      result = (bool)YAP_EnterGoal(ap, nullptr, &q_h);
    } else {
      LOCAL_AllowRestart = q_open;
      result = (bool)YAP_RetryGoal(&q_h);
    }
    if (result) {
      __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "vnames  %d %s %d",
                          q_state, names.text(), LOCAL_CurSlot);
    } else {
      __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "fail");
    }
    q_state = 1;
    if ((terr = Yap_GetException())) {
      if ((terr = Yap_GetException())) {
        LOCAL_RestartEnv = &buf;
        throw YAPError();
      }
    }
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "out  %d", result);

    if (!result) {
      YAP_LeaveGoal(false, &q_h);
      Yap_CloseHandles(q_handles);
      q_open = false;
    } else {
      q_handles = Yap_StartSlots();
    }
    RECOVER_MACHINE_REGS();
    LOCAL_RestartEnv = oldp;
    return result;
  } catch (YAPError e) {
    q_open = false;
    Yap_PopTermFromDB(LOCAL_ActiveError->errorTerm);
    memset(LOCAL_ActiveError, 0, sizeof(*LOCAL_ActiveError));
    YAP_LeaveGoal(false, &q_h);
    Yap_CloseHandles(q_handles);
    q_open = false;
    std::cerr << "Exception received by  " << __func__ << "( "
              << YAPTerm(terr).text() << ").\n Forwarded...\n\n";
    LOCAL_RestartEnv = oldp;
    throw e;
  }
}

PredEntry *YAPQuery::rewriteUndefQuery() {
  Term ts[3];
  ARG1 = ts[0] = goal.term();
  ARG2 = ts[1] = ap->ModuleOfPred;
  ARG3 = ts[2] = Yap_cp_as_integer(B PASS_REGS);
  goal = YAPApplTerm(FunctorUndefinedQuery, ts);
  return ap = PredUndefinedQuery;
}

PredEntry *YAPEngine::rewriteUndefEngineQuery(PredEntry *a, Term goal,
                                              Term mod) {
  Term ts[3];
  ARG1 = ts[0] = goal;
  ARG2 = ts[1] = mod;
  ARG3 = ts[2] = Yap_cp_as_integer(B PASS_REGS);
  return PredUndefinedQuery;
  // return YAPApplTerm(FunctorUndefinedQuery, ts);
}

void YAPQuery::cut() {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (!q_open || q_state == 0)
    return;
  YAP_LeaveGoal(FALSE, &q_h);
  q_open = false;
  // LOCAL_execution = this;
  RECOVER_MACHINE_REGS();
}

bool YAPQuery::deterministic() {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (!q_open || q_state == 0)
    return false;
  choiceptr myB = (choiceptr)(LCL0 - q_h.b);
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
  YAP_LeaveGoal(FALSE, &q_h);
  q_open = 0;
  Yap_CloseHandles(q_handles);
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

void YAPEngine::doInit(YAP_file_type_t BootMode) {
  if ((BootMode = YAP_Init(engine_args)) == YAP_FOUND_BOOT_ERROR) {
    return;
    throw YAPError();
  }
/* Begin preprocessor code */
/* live */
// yerror = YAPError();
#if YAP_PYTHON
  do_init_python();
#endif
  std::string s = "initialize_prolog";
  YAPPredicate p = YAPPredicate(YAPAtomTerm(s));
  YAPQuery initq = YAPQuery(YAPPredicate(p), nullptr);
  if (initq.next()) {
    initq.cut();
  }
  CurrentModule = TermUser;
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

  doInit(BootMode);
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
PredEntry *YAPPredicate::getPred(YAPTerm &tt, CELL *&outp) {
  CACHE_REGS
  Term m = Yap_CurrentModule(), t = tt.term();
  t = Yap_StripModule(t, &m);
  if (IsVarTerm(t) || IsNumTerm(t)) {
    if (IsVarTerm(t))
      Yap_ThrowError(INSTANTIATION_ERROR, tt.term(), 0);
    else if (IsNumTerm(t))
      Yap_ThrowError(TYPE_ERROR_CALLABLE, tt.term(), 0);
    throw YAPError();
  }
  tt.put(t);
  if (IsAtomTerm(t)) {
    ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
    outp = (Term *)NULL;
    return ap;
  } else if (IsPairTerm(t)) {
    Term ts[2];
    Functor FunctorConsult = Yap_MkFunctor(Yap_LookupAtom("consult"), 1);
    ts[1] = t;
    ts[0] = m;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
    t = Yap_MkApplTerm(FunctorConsult, 1, &t);
    tt.put(t);
    outp = RepAppl(t) + 1;
  }
  Functor f = FunctorOfTerm(t);
  if (IsExtensionFunctor(f)) {
    Yap_ThrowError(TYPE_ERROR_CALLABLE, t, 0);
  } else {
    ap = RepPredProp(PredPropByFunc(f, m));
    outp = RepAppl(t) + 1;
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
  char buf[256];
  std::string s = "";
  if (LOCAL_ActiveError->errorFunction) {
    s += LOCAL_ActiveError->errorFile;
    s += ":";
    sprintf(buf, "%ld", (long int)LOCAL_ActiveError->errorLine);
    s += buf;
    s += ":0 in C-code";
  }
  if (LOCAL_ActiveError->prologPredLine) {
    s += "\n";
    s += LOCAL_ActiveError->prologPredFile;
    s += ":";
    sprintf(buf, "%ld", (long int)LOCAL_ActiveError->prologPredLine);
    s += buf; // std::to_string(LOCAL_ActiveError->prologPredLine) ;
    // YAPIntegerTerm(LOCAL_ActiveError->prologPredLine).text();
    s += ":0   ";
    s += LOCAL_ActiveError->prologPredModule;
    s += ":";
    s += (LOCAL_ActiveError->prologPredName);
    s += "/";
    sprintf(buf, "%ld", (long int)LOCAL_ActiveError->prologPredArity);
    s += // std::to_string(LOCAL_ActiveError->prologPredArity);
        buf;
  }
  s += " error ";
  if (LOCAL_ActiveError->classAsText != nullptr)
    s += LOCAL_ActiveError->classAsText;
  s += ".";
  s += LOCAL_ActiveError->errorAsText;
  s += ".\n";
  if (LOCAL_ActiveError->errorTerm) {
    Term t = Yap_PopTermFromDB(LOCAL_ActiveError->errorTerm);
    if (t) {
      s += "error term is: ";
      s += YAPTerm(t).text();
      s += "\n";
    }
  }
  //      printf("%s\n", s.c_str());
  return s.c_str();
}

void YAPEngine::reSet() {
  /* ignore flags  for now */
  BACKUP_MACHINE_REGS();
  Yap_RebootHandles(worker_id);
  while (B && B->cp_b)
    B = B->cp_b;
  if (B) {
    P = FAILCODE;
    Yap_exec_absmi(true, YAP_EXEC_ABSMI);
    /* recover stack space */
    HR = B->cp_h;
    TR = B->cp_tr;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth;
#endif /* DEPTH_LIMIT */
    YENV = ENV = B->cp_env;
  }
  RECOVER_MACHINE_REGS();
}

YAPError::YAPError(yap_error_number id, YAPTerm culprit, std::string txt) {
  ID = id;
  goal = culprit.text();
  info = txt;
}

Term YAPEngine::top_level(std::string s) {
  /// parse string s and make term with var names
  /// available.
  Term tp;
  ARG1 = YAP_ReadBuffer(s.data(), &tp);
  ARG2 = tp;
  ARG3 = MkVarTerm();
  if (ARG1 == 0)
    YAPError(SYNTAX_ERROR);
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
