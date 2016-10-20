
#define YAP_CPP_INTERFACE 1

#include "yapi.hh"
#include <string>

extern "C" {

#if __ANDROID__
#include "android/log.h"
#endif

#include "YapInterface.h"
#include "blobs.h"

X_API char *Yap_TermToString(Term t, size_t *length, encoding_t encodingp,
                             int flags);

X_API void YAP_UserCPredicate(const char *, YAP_UserCPred, arity_t arity);
X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, arity_t,
                                      YAP_Term);
X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred,
                                  arity_t, arity_t);
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

YAPApplTerm::YAPApplTerm(YAPFunctor f, YAPTerm ts[]) : YAPTerm() {
  BACKUP_H();
  arity_t arity = ArityOfFunctor(f.f);
  Term *tt = new Term[arity];
  for (arity_t i = 0; i < arity; i++)
    tt[i] = ts[i].term();
  mk(Yap_MkApplTerm(f.f, arity, tt));
  delete[] tt;
  RECOVER_H();
}

YAPApplTerm::YAPApplTerm(const char *f, std::vector<YAPTerm> ts) : YAPTerm() {
  BACKUP_H();
  arity_t arity = ts.size();
  std::vector<Term> tt(arity);
  for (arity_t i = 0; i < arity; i++)
    tt[i] = ts[i].term();
  Functor ff = Yap_MkFunctor(Yap_LookupAtom(f), arity);
  t = Yap_MkApplTerm(ff, arity, &tt[0]);
  RECOVER_H();
}

YAPApplTerm::YAPApplTerm(YAPFunctor f) : YAPTerm() {
  BACKUP_H();
  arity_t arity = ArityOfFunctor(f.f);
  mk(Yap_MkNewApplTerm(f.f, arity));
  RECOVER_H();
}

YAPFunctor YAPApplTerm::getFunctor() { return YAPFunctor(FunctorOfTerm(gt())); }

YAPTerm &YAPTerm::operator[](arity_t i) {
  BACKUP_MACHINE_REGS();
  Term t0 = gt();
  Term tf = 0;
  if (IsApplTerm(t0)) {
    Functor f = FunctorOfTerm(t0);
    if (IsExtensionFunctor(f))
      return *new YAPTerm();
    tf = ArgOfTerm(i + 1, t0);
  } else if (IsPairTerm(t0)) {
    if (i == 0)
      tf = HeadOfTerm(t0);
    else if (i == 1)
      tf = TailOfTerm(t0);
  }
  RECOVER_MACHINE_REGS();
  return *new YAPTerm(tf);
}

YAPTerm &YAPListTerm::operator[](arity_t i) {
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
  return *new YAPTerm(tf);
}

YAPPairTerm::YAPPairTerm(YAPTerm th, YAPTerm tl) : YAPTerm() {
  CACHE_REGS
  BACKUP_H();
  mk(MkPairTerm(th.term(), tl.term()));
  RECOVER_H();
}

YAPPairTerm::YAPPairTerm() : YAPTerm() {
  BACKUP_H();
  t = Yap_MkNewPairTerm();
  RECOVER_H();
}

Term YAPTerm::gt() { CACHE_REGS return Yap_GetFromSlot(t); }

void YAPTerm::mk(Term t0) { CACHE_REGS t= Yap_InitSlot(t0); }


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

YAPTerm YAPTerm::deepCopy() {
  yhandle_t tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return *new YAPTerm(tn);
}

YAPListTerm YAPListTerm::dup() {
  yhandle_t tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return *new YAPListTerm(tn);
}

intptr_t YAPTerm::numberVars(intptr_t i0, bool skip_singletons) {
  BACKUP_MACHINE_REGS();

  intptr_t i = Yap_NumberVars(gt(), i0, skip_singletons);

  RECOVER_MACHINE_REGS();
  return i;
}

bool YAPTerm::exactlyEqual(YAPTerm t1) {
  bool out;
  BACKUP_MACHINE_REGS();

  out = Yap_eq(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

bool YAPTerm::unify(YAPTerm t1) {
  intptr_t out;
  BACKUP_MACHINE_REGS();

  out = Yap_unify(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

bool YAPTerm::unifiable(YAPTerm t1) {
  intptr_t out;
  BACKUP_MACHINE_REGS();

  out = Yap_Unifiable(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

bool YAPTerm::variant(YAPTerm t1) {
  intptr_t out;
  BACKUP_MACHINE_REGS();

  out = Yap_Variant(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

intptr_t YAPTerm::hashTerm(size_t sz, size_t depth, bool variant) {
  intptr_t out;

  BACKUP_MACHINE_REGS();

  out = Yap_TermHash(gt(), sz, depth, variant);

  RECOVER_MACHINE_REGS();
  return out;
}

const char *YAPTerm::text() {
  CACHE_REGS
  size_t length = 0;
  encoding_t enc = LOCAL_encoding;
  char *os;

  BACKUP_MACHINE_REGS();
  if (!(os = Yap_TermToString(Yap_GetFromSlot(t), &length, enc,
                              Handle_vars_f))) {
    RECOVER_MACHINE_REGS();
    return 0;
  }
  RECOVER_MACHINE_REGS();
  length = strlen(os) + 1;
  char *sm = (char *)malloc(length + 1);
  strcpy(sm, os);
  return sm;
}

const char *YAPQuery::text() { return goal.text(); }

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

YAPTerm YAPListTerm::car() {
  Term to = gt();
  if (IsPairTerm(to))
    return YAPTerm(HeadOfTerm(to));
  else {
    Yap_Error(TYPE_ERROR_LIST, to, "");
    throw YAPError();
  }
}

YAPTerm::YAPTerm(YAPFunctor f, YAPTerm ts[]) {
  CACHE_REGS
  BACKUP_H();
  Functor fun = f.f;
  arity_t arity = ArityOfFunctor(fun);
  while (HR + arity > ASP - 1024) {
    RECOVER_H();
    if (!Yap_dogc(0, NULL PASS_REGS)) {
      t = TermNil;
    }
    BACKUP_H();
  }
  if (fun == FunctorDot) {
    t = AbsPair(HR);
    HR[0] = ts[0].term();
    HR[1] = ts[1].term();
  } else {
    t = AbsAppl(HR);
    *HR++ = (CELL)fun;
    for (arity_t i = 0; i < arity; i++) {
      HR[i] = ts[i].term();
    }
    RECOVER_H();
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

YAPVarTerm::YAPVarTerm() {
  CACHE_REGS
  mk(MkVarTerm());
}

const char *YAPAtom::getName(void) {
  return Yap_AtomToUTF8Text( a, nullptr );
}




    void YAPQuery::openQuery() {
      CACHE_REGS
      arity_t arity = ap->ArityOfPE;
      if (arity) {
        Term *ts;
        Term t = goal.term();
        if (IsPairTerm(t)) {
          ts = RepPair(t);
        } else {
          ts = RepAppl(t) + 1;
        }
        for (arity_t i = 0; i < arity; i++) {
          XREGS[i + 1] = ts[i];
        }
      }
      // oq = LOCAL_execution;
      //  LOCAL_execution = this;
      q_open = true;
      q_state = 0;
      q_flags = true; // PL_Q_PASS_EXCEPTION;

      q_p = P;
      q_cp = CP;
      // make sure this is safe
      q_handles = Yap_StartSlots();
    }


bool YAPEngine::call(YAPPredicate ap, YAPTerm ts[]) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  arity_t arity = ap.getArity();
  bool result;
  YAP_dogoalinfo q;
  Term terr;
  jmp_buf q_env;
  for (arity_t i = 0; i < arity; i++)
    Yap_XREGS[i + 1] = ts[i].term();
  q.CurSlot = Yap_StartSlots();
  q.p = P;
  q.cp = CP;
  // make sure this is safe
  if (setjmp(q_env)) {
    if ((terr = Yap_PeekException())) {
      YAP_LeaveGoal(false, &q);
      Yap_CloseHandles(q.CurSlot);
      throw YAPError();
    }
    return false;
  }
  // don't forget, on success these l);
if (!result) {
    YAP_LeaveGoal(false, &q);
  } else {
    YAP_LeaveGoal(FALSE, &q);
  }
  RECOVER_MACHINE_REGS();
  return result;
}

bool YAPEngine::goalt(YAPTerm Yt) {
  return Yt.term();
   }


bool YAPEngine::goal(Term t) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  Term terr, tmod = CurrentModule, *ts = nullptr;
  PredEntry *ap = Yap_get_pred(t, tmod, "C++");
  arity_t arity = ap->ArityOfPE;
  bool result;
  jmp_buf q_env;

  if (IsApplTerm(t)) {
    ts = RepAppl(t) + 1;
  } else {
    ts = RepPair(t);
  }
  for (arity_t i = 0; i < arity; i++)
    XREGS[i + 1] = ts[i];
  q.CurSlot = Yap_StartSlots();
  q.p = P;
  q.cp = CP;
  // make sure this is safe

  if (setjmp(q_env)) {
    if ((terr = Yap_PeekException())) {
      YAP_LeaveGoal(false, &q);
      Yap_CloseHandles(q.CurSlot);
      throw YAPError();
    }
    return false;
  }
  // don't forget, on success these guys may create slots
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");


  result = (bool)YAP_EnterGoal(ap, nullptr, &q);
  if ((terr = Yap_GetException())) {
    YAP_LeaveGoal(false, &q);
    throw YAPError();
  }
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "out  %d", result);

  if (!result) {
    YAP_LeaveGoal(false, &q);
  } else {
    YAP_LeaveGoal(FALSE, &q);
  }
  RECOVER_MACHINE_REGS();
  return result;
}

void YAPEngine::release() {

  BACKUP_MACHINE_REGS();
   YAP_LeaveGoal(FALSE, &q);
  RECOVER_MACHINE_REGS();
 }

Term YAPEngine::fun(Term t) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  Term tmod = CurrentModule, *ts = nullptr;
  PredEntry *ap ;
  arity_t arity = arity;
  Functor f;
  jmp_buf q_env;
  Atom name;


  BACKUP_MACHINE_REGS();
      if (IsApplTerm(t)) {
    ts = RepAppl(t) + 1;
    f = (Functor)ts[-1];
    name = NameOfFunctor(f); 
    arity =ArityOfFunctor(f);
    for (arity_t i = 0; i < arity; i++)
      XREGS[i + 1] = ts[i];
  } else if (IsAtomTerm(t)) {
    name = AtomOfTerm(t);
    f = nullptr;
  } else if (IsPairTerm(t)) {
    XREGS[1] = ts[0];
    XREGS[2] = ts[1];
    name = AtomDot;
    f = FunctorDot;
  } else {
          Yap_Error(TYPE_ERROR_CALLABLE, t, 0);
          return 0L;
      }
 XREGS[arity+1] = MkVarTerm();
 arity ++;
 f = Yap_MkFunctor(name,arity);
 ap = (PredEntry *)(PredPropByFunc(f,tmod));
 q.CurSlot = Yap_StartSlots();
  q.p = P;
  q.cp = CP;
  // make sure this is safe
  yhandle_t o = Yap_InitHandle(XREGS[arity]);

  if (setjmp(q_env)) {
    Term terr;
     if ((terr = Yap_PeekException())) {
      YAP_LeaveGoal(false, &q);
      Yap_CloseHandles(q.CurSlot);
      throw YAPError();
    }
    return 0;
  }
  // don't forget, on success these guys may create slots
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exec  ");
       
  if ((o = (Term)YAP_EnterGoal(ap, nullptr, &q))==0)
    return 0;
  Term terr;
  if ((terr = Yap_GetException())) {
    YAP_LeaveGoal(false, &q);
      Yap_CloseHandles(q.CurSlot);
    throw YAPError();
  }
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "out  %d", result);

  Term result;
  t = Yap_GetFromSlot(q.CurSlot);
  Yap_CloseHandles(q.CurSlot);
  if (!t) {
    YAP_LeaveGoal(false, &q);
    result = 0;
  }
  RECOVER_MACHINE_REGS();
  return t;
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm ts[])
    : YAPPredicate(f, mod) {
  /* ignore flags  for now */
  BACKUP_MACHINE_REGS();
  goal = YAPTerm(f, ts);
  vnames = YAPListTerm();
  openQuery();
  RECOVER_MACHINE_REGS();
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm ts[]) : YAPPredicate(f) {
 /* ignore flags for now */
  BACKUP_MACHINE_REGS();
  goal = YAPTerm(f, ts);
  vnames = YAPListTerm();
  openQuery();
  RECOVER_MACHINE_REGS();
}

YAPQuery::YAPQuery(YAPPredicate p, YAPTerm ts[]) : YAPPredicate(p.ap) {
  BACKUP_MACHINE_REGS();
  goal = YAPTerm(YAPFunctor(ap->FunctorOfPred), ts);
  vnames = YAPListTerm();
  openQuery();
  RECOVER_MACHINE_REGS();
}

YAPListTerm YAPQuery::namedVars() {
  CACHE_REGS
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "vnames %s %d",
                      vnames.text(), LOCAL_CurSlot);
  return vnames; // should be o
}

YAPListTerm YAPQuery::namedVarsCopy() {
  CACHE_REGS
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "vnames %s %d",
                      vnames.text(), LOCAL_CurSlot);
  return YAPListTerm(YAP_CopyTerm(vnames.term())); // should be o
}

bool YAPQuery::next() {
  CACHE_REGS
  bool result;
  Term terr;

  BACKUP_MACHINE_REGS();
  if (!q_open)
    return false;
  if (setjmp(q_env)) {
    if ((terr = Yap_GetException())) {
      YAP_LeaveGoal(false, &q_h);
      Yap_CloseHandles(q_handles);
      throw YAPError();
    }
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
                        q_state, vnames.text(), LOCAL_CurSlot);
  } else {
    __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "fail");
  }
  q_state = 1;
  if ((terr = Yap_GetException())) {
    Yap_DebugPlWriteln(terr);
    YAP_LeaveGoal(false, &q_h);
    Yap_CloseHandles(q_handles);
    q_open = false;
    throw YAPError();
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
  return result;
}

void YAPQuery::cut() {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (!q_open || q_state == 0)
    return;
  YAP_LeaveGoal(FALSE, &q_h);
  q_open = 0;
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

static YAPEngine *curren;

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
    curren->run(Yap_AndroidBufp);
    Yap_AndroidSz = 0;
  }
}

#endif

void YAPEngine::doInit(YAP_file_type_t BootMode) {
  if ((BootMode = YAP_Init(&init_args)) == YAP_FOUND_BOOT_ERROR) {
    throw YAPError();
  }
  /* Begin preprocessor code */
  /* live */
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "$init_system");
#if __ANDROID__
  Yap_AndroidBufp = (char *)malloc(Yap_AndroidMax = 4096);
  Yap_AndroidBufp[0] = '\0';
  Yap_AndroidSz = 0;
#endif
  yerror = YAPError();

  YAPQuery initq = YAPQuery(YAPAtom("$init_system"));
  if (initq.next()) {
    initq.cut();
  } else {
    // should throw exception
  }
}

YAPEngine::YAPEngine(char *savedState, char *bootFile, size_t stackSize,
                     size_t trailSize, size_t maxStackSize, size_t maxTrailSize,
                     char *libDir, char *goal, char *topLevel, bool script,
                     bool fastBoot,
                     YAPCallback *cb)
    : _callback(0) { // a single engine can be active

  YAP_file_type_t BootMode;
  int Argc = 1;
  char **Argv;
  __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "YAP %s ", bootFile);

  // delYAPCallback()b
  // if (cb)
  //  setYAPCallback(cb);
  curren = this;
  {
    size_t l1 = 2 * sizeof(char *);
    if (!(Argv = (char **)malloc(l1)))
      return;
    Argv[0] = (char *)malloc(4);
    strcpy(Argv[0], "yap");
    Argv[1] = NULL;
  }
  BootMode = Yap_InitDefaults(&init_args, NULL, Argc, Argv);
  init_args.SavedState = savedState;
  init_args.StackSize = stackSize;
  init_args.TrailSize = trailSize;
  init_args.MaxStackSize = maxStackSize;
  init_args.MaxTrailSize = maxTrailSize;
  init_args.YapLibDir = libDir;
  init_args.YapPrologBootFile = bootFile;
  init_args.YapPrologGoal = goal;
  init_args.YapPrologTopLevelGoal = topLevel;
  init_args.HaltAfterConsult = script;
  init_args.FastBoot = fastBoot;
  doInit(BootMode);
}

YAPEngine::YAPEngine(int argc, char *argv[],
                     YAPCallback *cb)
    : _callback(0) { // a single engine can be active

  YAP_file_type_t BootMode;
  BootMode = YAP_parse_yap_arguments(argc, argv, &init_args);
  // delYAPCallback()b
  // if (cb)
  //  setYAPCallback(cb);
  curren = this;
  doInit(BootMode);
}

YAPPredicate::YAPPredicate(YAPAtom at) {
  CACHE_REGS
  ap = RepPredProp(PredPropByAtom(at.a, Yap_CurrentModule()));
}

YAPPredicate::YAPPredicate(YAPAtom at, arity_t arity) {
  CACHE_REGS
  if (arity) {
    Functor f = Yap_MkFunctor(at.a, arity);
    ap = RepPredProp(PredPropByFunc(f, Yap_CurrentModule()));
  } else {
    ap = RepPredProp(PredPropByAtom(at.a, Yap_CurrentModule()));
  }
}

/// auxiliary routine to find a predicate in the current module.
PredEntry *YAPPredicate::getPred(Term &t, Term *&outp) {
  CACHE_REGS
  Term m = Yap_CurrentModule();
  t = Yap_StripModule(t, &m);
  if (IsVarTerm(t) || IsNumTerm(t)) {
    if (IsVarTerm(t))
      Yap_Error(INSTANTIATION_ERROR, t, 0);
    else if (IsNumTerm(t))
      Yap_Error(TYPE_ERROR_CALLABLE, t, 0);
    throw YAPError();
  }
  if (IsAtomTerm(t)) {
    ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
    outp = (Term *)NULL;
    return ap;
  } else if (IsPairTerm(t)) {
    Term ts[2];
    ts[0] = t;
    ts[1] = m;
    t = Yap_MkApplTerm(FunctorCsult, 2, ts);
    outp = RepAppl(t) + 1;
  }
  Functor f = FunctorOfTerm(t);
  if (IsExtensionFunctor(f)) {
    Yap_Error(TYPE_ERROR_CALLABLE, t, 0);
    ;
    throw YAPError();
  } else {
    ap = RepPredProp(PredPropByFunc(f, m));
    outp = RepAppl(t) + 1;
  }
  return ap;
}

void *YAPPrologPredicate::assertClause(YAPTerm cl, bool last, YAPTerm source) {
  CACHE_REGS

  RECOVER_MACHINE_REGS();
  Term tt = cl.gt();
  Term sourcet;
  Term ntt = cl.gt();
  if (source.initialized())
    sourcet = source.gt();
  else
    sourcet = TermZERO;
  yamop *codeaddr = Yap_cclause(tt, ap->ArityOfPE, Yap_CurrentModule(),
                                sourcet); /* vsc: give the number of arguments
                                       to cclause in case there is overflow */
  if (LOCAL_ErrorMessage) {
    RECOVER_MACHINE_REGS();
    return 0;
  }
  Term *tref = &ntt;
  if (Yap_addclause(ntt, codeaddr, (last ? TermAssertz : TermAsserta),
                    Yap_CurrentModule(), tref)) {
    RECOVER_MACHINE_REGS();
  }
  return tref;
}

void *YAPPrologPredicate::assertFact(YAPTerm *cl, bool last) {
  CACHE_REGS
    arity_t i;
  RECOVER_MACHINE_REGS();
  Term tt = AbsAppl(HR);
  *HR++ = (CELL)(ap->FunctorOfPred);
  for (i = 0; i < ap->ArityOfPE; i++,cl++)
       *HR++ = cl->gt();
  yamop *codeaddr = Yap_cclause(tt, ap->ArityOfPE, Yap_CurrentModule(),
                                tt); /* vsc: give the number of arguments
                                       to cclause in case there is overflow */
  if (LOCAL_ErrorMessage) {
    RECOVER_MACHINE_REGS();
    return 0;
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

const char *YAPError::text() {

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
    s += LOCAL_ActiveError->prologPredFile->StrOfAE;
    s += ":";
    sprintf(buf, "%ld", (long int)LOCAL_ActiveError->prologPredLine);
    s += buf; // std::to_string(LOCAL_ActiveError->prologPredLine) ;
    // YAPIntegerTerm(LOCAL_ActiveError->prologPredLine).text();
    s += ":0   ";
    s += LOCAL_ActiveError->prologPredModule;
    s += ":";
    s += (LOCAL_ActiveError->prologPredName)->StrOfAE;
    s += "/";
    sprintf(buf, "%ld", (long int)LOCAL_ActiveError->prologPredArity);
    s += // std::to_string(LOCAL_ActiveError->prologPredArity);
        buf;
  }
  s += " error ";
  if (LOCAL_ActiveError->classAsText != nullptr)
    s += LOCAL_ActiveError->classAsText->StrOfAE;
  s += ".";
  s += LOCAL_ActiveError->errorAsText->StrOfAE;
  s += ".\n";
  if (LOCAL_ActiveError->errorTerm) {
    Term t = Yap_PopTermFromDB(LOCAL_ActiveError->errorTerm);
    if (t) {
      s += "error term is: ";
      s += YAPTerm(t).text();
      s += "\n";
    }
  }
  printf("%s\n", s.c_str());
  return s.c_str();
}

void YAPEngine::reSet()
   {
  /* ignore flags  for now */
  BACKUP_MACHINE_REGS();
  Yap_RebootHandles(worker_id);
     while (B->cp_b) B= B->cp_b;
    P = FAILCODE;
    Yap_exec_absmi(true, YAP_EXEC_ABSMI);
    /* recover stack space */
    HR = B->cp_h;
    TR = B->cp_tr;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth;
#endif /* DEPTH_LIMIT */
    YENV = ENV = B->cp_env;

  RECOVER_MACHINE_REGS();
   }


