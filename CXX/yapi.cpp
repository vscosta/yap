
#define YAP_CPP_INTERFACE 1

#include "yapi.hh"

extern "C" {

#include "YapInterface.h"
#include "blobs.h"

X_API char *Yap_TermToString(Term t, size_t *length, encoding_t encodingp,
                             int flags);

X_API void YAP_UserCPredicate(const char *, YAP_UserCPred, YAP_Arity arity);
X_API void YAP_UserCPredicateWithArgs(const char *, YAP_UserCPred, YAP_Arity,
                                      YAP_Term);
X_API void YAP_UserBackCPredicate(const char *, YAP_UserCPred, YAP_UserCPred,
                                  YAP_Arity, YAP_Arity);
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
  out.sz = len;
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
  out.sz = len;
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
  out.sz = len;
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
  out.sz = len;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk(out.val.t);
  else
    t = 0L;
  RECOVER_H();
}

YAPApplTerm::YAPApplTerm(YAPFunctor f, YAPTerm ts[]) : YAPTerm() {
  BACKUP_H();
  UInt arity = ArityOfFunctor(f.f);
  mk(Yap_MkApplTerm(f.f, arity, (Term *)ts));
  RECOVER_H();
}

YAPApplTerm::YAPApplTerm(YAPFunctor f) : YAPTerm() {
  BACKUP_H();
  UInt arity = ArityOfFunctor(f.f);
  mk(Yap_MkNewApplTerm(f.f, arity));
  RECOVER_H();
}

YAPTerm YAPApplTerm::getArg(int arg) {
  BACKUP_H();
  YAPTerm to = YAPTerm(ArgOfTerm(arg, gt()));
  RECOVER_H();
  return to;
}

YAPFunctor YAPApplTerm::getFunctor() { return YAPFunctor(FunctorOfTerm(gt())); }

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

void YAPTerm::mk(Term t0) { CACHE_REGS t = Yap_InitSlot(t0); }

Term YAPTerm::gt() { CACHE_REGS return Yap_GetFromSlot(t); }

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
  Term tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm(gt());

  RECOVER_MACHINE_REGS();
  return new YAPTerm(tn);
}

bool YAPTerm::exactlyEqual(YAPTerm t1) {
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_eq(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

bool YAPTerm::unify(YAPTerm t1) {
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_unify(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

bool YAPTerm::unifiable(YAPTerm t1) {
  int out;
  BACKUP_MACHINE_REGS();

  out = Yap_Unifiable(gt(), t1.term());

  RECOVER_MACHINE_REGS();
  return out;
}

bool YAPTerm::variant(YAPTerm t1) {
  int out;
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
  size_t length;
  encoding_t enc = LOCAL_encoding;
  char *os;

  BACKUP_MACHINE_REGS();
  if (!(os = Yap_TermToString(Yap_GetFromSlot(t), &length, enc, 0))) {
    RECOVER_MACHINE_REGS();
    return nullptr;
  }
  RECOVER_MACHINE_REGS();
  return os;
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

YAPTerm::YAPTerm(intptr_t i) {
  CACHE_REGS Term tn = MkIntegerTerm(i);
  mk(tn);
}

YAPTerm YAPListTerm::car() {
  Term to = gt();
  { LOG("to=%lx", to); }
  if (IsPairTerm(to))
    return YAPTerm(HeadOfTerm(to));
  else
    throw YAPError(TYPE_ERROR_LIST);
}

YAPVarTerm::YAPVarTerm() {
  CACHE_REGS
  mk(MkVarTerm());
}

char *YAPAtom::getName(void) {
  if (IsWideAtom(a)) {
    // return an UTF-8 version
    size_t sz = 512;
    wchar_t *ptr = a->WStrOfAE;
    utf8proc_int32_t ch = -1;
    char *s = new char[sz], *op = s;
    while (ch) {
      ch = *ptr++;
      op += put_utf8((unsigned char *)op, ch);
    }
    sz = strlen(s) + 1;
    char *os = new char[sz];
    memcpy(os, s, sz);
    delete[] s;
    return os;
  } else if (IsBlob(a)) {
    size_t sz = 1024;
    char *s = new char[sz + 1];
    return Yap_blob_to_string(RepAtom(a), s, sz);
  } else {
    return (char *)a->StrOfAE;
  }
}

void YAPQuery::initOpenQ() {
  CACHE_REGS
  oq = LOCAL_execution;
  LOCAL_execution = this;
  q_open = 1;
  q_state = 0;
  q_flags = true; // PL_Q_PASS_EXCEPTION;

  q_p = P;
  q_cp = CP;
}

int YAPError::get() { return errNo; }

const char *YAPError::text() { return "YAP Error"; }

void YAPQuery::initQuery(Term t) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  arity_t arity = ap->ArityOfPE;
  goal = YAPTerm(t);
  if (arity) {
    q_g = Yap_InitSlots(arity, RepAppl(t) + 1);
  } else {
    q_g = 0;
  }
  initOpenQ();
  RECOVER_MACHINE_REGS();
}

void YAPQuery::initQuery(YAPTerm ts[], arity_t arity) {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (arity) {
    q_g = Yap_NewSlots(arity);
    for (arity_t i = 0; i < arity; i++) {
      Yap_PutInSlot(q_g + i, ts[i].term());
    }
    Term t = Yap_MkApplTerm(ap->FunctorOfPred, ap->ArityOfPE,
                            Yap_AddressFromSlot(q_g));
    goal = YAPTerm(t);
  } else {
    q_g = 0;
    goal = YAPTerm(MkAtomTerm((Atom)ap->FunctorOfPred));
  }
  initOpenQ();
  RECOVER_MACHINE_REGS();
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm ts[])
    : YAPPredicate(f, mod) {
  /* ignore flags  for now */
  initQuery(ts, f.arity());
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm ts[]) : YAPPredicate(f) {
  /* ignore flags for now */
  initQuery(ts, f.arity());
}

YAPQuery::YAPQuery(YAPPredicate p, YAPTerm ts[]) : YAPPredicate(p.ap) {
  initQuery(ts, p.ap->ArityOfPE);
}

YAPListTerm YAPQuery::namedVars() {
  CACHE_REGS
  Term o = vnames.term();
  return o; // should be o
}

bool YAPQuery::next() {
  CACHE_REGS
  int result;

  BACKUP_MACHINE_REGS();
  if (q_open != 1)
    return false;
  if (setjmp(q_env))
    return false;
  // don't forget, on success these guys must create slots
  if (this->q_state == 0) {
    result = (bool)YAP_EnterGoal((YAP_PredEntryPtr)ap, q_g, &q_h);
  } else {
    LOCAL_AllowRestart = q_open;
    result = (bool)YAP_RetryGoal(&q_h);
  }
  q_state = 1;
  if (Yap_GetException()) {
    throw(YAPError(SYSTEM_ERROR_INTERNAL));
  }
  if (!result) {
    YAP_LeaveGoal(FALSE, &q_h);
    q_open = 0;
  }
  RECOVER_MACHINE_REGS();
  return result;
}

void YAPQuery::cut() {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (q_open != 1 || q_state == 0)
    return;
  YAP_LeaveGoal(FALSE, &q_h);
  q_open = 0;
  LOCAL_execution = this;
  RECOVER_MACHINE_REGS();
}

bool YAPQuery::deterministic() {
  CACHE_REGS

  BACKUP_MACHINE_REGS();
  if (q_open != 1 || q_state == 0)
    return false;
  choiceptr myB = (choiceptr)(LCL0 - q_h.b);
  return (B >= myB);
  RECOVER_MACHINE_REGS();
}

void YAPQuery::close() {
  CACHE_REGS

  RECOVER_MACHINE_REGS();
  Yap_ResetException(worker_id);
  /* need to implement backtracking here */
  if (q_open != 1 || q_state == 0) {
    RECOVER_MACHINE_REGS();
    return;
  }
  YAP_LeaveGoal(FALSE, &q_h);
  q_open = 0;
  LOCAL_execution = this;
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

extern "C" void Java_pt_up_fc_dcc_yap_JavaYap_load(JNIEnv *env0, jobject obj,
                                                   jobject mgr);

extern "C" void Java_pt_up_fc_dcc_yap_JavaYap_load(JNIEnv *env0, jobject obj,
                                                   jobject asset_manager) {
  AAssetManager *mgr = AAssetManager_fromJava(Yap_jenv, asset_manager);
  if (mgr == NULL) {
    LOG("we're doomed, mgr = 0; bip bip bip");
  } else {
    Yap_assetManager = mgr;
  }
}

#endif

YAPEngine::YAPEngine(char *savedState, size_t stackSize, size_t trailSize,
                     size_t maxStackSize, size_t maxTrailSize, char *libDir,
                     char *bootFile, char *goal, char *topLevel, bool script,
                     bool fastBoot,
                     YAPCallback *cb)
    : _callback(0) { // a single engine can be active
#if __ANDROID__
  Yap_AndroidBufp = (char *)malloc(Yap_AndroidMax = 4096);
  Yap_AndroidBufp[0] = '\0';
  Yap_AndroidSz = 0;
#endif
  memset((void *)&init_args, 0, sizeof(init_args));
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
  yerror = YAPError();
  delYAPCallback();
  if (cb)
    setYAPCallback(cb);
  curren = this;
  if (YAP_Init(&init_args) == YAP_BOOT_ERROR)
    throw(YAPError(SYSTEM_ERROR_INTERNAL));
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
    throw YAPError(TYPE_ERROR_NUMBER);
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
  }
  Functor f = FunctorOfTerm(t);
  if (IsExtensionFunctor(f)) {
    throw YAPError(TYPE_ERROR_NUMBER);
  } else {
    ap = RepPredProp(PredPropByFunc(f, m));
    outp = RepAppl(t) + 1;
  }
  return ap;
}

YAPPrologPredicate::YAPPrologPredicate(
    YAPAtom name, arity_t arity, YAPModule mod, bool tabled,
    bool logical_updates, bool is_thread_local, bool sourced,
    bool discontiguous, bool multiFile, bool hidden, bool untraceable,
    bool unspyable, bool meta, bool moduleTransparent, bool quasiQuotable,
    size_t mega_clause)
    : YAPPredicate(name, arity, mod) {
  if (!ap)
    return;
  if (is_thread_local) {
    if (ap->cs.p_code.NOfClauses || tabled)
      return;
    ap->PredFlags |= (ThreadLocalPredFlag | LogUpdatePredFlag);
  } else if (logical_updates) {
    if (ap->cs.p_code.NOfClauses || tabled)
      return;
    ap->PredFlags |= LogUpdatePredFlag;
    ap->CodeOfPred = FAILCODE;
    ap->OpcodeOfPred = FAILCODE->opc;
  }
  if (tabled) {
    ap->PredFlags |= TabledPredFlag;
    if (ap->cs.p_code.NOfClauses || tabled)
      return;
    ap->PredFlags |= TabledPredFlag;
  }
  if (sourced) {
    ap->PredFlags |= SourcePredFlag;
  }
  if (discontiguous) {
    ap->PredFlags |= DiscontiguousPredFlag;
  }
  if (multiFile) {
    ap->PredFlags |= MultiFileFlag;
  }
  if (hidden) {
    ap->PredFlags |= HiddenPredFlag;
  }
  if (untraceable) {
    ap->PredFlags |= SourcePredFlag;
  }
  if (unspyable) {
    ap->PredFlags |= NoSpyPredFlag;
  }
  if (meta) {
    ap->PredFlags |= MetaPredFlag;
  } else if (moduleTransparent) {
    ap->PredFlags |= ModuleTransparentPredFlag;
  }
  if (quasiQuotable) {
    ap->PredFlags |= QuasiQuotationPredFlag;
  }
  if (untraceable) {
    ap->PredFlags |= SourcePredFlag;
  }
  if (hidden) {
    ap->PredFlags |= SourcePredFlag;
  }
}

void *YAPPrologPredicate::assertClause(YAPTerm clause, bool last,
                                       YAPTerm source) {
  CACHE_REGS

  RECOVER_MACHINE_REGS();
  Term tt = clause.gt();
  Term sourcet = source.gt();
  yamop *codeaddr = Yap_cclause(tt, PP->ArityOfPE, Yap_CurrentModule(),
                                sourcet); /* vsc: give the number of arguments
                                       to cclause in case there is overflow */
  Term ntt = clause.gt();
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
  return 0;
}
void *YAPPrologPredicate::retractClause(YAPTerm skeleton, bool all) {
  return 0;
}
void *YAPPrologPredicate::clause(YAPTerm skeleton, YAPTerm &body) { return 0; }
