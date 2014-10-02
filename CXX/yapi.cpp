
#define YAP_CPP_INTERFACE 1

#include "yapi.hh"
#include "SWI-Stream.h"


YAPAtomTerm::YAPAtomTerm(char *s) { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOM;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( MkAtomTerm(out.val.a) );
  else t = 0L;
  RECOVER_H();
}


YAPAtomTerm::YAPAtomTerm(char *s, size_t len) { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_ATOM|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( MkAtomTerm(out.val.a) );
  else t = 0L;
  RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(wchar_t *s): YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( MkAtomTerm(out.val.a) );
  else t = 0L;
  RECOVER_H();
}


YAPAtomTerm::YAPAtomTerm(wchar_t *s, size_t len) : YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_ATOM|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( MkAtomTerm(out.val.a) );
  else t = 0L;
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
    mk ( out.val.t );
  else t = 0L;
  RECOVER_H();
}


YAPStringTerm::YAPStringTerm(char *s, size_t len) { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.c = s;
  inp.type = YAP_STRING_CHARS;
  out.type = YAP_STRING_STRING|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( out.val.t );
  else t = 0L;
  RECOVER_H();
}

YAPStringTerm::YAPStringTerm(wchar_t *s): YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( out.val.t );
  else t = 0L;
  RECOVER_H();
}


YAPStringTerm::YAPStringTerm(wchar_t *s, size_t len) : YAPTerm() { // build string
  BACKUP_H();

  CACHE_REGS
  seq_tv_t inp, out;
  inp.val.w = s;
  inp.type = YAP_STRING_WCHARS;
  out.type = YAP_STRING_STRING|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
  out.sz = len;
  out.max = len;
  if (Yap_CVT_Text(&inp, &out PASS_REGS))
    mk ( out.val.t );
  else t = 0L;
  RECOVER_H();
}


YAPApplTerm::YAPApplTerm(YAPFunctor f, YAPTerm ts[]) : YAPTerm() {
  UInt arity = ArityOfFunctor(f.f);
  mk ( Yap_MkApplTerm( f.f, arity, (Term *)ts) );
}

YAPApplTerm::YAPApplTerm(YAPFunctor f) : YAPTerm() {
  UInt arity = ArityOfFunctor(f.f);
  mk ( Yap_MkNewApplTerm( f.f, arity) );
}

YAPTerm  YAPApplTerm::getArg(int arg) {
  return YAPTerm( ArgOfTerm(arg, gt() ) );
}

YAPFunctor  YAPApplTerm::getFunctor() {
  return YAPFunctor( FunctorOfTerm( gt( )) );
}

YAPPairTerm::YAPPairTerm(YAPTerm th, YAPTerm tl) : YAPTerm() {
  CACHE_REGS
  mk ( MkPairTerm( th.term(), tl.term() ) );
}

YAPPairTerm::YAPPairTerm() : YAPTerm() {
  t = Yap_MkNewPairTerm( );
}

void YAPTerm::mk(Term t0) { CACHE_REGS t = Yap_InitSlot( t0 PASS_REGS);  }

Term YAPTerm::gt() { CACHE_REGS return Yap_GetFromSlot( t PASS_REGS); }

YAP_tag_t  YAPTerm::tag() {
  Term tt = gt( );
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

YAPTerm  YAPTerm::deepCopy() {
  Term tn;
  BACKUP_MACHINE_REGS();

  tn = Yap_CopyTerm( gt() );

  RECOVER_MACHINE_REGS();
  return new YAPTerm( tn );
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
  intptr_t 	out;

  BACKUP_MACHINE_REGS();

  out =  Yap_TermHash(gt(), sz, depth, variant) ;

  RECOVER_MACHINE_REGS();
  return out;
}

char *YAPTerm::text(void) {
  size_t sze = 4096, length;
  char *os;
  int enc;

  BACKUP_MACHINE_REGS();
  if (!(os = Yap_HandleToString(t, sze, &length, &enc, 0))) {
      RECOVER_MACHINE_REGS();
      return (char *)NULL;
  }
  RECOVER_MACHINE_REGS();
  return os;
}

char *YAPQuery::text(void) {
  size_t sze = 4096, length;
  char *os;
  int enc;

  { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "I LCL0+%p/t=(%d) %x", LCL0, *q_g, LCL0[-15]) ; }
  BACKUP_MACHINE_REGS();
  if (!(os = Yap_HandleToString(*q_g, sze, &length, &enc, 0))) {
      { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "IIa  LCL0+t=(%p) %x oz=%p %s",  LCL0, LCL0[-15], os, os) ; }
      RECOVER_MACHINE_REGS();
      return (char *)NULL;
  }
  { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "II  LCL0+t=(%p) %x", LCL0, LCL0[-15]) ; }
  RECOVER_MACHINE_REGS();
  return os;
}

bool YAPListTerm::nil() {
  CACHE_REGS
  __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "t=%d LCL0+t=(%p)",  t, LCL0+t) ;
  return gt() == TermNil;
}


YAPIntegerTerm::YAPIntegerTerm(intptr_t i) { CACHE_REGS Term tn = MkIntegerTerm( i ); mk( tn ); }



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

YAPTerm::YAPTerm(void *ptr) { CACHE_REGS mk( MkIntegerTerm( (Int)ptr )  );}

YAPTerm::YAPTerm(intptr_t i) { CACHE_REGS Term tn = MkIntegerTerm( i ); mk( tn ); }

YAPTerm YAPListTerm::car()
{
  { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "t=%d LCL0+t=(%p)",  t, LCL0+t) ; }
  Term to = gt();
  if (IsPairTerm(to))
    return YAPTerm(HeadOfTerm(to));
  else
    return MkIntTerm(-1);
}

YAPVarTerm::YAPVarTerm() { CACHE_REGS mk( MkVarTerm( ) ); }


char *YAPAtom::getName(void) {
  if (IsWideAtom(a)) {
      // return an UTF-8 version
      size_t sz = 512;
      wchar_t * ptr =  a->WStrOfAE;
      int ch = -1;
      char *s = new char[sz], *op = s;
      while (ch) {
	  ch = *ptr++;
	  utf8_put_char( op, ch );
      }
      sz = strlen(s)+1;
      char *os = new char[sz];
      memcpy(os, s, sz);
      delete[] s;
      return os;
  } else if (IsBlob(a)) {
      PL_blob_t *type = RepBlobProp(a->PropsOfAE)->blob_t;
      size_t sz = 512;

      if (type->write) {
	  char *s = new char[sz];
	  IOSTREAM *stream = Sopenmem(&s, &sz, "w");
	  stream->encoding = ENC_UTF8;
	  atom_t at = YAP_SWIAtomFromAtom(AbsAtom(a));
	  type->write(stream, at, 0);
	  Sclose(stream);
	  popOutputContext();
	  sz = strlen(s)+1;
	  char *os = new char[sz];
	  memcpy(os, s, sz);
	  delete s;
	  return os;
      } else {
	  char *s = new char[sz];
#if defined(__linux__) || defined(__APPLE__)
	  snprintf(s, sz, "'%s'(%p)", AtomSWIStream->StrOfAE, a);
#else
	  snprintf(s, sz, "'%s'(0x%p)", AtomSWIStream->StrOfAE, a);
#endif
	  char *os = new char[sz];
	  memcpy(os, s, sz);
	  delete[] s;
	  return os;
      }
  } else {
      return a->StrOfAE;
  }
}


YAPPredicate::YAPPredicate(const char *s, Term* &outp, YAPTerm &vnames) throw (int) {
  CACHE_REGS
  vnames = Yap_NewSlots(1 PASS_REGS);
  Term t = Yap_StringToTerm(s, strlen(s)+1, Yap_GetFromSlot( vnames.t PASS_REGS));
  if (t == 0L)
    throw YAPError::YAP_SYNTAX_ERROR;
  ap = getPred( t, outp );
}

YAPPredicate::YAPPredicate(YAPAtom at) {
  CACHE_REGS
  ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
}

YAPPredicate::YAPPredicate(YAPAtom at, arity_t arity) {
  CACHE_REGS
  if (arity) {
      Functor f = Yap_MkFunctor(at.a, arity);
      ap = RepPredProp(PredPropByFunc(f,CurrentModule));
  } else {
      ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
  }
}

/// auxiliary routine to find a predicate in the current module.
PredEntry  *YAPPredicate::getPred( Term t, Term* &outp ) {
  CACHE_REGS
  Term m = CurrentModule ;
  { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "H= %p, outp=%p", HR, outp) ; }
  t = Yap_StripModule(t, &m);
  if (IsVarTerm(t) || IsNumTerm(t)) {
      ap = (PredEntry  *)NULL;
      outp = (Term  *)NULL;
  }
  if (IsAtomTerm(t)) {
      ap = RepPredProp(PredPropByAtom(AtomOfTerm(t), m));
      if (outp) outp = (Term  *)NULL;
  }  else if (IsPairTerm(t)) {
      ap = RepPredProp(PredPropByFunc(FunctorCsult, PROLOG_MODULE));
       outp = HR;
      HR[0] = RepPair(t)[0];
      HR[1] = m;
      HR+=2;
  } else {
      Functor f = FunctorOfTerm(t);
      if (IsExtensionFunctor(f)) {
	  ap = (PredEntry  *)NULL;
	  outp = (Term *)NULL;
      } else {
      ap = RepPredProp(PredPropByFunc(f, m));
       outp = RepAppl(t)+1;
      }
  }
  { CACHE_REGS __android_log_print(ANDROID_LOG_ERROR,  __FUNCTION__, "done H= %p, outp=%p", HR, outp) ; }
 return ap;
}

YAPPredicate::YAPPredicate(YAPFunctor f) {
  CACHE_REGS
  ap = RepPredProp(PredPropByFunc(f.f,CurrentModule));
}


void
YAPQuery::initQuery( Term *ts )
{
  CACHE_REGS

  this->oq = (YAPQuery *)LOCAL_execution;
  LOCAL_execution = (struct open_query_struct *)this;
  this->q_open=1;
  this->q_state=0;
  this->q_flags = PL_Q_PASS_EXCEPTION;
  this->q_g = ts;
}

void
YAPQuery::initQuery( YAPTerm t[], arity_t arity )
{
  Term *ts = new Term[arity];
  for (arity_t i = 0; i < arity; i++)
    ts[i] = t[i].term();

  return initQuery( ts );
}


YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm t[]): YAPPredicate(f, mod)
{
  /* ignore flags  for now */
  initQuery( t , f.arity());
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm t[]): YAPPredicate(f)
{
  /* ignore flags for now */
  initQuery( t , f.arity());
}

YAPQuery::YAPQuery(YAPPredicate p, YAPTerm t[]): YAPPredicate(p.ap)
{
  initQuery( t , p.ap->ArityOfPE);
}

YAPListTerm YAPQuery::namedVars() {
  CACHE_REGS
    Term o = Yap_GetFromSlot( this->vnames.t PASS_REGS );
  return YAPListTerm( o );
}

bool YAPQuery::next()
{
  CACHE_REGS
  int result;
  if (q_open != 1) return false;
  if (setjmp(((YAPQuery *)LOCAL_execution)->q_env))
    return false;
  // don't forget, on success these guys must create slots
  if (q_state == 0) {
    // extern void toggle_low_level_trace(void);
      //toggle_low_level_trace();
      result = (bool)YAP_EnterGoal((YAP_PredEntryPtr)ap, q_g, &q_h);

  } else {
      LOCAL_AllowRestart = this->q_open;
      result = (bool)YAP_RetryGoal(&q_h);
  }
  this->q_state = 1;
  if (!result) {
      YAP_LeaveGoal(FALSE, &q_h);
      this->q_open = 0;
  }
  return result;
}

void YAPQuery::cut()
{
  CACHE_REGS

  if (this->q_open != 1 || this->q_state == 0) return;
  YAP_LeaveGoal(FALSE, &this->q_h);
  this->q_open = 0;
  LOCAL_execution = (struct open_query_struct *)this->oq;
}

void YAPQuery::close()
{
  CACHE_REGS

  if (EX && !(this->q_flags & (PL_Q_CATCH_EXCEPTION))) {
      EX = (struct DB_TERM *)NULL;
  }
  /* need to implement backtracking here */
  if (this->q_open != 1 || this->q_state == 0) {
      return;
  }
  YAP_LeaveGoal(FALSE, &this->q_h);
  this->q_open = 0;
  LOCAL_execution = (struct open_query_struct *)this->oq;
}

int YAPPredicate::call(YAPTerm t[])
{
  YAPQuery q = YAPQuery(*this, t);
  int ret;

  ret = q.next();
  q.cut();
  q.close();
  return ret;
}

static YAPEngine *curren;

#if __ANDROID__

#include <jni.h>
#include <string.h>

extern AAssetManager *assetManager;

extern char *Yap_AndroidBufp;
static size_t Yap_AndroidMax, Yap_AndroidSz;

extern void(*Yap_DisplayWithJava)(int c);

static void
displayWithJava(int c)
{
  char *ptr = Yap_AndroidBufp;
  ptr[ Yap_AndroidSz++ ] = c;
  if (Yap_AndroidMax-1 == Yap_AndroidSz) {
      if (Yap_AndroidMax < 32*1024) {
	  Yap_AndroidMax *= 2;
      } else {
	  Yap_AndroidMax += 32*1024;
      }
      Yap_AndroidBufp = (char *)realloc( ptr, Yap_AndroidMax);
  }
  Yap_AndroidBufp[Yap_AndroidSz] = '\0';
  if (c == '\n' ) {
      Yap_AndroidBufp[Yap_AndroidSz] = '\0';
      curren->run(Yap_AndroidBufp);
      Yap_AndroidSz = 0;
  }
}


#endif


YAPEngine::YAPEngine( char *savedState,
                             size_t stackSize,
                             size_t trailSize,
                             size_t maxStackSize,
                             size_t maxTrailSize,
                             char *libDir,
                             char *bootFile,
                             char *goal,
                             char *topLevel,
                             bool script,
                             bool fastBoot,
                             YAPCallback *cb):  _callback(0)
{ // a single engine can be active
#if __ANDROID__
  if (GLOBAL_assetManager == (AAssetManager *)NULL)
    return;
  Yap_DisplayWithJava = displayWithJava;
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
  delYAPCallback();
  if (cb) setYAPCallback(cb);
  curren = this;
  YAP_Init( &init_args );
}

YAPQuery *YAPEngine::query( char *s ) {
  YAPQuery *n = new YAPQuery( s );
  return n;
}


