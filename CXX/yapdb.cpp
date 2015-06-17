
#define YAP_CPP_INTERFACE 1

#include "yapi.hh"
#include "SWI-Stream.h"



YAPPredicate::YAPPredicate(const char *s, Term &tout, yhandle_t &vnames) throw (int) {
  CACHE_REGS
  BACKUP_MACHINE_REGS();
  yhandle_t  yvnames = Yap_InitSlot();
  Term tout = Yap_StringToTerm(s, strlen(s), Yap_DefaultEncoding(), yvnames, 1200);
  if (tout == 0L)
    throw YAPError::YAP_SYNTAX_ERROR;
  ap = getPred( tout );
  RECOVER_MACHINE_REGS();
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
    } else {LogUpdate
      ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
    }
}

/// auxiliary routine to find a predicate in the current module.
PredEntry  *YAPPredicate::getPred( Term t, Term* &outp ) {
  CACHE_REGS
    Term m = CurrentModule ;
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
  { REGS_LOG( "done H= %p, outp=%p", HR, outp) ; }
  return ap;
}

YAPPredicate::YAPPredicate(YAPFunctor f) {
  CACHE_REGS
    ap = RepPredProp(PredPropByFunc(f.f,CurrentModule));
}

int YAPPredicate::call(YAPTerm t[])
{
  YAPQuery q = YAPQuery(*this, t);
  int ret;

  BACKUP_MACHINE_REGS();
  ret = q.next();
  q.cut();
  q.close();
  RECOVER_MACHINE_REGS();
  return ret;
}
