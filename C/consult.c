


    #include "Yap.h"

#include "YapHeap.h"
#include "Yapproto.h"
#ifdef SCCS
static char SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "YapEval.h"
#include "clause.h"
#include "tracer.h"
#include "yapio.h"
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif
#include <assert.h>
#include <heapgc.h>
#include <iopreds.h>

static void retract_all(PredEntry *, int);
static void expand_consult(void);
static Int p_startconsult(USES_REGS1);
static Int p_showconslultlev(USES_REGS1);

static void InitConsultStack(void) {
  CACHE_REGS
    LOCAL_ConsultLow = (consult_obj *)Yap_AllocCodeSpace(sizeof(consult_obj) *
                                                       InitialConsultCapacity);
  if (LOCAL_ConsultLow == NULL) {
    Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil, "No Heap Space in InitCodes");
    return;
  }
  LOCAL_ConsultCapacity = InitialConsultCapacity;
  LOCAL_ConsultBase = LOCAL_ConsultSp =
    LOCAL_ConsultLow + LOCAL_ConsultCapacity;
}

Atom Yap_ConsultingFile(USES_REGS1) {
  int sno;  
  if ((sno = Yap_CheckAlias(AtomLoopStream)) >= 0) {
    //    if(sno ==0)
    //  return(AtomUserIn);                                                        
    Atom at = StreamFullName(sno);
    if (at) return at;
  }
  if (LOCAL_SourceFileName != NULL) {
    return LOCAL_SourceFileName;
  }
  if (LOCAL_consult_level ==  0) {
    return (AtomUser);
  } else {
    return (Yap_ULookupAtom(LOCAL_ConsultBase->f_layer->f_name));
  }
}
/* p is already locked */
static void retract_all(PredEntry *p, int in_use) {
  yamop *q;

  q = p->cs.p_code.FirstClause;
  if (q != NULL) {
    if (p->PredFlags & LogUpdatePredFlag) {
      LogUpdClause *cl = ClauseCodeToLogUpdClause(q);
      do {
        LogUpdClause *ncl = cl->ClNext;
        Yap_ErLogUpdCl(cl);
        cl = ncl;
      } while (cl != NULL);
    } else if (p->PredFlags & MegaClausePredFlag) {
      MegaClause *cl = ClauseCodeToMegaClause(q);

      if (in_use || cl->ClFlags & HasBlobsMask) {
        LOCK(DeadMegaClausesLock);
        cl->ClNext = DeadMegaClauses;
        DeadMegaClauses = cl;
        UNLOCK(DeadMegaClausesLock);
      } else {
        Yap_InformOfRemoval(cl);
        Yap_ClauseSpace -= cl->ClSize;
        Yap_FreeCodeSpace((char *)cl);
      }
      /* make sure this is not a MegaClause */
      p->PredFlags &= ~MegaClausePredFlag;
      p->cs.p_code.NOfClauses = 0;
    } else {
      StaticClause *cl = ClauseCodeToStaticClause(q);

      while (cl) {
        StaticClause *ncl = cl->ClNext;

        if (in_use || cl->ClFlags & HasBlobsMask) {
          LOCK(DeadStaticClausesLock);
          cl->ClNext = DeadStaticClauses;
          DeadStaticClauses = cl;
          UNLOCK(DeadStaticClausesLock);
        } else {
          Yap_InformOfRemoval(cl);
          Yap_ClauseSpace -= cl->ClSize;
          Yap_FreeCodeSpace((char *)cl);
        }
        p->cs.p_code.NOfClauses--;
        if (!ncl)
          break;
        cl = ncl;
      }
    }
  }
  p->cs.p_code.FirstClause = NULL;
  p->cs.p_code.LastClause = NULL;
  if (is_live(p)) {
    p->cs.p_code.TrueCodeOfPred = p->CodeOfPred =
     (yamop *)(&p->OpcodeOfPred);
    p->OpcodeOfPred =   FAIL_OPCODE;     
  } else {
    p->OpcodeOfPred = UNDEF_OPCODE;
    p->PredFlags |= UndefPredFlag;
  }
  p->cs.p_code.TrueCodeOfPred = p->CodeOfPred = (yamop *)(&(p->OpcodeOfPred));
  if (trueGlobalPrologFlag(PROFILING_FLAG)) {
    p->PredFlags |= ProfiledPredFlag;
    if (!Yap_initProfiler(p)) {
      return;
    }
  } else
    p->PredFlags &= ~ProfiledPredFlag;
  if (CALL_COUNTING) {
    p->PredFlags |= CountPredFlag;
  } else
    p->PredFlags &= ~CountPredFlag;
  Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
}

static void addcl_permission_error(AtomEntry *ap, Int Arity, int in_use) {
  CACHE_REGS

  LOCAL_Error_TYPE = PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE;
  LOCAL_ErrorMessage = Malloc(256);

  if (in_use) {
    if (Arity == 0)
      sprintf(LOCAL_ErrorMessage, "static predicate %s is in use", ap->StrOfAE);
    else
      sprintf(LOCAL_ErrorMessage,
              "static predicate %s/" Int_FORMAT " is in use", ap->StrOfAE,
              Arity);
  } else {
    if (Arity == 0)
      sprintf(LOCAL_ErrorMessage, "system predicate %s", ap->StrOfAE);
    else
      sprintf(LOCAL_ErrorMessage, "system predicate %s/" Int_FORMAT,
              ap->StrOfAE, Arity);
  }
}


int Yap_not_was_reconsulted(PredEntry *p, Term t, int mode) {
  CACHE_REGS
  register consult_obj *fp;
  Prop p0 = AbsProp((PropEntry *)p);

  if (p == LOCAL_LastAssertedPred)
    return FALSE;
  if (!LOCAL_ConsultSp) {
    InitConsultStack();
  }
  if (p->cs.p_code.NOfClauses) {
    for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
      if (fp->p == p0)
        break;
  } else {
    fp = LOCAL_ConsultBase;
  }
  if (fp != LOCAL_ConsultBase) {
    LOCAL_LastAssertedPred = p;
    return false;    /* careful */
  } else if (mode) { // consulting again a predicate in the original file.
    if ((p->cs.p_code.NOfClauses &&
         p->src.OwnerFile == Yap_ConsultingFile(PASS_REGS1) &&
         p->src.OwnerFile != AtomNil && !(p->PredFlags & MultiFileFlag) &&
         p->src.OwnerFile != AtomUserIn)) {
      // if (p->ArityOfPE)
      //	printf("+ %s %s
      //%d\n",NameOfFunctor(p->FunctorOfPred)->StrOfAE,p->src.OwnerFile->StrOfAE,
      // p->cs.p_code.NOfClauses);
      retract_all(p, Yap_static_in_use(p, TRUE));
    }
    //	printf("- %s
    //%s\n",NameOfFunctor(p->FunctorOfPred)->StrOfAE,p->src.OwnerFile->StrOfAE);
  }
  if (mode) {
    if (LOCAL_ConsultSp <= LOCAL_ConsultLow + 6) {
      expand_consult();
    }
    --LOCAL_ConsultSp;
    LOCAL_ConsultSp->p = p0;
    if (LOCAL_ConsultBase != LOCAL_ConsultLow + LOCAL_ConsultCapacity &&
        LOCAL_ConsultBase->f_layer->mode &&
        !(p->PredFlags & MultiFileFlag)) /* we are in reconsult mode */ {
      retract_all(p, Yap_static_in_use(p, TRUE));
    }
    // p->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  }
  LOCAL_LastAssertedPred = p;
  return TRUE; /* careful */
}

static Int parent_stream_file(USES_REGS1) {
  if (LOCAL_consult_level <=  0) {
    return Yap_unify(ARG1,TermUserIn);
  } else {
    return Yap_unify(ARG1,MkAtomTerm(Yap_ULookupAtom(LOCAL_ConsultBase->f_layer->f_name)));
  }
}

static Int parent_source_module(USES_REGS1) {
  if (LOCAL_consult_level <=  0) {
    return Yap_unify(ARG1,TermUser);
  } else {
    Term m = (LOCAL_ConsultBase->f_layer->m==0?TermProlog:LOCAL_ConsultBase->f_layer->m);
    return Yap_unify(ARG1,m);
  }
}

static Int parent_stream_line(USES_REGS1) {
  if (LOCAL_consult_level <=  0) {
    return Yap_unify(ARG1,MkIntTerm(0));
  } else {
    return Yap_unify(ARG1,MkIntTerm(LOCAL_ConsultBase->f_layer->line));
  }
}

static Int grandparent_source_module(USES_REGS1) {
  if (LOCAL_consult_level < 2) {
    return Yap_unify(ARG1,TermUser);
  } else {
    union CONSULT_OBJ *p = LOCAL_ConsultBase+LOCAL_ConsultBase->f_layer->c;
    if (p->f_layer->m)
      return Yap_unify(ARG1,p->f_layer->m);
    else
      return Yap_unify(ARG1,TermProlog);
  }
}


static void end_consult(USES_REGS1) {
  int osnow =  Yap_CheckAlias(AtomLoopStream);
  if (osnow > 0)
    Yap_CloseStream(osnow);
  setAtomicLocalPrologFlag(COMPILATION_MODE_FLAG, LOCAL_ConsultSp->f_layer->CompilationMode);
  Yap_ChDir(RepAtom(AtomOfTerm(LOCAL_ConsultBase->f_layer->cwd))->StrOfAE);
  CurrentModule = LOCAL_SourceModule = LOCAL_ConsultBase->f_layer->m;
  setBooleanLocalPrologFlag(VERBOSE_LOAD_FLAG, !LOCAL_ConsultBase->f_layer->silent);
  setBooleanLocalPrologFlag(AUTOLOAD_FLAG, !LOCAL_ConsultBase->f_layer->autoload);
  LOCAL_ConsultSp = LOCAL_ConsultBase;
  LOCAL_ConsultBase = LOCAL_ConsultSp + LOCAL_ConsultSp->f_layer->c;
  Yap_FreeCodeSpace(LOCAL_ConsultSp->f_layer);
  LOCAL_ConsultSp ++;
  if (LOCAL_consult_level>0) 
    LOCAL_consult_level--;
  LOCAL_LastAssertedPred = NULL;
#if !defined(YAPOR) && !defined(YAPOR_SBA)
/*  if (LOCAL_consult_level == 0)
    do_toggle_static_predicates_in_use(FALSE);*/
#endif
  if (LOCAL_consult_level==0)
    setBooleanLocalPrologFlag(COMPILING_FLAG, false);

}

static void expand_consult(void) {
  CACHE_REGS
  consult_obj *new_cl, *new_cs;
  UInt OldConsultCapacity = LOCAL_ConsultCapacity;

  /* now double consult capacity */
  LOCAL_ConsultCapacity += InitialConsultCapacity;
  /* I assume it always works ;-) */
  while ((new_cl = (consult_obj *)Yap_AllocCodeSpace(
              sizeof(consult_obj) * LOCAL_ConsultCapacity)) == NULL) {
    if (!Yap_growheap(FALSE, sizeof(consult_obj) * LOCAL_ConsultCapacity,
                      NULL)) {
      Yap_ThrowError(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
      return;
    }
  }
  new_cs = new_cl + InitialConsultCapacity;
  /* start copying */
  memcpy((void *)new_cs, (void *)LOCAL_ConsultLow,
         OldConsultCapacity * sizeof(consult_obj));
  /* copying done, release old space */
  Yap_FreeCodeSpace((char *)LOCAL_ConsultLow);
  /* next, set up pointers correctly */
  new_cs += (LOCAL_ConsultSp - LOCAL_ConsultLow);
  /* put LOCAL_ConsultBase at same offset as before move */
  LOCAL_ConsultBase = new_cl + ((LOCAL_ConsultBase - LOCAL_ConsultLow) +
                                InitialConsultCapacity);
  /* new consult pointer */
  LOCAL_ConsultSp =
      new_cl + ((LOCAL_ConsultSp - LOCAL_ConsultLow) + InitialConsultCapacity);
  /* new end of memory */
  LOCAL_ConsultLow = new_cl;
}


void Yap_ResetConsultStack(void) {
  CACHE_REGS
    while(LOCAL_consult_level) {
      end_consult();
    }
  Yap_FreeCodeSpace((char *)LOCAL_ConsultLow);
  LOCAL_ConsultBase = LOCAL_ConsultSp = LOCAL_ConsultLow = NULL;
  LOCAL_ConsultCapacity = InitialConsultCapacity;
}

/* consult file *file*, *mode* may be one of either consult or reconsult */
void Yap_init_consult(int mode, const char *filename, int sno, const char  *encoding) {
  CACHE_REGS
    char dir[MAX_PATH + 1];
  if (!LOCAL_ConsultSp) {
    InitConsultStack();
  }
  if (LOCAL_ConsultSp >= LOCAL_ConsultLow + 6) {
    expand_consult();
  }
  LOCAL_ConsultSp--;
  LOCAL_ConsultSp->f_layer = Yap_AllocCodeSpace(sizeof(struct  CONSULT_Layer));
  if (filename)
    LOCAL_ConsultSp->f_layer->f_name = (const unsigned char *)filename;
  else
    LOCAL_ConsultSp->f_layer->f_name =  (const unsigned char *)"user_input";

  LOCAL_ConsultSp->f_layer->sno = Yap_CheckAlias(AtomLoopStream);
  if (  LOCAL_ConsultSp->f_layer->sno > 0) {
     LOCAL_ConsultSp->f_layer->line =   GLOBAL_Stream[LOCAL_ConsultSp->f_layer->sno].linecount;
  } else {
    LOCAL_ConsultSp->f_layer->line=0;
  }
  LOCAL_ConsultSp->f_layer->encoding = enc_id(encoding, GLOBAL_Stream[sno].encoding);
  LOCAL_ConsultSp->f_layer->mode = mode;
  LOCAL_ConsultSp->f_layer->c = (LOCAL_ConsultBase - LOCAL_ConsultSp);
  LOCAL_ConsultSp->f_layer->m = LOCAL_SourceModule;
  LOCAL_ConsultSp->f_layer->CompilationMode = getAtomicLocalPrologFlag(COMPILATION_MODE_FLAG);
  LOCAL_ConsultSp->f_layer->must_be_module = false;
  LOCAL_ConsultSp->f_layer->autoload = trueLocalPrologFlag(AUTOLOAD_FLAG);
  LOCAL_ConsultSp->f_layer->silent = falseLocalPrologFlag(VERBOSE_LOAD_FLAG);
    LOCAL_ConsultSp->f_layer->cwd =             MkAtomTerm(Yap_LookupAtom(Yap_getcwd(dir, MAX_PATH)));
  LOCAL_ConsultBase = LOCAL_ConsultSp;
#if !defined(YAPOR) && !defined(YAPOR_SBA)
/*  if (LOCAL_consult_level == 0)
    do_toggle_static_predicates_in_use(TRUE); */
#endif
  LOCAL_consult_level++;
  LOCAL_LastAssertedPred = NULL;
  Yap_AddAlias(AtomLoopStream, sno);
  GLOBAL_Stream[sno].encoding =  LOCAL_ConsultSp->f_layer->encoding; 
}

static Int p_startconsult(USES_REGS1) { /* '$start_consult'(+Mode)	 */
  Term t;
  char *smode = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  int sno = Yap_CheckStream(Deref(ARG3),Input_Stream_f, " bad consult stream" );

  const char *enc = RepAtom(AtomOfTerm(Deref(ARG4)))->StrOfAE;
  int mode;
  setBooleanLocalPrologFlag(COMPILING_FLAG, true);
  mode = strcmp("consult", (char *)smode);
  Yap_init_consult(mode, RepAtom(AtomOfTerm(Deref(ARG2)))->StrOfAE, sno, enc);
  t = MkIntTerm(LOCAL_consult_level);
  return (Yap_unify_constant(ARG5, t));
}

static Int p_showconslultlev(USES_REGS1) {
  Term t;
  if (LOCAL_consult_level < 0)
    LOCAL_consult_level=0;
  t = MkIntTerm(LOCAL_consult_level);
  return (Yap_unify_constant(ARG1, t));
}



void Yap_end_consult(void) {
  CACHE_REGS
  end_consult(PASS_REGS1);
}

static Int p_endconsult(USES_REGS1) { /* '$end_consult'		 */
  end_consult(PASS_REGS1);
  return (TRUE);
}


static void purge_clauses(PredEntry *pred) {
  if (pred->PredFlags & UDIPredFlag) {
    Yap_udi_abolish(pred);
  }
  if (pred->cs.p_code.NOfClauses) {
    if (pred->PredFlags & IndexedPredFlag)
      Yap_RemoveIndexation(pred);
    Yap_PutValue(AtomAbol, MkAtomTerm(AtomTrue));
    retract_all(pred, Yap_static_in_use(pred, TRUE));
  }
}

void Yap_Abolish(PredEntry *pred) {
  purge_clauses(pred);
  pred->src.OwnerFile = AtomNil;
}


bool Yap_discontiguous(PredEntry *ap, Term mode USES_REGS) {
  register consult_obj *fp;

  if (ap->PredFlags & (DiscontiguousPredFlag | MultiFileFlag) ||
      falseGlobalPrologFlag(DISCONTIGUOUS_WARNINGS_FLAG))
    return false;
  if ((mode != TermConsult && mode != TermReconsult))
    return false;
  if (!LOCAL_ConsultSp) {
    return false;
  }

  if (ap == LOCAL_LastAssertedPred)
    return false;
  if (ap->cs.p_code.NOfClauses) {
    Term repeat = AbsPair((CELL *)AbsPredProp(ap));
    for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
      if (fp->p == AbsPredProp(ap)) {
        // detect repeated warnings
        if (LOCAL_ConsultSp == LOCAL_ConsultLow + 1) {
          expand_consult();
        }
        --LOCAL_ConsultSp;
        LOCAL_ConsultSp->r = repeat;
        return true;
      } else if (fp->r == repeat && ap->cs.p_code.NOfClauses > 4) {
        return false;
      }
  }
  return false;
}

static Int get_must_be_module(USES_REGS1)
{
  return LOCAL_ConsultSp->f_layer->must_be_module;
}


static Int set_must_be_module(USES_REGS1)
{
  Term t = Deref(ARG1);
  //must_be_boolean(t);
  LOCAL_ConsultSp->f_layer->must_be_module = (t == TermTrue ||t == TermOn)
    ;
  return true;
}

static Int p_is_discontiguous(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  Int out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "discontiguous");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);
  out = (pe->PredFlags & DiscontiguousPredFlag);
  UNLOCKPE(44, pe);
  return (out);
}

static Int
    p_new_discontiguous(USES_REGS1) { /* '$new_discontiguous'(+N,+Ar,+Mod)  */
  Atom at;
  int arity;
  PredEntry *pe;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG3);

  if (IsVarTerm(t))
    return false;
  if (IsAtomTerm(t))
    at = AtomOfTerm(t);
  else
    return false;
  t = Deref(ARG2);
  if (IsVarTerm(t))
    return false;
  if (IsIntTerm(t))
    arity = IntOfTerm(t);
  else
    return false;
  if (arity == 0)
    pe = RepPredProp(PredPropByAtom(at, mod));
  else
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, arity), mod));
  PELOCK(26, pe);
  pe->PredFlags |= DiscontiguousPredFlag;
  /* mutifile-predicates are weird, they do not seat really on the default
   * module */
  if (pe->cs.p_code.NOfClauses == 0) {
    pe->cs.p_code.TrueCodeOfPred = pe->CodeOfPred =
     (yamop *)(&pe->OpcodeOfPred);
    pe->OpcodeOfPred =   FAIL_OPCODE;     
  }
  UNLOCKPE(43, pe);
  return (TRUE);
}

bool Yap_multiple(PredEntry *ap, Term mode USES_REGS) {
   consult_obj *fp;

  if ((ap->PredFlags & (MultiFileFlag | LogUpdatePredFlag | DynamicPredFlag)) ||
      mode != TermReconsult)
    return false;
  if (LOCAL_consult_level == 0)
    return false;
  for (fp = LOCAL_ConsultSp; fp < LOCAL_ConsultBase; ++fp)
    if (fp->p == AbsPredProp(ap)) {
      return false;
    }
  return ap->cs.p_code.NOfClauses > 0 && ap->src.OwnerFile != AtomNil &&
         Yap_ConsultingFile(PASS_REGS1) != ap->src.OwnerFile &&
         LOCAL_Including != MkAtomTerm(ap->src.OwnerFile);
}


/*  @pred '$new_multifile'(+G,+Mod)
 *  sets the multi-file flag
 * */
static Int new_multifile(USES_REGS1) {
  PredEntry *pe;
  Atom at;
  arity_t arity;

  pe = Yap_new_pred(Deref(ARG1), Deref(ARG2), false,  "multifile");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(30, pe);
  arity = pe->ArityOfPE;
  if (arity == 0)
    at = (Atom)pe->FunctorOfPred;
  else
    at = NameOfFunctor(pe->FunctorOfPred);

  if (pe->PredFlags & MultiFileFlag) {
    UNLOCKPE(26, pe);
    return true;
  }
  if (pe->PredFlags & (TabledPredFlag | ForeignPredFlags)) {
    UNLOCKPE(26, pe);
    addcl_permission_error(RepAtom(at), arity, FALSE);
    return false;
  }
  if (pe->cs.p_code.NOfClauses) {
    UNLOCKPE(26, pe);
    addcl_permission_error(RepAtom(at), arity, FALSE);
    return false;
  }
  pe->PredFlags &= ~UndefPredFlag;
  pe->PredFlags |= MultiFileFlag;
  /* mutifile-predicates are weird, they do not seat really on the default
   * module */
  if (!(pe->PredFlags & (DynamicPredFlag | LogUpdatePredFlag))) {
    /* static */
    pe->PredFlags |= (SourcePredFlag | CompiledPredFlag);
  }
  pe->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  if (pe->cs.p_code.NOfClauses == 0) {
    pe->CodeOfPred = pe->cs.p_code.TrueCodeOfPred = FAILCODE;
    pe->OpcodeOfPred = FAIL_OPCODE;
  }
  UNLOCKPE(43, pe);
  return true;
}

static Int p_is_multifile(USES_REGS1) { /* '$is_multifile'(+S,+Mod)	 */
  PredEntry *pe;
  bool out;

  pe = Yap_get_pred(Deref(ARG1), Deref(ARG2), "$is_multifile");
  if (EndOfPAEntr(pe))
    return FALSE;
  PELOCK(27, pe);

  out = (pe->PredFlags & MultiFileFlag);
  UNLOCKPE(44, pe);
  return (out);
}


static Int p_purge_clauses(USES_REGS1) { /* '$purge_clauses'(+Func) */
  PredEntry *pred;
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  MegaClause *before = DeadMegaClauses;

  Yap_PutValue(AtomAbol, MkAtomTerm(AtomNil));
  if (IsVarTerm(t))
    return FALSE;
  if (IsVarTerm(mod) || !IsAtomTerm(mod)) {
    return FALSE;
  }
  if (IsAtomTerm(t)) {
    Atom at = AtomOfTerm(t);
    pred = RepPredProp(PredPropByAtom(at, mod));
  } else if (IsApplTerm(t)) {
    Functor fun = FunctorOfTerm(t);
    pred = RepPredProp(PredPropByFunc(fun, mod));
  } else
    return (FALSE);
  PELOCK(21, pred);
  if (pred->PredFlags & StandardPredFlag) {
    UNLOCKPE(33, pred);
    Yap_Error(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, t, "assert/1");
    return (FALSE);
  }
  purge_clauses(pred);
  UNLOCKPE(34, pred);
  /* try to use the garbage collector to recover the mega clause,
     in case the objs pointing to it are dead themselves */
  if (DeadMegaClauses != before) {
    gc_entry_info_t info;
    Yap_track_cpred( 0, P, 0,   &info);
    
    if (!Yap_gc(&info)) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    }
  }
  return TRUE;
}

Int being_consulted(USES_REGS1)
  
{
  int lvl;
  lvl =LOCAL_consult_level;
  union CONSULT_OBJ *sp = LOCAL_ConsultSp, *base = LOCAL_ConsultBase;
  
  while(lvl) {
    int sno=  Yap_CheckStream(Deref(ARG1),Input_Stream_f, " bad consult stream" );
    sp = base;
    if (sp==NULL) {
      return false;
    }
    if(sno == sp->f_layer->sno ||
	GLOBAL_Stream[sno].name ==
	       GLOBAL_Stream[sp->f_layer->sno].name)
      return true; /*  */
    base = sp + sp->f_layer->c;
    sp ++;
    lvl --; 
  }
  return false;
}

#if 0


#define LOAD_FILES_DEFS( )                                             \
  PAR("autoload", isatom, LOAD_FILES_AUTOLOAD ),                            \
    PAR("derived_from", isatom, LOAD_FILES_DERIVED_FROM),	\
    PAR("encoding", isatom, LOAD_FILES_ENCODING),		     \
    PAR("expand", booleanFlag, LOAD_FILES_EXPAND),		     \
    PAR("if", booleanFlag, LOAD_FILES_IF),			       \
    PAR("imports", ok, LOAD_FILES_IMPORTS),		       \
    PAR("qcompile", booleanFlag, LOAD_FILES_QCOMPILE),		       \
    PAR("file_errors", is_file_errors, LOAD_FILES_FILE_ERRORS),			\
    PAR("silent", booleanFlag, LOAD_FILES_SILENT),			\
    PAR("skip_unix_header", ok, LOAD_FILES_SKIP_UNIX_HEADER),	\
    PAR("compilation_mode", ok, LOAD_FILES_COMPILATION_MODE),		\
    PAR("consult", isatom, LOAD_FILES_CONSULT),	\
    PAR("stream", ok, LOAD_FILES_STREAM),			\
    PAR("dialect", isatom,    LOAD_FILES_DIALECT)  ,			\
    PAR("redefine_module", booleanFlag, LOAD_FILES_REDEFINE_MODULE),	\
    PAR("reexport", ok, LOAD_FILES_REEXPORT),				\
    PAR("must_be_module", booleanFlag, LOAD_FILES_MUST_BE_MODULE),	\
    PAR("initialization", ok, LOAD_FILES_INITIALIZATION),		\
      PAR(NULL, ok, LOAD_FILES_END)

#define PAR(x, y, z) z

typedef enum LOAD_FILES_enum_ {
  LOAD_FILES_DEFS()
} load_files_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t load_files_search_defs[] = {
    LOAD_FILES_DEFS()};
#undef PAR

static Int load_files_parameters(USES_REGS1) {
  Term tlist = Deref(ARG1), tf;
  /* get options */
  xarg *args = Malloc(sizeof(xarg)*LOAD_FILES_END);
  memset(args, 0, sizeof(xarg)*LOAD_FILES_END);
  args = Yap_ArgListToVector(tlist, load_files_search_defs,
                                   LOAD_FILES_END,args,
                                   DOMAIN_ERROR_LOAD_FILES_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
      Yap_Error(LOCAL_Error_TYPE, tlist, NULL);
    }
    return false;
  }
  /* done */
  if (args[LOAD_FILES_AUTOLOAD].used) {
    setBooleanLocalPrologFlag(LOAD_FILES_AUTOLOAD,
			      args[LOAD_FILES_AUTOLOAD].tvalue);
  }
  Term toProlog[11];
  if (args[LOAD_FILES_DERIVED_FROM].used) {
    /// unsupported for now
    toProlog[0] = args[LOAD_FILES_DERIVED_FROM].tvalue;
  } else {
    toProlog[0] = TermNil;
  }
  if (args[LOAD_FILES_EXPAND].used) {
   toProlog[1] = args[LOAD_FILES_EXPAND].tvalue;
  } else {
    toProlog[1] = TermFalse;
   }
  if (args[LOAD_FILES_IF].used) {
   toProlog[2] = args[LOAD_FILES_IF].tvalue;
  } else {
    toProlog[2] = TermTrue;
   }
  if (args[LOAD_FILES_IMPORTS].used) {
   toProlog[3] = args[LOAD_FILES_IMPORTS].tvalue;
  } else {
    toProlog[3] = TermAll;
   }
  if (args[LOAD_FILES_QCOMPILE].used) {
   toProlog[4] = args[LOAD_FILES_QCOMPILE].tvalue;
  } else {
    toProlog[4] = TermFalse;
   }
  if (args[LOAD_FILES_SILENT].used) {
    Term v;
    if (args[LOAD_FILES_SILENT].tvalue == TermTrue)
      v=TermFalse;
    else if (args[LOAD_FILES_SILENT].tvalue == TermFalse)
      v=TermTrue;
   else
      Yap_ThrowError(TYPE_ERROR_BOOLEAN,args[LOAD_FILES_SILENT].tvalue,NULL);
    setBooleanLocalPrologFlag(VERBOSE_LOAD_FLAG,v);
  }
   if (args[LOAD_FILES_SKIP_UNIX_HEADER].used) {
   toProlog[4] = args[LOAD_FILES_SKIP_UNIX_HEADER].tvalue;
  } else {
    toProlog[4] = TermFalse;
   }
   if (args[LOAD_FILES_SKIP_UNIX_HEADER].used) {
   toProlog[4] = args[LOAD_FILES_SKIP_UNIX_HEADER].tvalue;
  } else {
    toProlog[4] = TermFalse;
   }
  if (args[LOAD_FILES_COMPILATION_MODE].used) {
    Term v;
    if (args[LOAD_FILES_COMPILATION_MODE].tvalue == TermSource)
      v=TermTrue;
    else if (args[LOAD_FILES_COMPILATION_MODE].tvalue == TermCompact)
      v=TermFalse;
    else
      Yap_ThrowError(TYPE_ERROR_BOOLEAN,args[LOAD_FILES_COMPILATION_MODE].tvalue,NULL);
    setBooleanLocalPrologFlag(SOURCE_FLAG,v);
  }
   if (args[LOAD_FILES_CONSULT].used) {
   toProlog[5] = args[LOAD_FILES_CONSULT].tvalue;
  } else {
    toProlog[5] = TermReconsult;
   }
   if (args[LOAD_FILES_STREAM].used) {
   toProlog[6] = args[LOAD_FILES_STREAM].tvalue;
  } else {
     toProlog[6] = MkVarTerm();
   }
  if (args[LOAD_FILES_DIALECT].used) {
    setAtomicLocalPrologFlag(DIALECT_FLAG,
			   args[LOAD_FILES_DIALECT].tvalue);
  }
   if (args[LOAD_FILES_REDEFINE_MODULE].used) {
   toProlog[7] = args[LOAD_FILES_REDEFINE_MODULE].tvalue;
  } else {
     toProlog[7] = TermSource;
   }
   if (args[LOAD_FILES_REEXPORT].used) {
   toProlog[8] = args[LOAD_FILES_REEXPORT].tvalue;
  } else {
     toProlog[8] = TermTrue;
   }
   if (args[LOAD_FILES_MUST_BE_MODULE].used) {
   toProlog[9] = args[LOAD_FILES_MUST_BE_MODULE].tvalue;
  } else {
     toProlog[9] = TermTrue;
   }
   if (args[LOAD_FILES_INITIALIZATION].used) {
   toProlog[10] = args[LOAD_FILES_INITIALIZATION].tvalue;
  } else {
     toProlog[10] = TermTrue;
   }
   tf = Yap_MkApplTerm(Yap_MkFunctor(AtomDot,11),11,toProlog);
    return (Yap_unify(ARG2, tf));
  }

static Int get_load_files_parameter(USES_REGS1) {
  Term t = Deref(ARG1), topts = Deref(ARG2);
  /* get options */
  /* done */
  int i = Yap_ArgKey(AtomOfTerm(t), load_files_search_defs,
                     LOAD_FILES_END);
  if (i >= 0)
    return Yap_unify(ARG3, ArgOfTerm(i + 1, topts));
  Yap_Error(DOMAIN_ERROR_LOAD_FILES_OPTION, ARG1, NULL);
  return false;
}
#endif

void Yap_InitConsult(void) {
  CACHE_REGS
  Yap_InitCPred("$purge_clauses", 2, p_purge_clauses,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$new_multifile", 2, new_multifile,
		
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$is_multifile", 2, p_is_multifile,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$new_discontiguous", 3, p_new_discontiguous,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$is_discontiguous", 2, p_is_discontiguous,
                TestPredFlag | SafePredFlag);
  Yap_InitCPred("$start_consult", 5, p_startconsult,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$show_consult_level", 1, p_showconslultlev, SafePredFlag);
  Yap_InitCPred("$end_consult", 0, p_endconsult,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$parent_source_module", 1, parent_source_module,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$parent_stream_line", 1, parent_stream_line,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$parent_stream_file", 1, parent_stream_file,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$grandparent_source_module", 1, grandparent_source_module,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$get_must_be_module", 0, get_must_be_module,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$set_must_be_module", 1, set_must_be_module,
		SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$being_consulted", 1, being_consulted,
		SafePredFlag | SyncPredFlag);
  //  Yap_InitCPred("$load_files_parameters", 2, load_files_parameters,  HiddenPredFlag);
  // Yap_InitCPred("$lf_opt__", 3, get_load_files_parameter, HiddenPredFlag|SafePredFlag);
}
