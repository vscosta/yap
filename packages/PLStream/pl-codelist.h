#ifndef PL_CODELIST_H
#define PL_CODELIST_H


static inline Word
INIT_SEQ_STRING(size_t n)
{
  return (Word)YAP_OpenList(n);
}

static inline Word
EXTEND_SEQ_CODES(Word gstore, int c) {
  return (Word)YAP_ExtendList((YAP_Term)gstore, YAP_MkIntTerm(c));
}

static inline Word
EXTEND_SEQ_CHARS(Word gstore, int c) {
  return (Word)YAP_ExtendList((YAP_Term)gstore, codeToAtom(c));
}

static inline int 
CLOSE_SEQ_STRING(Word gstore, Word lp, word arg2, word arg3, term_t l) {
  if (arg2 == 0) {
    if (!YAP_CloseList((YAP_Term)gstore, YAP_TermNil()))
      return FALSE;
  } else {
    if (!YAP_CloseList((YAP_Term)gstore, YAP_GetFromSlot(arg2)))
      return FALSE;
  }
  return YAP_Unify(YAP_GetFromSlot(arg3), (YAP_Term)lp);
}

#endif
