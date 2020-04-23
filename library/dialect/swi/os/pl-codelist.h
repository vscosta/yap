#ifndef PL_CODELIST_H
#define PL_CODELIST_H


static inline Word
INIT_SEQ_STRING(size_t n)
{
  CACHE_REGS
  return RepPair(OpenList(n  PASS_REGS));
}

static inline Word
EXTEND_SEQ_CODES(Word ptr, int c) {
  CACHE_REGS
  ptr[0] = MkIntegerTerm(c);
  ptr[1] = AbsPair(ptr+2);

  return ptr+2;
}

static inline Word
EXTEND_SEQ_CHARS(Word ptr, int c) {
  ptr[0] = codeToAtom(c);
  ptr[1] = AbsPair(ptr+2);

  return ptr+2;
}

static inline int
CLOSE_SEQ_STRING(Word p, Word p0, term_t tail, term_t term, term_t l) {
  CACHE_REGS
  Yap_PutInSlot(l, AbsPair(p0) PASS_REGS);
  p--;
  if (tail) {
    RESET_VARIABLE(p);
    if (Yap_unify(Yap_GetFromSlot(l), Yap_GetFromSlot(term)))  {
      Yap_PutInSlot(tail, (CELL)(p) PASS_REGS);
      return TRUE;
    }
    return FALSE;
  } else {
    p[0] = TermNil;
    return Yap_unify(Yap_GetFromSlot(l), Yap_GetFromSlot(term ));
  }
}

#endif
