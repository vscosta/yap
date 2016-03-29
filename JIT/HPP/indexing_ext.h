p #define USER_SWITCH_INSTINIT BLOCKADDRESS = (CELL)(*_PREG);
{
  yamop *new = Yap_udi_search((*_PREG)->u.lp.p);
  if (!new) {
    (*_PREG) = (*_PREG)->u.lp.l;
    JMPNext();
  } else {
    (*_PREG) = new;
    JMPNext();
  }
}

#define USER_SWITCH_END BLOCK = (CELL)USER_SWITCH_END;

#define SWITCH_ON_TYPE_INSTINIT                                                \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  d0 = CACHED_A1();                                                            \
  Int nonvar = 1;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    nonvar = 0;                                                                \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      copy_jmp_address((*_PREG)->u.llll.l4);                                   \
      (*_PREG) = (*_PREG)->u.llll.l4;                                          \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    if (IsPairTerm(d0)) {                                                      \
      (*_SREG) = RepPair(d0);                                                  \
      copy_jmp_address((*_PREG)->u.llll.l1);                                   \
      (*_PREG) = (*_PREG)->u.llll.l1;                                          \
      JMPNext();                                                               \
    } else if (!IsApplTerm(d0)) {                                              \
      copy_jmp_address((*_PREG)->u.llll.l2);                                   \
      (*_PREG) = (*_PREG)->u.llll.l2;                                          \
      I_R = d0;                                                                \
      JMPNext();                                                               \
    } else {                                                                   \
      copy_jmp_address((*_PREG)->u.llll.l3);                                   \
      (*_PREG) = (*_PREG)->u.llll.l3;                                          \
      (*_SREG) = RepAppl(d0);                                                  \
      JMPNext();                                                               \
    }                                                                          \
  }

#define SWITCH_ON_TYPE_END BLOCK = (CELL)SWITCH_ON_TYPE_END;

#if UNIQUE_TAG_FOR_PAIRS
#define SWITCH_LIST_NL_INSTINIT                                                \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  ALWAYS_LOOKAHEAD((*_PREG)->u.ollll.pop);                                     \
  d0 = CACHED_A1();                                                            \
  Int nonvar = 0;                                                              \
  Int pair = 1;                                                                \
  if (!IsPairTerm(d0)) {                                                       \
    pair = 0;                                                                  \
    do {                                                                       \
      if (!IsVarTerm(d0)) {                                                    \
        if (d0 == TermNil) {                                                   \
          (*_PREG) = (*_PREG)->u.ollll.l2;                                     \
          JMPNext();                                                           \
        } else {                                                               \
          if (IsApplTerm(d0)) {                                                \
            copy_jmp_address((*_PREG)->u.ollll.l3);                            \
            (*_PREG) = (*_PREG)->u.ollll.l3;                                   \
            (*_SREG) = RepAppl(d0);                                            \
            JMPNext();                                                         \
          } else {                                                             \
            copy_jmp_address((*_PREG)->u.ollll.l3);                            \
            (*_PREG) = (*_PREG)->u.ollll.l3;                                   \
            I_R = d0;                                                          \
            JMPNext();                                                         \
          }                                                                    \
        }                                                                      \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(pt0);                                                           \
      if (Unsigned(pt0) == (d0))                                               \
        break;                                                                 \
      if (IsPairTerm(d0)) {                                                    \
        pair = 1;                                                              \
        break;                                                                 \
      }                                                                        \
    } while (TRUE);                                                            \
    if (!nonvar && !pair) {                                                    \
      copy_jmp_address((*_PREG)->u.ollll.l4);                                  \
      (*_PREG) = (*_PREG)->u.ollll.l4;                                         \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (!nonvar && pair) {                                                       \
    copy_jmp_address((*_PREG)->u.ollll.l1);                                    \
    (*_PREG) = (*_PREG)->u.ollll.l1;                                           \
    (*_SREG) = RepPair(d0);                                                    \
    ALWAYS_GONext();                                                           \
  }
#else
#define SWITCH_LIST_NL_INSTINIT                                                \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  ALWAYS_LOOKAHEAD((*_PREG)->u.ollll.pop);                                     \
  d0 = CACHED_A1();                                                            \
  Int nonvar = 0;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      copy_jmp_address((*_PREG)->u.ollll.l4);                                  \
      (*_PREG) = (*_PREG)->u.ollll.l4;                                         \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    if (__builtin_expect(IsPairTerm(d0), 1)) {                                 \
      copy_jmp_address((*_PREG)->u.ollll.l1);                                  \
      (*_PREG) = (*_PREG)->u.ollll.l1;                                         \
      (*_SREG) = RepPair(d0);                                                  \
      ALWAYS_GONext();                                                         \
    }                                                                          \
    if (d0 == TermNil) {                                                       \
      (*_PREG) = (*_PREG)->u.ollll.l2;                                         \
      JMPNext();                                                               \
    } else {                                                                   \
      if (IsApplTerm(d0)) {                                                    \
        copy_jmp_address((*_PREG)->u.ollll.l3);                                \
        (*_PREG) = (*_PREG)->u.ollll.l3;                                       \
        (*_SREG) = RepAppl(d0);                                                \
        JMPNext();                                                             \
      } else {                                                                 \
        copy_jmp_address((*_PREG)->u.ollll.l3);                                \
        (*_PREG) = (*_PREG)->u.ollll.l3;                                       \
        I_R = d0;                                                              \
        JMPNext();                                                             \
      }                                                                        \
    }                                                                          \
  }
#endif

#define SWITCH_LIST_NL_END BLOCK = (CELL)SWITCH_LIST_NL_END;

#define SWITCH_ON_ARG_TYPE_INSTINIT                                            \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  d0 = XREG((*_PREG)->u.xllll.x);                                              \
  Int nonvar = 1;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    nonvar = 0;                                                                \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      copy_jmp_address((*_PREG)->u.xllll.l4);                                  \
      (*_PREG) = (*_PREG)->u.xllll.l4;                                         \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    if (IsPairTerm(d0)) {                                                      \
      copy_jmp_address((*_PREG)->u.xllll.l1);                                  \
      (*_PREG) = (*_PREG)->u.xllll.l1;                                         \
      (*_SREG) = RepPair(d0);                                                  \
      JMPNext();                                                               \
    } else if (!IsApplTerm(d0)) {                                              \
      copy_jmp_address((*_PREG)->u.xllll.l2);                                  \
      (*_PREG) = (*_PREG)->u.xllll.l2;                                         \
      I_R = d0;                                                                \
      JMPNext();                                                               \
    } else {                                                                   \
      copy_jmp_address((*_PREG)->u.xllll.l3);                                  \
      (*_PREG) = (*_PREG)->u.xllll.l3;                                         \
      (*_SREG) = RepAppl(d0);                                                  \
      JMPNext();                                                               \
    }                                                                          \
  }

#define SWITCH_ON_ARG_TYPE_END BLOCK = (CELL)SWITCH_ON_ARG_TYPE_END;

#define SWITCH_ON_SUB_ARG_TYPE_INSTINIT                                        \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  d0 = (*_SREG)[(*_PREG)->u.sllll.s];                                          \
  Int nonvar = 1;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    nonvar = 0;                                                                \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      copy_jmp_address((*_PREG)->u.sllll.l4);                                  \
      (*_PREG) = (*_PREG)->u.sllll.l4;                                         \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    if (IsPairTerm(d0)) {                                                      \
      copy_jmp_address((*_PREG)->u.sllll.l1);                                  \
      (*_PREG) = (*_PREG)->u.sllll.l1;                                         \
      (*_SREG) = RepPair(d0);                                                  \
      JMPNext();                                                               \
    } else if (!IsApplTerm(d0)) {                                              \
      copy_jmp_address((*_PREG)->u.sllll.l2);                                  \
      (*_PREG) = (*_PREG)->u.sllll.l2;                                         \
      I_R = d0;                                                                \
      JMPNext();                                                               \
    } else {                                                                   \
      copy_jmp_address((*_PREG)->u.sllll.l3);                                  \
      (*_PREG) = (*_PREG)->u.sllll.l3;                                         \
      (*_SREG) = RepAppl(d0);                                                  \
      JMPNext();                                                               \
    }                                                                          \
  }

#define SWITCH_ON_SUB_ARG_TYPE_END BLOCK = (CELL)SWITCH_ON_SUB_ARG_TYPE_END;

#define JUMP_IF_VAR_INSTINIT                                                   \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  d0 = CACHED_A1();                                                            \
  Int nonvar = 1;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    nonvar = 0;                                                                \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      copy_jmp_address((*_PREG)->u.l.l);                                       \
      (*_PREG) = (*_PREG)->u.l.l;                                              \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    (*_PREG) = NEXTOP((*_PREG), l);                                            \
    JMPNext();                                                                 \
  }

#define JUMP_IF_VAR_END BLOCK = (CELL)JUMP_IF_VAR_END;

#define JUMP_IF_NONVAR_INSTINIT                                                \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  d0 = XREG((*_PREG)->u.xll.x);                                                \
  Int nonvar = 0;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      (*_PREG) = NEXTOP((*_PREG), xll);                                        \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    copy_jmp_address((*_PREG)->u.xll.l1);                                      \
    (*_PREG) = (*_PREG)->u.xll.l1;                                             \
    JMPNext();                                                                 \
  }

#define JUMP_IF_NONVAR_END BLOCK = (CELL)JUMP_IF_NONVAR_END;

#define IF_NOT_THEN_INSTINIT                                                   \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  register CELL *pt0;                                                          \
  d0 = CACHED_A1();                                                            \
  Int nonvar = 1;                                                              \
  if (IsVarTerm(d0)) {                                                         \
    nonvar = 0;                                                                \
    (pt0) = (CELL *)(d0);                                                      \
    (d0) = *(CELL *)(d0);                                                      \
    while (Unsigned(pt0) != (d0)) {                                            \
      if (!IsVarTerm(d0)) {                                                    \
        nonvar = 1;                                                            \
        break;                                                                 \
      }                                                                        \
      (pt0) = (CELL *)(d0);                                                    \
      (d0) = *(CELL *)(d0);                                                    \
    }                                                                          \
    if (!nonvar) {                                                             \
      copy_jmp_address((*_PREG)->u.clll.l3);                                   \
      (*_PREG) = (*_PREG)->u.clll.l3;                                          \
      JMPNext();                                                               \
    }                                                                          \
  }                                                                            \
  if (nonvar) {                                                                \
    if (d0 == (*_PREG)->u.clll.c) {                                            \
      copy_jmp_address((*_PREG)->u.clll.l2);                                   \
      (*_PREG) = (*_PREG)->u.clll.l2;                                          \
      JMPNext();                                                               \
    } else {                                                                   \
      copy_jmp_address((*_PREG)->u.clll.l1);                                   \
      (*_PREG) = (*_PREG)->u.clll.l1;                                          \
      JMPNext();                                                               \
    }                                                                          \
  }

#define IF_NOT_THEN_END BLOCK = (CELL)IF_NOT_THEN_END;

#define HRASH_SHIFT 6

#define SWITCH_ON_FUNC_INSTINIT                                                \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0, d1;                                                        \
  register CELL *pt0;                                                          \
  d1 = *(*_SREG)++;                                                            \
  {                                                                            \
    CELL Mask = ((*_PREG)->u.sssl.s - 1) << 1,                                 \
         hash = d1 >> (HRASH_SHIFT - 1) & Mask;                                \
    CELL *base;                                                                \
    base = (CELL *)(*_PREG)->u.sssl.l;                                         \
    pt0 = base + hash;                                                         \
    d0 = pt0[0];                                                               \
    if (d0 == d1 || d0 == 0) {                                                 \
      copy_jmp_addressa(pt0 + 1);                                              \
      (*_PREG) = (yamop *)(pt0[1]);                                            \
      JMPNext();                                                               \
    } else {                                                                   \
      register CELL d = ((d1 | 1) << 1) & Mask;                                \
      while (1) {                                                              \
        hash = (hash + d) & Mask;                                              \
        pt0 = base + hash;                                                     \
        d0 = pt0[0];                                                           \
        if (d0 == d1 || d0 == 0) {                                             \
          copy_jmp_addressa(pt0 + 1);                                          \
          (*_PREG) = (yamop *)pt0[1];                                          \
          break;                                                               \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  }

#define SWITCH_ON_FUNC_END BLOCK = (CELL)SWITCH_ON_FUNC_END;

#define SWITCH_ON_CONS_INSTINIT                                                \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0, d1;                                                        \
  register CELL *pt0;                                                          \
  d1 = I_R;                                                                    \
  {                                                                            \
    CELL Mask = ((*_PREG)->u.sssl.s - 1) << 1,                                 \
         hash = d1 >> (HRASH_SHIFT - 1) & Mask;                                \
    CELL *base;                                                                \
    base = (CELL *)(*_PREG)->u.sssl.l;                                         \
    pt0 = base + hash;                                                         \
    d0 = pt0[0];                                                               \
    if (d0 == d1 || d0 == 0) {                                                 \
      copy_jmp_addressa(pt0 + 1);                                              \
      (*_PREG) = (yamop *)(pt0[1]);                                            \
      JMPNext();                                                               \
    } else {                                                                   \
      register CELL d = ((d1 | 1) << 1) & Mask;                                \
      while (1) {                                                              \
        hash = (hash + d) & Mask;                                              \
        pt0 = base + hash;                                                     \
        d0 = pt0[0];                                                           \
        if (d0 == d1 || d0 == 0) {                                             \
          copy_jmp_addressa(pt0 + 1);                                          \
          (*_PREG) = (yamop *)pt0[1];                                          \
          break;                                                               \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  }

#define SWITCH_ON_CONS_END BLOCK = (CELL)SWITCH_ON_CONS_END;

#define GO_ON_FUNC_INSTINIT                                                    \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  {                                                                            \
    CELL *pt = (CELL *)((*_PREG)->u.sssl.l);                                   \
    d0 = *(*_SREG)++;                                                          \
    if (d0 == pt[0]) {                                                         \
      copy_jmp_addressa(pt + 1);                                               \
      (*_PREG) = (yamop *)pt[1];                                               \
      JMPNext();                                                               \
    } else {                                                                   \
      copy_jmp_addressa(pt + 3);                                               \
      (*_PREG) = (yamop *)pt[3];                                               \
      JMPNext();                                                               \
    }                                                                          \
  }

#define GO_ON_FUNC_END BLOCK = (CELL)GO_ON_FUNC_END;

#define GO_ON_CONS_INSTINIT                                                    \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d0;                                                            \
  {                                                                            \
    CELL *pt = (CELL *)((*_PREG)->u.sssl.l);                                   \
    d0 = I_R;                                                                  \
    if (d0 == pt[0]) {                                                         \
      copy_jmp_addressa(pt + 1);                                               \
      (*_PREG) = (yamop *)pt[1];                                               \
      JMPNext();                                                               \
    } else {                                                                   \
      copy_jmp_addressa(pt + 3);                                               \
      (*_PREG) = (yamop *)pt[3];                                               \
      JMPNext();                                                               \
    }                                                                          \
  }

#define GO_ON_CONS_END BLOCK = (CELL)GO_ON_CONS_END;

#define IF_FUNC_INSTINIT                                                       \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d1;                                                            \
  register CELL *pt0;                                                          \
  pt0 = (CELL *)(*_PREG)->u.sssl.l;                                            \
  d1 = *(*_SREG)++;                                                            \
  while (pt0[0] != d1 && pt0[0] != (CELL)NULL) {                               \
    pt0 += 2;                                                                  \
  }                                                                            \
  copy_jmp_addressa(pt0 + 1);                                                  \
  (*_PREG) = (yamop *)(pt0[1]);                                                \
  JMPNext();

#define IF_FUNC_END BLOCK = (CELL)IF_FUNC_END;

#define IF_CONS_INSTINIT                                                       \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  register CELL d1;                                                            \
  register CELL *pt0;                                                          \
  pt0 = (CELL *)(*_PREG)->u.sssl.l;                                            \
  d1 = I_R;                                                                    \
  while (pt0[0] != d1 && pt0[0] != 0L) {                                       \
    pt0 += 2;                                                                  \
  }                                                                            \
  copy_jmp_addressa(pt0 + 1);                                                  \
  (*_PREG) = (yamop *)(pt0[1]);                                                \
  JMPNext();

#define IF_CONS_END BLOCK = (CELL)IF_CONS_END;

#define INDEX_DBREF_INSTINIT                                                   \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  (*_PREG) = NEXTOP((*_PREG), e);                                              \
  I_R = AbsAppl((*_SREG) - 1);                                                 \
  GONext();

#define INDEX_DBREF_END BLOCK = (CELL)INDEX_DBREF_END;

#define INDEX_BLOB_INSTINIT                                                    \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  (*_PREG) = NEXTOP((*_PREG), e);                                              \
  I_R = Yap_DoubleP_key((*_SREG));                                             \
  GONext();

#define INDEX_BLOB_END BLOCK = (CELL)INDEX_BLOB_END;

#define INDEX_LONG_INSTINIT                                                    \
  BLOCKADDRESS = (CELL)(*_PREG);                                               \
  (*_PREG) = NEXTOP((*_PREG), e);                                              \
  I_R = Yap_IntP_key((*_SREG));                                                \
  GONext();

#define INDEX_LONG_END BLOCK = (CELL)INDEX_LONG_INSTINIT;
