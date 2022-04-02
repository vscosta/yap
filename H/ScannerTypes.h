#ifndef SCANNER_TYPES_H
#define SCANNER_TYPES_H

typedef enum TokenKinds {
  Name_tok,
  Number_tok,
  Var_tok,
  String_tok,
  BQString_tok,
  Ponctuation_tok,
  Error_tok,
  QuasiQuotes_tok,
  eot_tok
} tkinds;

typedef struct TOKEN {
  enum TokenKinds Tok;
  YAP_Term TokInfo;
  intptr_t TokLinePos, TokLine, TokOffset;
  struct TOKEN *TokNext;
} TokEntry;

#define tok_pos(t) (t->TokLinePos + t->TokOffset - 2)
#define Ord(X) ((enum TokenKinds)(X))

#define NextToken GNextToken(PASS_REGS1)

typedef struct VARSTRUCT {
  YAP_Term VarAdr;
  YAP_CELL hv;
  YAP_UInt refs;
  struct VARSTRUCT *VarLeft, *VarRight;
  YAP_Atom VarRep;
  //  struct  *
  struct VARSTRUCT *VarNext;
} VarEntry;

/* routines in scanner.c */
extern TokEntry *Yap_tokenizer(void *streamp, void *sp);
extern void Yap_clean_tokenizer(void);
extern char *Yap_AllocScannerMemory(unsigned int);

#endif
