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
  Term TokInfo;
  intptr_t TokPos, TokLine, TokOffset;
  struct TOKEN *TokNext;
} TokEntry;

#define Ord(X) ((enum TokenKinds)(X))

#define NextToken GNextToken(PASS_REGS1)

typedef struct VARSTRUCT {
  Term VarAdr;
  CELL hv;
  UInt refs;
  struct VARSTRUCT *VarLeft, *VarRight;
  Atom VarRep;
  //  struct  *
  struct VARSTRUCT *VarNext;
} VarEntry;

/* routines in scanner.c */
extern TokEntry *Yap_tokenizer(void *streamp, void *sp);
extern void Yap_clean_tokenizer(void);
extern char *Yap_AllocScannerMemory(unsigned int);
