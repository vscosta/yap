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
  intptr_t TokPos, TokLine;
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
  struct VARSTRUCT *VarNext;
} VarEntry;
