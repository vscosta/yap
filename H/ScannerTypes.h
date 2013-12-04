typedef enum TokenKinds {
  Name_tok,
  Number_tok,
  Var_tok,
  String_tok,
  WString_tok,
  Ponctuation_tok,
  Error_tok,
  QuasiQuotes_tok,
  WQuasiQuotes_tok,
  eot_tok
} tkinds;

typedef	 struct	TOKEN {
  enum TokenKinds Tok;
  Term TokInfo;
  int	TokPos;
  struct TOKEN *TokNext;
} TokEntry;

#define	Ord(X) ((enum TokenKinds) (X))

#define	NextToken GNextToken( PASS_REGS1 )

typedef	struct VARSTRUCT {
  Term VarAdr;
  CELL hv;
  UInt refs;
  struct VARSTRUCT *VarLeft, *VarRight;
  char VarRep[1];
} VarEntry;

