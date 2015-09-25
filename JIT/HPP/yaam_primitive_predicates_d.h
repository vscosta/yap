#define P_ATOM_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);
	  
#define P_ATOM_X_ATOM \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext(); \
	  
#define P_ATOM_X_NOATOM \
	(*_PREG) = (*_PREG)->u.xl.F; \
	GONext();

#define P_ATOM_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;
	  
#define P_ATOM_Y_IFOK \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();
	  
#define P_ATOM_Y_NOIF \
	(*_PREG) = (*_PREG)->u.yl.F; \
	GONext();
  
#define P_ATOM_Y_END \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_ATOMIC_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);
	  
#define P_ATOMIC_X_NONVAR \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();
	  
#define P_ATOMIC_X_VAR \
	(*_PREG) = (*_PREG)->u.xl.F; \
	GONext();
	  
#define P_ATOMIC_X_END \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_ATOMIC_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;
	  
#define P_ATOMIC_Y_NONVAR \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();
	  
#define P_ATOMIC_Y_VAR \
	(*_PREG) = (*_PREG)->u.yl.F; \
	GONext();
	  
#define P_ATOMIC_Y_END \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_INTEGER_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x); \
	  
#define P_INTEGER_X_INTEGER_X_NVAR_OK \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();
	
#define P_INTEGER_X_INTEGER_X_NVAR_NOOK \
	    (*_PREG) = (*_PREG)->u.xl.F; \
	    GONext();

#define P_INTEGER_X_INTEGER_X_UNK \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_INTEGER_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;
	  
#define P_INTEGER_Y_INTEGER_Y_NVAR_OK \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();
	  
#define P_INTEGER_Y_INTEGER_Y_NVAR_NOOK \
	    (*_PREG) = (*_PREG)->u.yl.F; \
	    GONext();
		
#define P_INTEGER_Y_INTEGER_Y_UNK \
	    (*_PREG) = (*_PREG)->u.yl.F; \
	    GONext();

#define P_NONVAR_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);
	
#define P_NONVAR_X_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), xl); \
      GONext();
	  
#define P_NONVAR_X_NONONVAR \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_NONVAR_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;

#define P_NONVAR_Y_NONVAR \
      (*_PREG) = NEXTOP((*_PREG), yl); \
      GONext();

#define P_NONVAR_Y_NONONVAR \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_NUMBER_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);
	  
#define P_NUMBER_X_INT \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();

#define P_NUMBER_X_FUNCTORINT \
	    (*_PREG) = NEXTOP((*_PREG), xl); \
	    GONext();

#define P_NUMBER_X_FUNCTORDEFAULT \
	    (*_PREG) = (*_PREG)->u.xl.F; \
	    GONext();

#define P_NUMBER_X_POST_IF \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_NUMBER_X_NUMBER_X_UNK \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_NUMBER_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;

#define P_NUMBER_Y_INT \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();

#define P_NUMBER_Y_FUNCTORINT \
	    (*_PREG) = NEXTOP((*_PREG), yl); \
	    GONext();

#define P_NUMBER_Y_FUNCTORDEFAULT \
	    (*_PREG) = (*_PREG)->u.yl.F; \
	    GONext();

#define P_NUMBER_Y_POST_IF \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_NUMBER_Y_NUMBER_Y_UNK \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_VAR_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);

#define P_VAR_X_NONVAR \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_VAR_X_VAR \
      (*_PREG) = NEXTOP((*_PREG), xl); \
      GONext();

#define P_VAR_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;

#define P_VAR_Y_NONVAR \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_VAR_Y_VAR \
      (*_PREG) = NEXTOP((*_PREG), yl); \
      GONext();

#define P_DB_REF_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xl.x);

#define P_DB_REF_X_DBREF \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();

#define P_DB_REF_X_NODBREF \
	(*_PREG) = (*_PREG)->u.xl.F; \
	GONext();

#define P_DB_REF_X_DBREF_X_UNK \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_DB_REF_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;
	  
#define P_DB_REF_Y_DBREF \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();
	  
#define P_DB_REF_Y_NODBREF \
	(*_PREG) = (*_PREG)->u.yl.F; \
	GONext();

#define P_DB_REF_Y_DBREF_Y_UNK \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_PRIMITIVE_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xl.x);

#define P_PRIMITIVE_X_PRIMITIVE \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();

#define P_PRIMITIVE_X_NOPRIMITIVE \
	(*_PREG) = (*_PREG)->u.xl.F; \
	GONext();

#define P_PRIMITIVE_X_PRIMI_X_UNK \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_PRIMITIVE_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;

#define P_PRIMITIVE_Y_PRIMITIVE \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();

#define P_PRIMITIVE_Y_NOPRIMITIVE \
	(*_PREG) = (*_PREG)->u.yl.F; \
	GONext();

#define P_PRIMITIVE_Y_PRIMI_Y_UNK \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_COMPOUND_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);

#define P_COMPOUND_X_PAIR \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();

#define P_COMPOUND_X_APPL_IFOK \
	  (*_PREG) = (*_PREG)->u.xl.F; \
	  GONext();

#define P_COMPOUND_X_APPL \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();
	  
#define P_COMPOUND_X_NOAPPL \
	(*_PREG) = (*_PREG)->u.xl.F; \
	GONext();

#define P_COMPOUND_X_COMPOUND_X_UNK \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_COMPOUND_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;

#define P_COMPOUND_Y_PAIR \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();

#define P_COMPOUND_Y_APPL_IFOK \
	  (*_PREG) = (*_PREG)->u.yl.F; \
	  GONext();

#define P_COMPOUND_Y_APPL \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();
	
#define P_COMPOUND_Y_NOAPPL \
	(*_PREG) = (*_PREG)->u.yl.F; \
	GONext();

#define P_COMPOUND_Y_COMPOUND_Y_UNK \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_FLOAT_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xl.x);

#define P_FLOAT_X_FLOAT \
	(*_PREG) = NEXTOP((*_PREG), xl); \
	GONext();

#define P_FLOAT_X_POST_IF \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_FLOAT_X_FLOAT_X_UNK \
      (*_PREG) = (*_PREG)->u.xl.F; \
      GONext();

#define P_FLOAT_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      pt0 = YREG + (*_PREG)->u.yl.y; \
      d0 = *pt0;

#define P_FLOAT_Y_FLOAT \
	(*_PREG) = NEXTOP((*_PREG), yl); \
	GONext();

#define P_FLOAT_Y_POST_IF \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_FLOAT_Y_FLOAT_Y_UNK \
      (*_PREG) = (*_PREG)->u.yl.F; \
      GONext();

#define P_PLUS_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_PLUS_VV_PLUS_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);
	  
#define P_PLUS_VV_PLUS_VV_NVAR_NVAR_INT \
    if (!IsIntTerm(d0) || !IsIntTerm(d1)) { BACK(); } \
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1)); \
	  XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_PLUS_VV_PLUS_VV_NVAR_NVAR_NOINT \
	saveregs(); \
	d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	  XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_PLUS_VV_PLUS_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is _+B"); \
      setregs(); \
      FAIL();

#define P_PLUS_VV_PLUS_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B"); \
      setregs(); \
      FAIL();

#define P_PLUS_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi); \
	  Int d1 = (*_PREG)->u.xxn.c;
	  
#define P_PLUS_VC_PLUS_VC_NVAR_INT \
	d0 = MkIntegerTerm(IntOfTerm(d0) + d1); \
	  XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	  
#define P_PLUS_VC_PLUS_VC_NVAR_NOINT \
	saveregs(); \
	d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1)); \
	setregs(); \
	  XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_PLUS_VC_PLUS_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL();

#define P_PLUS_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_PLUS_Y_VV_PLUS_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);
	  
#define P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_INT \
	if (!IsIntTerm(d0) || !IsIntTerm(d1)) { BACK(); } \
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1)); \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();
    
#define P_PLUS_Y_VV_PLUS_Y_VV_NVAR_NVAR_NOINT \
	saveregs(); \
	d0 = p_plus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_PLUS_Y_VV_PLUS_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B"); \
      setregs(); \
      FAIL();

#define P_PLUS_Y_VV_PLUS_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A+B"); \
      setregs(); \
      FAIL();

#define P_PLUS_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi); \
	  Int d1 = (*_PREG)->u.yxn.c;

#define P_PLUS_Y_VC_PLUS_Y_VC_NVAR_INT \
	d0 = MkIntegerTerm(IntOfTerm(d0) + d1); \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();
	  
#define P_PLUS_Y_VC_PLUS_Y_VC_NVAR_NOINT \
	saveregs(); \
	d0 = p_plus(Yap_Eval(d0), MkIntegerTerm(d1)); \
	setregs(); \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_PLUS_Y_VC_PLUS_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A + " Int_FORMAT, (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL();

#define P_MINUS_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_MINUS_VV_MINUS_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_MINUS_VV_MINUS_VV_NVAR_NVAR_INT \
    if (!IsIntTerm(d0) || !IsIntTerm(d1)) { BACK(); } \
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1)); \
	  XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();
	  
#define P_MINUS_VV_MINUS_VV_NVAR_NVAR_NOINT \
	saveregs(); \
	d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
	  XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_MINUS_VV_MINUS_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B"); \
      setregs(); \
      FAIL();

#define P_MINUS_VV_MINUS_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B"); \
      setregs(); \
      FAIL();

#define P_MINUS_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi); \
	  Int d1 = (*_PREG)->u.xxn.c;

#define P_MINUS_CV_MINUS_CV_NVAR_INT \
	d0 = MkIntegerTerm(d1 - IntOfTerm(d0)); \
	  XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	
#define P_MINUS_CV_MINUS_CV_NVAR_NOINT \
	saveregs(); \
	d0 = p_minus(MkIntegerTerm(d1),Yap_Eval(d0)); \
	setregs(); \
	  XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_MINUS_CV_MINUS_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "-A", (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL();

#define P_MINUS_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_MINUS_Y_VV_MINUS_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_MINUS_Y_VV_INTTERM \
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));

#define P_MINUS_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_minus(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_MINUS_Y_VV_D0EQUALS0L \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL();
	  
#define P_MINUS_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_MINUS_Y_VV_MINUS_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B"); \
      setregs(); \
      FAIL();

#define P_MINUS_Y_VV_MINUS_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A-B"); \
      setregs(); \
      FAIL();

#define P_MINUS_Y_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_MINUS_Y_CV_MINUS_Y_CV_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_MINUS_Y_CV_INTTERM \
	  d0 = MkIntegerTerm(d1 - IntOfTerm(d0));

#define P_MINUS_Y_CV_NOINTTERM \
	  saveregs(); \
	  d0 = p_minus(MkIntegerTerm(d1), Yap_Eval(d0)); \
	  setregs();

#define P_MINUS_Y_CV_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_MINUS_Y_CV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_MINUS_Y_CV_MINUS_Y_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "-A", (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL();

#define P_TIMES_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      register CELL d0, d1; \
      register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_TIMES_VV_TIMES_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);
  
#define P_TIMES_VV_TIMES_VV_NVAR_NVAR_INT \
	if (!IsIntTerm(d0) || !IsIntTerm(d1)) { BACK(); } \
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1)); \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();
	  
#define P_TIMES_VV_TIMES_VV_NVAR_NVAR_NOINT \
	saveregs(); \
	d0 = p_times(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();
  
#define P_TIMES_VV_TIMES_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B"); \
      setregs(); \
      FAIL();

#define P_TIMES_VV_TIMES_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B"); \
      setregs(); \
      FAIL();

#define P_TIMES_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi); \
	  Int d1 = (*_PREG)->u.xxn.c;
    
#define P_TIMES_VC_TIMES_VC_NVAR_INT \
	d0 = times_int(IntOfTerm(d0), d1); \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	  
#define P_TIMES_VC_TIMES_VC_NVAR_NOINT \
	saveregs(); \
	d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1)); \
	setregs(); \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_TIMES_VC_TIMES_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL();

#define P_TIMES_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_TIMES_Y_VV_TIMES_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_TIMES_Y_VV_INTTERM \
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1));

#define P_TIMES_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_times(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_TIMES_Y_VV_D0EQUALS0L \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL();

#define P_TIMES_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_TIMES_Y_VV_TIMES_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B"); \
      setregs(); \
      FAIL();
	  
#define P_TIMES_Y_VV_TIMES_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A*B"); \
      setregs(); \
      FAIL();

#define P_TIMES_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi); \
	  Int d1 = (*_PREG)->u.yxn.c;

#define P_TIMES_Y_VC_TIMES_Y_VC_NVAR_INT \
	d0 = times_int(IntOfTerm(d0), d1); \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();
	  
#define P_TIMES_Y_VC_TIMES_Y_VC_NVAR_NOINT \
	saveregs(); \
	d0 = p_times(Yap_Eval(d0), MkIntegerTerm(d1)); \
	setregs(); \
	  pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_TIMES_Y_VC_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_TIMES_Y_VC_TIMES_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A* " Int_FORMAT, (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL();

#define P_DIV_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_DIV_VV_DIV_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_DIV_VV_DIV_VV_NVAR_NVAR_INT \
      BLOCK = (CELL)P_DIV_VV_DIV_VV_NVAR_NVAR_INT; \
      FAILED = 0; \
	  if (!IsIntTerm(d0) || !IsIntTerm(d1)) { BACK(); } \
	  Int div = IntOfTerm(d1); \
	  if (div == 0) { \
	    saveregs(); \
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2"); \
	    setregs(); \
	    YAAM_FAIL; \
	  } \
	  else { \
	    d0 = MkIntTerm(IntOfTerm(d0) / div); \
        XREG((*_PREG)->u.xxx.x) = d0; \
        (*_PREG) = NEXTOP((*_PREG), xxx); \
        GONext(); \
	  }
	  
#define P_DIV_VV_DIV_VV_NVAR_NVAR_NOINT \
	  saveregs(); \
	  d0 = p_div(Yap_Eval(d0), Yap_Eval(d1)); \
	  setregs(); \
        XREG((*_PREG)->u.xxx.x) = d0; \
        (*_PREG) = NEXTOP((*_PREG), xxx); \
        GONext();

#define P_DIV_VV_DIV_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL();

#define P_DIV_VV_DIV_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL();

#define P_DIV_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi);

#define P_DIV_VC_DIV_VC_NVAR \
	Int d1 = (*_PREG)->u.xxn.c;

#define P_DIV_VC_INTTERM \
	  d0 = MkIntTerm(IntOfTerm(d0) / d1);

#define P_DIV_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_div(Yap_Eval(d0),MkIntegerTerm(d1)); \
	  setregs();

#define P_DIV_VC_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_DIV_VC_NVAR_END \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	  
#define P_DIV_VC_DIV_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL();

#define P_DIV_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi);

#define P_DIV_CV_DIV_CV_NVAR \
	Int d1 = (*_PREG)->u.xxn.c;

#define P_DIV_CV_INTTERM_INIT \
	  Int div = IntOfTerm(d0);

#define P_DIV_CV_INTTERM_DIVEQUALS0 \
	    saveregs(); \
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2"); \
	    setregs(); \
	    FAIL();

#define P_DIV_CV_INTTERM_END \
	  d0 = MkIntegerTerm(d1 / div);

#define P_DIV_CV_NOINTTERM \
	  saveregs(); \
	  d0 = p_div(MkIntegerTerm(d1),Yap_Eval(d0));

#define P_DIV_CV_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_DIV_CV_NVAR_END \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_DIV_CV_DIV_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "// A", (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL();

#define P_DIV_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_DIV_Y_VV_DIV_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_DIV_Y_VV_INTTERM_INIT \
	Int div = IntOfTerm(d1);
	
#define P_DIV_Y_VV_INTTERM_DIVEQUALS0 \
	  saveregs(); \
	  Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2"); \
	  setregs(); \
	  FAIL();

#define P_DIV_Y_VV_INTTERM_END \
	d0 = MkIntTerm(IntOfTerm(d0) / div);

#define P_DIV_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_div(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_DIV_Y_VV_D0EQUALS0L \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL();

#define P_DIV_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_DIV_Y_VV_DIV_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL();

#define P_DIV_Y_VV_DIV_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL();

#define P_DIV_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_DIV_Y_VC_DIV_Y_VC_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_DIV_Y_VC_INTTERM \
	  d0 = MkIntTerm(IntOfTerm(d0)/d1);

#define P_DIV_Y_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_div(Yap_Eval(d0),MkIntegerTerm(d1)); \
	  setregs();

#define P_DIV_Y_VC_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_DIV_Y_VC_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_DIV_Y_VC_DIV_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A//B"); \
      setregs(); \
      FAIL();

#define P_DIV_Y_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_DIV_Y_CV_DIV_Y_CV_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_DIV_Y_CV_INTTERM_INIT \
	  Int div = IntOfTerm(d0);

#define P_DIV_Y_CV_INTTERM_DIVEQUALS0 \
	    saveregs(); \
	    Yap_NilError(EVALUATION_ERROR_ZERO_DIVISOR,"// /2"); \
	    setregs(); \
	    FAIL();

#define P_DIV_Y_CV_INTTERM_END \
	  d0 = MkIntegerTerm(d1 / div);

#define P_DIV_Y_CV_NOINTTERM \
	  saveregs(); \
	  d0 = p_div(MkIntegerTerm(d1), Yap_Eval(d0)); \
	  setregs();

#define P_DIV_Y_CV_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_DIV_Y_CV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_DIV_Y_CV_DIV_Y_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is " Int_FORMAT "// A", (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL();

#define P_AND_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_AND_VV_AND_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);
      
#define P_AND_VV_AND_VV_NVAR_NVAR_INT \
	if (!IsIntTerm(d0) || !IsIntTerm(d1)) { BACK(); } \
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1)); \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();
	  
#define P_AND_VV_AND_VV_NVAR_NVAR_NOINT \
	saveregs(); \
	d0 = p_and(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs(); \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_AND_VV_AND_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B"); \
      setregs(); \
      FAIL();

#define P_AND_VV_AND_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B"); \
      setregs(); \
      FAIL();

#define P_AND_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi); \
	  Int d1 = (*_PREG)->u.xxn.c;

#define P_AND_VC_AND_VC_NVAR_INT \
	d0 = MkIntegerTerm(IntOfTerm(d0) & d1); \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	  
#define P_AND_VC_AND_VC_NVAR_NOINT \
	saveregs(); \
	d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1)); \
	setregs(); \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_AND_VC_AND_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL();

#define P_AND_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_AND_Y_VV_AND_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_AND_Y_VV_INTTERM \
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));

#define P_AND_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_and(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_AND_Y_VV_D0EQUALS0L \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL();

#define P_AND_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_AND_Y_VV_AND_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B"); \
      setregs(); \
      FAIL();

#define P_AND_Y_VV_AND_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A/\\B"); \
      setregs(); \
      FAIL();

#define P_AND_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_AND_Y_VC_AND_Y_VC_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_AND_Y_VC_INTTERM \
	  d0 = MkIntegerTerm(IntOfTerm(d0) & d1);

#define P_AND_Y_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_and(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs();

#define P_AND_Y_VC_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_AND_Y_VC_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_AND_Y_VC_AND_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A /\\ " Int_FORMAT , (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL();

#define P_OR_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_OR_VV_OR_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_OR_VV_INTTERM \
	d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));

#define P_OR_VV_NOINTTERM \
	saveregs(); \
	d0 = p_or(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_OR_VV_D0EQUALS0L \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL();

#define P_OR_VV_NVAR_END \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_OR_VV_OR_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B"); \
      setregs(); \
      FAIL();

#define P_OR_VV_OR_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B"); \
      setregs(); \
      FAIL();

#define P_OR_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi);

#define P_OR_VC_OR_VC_NVAR \
	Int d1 = (*_PREG)->u.xxn.c;

#define P_OR_VC_INTTERM \
	  d0 = MkIntegerTerm(IntOfTerm(d0) | d1);

#define P_OR_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1));

#define P_OR_VC_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_OR_VC_NVAR_END \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_OR_VC_OR_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A \\/ " Int_FORMAT , (*_PREG)->u.xxn.c); \
      setregs(); \
      FAIL();

#define P_OR_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_OR_Y_VV_OR_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_OR_Y_VV_INTTERM \
	d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));

#define P_OR_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_or(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_OR_Y_VV_D0EQUALS0L \
	  saveregs(); \
	  Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	  setregs(); \
	  FAIL();

#define P_OR_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_OR_Y_VV_OR_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B"); \
      setregs(); \
      FAIL();

#define P_OR_Y_VV_OR_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A\\/B"); \
      setregs(); \
      FAIL();

#define P_OR_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_OR_Y_VC_OR_Y_VC_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_OR_Y_VC_INTTERM \
	  d0 = MkIntegerTerm(IntOfTerm(d0) | d1);

#define P_OR_Y_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_or(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs();

#define P_OR_Y_VC_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_OR_Y_VC_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_OR_Y_VC_OR_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A \\/ " Int_FORMAT , (*_PREG)->u.yxn.c); \
      setregs(); \
      FAIL();

#define P_SLL_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_SLL_VV_SLL_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_SLL_VV_INTTERM_INIT \
	Int i2 = IntOfTerm(d1);

#define P_SLL_VV_INTTERM_LESS \
	  d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));

#define P_SLL_VV_INTTERM_GREATER \
	  d0 = do_sll(IntOfTerm(d0),i2);
	  
#define P_SLL_VV_NOINTTERM \
	saveregs(); \
	d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_SLL_VV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLL_VV_NVAR_END \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_SLL_VV_SLL_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_VV_SLL_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi);

#define P_SLL_VC_SLL_VC_NVAR \
	Int d1 = (*_PREG)->u.xxn.c;

#define P_SLL_VC_INTTERM \
	  d0 = do_sll(IntOfTerm(d0), (Int)d1);

#define P_SLL_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs();

#define P_SLL_VC_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLL_VC_NVAR_END \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_SLL_VC_SLL_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi); \
	  Int d1 = (*_PREG)->u.xxn.c;

#define P_SLL_CV_SLL_CV_NVAR_INT \
    Int i2 = IntOfTerm(d0); \
	if (i2 < 0) { \
	  d0 = MkIntegerTerm(SLR(d1, -i2)); \
	} else { \
	  d0 = do_sll(d1,i2); \
	} \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	  
#define P_SLL_CV_SLL_CV_NVAR_NOINT \
    saveregs(); \
	d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(d0)); \
	setregs(); \
      XREG((*_PREG->u.xxn.x) = d0; \
      (*_PREG = NEXTOP((*_PREG, xxn); \
      GONext();

#define P_SLL_CV_SLL_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_SLL_Y_VV_SLL_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_SLL_Y_VV_INTTERM_INIT \
	Int i2 = IntOfTerm(d1);

#define P_SLL_Y_VV_INTERM_LESS \
	  d0 = MkIntegerTerm(SLR(IntOfTerm(d0), -i2));

#define P_SLL_Y_VV_INTTERM_GREATER \
	  d0 = do_sll(IntOfTerm(d0),i2);

#define P_SLL_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_sll(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_SLL_Y_VV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();
	  
#define P_SLL_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_SLL_Y_VV_SLL_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_Y_VV_SLL_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_SLL_Y_VC_SLL_Y_VC_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_SLL_Y_VC_INTTERM \
	  d0 = do_sll(IntOfTerm(d0), Yap_Eval(d1));

#define P_SLL_Y_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_sll(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs();

#define P_SLL_Y_VC_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLL_Y_VC_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();
	  
#define P_SLL_Y_VC_SLL_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLL_Y_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_SLL_Y_CV_SLL_Y_CV_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;
	
#define P_SLL_Y_CV_INTTERM_INIT \
	  Int i2 = IntOfTerm(d0);

#define P_SLL_Y_CV_INTTERM_LESS \
	    d0 = MkIntegerTerm(SLR(d1, -i2));

#define P_SLL_Y_CV_INTTERM_GREATER \
	    d0 = do_sll(d1,i2);

#define P_SLL_Y_CV_NOINTTERM \
	  saveregs(); \
	  d0 = p_sll(MkIntegerTerm(d1), Yap_Eval(0)); \
	  setregs();

#define P_SLL_Y_CV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLL_Y_CV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_SLL_Y_CV_SLL_Y_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A<<B"); \
      setregs(); \
      FAIL();

#define P_SLR_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_SLR_VV_SLR_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_SLR_VV_INTTERM_INIT \
	Int i2 = IntOfTerm(d1);

#define P_SLR_VV_INTTERM_LESS \
	  d0 = do_sll(IntOfTerm(d0), -i2);

#define P_SLR_VV_INTTERM_GREATER \
	  d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));

#define P_SLR_VV_NOINTTERM \
	saveregs(); \
	d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_SLR_VV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLR_VV_NVAR_END \
      XREG((*_PREG)->u.xxx.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxx); \
      GONext();

#define P_SLR_VV_SRL_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define P_SLR_VV_SRL_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define P_SLR_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi); \
	  Int d1 = (*_PREG)->u.xxn.c;

#define P_SLR_VC_SLR_VC_NVAR_INT \
	d0 = MkIntTerm(SLR(IntOfTerm(d0), d1)); \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();
	  
#define P_SLR_VC_SLR_VC_NVAR_NOINT \
	saveregs(); \
	d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1)); \
    setregs(); \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_SLR_VC_SRL_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define P_SLR_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.xxn.xi);

#define P_SLR_CV_SLR_CV_NVAR \
	Int d1 = (*_PREG)->u.xxn.c;

#define P_SLR_CV_INTTERM_INIT \
	 Int i2 = IntOfTerm(d0);

#define P_SLR_CV_INTTERM_LESS \
	   d0 = do_sll(d1, -i2);

#define P_SLR_CV_INTTERM_GREATER \
	   d0 = MkIntegerTerm(SLR(d1, i2));

#define P_SLR_CV_NOINTTERM \
	  saveregs(); \
	  d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0)); \
	  setregs();

#define P_SLR_CV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLR_CV_NVAR_END \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP((*_PREG), xxn); \
      GONext();

#define P_SLR_CV_SLR_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define P_SLR_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0, d1; \
	  register CELL8 pt0; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_SLR_Y_VV_SLR_Y_VV_NVAR \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_SLR_Y_VV_INTTERM_INIT \
	 Int i2 = IntOfTerm(d1);

#define P_SLR_Y_VV_INTTERM_LESS \
	   d0 = do_sll(IntOfTerm(d0), -i2);

#define P_SLR_Y_VV_INTTERM_GREATER \
	   d0 = MkIntTerm(SLR(IntOfTerm(d0), i2));

#define P_SLR_Y_VV_NOINTTERM \
	saveregs(); \
	d0 = p_slr(Yap_Eval(d0), Yap_Eval(d1)); \
	setregs();

#define P_SLR_Y_VV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLR_Y_VV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxx.y; \
      (*_PREG) = NEXTOP((*_PREG), yxx); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_SLR_Y_VV_SLR_Y_VV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define P_SLR_Y_VV_SLR_Y_VV_NVAR_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define P_SLR_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL8 pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_SLR_Y_VC_SLR_Y_VC_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_SLR_Y_VC_INTTERM \
	  d0 = MkIntTerm(SLR(IntOfTerm(d0), d1));

#define P_SLR_Y_VC_NOINTTERM \
	  saveregs(); \
	  d0 = p_slr(Yap_Eval(d0), MkIntegerTerm(d1)); \
	  setregs();

#define P_SLR_Y_VC_D0EQUALS0L \
	    saveregs(); \
	    Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	    setregs(); \
	    FAIL();

#define P_SLR_Y_VC_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_SLR_Y_VC_SLR_Y_VC_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs();

#define P_SLR_Y_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
	  register CELL d0; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_SLR_Y_CV_SLR_Y_CV_NVAR \
	Int d1 = (*_PREG)->u.yxn.c;

#define P_SLR_Y_CV_INTTERM_INIT \
	 Int i2 = IntOfTerm(d0);

#define P_SLR_Y_CV_INTTERM_LESS \
	   d0 = do_sll(d1, -i2);

#define P_SLR_Y_CV_INTTERM_GREATER \
	   d0 = MkIntegerTerm(SLR(d1, i2));

#define P_SLR_Y_CV_NOINTTERM \
	  saveregs(); \
	  d0 = p_slr(MkIntegerTerm(d1), Yap_Eval(d0)); \
	  setregs();

#define P_SLR_Y_CV_D0EQUALS0L \
	saveregs(); \
	Yap_Error(LOCAL_Error_TYPE, LOCAL_Error_Term, LOCAL_ErrorMessage); \
	setregs(); \
	FAIL();

#define P_SLR_Y_CV_NVAR_END \
      pt0 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP((*_PREG), yxn); \
      INITIALIZE_PERMVAR(pt0,d0); \
      GONext();

#define P_SLR_Y_CV_SLR_Y_CV_UNK \
      saveregs(); \
      Yap_NilError(INSTANTIATION_ERROR, "X is A>>B"); \
      setregs(); \
      FAIL();

#define CALL_BFUNC_XX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL* pt0; \
      d0 = XREG((*_PREG)->u.plxxs.x1);

#define CALL_BFUNC_XX_CALL_BFUNC_XX_NVAR \
      d1 = XREG((*_PREG)->u.plxxs.x2);

#define CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT \
      BLOCK = (CELL)CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_INT; \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	    COUNT flags; \
	    Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	    flags = (*_PREG)->u.plxxs.flags; \
	    if (v > 0) { \
	      if (flags & GT_OK_IN_CMP) { \
	        yamop *nextp = NEXTOP((*_PREG), plxxs); \
	        (*_PREG) = nextp; \
	        ALWAYS_GONext(); \
          } \
	      else { \
	        yamop *nextp = (*_PREG)->u.plxxs.f; \
	        (*_PREG) = nextp; \
	        ALWAYS_GONext(); \
          } \
        } \
	    else if (v < 0) { \
	      if (flags & LT_OK_IN_CMP) { \
	        yamop *nextp = NEXTOP((*_PREG), plxxs); \
	        (*_PREG) = nextp; \
	        ALWAYS_GONext(); \
          } \
	      else { \
	        yamop *nextp = (*_PREG)->u.plxxs.f; \
	        (*_PREG) = nextp; \
	        ALWAYS_GONext(); \
          } \
	    } \
	    else { \
	      if (flags & EQ_OK_IN_CMP) { \
	        yamop *nextp = NEXTOP((*_PREG), plxxs); \
	        (*_PREG) = nextp; \
	        ALWAYS_GONext(); \
	      } \
	      else { \
	        yamop *nextp = (*_PREG)->u.plxxs.f; \
	        (*_PREG) = nextp; \
	        ALWAYS_GONext(); \
	      } \
	    } \
      }

#define CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT \
      BLOCK = (CELL)CALL_BFUNC_XX_CALL_BFUNC_XX2_NVAR_NOINT; \
	    CmpPredicate f = (*_PREG)->u.plxxs.p->cs.d_code; \
	    saveregs(); \
	    d0 = (CELL) (f) (d0,d1); \
	    setregs(); \
        if ((*_PREG) == FAILCODE) { \
	      JMPNext(); \
        } \
        else if (!d0) { \
	      (*_PREG) = (*_PREG)->u.plxxs.f; \
	      JMPNext(); \
        } \
        else { \
          (*_PREG) = NEXTOP((*_PREG), plxxs); \
          JMPNext(); \
        }

#define CALL_BFUNC_XX_CALL_BFUNC_XX_UNK \
      d1 = Deref(d1);

#define CALL_BFUNC_YX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.plxys.y; \
      d1 = XREG((*_PREG)->u.plxys.x); \
      d0 = *pt0;

#define CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT \
      BLOCK = (CELL)CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_INT; \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	    int flags; \
	    Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	    flags = (*_PREG)->u.plxys.flags; \
	    if (v > 0) { \
	      if (flags & GT_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plxys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	        JMPNext(); \
	      } \
	    } else if (v < 0) { \
	      if (flags & LT_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plxys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	        JMPNext(); \
	      } \
	    } else { \
	      if (flags & EQ_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plxys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	        JMPNext(); \
	      } \
	    } \
      }

#define CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT \
      BLOCK = (CELL)CALL_BFUNC_YX_CALL_BFUNC_YX2_NVAR_NOINT; \
	    CmpPredicate f = (*_PREG)->u.plxys.p->cs.d_code; \
	    saveregs(); \
	    d0 = (CELL) (f) (d0,d1); \
	    setregs(); \
        if (!d0 || (*_PREG) == FAILCODE) { \
	      if ((*_PREG) != FAILCODE) { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	      } \
	      JMPNext(); \
        } \
        else { \
          (*_PREG) = NEXTOP((*_PREG), plxys); \
          JMPNext(); \
        }

#define CALL_BFUNC_YX_CALL_BFUNC_YX_UNK \
      d1 = Deref(d1);

#define CALL_BFUNC_XY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      pt0 = YREG + (*_PREG)->u.plxys.y; \
      d0 = XREG((*_PREG)->u.plxys.x); \
      d1 = *pt0;

#define CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT \
      BLOCK = (CELL)CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_INT; \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	    int flags; \
	    Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	    flags = (*_PREG)->u.plxys.flags; \
	    if (v > 0) { \
	      if (flags & GT_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plxys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	        JMPNext(); \
	      } \
	    } else if (v < 0) { \
	      if (flags & LT_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plxys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	        JMPNext(); \
	      } \
	    } else { \
	      if (flags & EQ_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plxys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	        JMPNext(); \
	      } \
	    } \
      }

#define CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT \
      BLOCK = (CELL)CALL_BFUNC_XY_CALL_BFUNC_XY2_NVAR_NOINT; \
	    CmpPredicate f = (*_PREG)->u.plxys.p->cs.d_code; \
	    saveregs(); \
	    d0 = (CELL) (f) (d0,d1); \
	    setregs(); \
        if (!d0 || (*_PREG) == FAILCODE) { \
	      if ((*_PREG) != FAILCODE) { \
	        (*_PREG) = (*_PREG)->u.plxys.f; \
	      } \
	      JMPNext(); \
        } \
        else { \
          (*_PREG) = NEXTOP((*_PREG), plxys); \
          JMPNext(); \
        }

#define CALL_BFUNC_XY_CALL_BFUNC_XY_UNK \
      d1 = Deref(d1);

#define CALL_BFUNC_YY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      pt0 = YREG + (*_PREG)->u.plyys.y1; \
      pt1 = YREG + (*_PREG)->u.plyys.y2; \
      d0 = *pt0; \
      d1 = *pt1;

#define CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT \
      BLOCK = (CELL)CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_INT; \
      if (IsIntTerm(d0) && IsIntTerm(d1)) { \
	    int flags; \
	    Int v = IntOfTerm(d0) - IntOfTerm(d1); \
	    flags = (*_PREG)->u.plyys.flags; \
	    if (v > 0) { \
	      if (flags & GT_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plyys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plyys.f; \
	        JMPNext(); \
	      } \
	    } else if (v < 0) { \
	      if (flags & LT_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plyys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plyys.f; \
	        JMPNext(); \
	      } \
	    } else { \
	      if (flags & EQ_OK_IN_CMP) { \
	        (*_PREG) = NEXTOP((*_PREG), plyys); \
	        JMPNext(); \
	      } else { \
	        (*_PREG) = (*_PREG)->u.plyys.f; \
	        JMPNext(); \
	      } \
	    } \
      }

#define CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT \
      BLOCK = (CELL)CALL_BFUNC_YY_CALL_BFUNC_YY2_NVAR_NOINT; \
	    CmpPredicate f = (*_PREG)->u.plyys.p->cs.d_code; \
	    saveregs(); \
	    d0 = (CELL) (f) (d0,d1); \
	    setregs(); \
        if (!d0 || (*_PREG) == FAILCODE) { \
	      if ((*_PREG) != FAILCODE) { \
	        (*_PREG) = (*_PREG)->u.plyys.f; \
	      } \
	      JMPNext(); \
        } \
        else { \
          (*_PREG) = NEXTOP((*_PREG), plyys); \
          JMPNext(); \
        }

#define CALL_BFUNC_YY_NOINTTERM_NOFAILCODE \
	  (*_PREG) = (*_PREG)->u.plyys.f;

#define CALL_BFUNC_YY_NOINTTERM_NOD0 \
	JMPNext();

#define CALL_BFUNC_YY_NOINTTERM_END \
      (*_PREG) = NEXTOP((*_PREG), plyys); \
      JMPNext();

#define CALL_BFUNC_YY_CALL_BFUNC_YY_UNK \
      d1 = Deref(d1);

#define P_EQUAL_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      save_hb();

#define P_EQUAL_END \
      (*_PREG) = NEXTOP((*_PREG), e); \
      GONext();

#include "p_dif.i"

#include "p_eq.i"

#define P_ARG_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \

#ifdef LOW_LEVEL_TRACER
#define P_ARG_VV_LOW_LEVEL_TRACER \
	H[0] = XREG((*_PREG)->u.xxx.x1); \
	H[1] = XREG((*_PREG)->u.xxx.x2); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H);
#endif

#define P_ARG_VV_TEST_D0 \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = XREG((*_PREG)->u.xxx.x1);
      
#define P_ARG_VV_ARG_ARG1_NVAR \
      FAILED = 0; \
      if (IsIntTerm(d0)) { \
	d0 = IntOfTerm(d0); \
      } else if (IsLongIntTerm(d0)) { \
	d0 = LongIntOfTerm(d0); \
      } else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3"); \
	setregs(); \
	YAAM_FAIL; \
      }

#define P_ARG_VV_TEST_D1 \
    if (!FAILED) { \
      d1 = XREG((*_PREG)->u.xxx.x2);
      
#define P_ARG_VV_ARG_ARG2_NVAR \
      BLOCK = (CELL)P_ARG_VV_ARG_ARG2_NVAR; \
      if (IsApplTerm(d1)) { \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  YAAM_FAIL; \
	} \
	else if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  YAAM_FAIL; \
	} \
	else { \
	  XREG((*_PREG)->u.xxx.x) = pt0[d0]; \
	  (*_PREG) = NEXTOP((*_PREG), xxx); \
	  GONext(); \
  } \
      } \
      else if (IsPairTerm(d1)) { \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  YAAM_FAIL; \
	} \
	else { \
	  XREG((*_PREG)->u.xxx.x) = pt0[d0-1]; \
	  (*_PREG) = NEXTOP((*_PREG), xxx); \
	  GONext(); \
	} \
      } \
      else { \
	YAAM_FAIL; \
      } \
  }

#define P_ARG_VV_ARG_ARG2_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      FAIL();

#define P_ARG_VV_ARG_ARG1_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3"); \
      setregs(); \
      FAIL();

#define P_ARG_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \

#ifdef LOW_LEVEL_TRACER
#define P_ARG_CV_LOW_LEVEL_TRACER \
	CELL *Ho = HR; \
	Term t = MkIntegerTerm((*_PREG)->u.xxn.c); \
	H[0] =  t; \
	H[1] = XREG((*_PREG)->u.xxn.xi); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H); \
	H = HRo;
#endif

#define P_ARG_CV_TEST_D1 \
	  register CELL d0, d1; \
	  register CELL *pt0; \
      d0 = (*_PREG)->u.xxn.c; \
      d1 = XREG((*_PREG)->u.xxn.xi);

#define P_ARG_CV_ARG_ARG2_VC_NVAR \
      BLOCK = (CELL)P_ARG_CV_ARG_ARG2_VC_NVAR; \
      FAILED = 0; \
      if (IsApplTerm(d1)) { \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  YAAM_FAIL; \
	} \
	else if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  YAAM_FAIL; \
	} \
	else { \
	  XREG((*_PREG)->u.xxn.x) = pt0[d0]; \
	  (*_PREG) = NEXTOP((*_PREG), xxn); \
	  GONext(); \
	} \
      } \
      else if (IsPairTerm(d1)) { \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  YAAM_FAIL; \
	} \
	else { \
	  XREG((*_PREG)->u.xxn.x) = pt0[d0-1]; \
	  (*_PREG) = NEXTOP((*_PREG), xxn); \
	  GONext(); \
	} \
      } \
      else { \
	YAAM_FAIL; \
      }

#define P_ARG_CV_ARG_ARG2_VC_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      FAIL();

#define P_ARG_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \

#ifdef LOW_LEVEL_TRACER
#define P_ARG_Y_VV_LOW_LEVEL_TRACER \
	H[0] = XREG((*_PREG)->u.yxx.x1); \
	H[1] = XREG((*_PREG)->u.yxx.x2); \
	H[2] = YREG[(*_PREG)->u.yxx.y]; \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H);
#endif

#define P_ARG_Y_VV_TEST_D0 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_ARG_Y_VV_ARG_Y_ARG1_NVAR \
    FAILED = 0; \
      if (IsIntTerm(d0)) { \
	d0 = IntOfTerm(d0); \
      } else if (IsLongIntTerm(d0)) { \
	d0 = LongIntOfTerm(d0); \
      } else { \
	saveregs(); \
	Yap_Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3"); \
	setregs(); \
	YAAM_FAIL; \
      }

#define P_ARG_Y_VV_TEST_D1 \
    if (!FAILED) { \
      d1 = XREG((*_PREG)->u.yxx.x2);

#define P_ARG_Y_VV_ARG_Y_ARG2_NVAR \
      BLOCK = (CELL)P_ARG_Y_VV_ARG_Y_ARG2_NVAR; \
      if (IsApplTerm(d1)) { \
	pt0 = RepAppl(d1); \
	d1 = *pt0; \
	if (IsExtensionFunctor((Functor) d1)) { \
	  YAAM_FAIL; \
	} \
	else if ((Int)d0 <= 0 || \
	    (Int)d0 > ArityOfFunctor((Functor) d1)) { \
	  YAAM_FAIL; \
	} \
	else { \
	  pt1 = YREG + (*_PREG)->u.yxx.y; \
	  (*_PREG) = NEXTOP((*_PREG), yxx); \
	  INITIALIZE_PERMVAR(pt1,pt0[d0]); \
	  GONext(); \
    } \
      } \
      else if (IsPairTerm(d1)) { \
	pt0 = RepPair(d1); \
	if (d0 != 1 && d0 != 2) { \
	  if ((Int)d0 < 0) { \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs(); \
	  } \
	  YAAM_FAIL; \
	} \
	else { \
	  pt1 = YREG + (*_PREG)->u.yxx.y; \
	  (*_PREG) = NEXTOP((*_PREG), yxx); \
	  INITIALIZE_PERMVAR(pt1,pt0[d0-1]); \
	  GONext(); \
    } \
      } \
      else { \
	YAAM_FAIL; \
      } \
  }

#define P_ARG_Y_VV_ARG_Y_ARG2_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      FAIL();

#define P_ARG_Y_VV_ARG_Y_ARG1_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3"); \
      setregs(); \
      FAIL();

#define P_ARG_Y_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_ARG_Y_CV_LOW_LEVEL_TRACER \
	CELL *Ho = HR; \
	Term t = MkIntegerTerm((*_PREG)->u.yxn.c); \
	H[0] =  t; \
	H[1] = XREG((*_PREG)->u.yxn.xi); \
	H[2] = YREG[(*_PREG)->u.yxn.y]; \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorArg,0)),H); \
	H = HRo;
#endif

#define P_ARG_Y_CV_TEST_D1 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = (*_PREG)->u.yxn.c; \
      d1 = XREG((*_PREG)->u.yxn.xi);

#define P_ARG_Y_CV_D1APPL_INIT \
	pt0 = RepAppl(d1); \
	d1 = *pt0;

#define P_ARG_Y_CV_D1APPL_END \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP((*_PREG), yxn); \
	INITIALIZE_PERMVAR(pt1,pt0[d0]); \
	GONext();

#define P_ARG_Y_CV_D1PAIR_INIT \
	pt0 = RepPair(d1);

#define P_ARG_Y_CV_D1PAIR_LESS0 \
	    saveregs(); \
	    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, \
		  MkIntegerTerm(d0),"arg 1 of arg/3"); \
	    setregs();

#define P_ARG_Y_CV_D1PAIR_END \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP((*_PREG), yxn); \
	INITIALIZE_PERMVAR(pt1,pt0[d0-1]); \
	GONext();

#define P_ARG_Y_CV_ARG_Y_ARG2_VC_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_VV_INSTINIT
	  
#ifdef LOW_LEVEL_TRACER
#define P_FUNC2S_VV_LOW_LEVEL_TRACER \
	RESET_VARIABLE(HR); \
	H[1] = XREG((*_PREG)->u.xxx.x1); \
	H[2] = XREG((*_PREG)->u.xxx.x2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2S_TEST_D0 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xxx.x1);

#define P_FUNC2S_VV_TEST_D1 \
      d1 = XREG((*_PREG)->u.xxx.x2);

#define P_FUNC2S_VV_D1INT \
	d1 = IntegerOfTerm(d1);
	
#define P_FUNC2S_VV_D1NOTINT \
	saveregs();

#define P_FUNC2S_VV_D1BIGINT \
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");

#define P_FUNC2S_VV_D1NOTBIGINT \
	  Yap_Error(TYPE_ERROR_INTEGER, d1, "functor/3");

#define P_FUNC2S_VV_D1NOTINT_END \
	setregs(); \
	FAIL();

#define P_FUNC2S_VV_D0NOTATOMIC \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_VV_FIRSTIFOK \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
	XREG((*_PREG)->u.xxx.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxx),Osbpp),l); \
	GONext();

#define P_FUNC2S_VV_SECONDIFOK_D0NOTATOM \
	  saveregs(); \
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	  setregs(); \
	  FAIL();

#define P_FUNC2S_VV_SECONDIFOK_D0ATOM \
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);

#define P_FUNC2S_VV_SECONDIFOK_POST_D0ATOM \
	pt1 = HR; \
	*pt1++ = d0; \
	d0 = AbsAppl(HR);

#define P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_INIT \
	  saveregs();

#define P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_IFOK \
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	    setregs(); \
	    JMPNext();

#define P_FUNC2S_VV_SECONDIFOK_FIRSTIFOK_NOIF \
	    setregs();

#define P_FUNC2S_VV_SECONDIFOK_INSIDEWHILE \
	  RESET_VARIABLE(pt1); \
	  pt1++;

#define P_FUNC2S_VV_SECONDIFOK_END \
	H = pt1; \
	XREG((*_PREG)->u.xxx.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxx),Osbpp),l); \
	GONext();

#define P_FUNC2S_VV_THIRDIFOK \
	XREG((*_PREG)->u.xxx.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxx),Osbpp),l); \
	GONext();

#define P_FUNC2S_VV_ELSE \
	saveregs(); \
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_VV_FUNC2S_UNK2 \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_VV_FUNC2S_UNK \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2S_CV_LOW_LEVEL_TRACER \
	RESET_VARIABLE(HR); \
	H[1] = (*_PREG)->u.xxc.c; \
	H[2] = XREG((*_PREG)->u.xxc.xi); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2S_CV_TEST_D1 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = (*_PREG)->u.xxc.c; \
      d1 = XREG((*_PREG)->u.xxc.xi);

#define P_FUNC2S_CV_D1INT \
	d1 = IntegerOfTerm(d1);
	
#define P_FUNC2S_CV_D1NOTINT \
	saveregs();

#define P_FUNC2S_CV_D1NOINT_D1BIGINT \
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");

#define P_FUNC2S_CV_D1NOTBIGINT \
	  Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");

#define P_FUNC2S_CV_POST_IF \
	setregs(); \
	FAIL();

#define P_FUNC2S_CV_FIRSTIFOK \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
	XREG((*_PREG)->u.xxc.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxc),Osbpp),l); \
	GONext();

#define P_FUNC2S_CV_D1GREATER_D0NOTATOM \
	  saveregs(); \
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	  setregs(); \
	  FAIL();

#define P_FUNC2S_CV_D1GREATER_D0ATOM \
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);

#define P_FUNC2S_CV_D1GREATER_POST_IF \
	pt1 = HR; \
	*pt1++ = d0; \
	d0 = AbsAppl(HR);

#define P_FUNC2S_CV_D1GREATER_IFOK_INIT \
	  saveregs();

#define P_FUNC2S_CV_D1GREATER_IFOK_IFOK \
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	    setregs(); \
	    JMPNext();

#define P_FUNC2S_CV_D1GREATER_IFOK_NOIF \
	    setregs();

#define P_FUNC2S_CV_D1GREATER_INSIDEWHILE \
	  RESET_VARIABLE(pt1); \
	  pt1++;

#define P_FUNC2S_CV_D1GREATER_END \
	H = pt1; \
	XREG((*_PREG)->u.xxc.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxc),Osbpp),l); \
	GONext();

#define P_FUNC2S_CV_D1ISZERO \
	XREG((*_PREG)->u.xxc.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxc),Osbpp),l); \
	GONext();

#define P_FUNC2S_CV_ELSE \
	saveregs(); \
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_CV_END \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2S_VC_LOW_LEVEL_TRACER \
	Term ti; \
	CELL *hi = HR; \
	ti = MkIntegerTerm((*_PREG)->u.xxn.c); \
	RESET_VARIABLE(HR); \
	H[1] = XREG((*_PREG)->u.xxn.xi); \
	H[2] = ti; \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H); \
	H = hi;
#endif

#define P_FUNC2S_VC_TEST_D0 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.xxn.xi);

#define P_FUNC2S_VC_FUNC2S_NVAR_VC \
      d1 = (*_PREG)->u.xxn.c;

#define P_FUNC2S_VC_D0NOATOMIC \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_VC_EQUALS \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
	XREG((*_PREG)->u.xxn.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxn),Osbpp),l); \
	GONext();

#define P_FUNC2S_VC_D1ISZERO \
	XREG((*_PREG)->u.xxn.x) = d0; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxn),Osbpp),l); \
	GONext();

#define P_FUNC2S_VC_D0NOATOM \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_VC_D0ATOM \
	d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);

#define P_FUNC2S_VC_POST_ELSE \
      pt1 = HR; \
      *pt1++ = d0; \
      d0 = AbsAppl(HR);

#define P_FUNC2S_VC_IFOK_INIT \
	saveregs();

#define P_FUNC2S_VC_IFOK_IFOK \
	  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	  setregs(); \
	  JMPNext();

#define P_FUNC2S_VC_IFOK_NOIF \
	  setregs();

#define P_FUNC2S_VC_INSIDEWHILE \
	RESET_VARIABLE(pt1); \
	pt1++;

#define P_FUNC2S_VC_END1 \
      HR = pt1; \
      XREG((*_PREG)->u.xxn.x) = d0; \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xxn),Osbpp),l); \
      GONext();

#define P_FUNC2S_VC_END2 \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_Y_VV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2S_Y_VV_LOW_LEVEL_TRACER \
	RESET_VARIABLE(HR); \
	H[1] = XREG((*_PREG)->u.yxx.x1); \
	H[2] = XREG((*_PREG)->u.yxx.x2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2S_Y_VV_TEST_D0 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.yxx.x1);

#define P_FUNC2S_Y_VV_TEST_D1 \
      d1 = XREG((*_PREG)->u.yxx.x2);

#define P_FUNC2S_Y_VV_D1INT \
	d1 = IntegerOfTerm(d1);

#define P_FUNC2S_Y_VV_D1NOTINT \
	saveregs();

#define P_FUNC2S_Y_VV_D1BIGINT \
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");

#define P_FUNC2S_Y_VV_D1NOTBIGINT \
	  Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");

#define P_FUNC2S_Y_VV_POST_IF \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_VV_D0NOATOMIC \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_VV_EQUALS \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxx),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_VV_D1GREATER_D0NOATOM \
	  saveregs(); \
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	  setregs(); \
	  FAIL();

#define P_FUNC2S_Y_VV_D1GREATER_D0ATOM \
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);

#define P_FUNC2S_Y_VV_D1GREATER_POST_ELSE \
	pt1 = HR; \
	*pt1++ = d0; \
	d0 = AbsAppl(HR);

#define P_FUNC2S_Y_VV_D1GREATER_IFOK_INIT \
	  saveregs();

#define P_FUNC2S_Y_VV_D1GREATER_IFOK_IFOK \
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	    setregs(); \
	    JMPNext();

#define P_FUNC2S_Y_VV_D1GREATER_IFOK_NOIF \
	    setregs();

#define P_FUNC2S_Y_VV_D1GREATER_INSIDEWHILE \
	  RESET_VARIABLE(pt1); \
	  pt1++;

#define P_FUNC2S_Y_VV_D1GREATER_END \
	H = pt1; \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxx),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_VV_D1ISZERO \
	pt1 = YREG + (*_PREG)->u.yxx.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxx),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_VV_ELSE \
	saveregs(); \
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_VV_END1 \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_Y_VV_END2 \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_Y_CV_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2S_Y_CV_LOW_LEVEL_TRACER \
	RESET_VARIABLE(HR); \
	H[1] = (*_PREG)->u.yxn.c; \
	H[2] = XREG((*_PREG)->u.yxn.xi); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2S_Y_CV_TEST_D1 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = (*_PREG)->u.yxn.c; \
      d1 = XREG((*_PREG)->u.yxn.xi);

#define P_FUNC2S_Y_CV_D1INT \
	d1 = IntegerOfTerm(d1);

#define P_FUNC2S_Y_CV_D1NOTINT \
	saveregs();

#define P_FUNC2S_Y_CV_D1BIGINT \
	  Yap_Error(RESOURCE_ERROR_STACK, d1, "functor/3");

#define P_FUNC2S_Y_CV_D1NOTBIGINT \
	  Yap_Error(TYPE_ERROR_INTEGER,d1,"functor/3");

#define P_FUNC2S_Y_CV_POST_IF \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_CV_EQUALS \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxn),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_CV_D1GREATER_D0NOATOM \
	  saveregs(); \
	  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	  setregs(); \
	  FAIL();

#define P_FUNC2S_Y_CV_D1GREATER_D0ATOM \
	  d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);

#define P_FUNC2S_Y_CV_D1GREATER_POST_ELSE \
	pt1 = HR; \
	*pt1++ = d0; \
	d0 = AbsAppl(HR);

#define P_FUNC2S_Y_CV_D1GREATER_IFOK_INIT \
	  saveregs();

#define P_FUNC2S_Y_CV_D1GREATER_IFOK_IFOK \
	    Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	    setregs(); \
	    JMPNext();

#define P_FUNC2S_Y_CV_D1GREATER_IFOK_NOIF \
	    setregs();

#define P_FUNC2S_Y_CV_D1GREATER_INSIDEWHILE \
	  RESET_VARIABLE(pt1); \
	  pt1++;

#define P_FUNC2S_Y_CV_D1GREATER_END \
	H = pt1; \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxn),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_CV_D1ISZERO \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxn),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_CV_ELSE \
	saveregs(); \
	Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_CV_END \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2S_Y_VC_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2S_Y_VC_LOW_LEVEL_TRACER \
	Term ti; \
	CELL *hi = HR; \
	ti = MkIntegerTerm((Int)((*_PREG)->u.yxn.c)); \
	RESET_VARIABLE(HR); \
	H[1] = XREG((*_PREG)->u.yxn.xi); \
	H[2] = ti; \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H); \
	H = hi;
#endif

#define P_FUNC2S_Y_VC_TEST_D0 \
	  register CELL d0, d1; \
	  register CELL *pt0, *pt1; \
      d0 = XREG((*_PREG)->u.yxn.xi);

#define P_FUNC2S_Y_VC_FUNC2S_Y_NVAR_VC \
      d1 = (*_PREG)->u.yxn.c;
	  
#define P_FUNC2S_Y_VC_D0NOATOMIC \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_VC_EQUALS \
	RESET_VARIABLE(HR); \
	RESET_VARIABLE(HR+1); \
	d0 = AbsPair(HR); \
	H += 2; \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxn),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_VC_D1ISZERO \
	pt1 = YREG + (*_PREG)->u.yxn.y; \
	(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxn),Osbpp),l); \
	INITIALIZE_PERMVAR(pt1,d0); \
	GONext();

#define P_FUNC2S_Y_VC_D0NOATOM1 \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_VC_D0NOATOM2 \
	saveregs(); \
	Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	setregs(); \
	FAIL();

#define P_FUNC2S_Y_VC_D0ATOM \
	d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1);

#define P_FUNC2S_Y_VC_POST_ELSE \
      pt1 = HR; \
      *pt1++ = d0; \
      d0 = AbsAppl(HR);

#define P_FUNC2S_Y_VC_IFOK_INIT \
	saveregs();

#define P_FUNC2S_Y_VC_IFOK_IFOK \
	  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	  setregs(); \
	  JMPNext();

#define P_FUNC2S_Y_VC_IFOK_NOIF \
	  setregs();

#define P_FUNC2S_Y_VC_INSIDEWHILE \
	RESET_VARIABLE(pt1); \
	pt1++;

#define P_FUNC2S_Y_VC_END1 \
      HR = pt1; \
      pt1 = YREG + (*_PREG)->u.yxn.y; \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yxn),Osbpp),l); \
      INITIALIZE_PERMVAR(pt1,d0); \
      GONext();

#define P_FUNC2S_Y_VC_END2 \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2F_XX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2F_XX_LOW_LEVEL_TRACER \
	H[0] = XREG((*_PREG)->u.xxx.x); \
	RESET_VARIABLE(HR+1); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2F_XX_TEST_D0 \
	  register CELL d0; \
	  register CELL* pt1; \
      d0 = XREG((*_PREG)->u.xxx.x);

#define P_FUNC2F_XX_D0APPL \
	Functor d1 = FunctorOfTerm(d0);

#define P_FUNC2F_XX_D0APPL_D1EXTFUNC \
	  XREG((*_PREG)->u.xxx.x1) = d0; \
	  XREG((*_PREG)->u.xxx.x2) = MkIntTerm(0); \
	  (*_PREG) = NEXTOP((*_PREG), xxx); \
	  GONext();

#define P_FUNC2F_XX_D0APPL_END \
	XREG((*_PREG)->u.xxx.x1) = MkAtomTerm(NameOfFunctor(d1)); \
	XREG((*_PREG)->u.xxx.x2) = MkIntegerTerm(ArityOfFunctor(d1)); \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONext();


#define P_FUNC2F_XX_D0PAIR \
	XREG((*_PREG)->u.xxx.x1) = TermDot; \
	XREG((*_PREG)->u.xxx.x2) = MkIntTerm(2); \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONext();

#define P_FUNC2F_XX_D0NOCOMPOUND \
	XREG((*_PREG)->u.xxx.x1) = d0; \
	XREG((*_PREG)->u.xxx.x2) = MkIntTerm(0); \
	(*_PREG) = NEXTOP((*_PREG), xxx); \
	GONext();

#define P_FUNC2F_XX_END \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2F_XY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2F_XY_LOW_LEVEL_TRACER \
	H[0] = XREG((*_PREG)->u.xxy.x); \
	RESET_VARIABLE(HR+1); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2F_XY_TEST_D0 \
	  register CELL d0; \
	  register CELL *pt1; \
      d0 = XREG((*_PREG)->u.xxy.x);

#define P_FUNC2F_XY_D0APPL \
	Functor d1 = FunctorOfTerm(d0); \
	CELL *pt0 = YREG+(*_PREG)->u.xxy.y2;

#define P_FUNC2F_XY_D0APPL_D1EXTFUNC \
	  XREG((*_PREG)->u.xxy.x1) = d0; \
	  (*_PREG) = NEXTOP((*_PREG), xxy); \
	  INITIALIZE_PERMVAR(pt0, MkIntTerm(0)); \
	  GONext();

#define P_FUNC2F_XY_D0APPL_END \
	XREG((*_PREG)->u.xxy.x1) = MkAtomTerm(NameOfFunctor(d1)); \
	(*_PREG) = NEXTOP((*_PREG), xxy); \
	INITIALIZE_PERMVAR(pt0, MkIntegerTerm(ArityOfFunctor(d1))); \
	GONext();

#define P_FUNC2F_XY_D0PAIR \
	CELL *pt0 = YREG+(*_PREG)->u.xxy.y2; \
	XREG((*_PREG)->u.xxy.x1) = TermDot; \
	(*_PREG) = NEXTOP((*_PREG), xxy); \
	INITIALIZE_PERMVAR(pt0, MkIntTerm(2)); \
	GONext();

#define P_FUNC2F_XY_D0NOCOMPOUND \
	CELL *pt0 = YREG+(*_PREG)->u.xxy.y2; \
	XREG((*_PREG)->u.xxy.x1) = d0; \
	(*_PREG) = NEXTOP((*_PREG), xxy); \
	INITIALIZE_PERMVAR(pt0, MkIntTerm(0)); \
	GONext();

#define P_FUNC2F_XY_END \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2F_YX_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2F_YX_LOW_LEVEL_TRACER \
	H[0] = XREG((*_PREG)->u.yxx.x2); \
	RESET_VARIABLE(HR+1); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2F_YX_TEST_D0 \
	  register CELL d0; \
	  register CELL *pt1; \
      d0 = XREG((*_PREG)->u.yxx.x2);

#define P_FUNC2F_YX_D0APPL \
	Functor d1 = FunctorOfTerm(d0); \
	CELL *pt0 = YREG+(*_PREG)->u.yxx.y;

#define P_FUNC2F_YX_D0APPL_D1EXTFUNC \
	  XREG((*_PREG)->u.yxx.x1) = MkIntTerm(0); \
	  (*_PREG) = NEXTOP((*_PREG), yxx); \
	  INITIALIZE_PERMVAR(pt0, d0); \
	  GONext();

#define P_FUNC2F_YX_D0APPL_END \
	XREG((*_PREG)->u.yxx.x1) = MkIntegerTerm(ArityOfFunctor(d1)); \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt0,  MkAtomTerm(NameOfFunctor(d1))); \
	GONext();

#define P_FUNC2F_YX_D0PAIR \
	CELL *pt0 = YREG+(*_PREG)->u.yxx.y; \
	XREG((*_PREG)->u.yxx.x1) = MkIntTerm(2); \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt0 ,TermDot); \
	GONext();
	  
#define P_FUNC2F_YX_D0NOCOMPOUND \
	CELL *pt0 = YREG+(*_PREG)->u.yxx.y; \
	XREG((*_PREG)->u.yxx.x1) = MkIntTerm(0); \
	(*_PREG) = NEXTOP((*_PREG), yxx); \
	INITIALIZE_PERMVAR(pt0, d0); \
	GONext();

#define P_FUNC2F_YX_END \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#define P_FUNC2F_YY_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef LOW_LEVEL_TRACER
#define P_FUNC2F_YY_LOW_LEVEL_TRACER \
	H[0] = XREG((*_PREG)->u.yyx.x); \
	RESET_VARIABLE(HR+1); \
	RESET_VARIABLE(HR+2); \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),H);
#endif

#define P_FUNC2F_YY_TEST_D0 \
	  register CELL d0; \
      d0 = XREG((*_PREG)->u.yyx.x);

#define P_FUNC2F_YY_D0APPL \
	Functor d1 = FunctorOfTerm(d0); \
	CELL *pt0 = YREG+(*_PREG)->u.yyx.y1; \
	CELL *pt1 = YREG+(*_PREG)->u.yyx.y2;

#define P_FUNC2F_YY_D0APPL_D1EXTFUNC \
	  (*_PREG) = NEXTOP((*_PREG), yyx); \
	  INITIALIZE_PERMVAR(pt0,  d0); \
	  INITIALIZE_PERMVAR(pt1, MkIntTerm(0)); \
	  GONext();

#define P_FUNC2F_YY_D0APPL_END \
	(*_PREG) = NEXTOP((*_PREG), yyx); \
	INITIALIZE_PERMVAR(pt0, MkAtomTerm(NameOfFunctor(d1))); \
	INITIALIZE_PERMVAR(pt1, MkIntegerTerm(ArityOfFunctor(d1))); \
	GONext();

#define P_FUNC2F_YY_D0PAIR \
	CELL *pt0 = YREG+(*_PREG)->u.yyx.y1; \
	CELL *pt1 = YREG+(*_PREG)->u.yyx.y2; \
	(*_PREG) = NEXTOP((*_PREG), yyx); \
	INITIALIZE_PERMVAR(pt0, TermDot); \
	INITIALIZE_PERMVAR(pt1, MkIntTerm(2)); \
	GONext();

#define P_FUNC2F_YY_D0NOCOMPOUND \
	CELL *pt0 = YREG+(*_PREG)->u.yyx.y1; \
	CELL *pt1 = YREG+(*_PREG)->u.yyx.y2; \
	(*_PREG) = NEXTOP((*_PREG), yyx); \
	INITIALIZE_PERMVAR(pt0, d0); \
	INITIALIZE_PERMVAR(pt1, MkIntTerm(0)); \
	GONext();

#define P_FUNC2F_YY_END \
      saveregs(); \
      Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
      setregs(); \
      FAIL();

#include "p_functor.i"
