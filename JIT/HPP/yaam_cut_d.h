#define CUT_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef COROUTINING
#define CUT_COROUTINING \
      if (FALSE) { \
	    NoStackCut_Exception; \
      }
#endif

#define CUT_NOCOROUTINING \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      saveregs(); \
      prune((choiceptr)YREG[E_CB]); \
      setregs(); \
      GONext();

#define CUT_T_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef COROUTINING
#define CUT_T_COROUTINING \
      if (FALSE) { \
	    NoStackCutT_Exception; \
      }
#endif

#define CUT_T_NOCOROUTINING \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      saveregs(); \
      prune((choiceptr)YREG[E_CB]); \
      setregs(); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      GONext();

#define CUT_E_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \

#ifdef COROUTINING
#define CUT_E_COROUTINING \
      if (FALSE) { \
	    NoStackCutE_Exception; \
      }
#endif

#define CUT_E_NOCOROUTINING \
      SET_ASP(YREG, (*_PREG)->u.s.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), s),Osbpp),l); \
      saveregs(); \
      prune((choiceptr)(*_SREG)[E_CB]); \
      setregs(); \
      GONext();

#define SAVE_B_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      d0 = (*_PREG)->u.x.x;
	  
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define SAVE_B_X_YSBA_FROZEN \
      XREG(d0) = MkIntegerTerm((Int)B);
#else
#define SAVE_B_X_NOYSBA_NOFROZEN \
      XREG(d0) = MkIntegerTerm(LCL0-(CELL *) (B));
#endif

#define SAVE_B_X_END \
      (*_PREG) = NEXTOP((*_PREG), x); \
      GONext();

#define SAVE_B_Y_INSTINIT
	  
#if defined(YAPOR_SBA)
#define SAVE_B_Y_YSBA \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.y.y,MkIntegerTerm((Int)B));
#else
#define SAVE_B_Y_NOYSBA \
      INITIALIZE_PERMVAR(YREG+(*_PREG)->u.y.y,MkIntegerTerm(LCL0-(CELL *)(B)));
#endif

#define SAVE_B_Y_END \
      (*_PREG) = NEXTOP((*_PREG), y); \
      GONext();

#define COMMIT_B_X_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      register CELL *pt1;

#define COMMIT_B_X_DO_COMMIT_B_X \
      d0 = XREG((*_PREG)->u.xps.x);

#define COMMIT_B_X_COMMIT_B_X_NVAR \
      SET_ASP(YREG, (*_PREG)->u.xps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), xps),Osbpp),l); \
	choiceptr pt0;
	
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define COMMIT_B_X_YSBA_FROZEN \
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
#define COMMIT_B_X_NOYSBA_NOFROZEN \
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif

#define COMMIT_B_X_POST_YSBA_FROZEN \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      GONext();

#define COMMIT_B_X_END

#define COMMIT_B_Y_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      register CELL d0; \
      register CELL *pt1;

#define COMMIT_B_Y_DO_COMMIT_B_Y \
      d0 = YREG[(*_PREG)->u.yps.y];

#define COMMIT_B_Y_COMMIT_B_Y_NVAR \
      SET_ASP(YREG, (*_PREG)->u.yps.s); \
      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), yps),Osbpp),l); \
	choiceptr pt0;
	
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
#define COMMIT_B_Y_YSBA_FROZEN \
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
#define COMMIT_B_Y_NOYSBA_NOFROZEN \
	pt0 = (choiceptr)(LCL0-IntegerOfTerm(d0));
#endif

#define COMMIT_B_Y_POST_YSBA_FROZEN \
	saveregs(); \
	prune(pt0); \
	setregs(); \
      GONext();

