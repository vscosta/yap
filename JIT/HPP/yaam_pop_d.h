#define POP_N_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      register CELL d0; \
      d0 = (*_PREG)->u.os.s; \
      SP = (CELL *) (((char *) SP) + d0); \
      d0 = SP[0]; \
      if (d0) { \
	(*_SREG) = (CELL *) (SP[1]); \
	SP += 2; \
	(*_PREG) = NEXTOP((*_PREG), s); \
	GONext(); \
      } \
      else { \
	(*_SREG) = (CELL *) (SP[1]); \
	SP += 2; \
	(*_PREG) = NEXTOP((*_PREG), s); \
	GONextW(); \
      }
	  
#define POP_N_END \
      BLOCK = (CELL)POP_N_END;

#define POP_INSTINIT \
      print_instruction((*_PREG), ON_NATIVE); \
      BLOCKADDRESS = (CELL)(*_PREG); \
      register CELL d0; \
      d0 = SP[0]; \
      (*_SREG) = (CELL *) (SP[1]); \
      SP += 2; \
      if (d0) { \
	(*_PREG) = NEXTOP((*_PREG), e); \
	GONext(); \
      } \
      else { \
	(*_PREG) = NEXTOP((*_PREG), e); \
	GONextW(); \
      }

#define POP_END \
      BLOCK = (CELL)POP_END;
