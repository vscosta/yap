#define POP_N_INIT \
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

#define _pop_instinit \
      BEGD(d0); \
      d0 = SP[0]; \
      (*_SREG) = (CELL *) (SP[1]); \
      SP += 2; \
      if (d0) { \
	(*_PREG) = NEXTOP((*_PREG), e); \
	GONEXT(); \
      } \
      else { \
	(*_PREG) = NEXTOP((*_PREG), e); \
	GONEXTW(); \
      } \
      ENDD(d0);
