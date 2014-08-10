void print_main_when_head(yamop*, enumPlace);

inline void
print_main_when_head(yamop* p, enumPlace place)
{
  if((ExpEnv.debug_struc.pmainclause_on_head.print & place) == place) {
    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);
    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pmainclause_on_head.msg_before);
    print_preg(p);
    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pmainclause_on_head.msg_after);
  }
}

void print_instruction(yamop*, enumPlace);

inline void
print_instruction(yamop* p, enumPlace place)
{
  op_numbers op = Yap_op_from_opcode(p->opc);
  switch(op) {
  #define OPCODE(OP,TYPE) \
  case _##OP: \
  if((ExpEnv.debug_struc.pyaam_##OP.print & place) == place) { \
    char *tmp = (char*)malloc((16+strlen((char*)ExpEnv.debug_struc.pyaam_##OP.msg_after))*sizeof(char)); \
    switch(place) { \
    case ON_INTERPRETER: \
      strcpy(tmp, " (as standard)"); \
      break; \
    case ON_PROFILED_INTERPRETER: \
      strcpy(tmp, " (as profiled)"); \
      break; \
    case ON_NATIVE: \
      strcpy(tmp, " (as native)"); \
      break; \
    default:; \
    } \
    strcat(tmp, (char*)ExpEnv.debug_struc.pyaam_##OP.msg_after); \
	fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
    print_op((char*)ExpEnv.debug_struc.pyaam_##OP.msg_before, op, tmp); \
  } \
  break;
  #include "YapAppliedOpcodes.h"
  #undef OPCODE
  default:;
  }
}

#if YAP_JIT
void print_block(YAP_BBs, enumPlace);

inline void
print_block(YAP_BBs block, enumPlace place)
{
  switch(block) {
  #define BBLOCK(BB) \
  case BB: \
  if((ExpEnv.debug_struc.pbbs_##BB.print & place) == place) { \
    fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); \
    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pbbs_##BB.msg_before); \
    fprint_block(block); \
    { \
      if (place == ON_NATIVE) fprintf(stderr, " (on native)"); \
      else fprintf(stderr, " (on interpreter)"); \
    } \
    fprintf(stderr, "%s", (char*)ExpEnv.debug_struc.pbbs_##BB.msg_after); \
  } \
  break;
  #include "Yap_AppliedBasicBlocks.h"
  #undef BBLOCK
  default:;
  }
}
#endif
