#ifdef CUT_C

#include "Yap.h"
#include "cut_c.h"
#include <stdio.h>

void cut_c_initialize(void){
  Yap_REGS.CUT_C_TOP=(cut_c_str_ptr)Yap_LocalBase;
}

/*Removes a choice_point from the stack*/
void cut_c_pop(void){
  cut_c_str_ptr to_delete = NULL;
  if (((CELL *)Yap_REGS.CUT_C_TOP) == ((CELL *)Yap_LocalBase))
    {
      return;
    }
  else
    { /* removes the top element
	 from the stack */
      to_delete = Yap_REGS.CUT_C_TOP;
      Yap_REGS.CUT_C_TOP = to_delete->before;
      return;
    }
}

/*Insert a choice_point in the stack*/
void cut_c_push(cut_c_str_ptr new_top){
  new_top->before = Yap_REGS.CUT_C_TOP;
  Yap_REGS.CUT_C_TOP=new_top;
  return;
}

#endif /*CUT_C*/
