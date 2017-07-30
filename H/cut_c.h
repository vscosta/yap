#ifndef __CUT_C_H__
#define __CUT_C_H__

/* Some definitions */
#define Choice_Point_Type void *

/* necessary for not redefine NULL*/
#ifndef NULL
#define NULL nil
#endif

typedef struct cut_c_str *cut_c_str_ptr;
struct cut_c_str {
  cut_c_str_ptr before;
  void *try_userc_cut_yamop;
};

#define CUT_C_STR_SIZE ((sizeof(struct cut_c_str)) / (sizeof(CELL)))

#define EXTRA_CBACK_CUT_ARG(Type, Offset) EXTRA_CBACK_ARG(PP->ArityOfPE, Offset)

#define CBACK_CUT_ARG(Offset) B->cp_args[(Offset)-1]

#define CUT_C_PUSH(YAMOP, S_YREG)

#define POP_CHOICE_POINT(cp) false
#define POP_EXECUTE() 

#define POP_FAIL(handler) 
#define POP_FAIL_EXECUTE(handler) 

/*Initializes CUT_C_TOP*/
void cut_c_initialize(int wid);

/*Removes a choice_point from the stack*/
void cut_c_pop(void);

/*Insert a choice_point in the stack*/
void cut_c_push(cut_c_str_ptr);

#endif /*_CUT_C_H__*/
