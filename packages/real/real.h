#include <Rembedded.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>

#include <YapInterface.h>
#include <c_interface.h>

#define BUFSIZE 256

typedef unsigned int PL_Type;

#define PL_Nil       0
#define PL_Var       1
#define PL_Atom      2
#define PL_Appl      3
#define PL_Pair      4
#define PL_Int       5
#define PL_Float     6
#define PL_DbRef     7
#define PL_Unknown   8



typedef enum {
  r_undefined,
  r_double,
  r_int,
  r_character
} r_basic_types;

typedef struct
{
  r_basic_types    type;
  union {
    int     int_val;
    double  double_val;
    char    *char_val;
  } real_u;
} list_cell;

typedef struct
{
    int         size;
    int         nDims;
    int         dims[BUFSIZE];
    list_cell   values[BUFSIZE];
} list;

#define real_Int       1
#define real_Float     2
#define real_Char      3
#define real_Bool      4

#define real_ty_Vector      1
#define real_ty_Matrix      2
#define real_ty_List        3
#define real_ty_Array       4  //not used, yet

extern void    init_R(void);
extern void    end_R(void);
extern void    send_command(char * expression);
extern int     set_list_values(void);
extern int     set_vec_values(void);
extern int     set_array_values(void);
extern SEXP process_expression(char * expression);
extern YAP_Term sexp_pl(SEXP s);
