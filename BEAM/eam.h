/*************************************************************************
*									 *
*	       BEAM -> Basic Extended Andorra Model                      *
*         BEAM extends the YAP Prolog system to support the EAM          *
*									 *
* Copyright: Ricardo Lopes and NCC - University of Porto, Portugal       *
*									 *
**************************************************************************
* comments:	eam compiler data structures and routines		 *
*************************************************************************/

#define Print_Code 0
/*      To help on compiler debuging
   1 -> show predicates info
   2 -> show YAP abstract machine code (YAAM)
   4 -> show YAAM after transformation
   8 -> show indexing code

  16 -> show EAM intermediate code
  32 -> show EAM intermediate code with direct_calls
 128 -> show EAM abstrac machine code
*/

#define Variavel  1
#define Lista     2
#define Estrutura 4
#define Constante 8

typedef unsigned long Cell;


typedef struct  PCODE{
  struct PCODE *nextInst;
  int op, new1; 
  unsigned long new4;
} CInstr;

struct Clauses {
  unsigned int idx;           /* info for indexing on first arg */
  Cell val;                   /* atom or functor in first arg   */
  unsigned int nr_vars;       /* nr of local vars */
  struct Predicates *predi;   /* predicate struct */
  int side_effects;           /* clause has side effects */
  Cell *code;

  struct Clauses *next;        /* next clause within the same predicate */
};


struct HASH_TABLE {
  Cell value;
  Cell *code;
  struct HASH_TABLE *next;
};

struct Predicates {           /* To register information about predicates */
  unsigned long id;
  unsigned char *name;
  unsigned int arity;         
  unsigned int nr_alt;        /* nr of alternativas */
  unsigned int calls;         /* nr of existent calls to this predicate */
  struct Clauses *first;
  struct Clauses *last;
  int idx;                    /* is code indexed ? 0= needs compilation  -1= no indexing possible  1= indexed */
  unsigned int idx_var;       /* nr clauses with 1st argument var */
  unsigned int idx_list;      /* nr clauses with 1st argument list */
  unsigned int idx_atom;      /* nr clauses with 1st argument atom */
  unsigned int idx_functor;   /* nr clauses with 1st argument functor */
  short int eager_split;      /* allow eager splitting */

  Cell *code;                 /* try, retry and trust code or Indexing code */
  struct HASH_TABLE **atom;
  struct HASH_TABLE **functor;
  Cell *list;
  Cell *vars;
  struct Predicates *next;
};

/****************************  EAM TRUE STUFF *************/

struct SUSPENSIONS {
  struct AND_BOX *and_box;         /* And_box where the variable has suspended        */
  short int reason;                /* suspended before executing call number nr_call  */ 
  struct SUSPENSIONS *next;        /* Pointer to the next suspention                  */
  struct SUSPENSIONS *prev;
};

struct SUSPENSIONS_VAR {
  struct AND_BOX *and_box;         /* And_box where the variable has suspended */
  struct SUSPENSIONS_VAR *next;    /* Pointer to the next suspention           */
};

struct PERM_VAR {
  Cell value;                      /* value assigned to the variable                    */
  struct AND_BOX *home;            /* pointer to the goal_box structure of the variable */
  Cell *yapvar;
  struct SUSPENSIONS_VAR *suspensions; /* Pointer to a Suspension List                  */
  struct PERM_VAR *next;
};

struct EXTERNAL_VAR {              /* to be used as some kind of trail */
  Cell value;                      /* value assign to the variable     */
  struct PERM_VAR *var;           /* pointer to the local_var struct  */
  struct EXTERNAL_VAR *next;
};

struct status_and {
  struct OR_BOX *call;             /* POINTER TO A OR_BOX       */
  Cell *locals;                    /* temporary vars vector     */
  Cell *code;                      /* Pointer to the start code */
  int state;                 /* State of the OR_BOX       */
  struct status_and *previous;
  struct status_and *next;
};

struct status_or {
  struct AND_BOX *alternative;     /* POINTER TO A AND_BOX      */
  Cell *args;                      /* Saved Arguments           */
  Cell *code;                      /* Pointer to Start Code     */
  int state;                 /* State of the AND_BOX      */
  struct status_or *previous;
  struct status_or *next;
};

struct OR_BOX {
  struct AND_BOX *parent;
  struct status_and *nr_call;      /* order of this box              */
  short int nr_all_alternatives;   /* number of existing alternatives */
  struct status_or *alternatives;  /* alternatives of the or_box      */
  short int eager_split; 
};

struct AND_BOX {
  struct OR_BOX *parent;            /* pointer to the parent or-box          */
  struct status_or *nr_alternative; /* This box is alternative id       */
  short int nr_all_calls;           /* numger of all goals                   */
  struct PERM_VAR *perms;
  struct status_and *calls;

  short int level;                 /* indicates the level in the tree       */
  struct EXTERNAL_VAR *externals;  /* pointer to a list of external_vars    */
  struct SUSPENSIONS *suspended;   /* pointer to a list of suspended boxes  */
  short int side_effects;          /* to mark if are calls to builtins with side_efects (like write) */
};


/* TYPE OF STATES */
#define ZERO        0    /* No State yet */
#define SUCCESS     1   
#define FAILS       2
#define READY       4    /* Is ready to start execution */
#define RUNNING     8    /* Is running                  */
#define RUNAGAIN    16   /* Is running again       */
#define SUSPEND     32   /* Has suspended               */
#define WAKE        64   /* Was Suspended, but now is Ready again      */
#define CHANGED     128  /* Has received some change on it's external variables, needs to re-run */
#define END         256  /* Has suspended on end, on wake up can pass to a success state */
#define WAITING     512  /* The clause is waiting for the previous predicates to leave the Suspended state */
#define FAILED     1024  /* has failed */

#define CUT_RIGHT       2048
#define SKIP_VAR        4096
#define LEFTMOST_PARENT 8192
#define FIRST          16384
#define LEFTMOST       32768

#define WAITING_TO_BE_FIRST             (WAITING + FIRST)
#define WAITING_TO_BE_LEFTMOST          (WAITING + LEFTMOST)
#define WAITING_TO_BE_LEFTMOST_PARENT   (WAITING + LEFTMOST_PARENT)
#define WAITING_TO_CUT                  (WAITING + CUT_RIGHT)
#define WAITING_SKIP_VAR                (WAITING + SKIP_VAR)
#define SUSPEND_END                     (SUSPEND+END)
#define WAKE_END                        (WAKE+END)


#define NORMAL_SUSPENSION    0
#define LEFTMOST_SUSPENSION  1
#define WAIT_SUSPENSION      2
#define CUT_SUSPENSION       3
#define WRITE_SUSPENSION     4
#define VAR_SUSPENSION       5
#define YAP_VAR_SUSPENSION   6

/* TYPE OF SIDE_EFFECTS */

#define WRITE       1
#define COMMIT      2
#define VAR         4
#define SEQUENCIAL  8

#define CUT         32  /* Greater than 32 always cut */


/**********************************************************************************/

struct EAM_TEMP {
  


  struct EAM_TEMP *previous;
  struct EAM_TEMP *next;
};

struct EAM_Global {
  Cell *pc;
  Cell *_H;
  Cell *_S;
  short _Mode;            /* read or write mode                     */
  short ES;               /* goal shoud do Eager Split yes or no ?  */ 
  short MemGoing;        /* Direction the that stacks use to grow  */
  Cell *varlocals;        /* local vars to the working AND-BOX      */
  struct AND_BOX  *ABX;   /* working AND-BOX                        */ 
  struct OR_BOX   *OBX;   /* working OR-BOX                         */
  struct SUSPENSIONS *su; /* list with suspended work               */
  struct AND_BOX  *top;

  struct status_and *USE_SAME_ANDBOX;  /* when only 1 alternative   */
  struct status_or *nr_alternative;    /* working alternative       */
  struct status_and *nr_call;          /* working goal              */

  Cell *VAR_TRAIL;        
  int VAR_TRAIL_NR;
  int Mem_FULL;           /*  if mem_full, then perform GC          */
  int nr_call_forking;    /* number of splits already performed     */
  unsigned long START_ADDR_HEAP, START_ADDR_BOXES, END_BOX, END_H;
  unsigned int nr_gc_heap;
  unsigned int nr_gc_boxed; 
  Cell **IndexFree;
  Cell *NextFree;
  Cell *sp;
  struct PERM_VAR *NextVar;

#if Memory_Stat
   unsigned long TOTAL_MEM, MEM_REUSED, TOTAL_TEMPS,TEMPS_REUSED, TOTAL_PERMS, PERMS_REUSED;
   unsigned long Memory_STAT[5000][5];
#endif
};


#define beam_X   XREGS      /* use the same X-Regs as YAP */

#define beam_pc (eamGlobal->pc)
#define beam_H (eamGlobal->_H)
#define beam_S (eamGlobal->_S)
#define beam_Mode (eamGlobal->_Mode)
#define beam_ES (eamGlobal->ES)
#define beam_MemGoing (eamGlobal->MemGoing)
#define beam_varlocals (eamGlobal->varlocals)
#define beam_ABX (eamGlobal->ABX)
#define beam_OBX (eamGlobal->OBX)
#define beam_su (eamGlobal->su)
#define beam_top (eamGlobal->top)
#define beam_USE_SAME_ANDBOX (eamGlobal->USE_SAME_ANDBOX)
#define beam_nr_alternative (eamGlobal->nr_alternative)
#define beam_nr_call (eamGlobal->nr_call)
#define beam_VAR_TRAIL (eamGlobal->VAR_TRAIL)
#define beam_VAR_TRAIL_NR (eamGlobal->VAR_TRAIL_NR)
#define beam_Mem_FULL (eamGlobal->Mem_FULL)
#define beam_nr_call_forking (eamGlobal->nr_call_forking)
#define beam_START_ADDR_HEAP (eamGlobal->START_ADDR_HEAP)
#define beam_START_ADDR_BOXES (eamGlobal->START_ADDR_BOXES)
#define beam_END_BOX (eamGlobal->END_BOX)
#define beam_END_H (eamGlobal->END_H)
#define beam_nr_gc_heap (eamGlobal->nr_gc_heap)
#define beam_nr_gc_boxed (eamGlobal->nr_gc_boxed)
#define beam_IndexFree (eamGlobal->IndexFree)
#define beam_NextFree (eamGlobal->NextFree)
#define beam_sp (eamGlobal->sp)
#define beam_NextVar (eamGlobal->NextVar)
#if Memory_Stat
 #define beam_TOTAL_MEM (eamGlobal->TOTAL_MEM)
 #define beam_MEM_REUSED (eamGlobal->MEM_REUSED)
 #define beam_TOTAL_TEMPS (eamGlobal->TOTAL_TEMPS)
 #define beam_TEMPS_REUSED (eamGlobal->TEMPS_REUSED)
 #define beam_TOTAL_PERMS (eamGlobal->TOTAL_PERMS)
 #define beam_PERMS_REUSED (eamGlobal->PERMS_REUSED)
 #define beam_Memory_STAT (eamGlobal->Memory_STAT)
#endif

#define arg1  *(beam_pc+1)
#define arg2  *(beam_pc+2)
#define arg3  *(beam_pc+3)
#define arg4  *(beam_pc+4)

#define CELL_SIZE  (sizeof(Cell))
#define POINTER_SIZE (sizeof(Cell *))
#define ANDBOX_SIZE (sizeof(struct AND_BOX))
#define ORBOX_SIZE (sizeof(struct OR_BOX))
#define PERM_VAR_SIZE (sizeof(struct PERM_VAR))
#define EXTERNAL_VAR_SIZE (sizeof(struct EXTERNAL_VAR))
#define SUSPENSIONS_SIZE (sizeof(struct SUSPENSIONS))
#define SUSPENSIONS_VAR_SIZE (sizeof(struct SUSPENSIONS_VAR))
#define STATUS_AND_SIZE (sizeof(struct status_and))
#define STATUS_OR_SIZE (sizeof(struct status_or))

