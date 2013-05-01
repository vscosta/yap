/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		myddas_top_level.c					 *
* Last rev:	27/01/06						 *
* mods:									 *
* comments:	Top Level of the MYDDAS Interface                        *
*									 *
*************************************************************************/

#if defined MYDDAS_TOP_LEVEL && defined MYDDAS_MYSQL

#include "Yap.h"
#include "Yatom.h"
#include "myddas.h"
#include "myddas_structs.h"
#include "myddas_statistics.h"
#include <mysql/mysql.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
#include <sys/times.h>


static Int c_db_tl_readline(void);


void Yap_InitMYDDAS_TopLevelPreds(void)
{
  /* c_db_readline: +Prompt x -Line */
  Yap_InitCPred("c_db_tl_readline", 2, c_db_tl_readline,  SafePredFlag|SyncPredFlag);
  
}


typedef struct {
  const char *name;		/* User printable name of the function. */
  char cmd_char;		/* msql command character */
  Int (*func)(char *str,char *); /* Function to call to do the job. */
  //bool takes_params;		/* Max parameters for command */
  const char *doc;		/* Documentation for this function.  */
} COMMANDS;


static Int
c_db_tl_readline(void) {
  Term arg_prompt = Deref(ARG1);
  Term arg_line = Deref(ARG2);

  char *prompt = AtomName(AtomOfTerm(arg_prompt));
  char *line;
 
  while (strlen(line = readline(prompt)) == 0) {
    free(line);         
  }
  add_history(line);
  
  Term line_read = MkAtomTerm(Yap_LookupAtom(line));
  free(line);
  
  if (!Yap_unify(arg_line,line_read))
    return FALSE;
  return TRUE;
  
}

static void
myddas_top_level_print_time(MYDDAS_STATS_TIME time){

  //TODO test for big queries, and see the output of mysql
  printf("(");
  
  printf("%d",time->u.time_final.seconds);
  //MiliSeconds 2 decimal points
  printf(".%d",time->u.time_final.miliseconds/10); 
  printf (" sec)");
}

#endif


