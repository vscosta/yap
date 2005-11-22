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
* File:		myddas_shared.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for maintaining MYDDAS                        *
*									 *
*************************************************************************/

#if defined CUT_C && (defined MYDDAS_MYSQL || defined MYDDAS_ODBC)

#include "Yap.h"
#include "Yatom.h"
#include "cut_c.h"
#include "myddas_util.h"


STATIC_PROTO(int c_db_preds_conn_start ,(void));
STATIC_PROTO(int c_db_preds_conn_continue ,(void));
STATIC_PROTO(int c_db_add_preds,(void));
STATIC_PROTO(int c_db_check_if_exists_pred,(void));

#ifdef DEBUG
STATIC_PROTO(int c_db_check,(void));
#endif


/* db_add_preds: PredName * Arity * Module * Connection*/
static int 
c_db_add_preds (void){
  Term arg_nome = Deref(ARG1);
  Term arg_aridade = Deref(ARG2);
  Term arg_module = Deref(ARG3);
  Term arg_conn = Deref(ARG4);
  
  char *nome = AtomName(AtomOfTerm(arg_nome));
  char *module = AtomName(AtomOfTerm(arg_module));
  int aridade = IntegerOfTerm(arg_aridade);
  int *conn = (int *) IntegerOfTerm(arg_conn);

  if (myddas_util_add_predicate(nome,aridade,module,conn) == NULL)
    {
      printf ("ERRO : Nao consegui adicionar predicado\n");
      return FALSE;
    }
  
  return TRUE;
}

/* db_add_preds: PredName * Arity */
static int 
c_db_check_if_exists_pred (void){
  Term arg_nome = Deref(ARG1);
  Term arg_aridade = Deref(ARG2);
  Term arg_module = Deref(ARG3);
    
  char *nome = AtomName(AtomOfTerm(arg_nome));
  char *module = AtomName(AtomOfTerm(arg_module));
  int aridade = IntegerOfTerm(arg_aridade);
  
  if (myddas_util_search_predicate(nome,aridade,module) == NULL)
    return FALSE;
  else
    return TRUE;
}


/* db_preds_conn : Connection(+) * Pred_name(-) * Pred_arity */
static int
c_db_preds_conn_start (void){
  Term arg_conn = Deref(ARG1);
  Term nome = Deref(ARG2);
  Term aridade = Deref(ARG3);
  
  int *conn = (int *) IntegerOfTerm(arg_conn);
  MYDDAS_UTIL_CONNECTION node = 
    myddas_util_search_connection(conn);
  
  /* Caso a ligacao já tenha sido apagada*/
  if (node == NULL)
    {
      cut_fail();
      return FALSE;
    }
  
  void *pointer = myddas_util_get_list_pred(node);
  EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((int)pointer);
  
  if (IsVarTerm(nome) && IsVarTerm(aridade))
    return (c_db_preds_conn_continue());
      
  cut_fail();
  return FALSE;
}

/* db_preds_conn : Connection(+) * Pred_name(-) * Pred_arity*/
static int 
c_db_preds_conn_continue (void){
  Term nome = Deref(ARG2);
  Term aridade = Deref(ARG3);

  void *pointer;
  pointer = (void *) IntegerOfTerm(EXTRA_CBACK_ARG(3,1));
    
  if (pointer != NULL)
    {
      Yap_unify(nome, MkAtomTerm(Yap_LookupAtom(myddas_util_get_pred_name(pointer))));
      Yap_unify(aridade, MkIntegerTerm((int)myddas_util_get_pred_arity(pointer)));
      
      EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((int)myddas_util_get_pred_next(pointer));
      return TRUE;
    }
  else
    {
      cut_fail();
      return FALSE;
    }
}

#ifdef DEBUG
static int 
c_db_check(void){
  check_int();
  return TRUE;
}
#endif /*DEBUG*/


void Yap_InitMYDDAS_SharedPreds(void)
{
  /* db_add_preds : PredName * Arity * Connection */
  Yap_InitCPred("c_db_add_preds",4,c_db_add_preds, 0);

  /* db_check_if_exists_pred : PredName * Arity * Connection */
  Yap_InitCPred("c_db_check_if_exists_pred",3,c_db_check_if_exists_pred, 0);

#ifdef DEBUG
  Yap_InitCPred("c_db_check",0, c_db_check, 0);
#endif
}

void Yap_InitBackMYDDAS_SharedPreds(void)
{
  Yap_InitCPredBack("c_db_preds_conn", 3, sizeof(int),
		    c_db_preds_conn_start, 
		    c_db_preds_conn_continue,  0); 

}




#endif /*CUT_C && (MYDDAS_MYSQL || MYDDAS_ODBC)*/
