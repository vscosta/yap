
#if (defined MYDDAS_MYSQL || defined MYDDAS_ODBC) && defined CUT_C


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mysql/mysql.h>
#include "Yap.h"
#include "Yatom.h"
#include "cut_c.h"
#include "myddas_util.h"
#ifdef MYDDAS_STATS
#include "myddas_statistics.h"
#endif


#define IS_SQL_INT(FIELD) FIELD == FIELD_TYPE_INT24    || \
                          FIELD == FIELD_TYPE_LONG     || \
                          FIELD == FIELD_TYPE_LONGLONG || \
                          FIELD == FIELD_TYPE_SHORT    || \
                          FIELD == FIELD_TYPE_TINY

#define IS_SQL_FLOAT(FIELD) FIELD == FIELD_TYPE_DECIMAL    || \
                            FIELD == FIELD_TYPE_DOUBLE     || \
                            FIELD == FIELD_TYPE_FLOAT


static int null_id = 0;


STATIC_PROTO(int c_db_my_query_no_result,(void));
STATIC_PROTO(int c_db_my_query_result,(void));

STATIC_PROTO(int c_db_my_row,(void));
STATIC_PROTO(int c_db_my_row_cut,(void));
STATIC_PROTO(int c_db_my_row_unify,(void));

static int 
c_db_my_query_no_result(void) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  
  char *sql = AtomName(AtomOfTerm(arg_sql_query));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));

  int length=strlen(sql);
  if (mysql_real_query(conn, sql, length) != 0){
    printf("Erro na query!\n");
    return FALSE;
  }
  /* With an INSERT statement, 
     mysql_(use or store)_result() returns 
     a NULL pointer, so it isn't necessary to 
     use mysql_(use or store)_result*/
  
  return TRUE;
}

/* Only use this function, with querys that return result sets*/
/* db_query: SQLQuery x ResultSet x Connection */
static int 
c_db_my_query_result(void) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_conn = Deref(ARG3);
  Term arg_mode = Deref(ARG4);
  
  char *sql = AtomName(AtomOfTerm(arg_sql_query));
  char *mode = AtomName(AtomOfTerm(arg_mode));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));
  
  MYSQL_RES *res_set;
  
  int length=strlen(sql);

  /* executar a query SQL */
  if (mysql_real_query(conn, sql, length) != 0)
    {
      printf("Erro na query!\n");
      return FALSE;
    }
  
  /* guardar os tuplos do lado do cliente */
  if (strcmp(mode,"store_result")!=0) //Verdadeiro
    {
      res_set = mysql_use_result(conn);
    }
  else
    {
      res_set = mysql_store_result(conn);
      
      int count = mysql_num_rows(res_set);
      if (count == 0){
	mysql_free_result(res_set);
	return FALSE;
      }
      
#ifdef MYDDAS_STATS
      MYDDAS_UTIL_CONNECTION node = 
	myddas_util_search_connection(conn);
      
      /* This only works if we use mysql_store_result */ 
      int numberRows = mysql_num_rows(res_set);
      numberRows = numberRows + myddas_util_get_conn_total_rows(node);
      myddas_util_set_conn_total_rows(node,numberRows);
#endif 
      
    }
  
  Bind(VarOfTerm(arg_result_set), MkIntegerTerm((int) res_set));
  return TRUE;
}


static int
c_db_my_row_cut(void) {
  MYSQL_RES *mysql_res=NULL;

  mysql_res = (MYSQL_RES *) IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,1));
  mysql_free_result(mysql_res);
  return TRUE;
}

/* db_row: ResultSet x Arity_ListOfArgs x ListOfArgs -> */
static int
c_db_my_row(void) {
  Term arg_result_set = Deref(ARG1);
  Term arg_arity = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);

  MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_result_set);
  EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((int)res_set);
  MYSQL_ROW row;
  MYSQL_FIELD *field;

  Term head, list, null_atom[1];
  int i, arity;

  arity = IntegerOfTerm(arg_arity);

  while(TRUE)
    {
      if ((row = mysql_fetch_row(res_set)) != NULL)
        {
          mysql_field_seek(res_set,0);
          list = arg_list_args;

          for (i = 0; i < arity; i++)
            {
              /* Aqui sero feitas as converses de tipos de dados */
              field = mysql_fetch_field(res_set);
              head = HeadOfTerm(list);
              list = TailOfTerm(list);

              if (row[i] == NULL)
                {
                  null_atom[0] = MkIntegerTerm(null_id++);

                  //if (!Yap_unify(head, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom)))
		  Bind(VarOfTerm(head), Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom));
		  continue;
                }
              else
                {
                  if (IS_SQL_INT(field->type))
                    {
                      //if (!Yap_unify(head, MkIntegerTerm(atoi(row[i])))){
                      Bind(VarOfTerm(head), MkIntegerTerm(atoi(row[i])));
                      continue;
                    }
                  else if (IS_SQL_FLOAT(field->type))
                    {
                      //if (!Yap_unify(head, MkFloatTerm(atof(row[i]))))
                      Bind(VarOfTerm(head), MkFloatTerm(atof(row[i])));
                      continue;
                    }
                  else
                    {
                      //if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(row[i]))))
                      Bind(VarOfTerm(head), MkAtomTerm(Yap_LookupAtom(row[i])));
                      continue;
                    }
		}
            }
          return TRUE;
        }
      else
        {
          mysql_free_result(res_set);
          cut_fail();
          return FALSE;
        }
    }
}
/* db_row: ResultSet x Arity_ListOfArgs x ListOfArgs -> */
static int
c_db_my_row_unify(void) {
  Term arg_result_set = Deref(ARG1);
  Term arg_arity = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);

  MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_result_set);
  EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((int)res_set);
  MYSQL_ROW row;
  MYSQL_FIELD *field;

  Term head, list, null_atom[1];
  int i, arity;

  arity = IntegerOfTerm(arg_arity);

  while(TRUE)
    {
      if ((row = mysql_fetch_row(res_set)) != NULL)
        {
          mysql_field_seek(res_set,0);
          list = arg_list_args;

          for (i = 0; i < arity; i++)
            {
              /* Aqui sero feitas as converses de tipos de dados */
              field = mysql_fetch_field(res_set);
              head = HeadOfTerm(list);
              list = TailOfTerm(list);

              if (row[i] == NULL)
                {
                  null_atom[0] = MkIntegerTerm(null_id++);

                  if (!Yap_unify(head, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom)))
		    //Bind(VarOfTerm(head), Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom));
		    continue;
                }
              else
                {
                  if (IS_SQL_INT(field->type))
                    {
                      if (!Yap_unify(head, MkIntegerTerm(atoi(row[i]))))
			//Bind(VarOfTerm(head), MkIntegerTerm(atoi(row[i])));
			continue;
                    }
                  else if (IS_SQL_FLOAT(field->type))
                    {
                      if (!Yap_unify(head, MkFloatTerm(atof(row[i]))))
			//Bind(VarOfTerm(head), MkFloatTerm(atof(row[i])));
			continue;
                    }
                  else
                    {
                      if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(row[i]))))
			//Bind(VarOfTerm(head), MkAtomTerm(Yap_LookupAtom(row[i])));
                      continue;
                    }
		}
            }
          return TRUE;
        }
      else
        {
          mysql_free_result(res_set);
          cut_fail();
          return FALSE;
        }
    }
}

/* static int */
/* c_db_my_row_term_cut(void) { */
/*   MYSQL_RES *mysql_res=NULL; */
  
/*   mysql_res = (MYSQL_RES *) IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,1)); */
/*   mysql_free_result(mysql_res); */
/*    return TRUE; */
/* } */

/* /\* db_row: ResultSet x ListOfArgs -> *\/ */
/* static int */
/* c_db_my_row_term(void) { */
/*   Term arg_result_set = Deref(ARG1); */
/*   Term arg_functor_name = Deref(ARG2); */
/*   Term arg_term = Deref(ARG3); */
    
/*   MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_result_set); */
/*   EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((int)res_set); */
/*   MYSQL_ROW row; */
/*   MYSQL_FIELD *field; */
/*   char *functor_name = AtomName(AtomOfTerm(arg_functor_name)); */
/*   Term null_atom[1]; */
  
/*   int i, arity; */
/*   arity = mysql_num_fields(res_set); */
  
/*   Functor functor = Yap_MkFunctor(Yap_LookupAtom(functor_name),arity); */
/*   Term properties[arity]; */
  
/*   while(TRUE) */
/*     { */
/*       if ((row = mysql_fetch_row(res_set)) != NULL) */
/* 	{ */
/* 	  mysql_field_seek(res_set,0);  */
/* 	  for (i = 0; i < arity; i++) */
/* 	    { */
/* 	      /\* Aqui serão feitas as conversões de tipos de dados *\/ */
/* 	      field = mysql_fetch_field(res_set); */
	      
/* 	      if (row[i] == NULL) */
/* 		{ */
/* 		  null_atom[0] = MkIntegerTerm(null_id++); */
/* 		  properties[i]= Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom); */
/* 		} */
/* 	      else */
/* 		{ */
/* 		  if (IS_SQL_INT(field->type))  */
/* 		    { */
/* 		      properties[i]= MkIntegerTerm(atoi(row[i])); */
/* 		    } */
/* 		  else if (IS_SQL_FLOAT(field->type)) */
/* 		    { */
/* 		      properties[i]= MkFloatTerm(atof(row[i])); */
/* 		    } */
/* 		  /\* This if is for case if we have a data type */
/* 		   like int(11)*\/ */
/* 		  else if (strchr(row[i],'(') && strchr(row[i],')')) */
/* 		    { */
/* 		      char *type =  strtok(row[i],"("); */
/* 		      char *num = strtok(NULL,")"); */
/* 		      Term size[1]; */
/* 		      size[0] = MkIntegerTerm(atoi(num)); */
/* 		      properties[i]= Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom(type),1),1,size); */
		      
/* 		      //printf ("-->%s %s %s\n",row[i],type,num); */
		      
/* 		    } */
/* 		  else  */
/* 		    { */
/* 		      properties[i]= MkAtomTerm(Yap_LookupAtom(row[i])); */
/* 		    } */
/* 		} */
/* 	    } */
/* 	  if (!Yap_unify(arg_term, Yap_MkApplTerm(functor,arity,properties))){ */
/* 	    mysql_free_result(res_set); */
/* 	    cut_fail(); */
/* 	    return FALSE; */
/* 	  } */
/* 	  return TRUE; */
/* 	} */
/*       else  */
/* 	{ */
/* 	  mysql_free_result(res_set); */
/* 	  cut_fail(); */
/* 	  return FALSE; */
/* 	} */
/*     } */
/* } */




void Yap_InitMYDDAS_testPreds(void)
{

  /* db_query: SQLQuery x ResultSet x Connection */
  Yap_InitCPred("c_db_my_query_result", 4, c_db_my_query_result, 0);  
  /* db_query: SQLQuery x ResultSet x Connection */
  Yap_InitCPred("c_db_my_query_no_result", 2, c_db_my_query_no_result, 0);  


}



void Yap_InitBackMYDDAS_testPreds(void)
{
  /* db_row: ResultSet x Arity x ListOfArgs */
  Yap_InitCPredBackCut("c_db_my_row_bind", 3, sizeof(int),
		       c_db_my_row,
		       c_db_my_row,
		       c_db_my_row_cut, 0);
  /* db_row: ResultSet x Arity x ListOfArgs */
  Yap_InitCPredBackCut("c_db_my_row_unify", 3, sizeof(int),
		       c_db_my_row_unify,
		       c_db_my_row_unify,
		       c_db_my_row_cut, 0);
  /* /\* db_row_term: ResultSet x NameFunctor x Term *\/ */
  /*   Yap_InitCPredBackCut("c_db_my_row_term", 3, sizeof(int), */
  /* 		    c_db_my_row_term,  */
  /* 		    c_db_my_row_term,   */
  /* 		    c_db_my_row_term_cut, 0); */
}

#endif /*MYDDAS_MYSQL && CUT_C*/
