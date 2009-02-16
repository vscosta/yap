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
* File:		myddas_odbc.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for comunicating with ODBC drivers   	 *
*									 *
*************************************************************************/

#if defined MYDDAS_ODBC && defined CUT_C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Yap.h"
#include "Yatom.h"
#include "myddas.h"
#include "cut_c.h"
#include <sql.h>
#include <sqlucode.h>

static Int null_id = 0;

STATIC_PROTO(Int c_db_odbc_connect,(void));
STATIC_PROTO(Int c_db_odbc_disconnect,(void));
STATIC_PROTO(Int c_db_odbc_number_of_fields,(void));
STATIC_PROTO(Int c_db_odbc_get_attributes_types,(void));
STATIC_PROTO(Int c_db_odbc_query,(void));
STATIC_PROTO(Int c_db_odbc_row,(void));
STATIC_PROTO(Int c_db_odbc_row_cut,(void));
STATIC_PROTO(Int c_db_odbc_get_fields_properties,(void));
STATIC_PROTO(Int c_db_odbc_number_of_fields_in_query,(void));


#define SQLALLOCHANDLE(A,B,C,print)                               \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLAllocHandle(A,B,C);                                \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLAllocHandle(ENV) %s\n",print);           \
      return FALSE;                                               \
    }                                                             \
}

#define SQLSETENVATTR(A,B,C,D,print)                              \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLSetEnvAttr(A,B,C,D);                               \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLSetEnvAttr %s\n",print);                 \
      return FALSE;                                               \
    }                                                             \
}

#define SQLCONNECT(A,B,C,D,E,F,G,print)                           \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLConnect(A,B,C,D,E,F,G);                            \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLConnect %s\n",print);                    \
      return FALSE;                                               \
    }                                                             \
}

#define SQLEXECDIRECT(A,B,C,print)                                \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLExecDirect(A,B,C);                                 \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLExecDirect %s \n",print);                \
      return FALSE;                                               \
    }                                                             \
}

#define SQLDESCRIBECOL(A,B,C,D,E,F,G,H,I,print)                   \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLDescribeCol(A,B,C,D,E,F,G,H,I);                    \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLDescribeCol %s\n",print);                \
      return FALSE;                                               \
    }                                                             \
}

#define SQLSETCONNECTATTR(A,B,C,D,print)                          \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLSetConnectAttr(A,B,C,D);                           \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLSetConnectAttr %s\n",print);             \
      return FALSE;                                               \
    }                                                             \
}

#define SQLBINDCOL(A,B,C,D,E,F,print)                             \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLBindCol(A,B,C,D,E,F);                              \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLbindCol %s\n",print);                    \
      return FALSE;                                               \
    }                                                             \
}

#define SQLFREESTMT(A,B,print)                                    \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLFreeStmt(A,B);                                     \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLFreeStmt %s\n",print);                   \
      return FALSE;                                               \
    }                                                             \
}

#define SQLNUMRESULTCOLS(A,B,print)                               \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLNumResultCols(A,B);                                \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLNumResultCols %s\n",print);              \
      return FALSE;                                               \
    }                                                             \
}


#define SQLCLOSECURSOR(A,print)                                   \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLCloseCursor(A);                                    \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLCloseCursor %s\n",print);                \
      return FALSE;                                               \
    }                                                             \
}

/* no db_odbc_row não é utilizada esta macro*/
#define SQLFETCH(A,print)                                         \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLFetch(A);                                          \
  if (retcode == SQL_NO_DATA)                                     \
    break;                                                        \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLFETCH %s\n",print);                      \
      return FALSE;                                               \
    }                                                             \
}

#define SQLGETDATA(A,B,C,D,E,F,print)                             \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLGetData(A,B,C,D,E,F);                              \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLgetdata %s\n",print);                    \
      return FALSE;                                               \
    }                                                             \
}

#define SQLDISCONNECT(A,print)                                    \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLDisconnect(A);                                     \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLDisconnect %s\n",print);                 \
      return FALSE;                                               \
    }                                                             \
}

#define SQLFREEHANDLE(A,B,print)                                  \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLFreeHandle(A,B);                                   \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLFreeHandle %s\n",print);                 \
      return FALSE;                                               \
    }                                                             \
}

#define SQLPRIMARYKEYS(A,B,C,D,E,F,G,print)                       \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLPrimaryKeys(A,B,C,D,E,F,G);                        \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLPrimaryKeys %s\n",print);                \
      return FALSE;                                               \
    }                                                             \
}

#define SQLGETTYPEINFO(A,B,print)                                 \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLGetTypeInfo(A,B);                                  \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLGetTypeInfo %s\n",print);                \
      return FALSE;                                               \
    }                                                             \
}

#define SQLCOLATTRIBUTE(A,B,C,D,E,F,G,print)                      \
{                                                                 \
  SQLRETURN retcode;                                              \
  retcode = SQLColAttribute(A,B,C,D,E,F,G);                       \
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) \
    {                                                             \
      printf("erro no SQLColAttribute %s\n",print);               \
      return FALSE;                                               \
    }                                                             \
}



/* Verificar tipo de dados*/
#define IS_SQL_INT(FIELD) FIELD == SQL_DECIMAL    ||  \
	                  FIELD == SQL_NUMERIC    ||  \
	                  FIELD == SQL_SMALLINT   ||  \
	                  FIELD == SQL_INTEGER    ||  \
	                  FIELD == SQL_TINYINT    ||  \
                          FIELD == SQL_BIGINT

#define IS_SQL_FLOAT(FIELD) FIELD == SQL_FLOAT    ||  \
	                    FIELD == SQL_DOUBLE   ||  \
	                    FIELD == SQL_REAL 




static Int
c_db_odbc_connect(void) {
  Term arg_driver = Deref(ARG1); 
  Term arg_user = Deref(ARG2);
  Term arg_passwd = Deref(ARG3);
  Term arg_conn = Deref(ARG4);  

  MYDDAS_UTIL_CONNECTION new = NULL;

  char *driver = AtomName(AtomOfTerm(arg_driver));
  char *user = AtomName(AtomOfTerm(arg_user));
  char *passwd = AtomName(AtomOfTerm(arg_passwd));
    
  SQLHENV     henv;
  SQLHDBC     hdbc;
   
  /*Allocate environment handle */
  SQLALLOCHANDLE(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv, "connect"); 
  /* Set the ODBC version environment attribute */
  SQLSETENVATTR(henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, 0, "connect"); 
  /* Allocate connection handle */
  SQLALLOCHANDLE(SQL_HANDLE_DBC, henv, &hdbc, "connect"); 
  /* Set login timeout to 6 seconds. */
  SQLSETCONNECTATTR(hdbc, SQL_LOGIN_TIMEOUT,(SQLPOINTER) 6, 0, "connect");
  /* Connect to data source */
  SQLCONNECT(hdbc,
	     (SQLCHAR*) driver, SQL_NTS,
	     (SQLCHAR*) user, SQL_NTS,
	     (SQLCHAR*) passwd, SQL_NTS, "connect");
  
  if (!Yap_unify(arg_conn, MkIntegerTerm((Int)(hdbc))))
    return FALSE;
  else
    {
      /* Criar um novo no na lista de ligacoes*/
      //new = add_connection(&TOP,hdbc,henv);
      new = myddas_util_add_connection(hdbc,henv);
      if (new == NULL){
	printf("Erro ao alocar memoria para lista\n");
	return FALSE;
      }
      return TRUE;
    }
}

/* db_query: SQLQuery x ResultSet x Arity x BindList x Connection */
static Int
c_db_odbc_query(void) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_arity = Deref(ARG3);
  Term arg_bind_list = Deref(ARG4);
  Term arg_conn = Deref(ARG5);

  SQLCHAR *sql = AtomName(AtomOfTerm(arg_sql_query));
  
  
  SQLHDBC  hdbc =(SQLHDBC) (IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;
  SQLSMALLINT type;
  
  /*Allocate an handle for the query*/ 
  SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_query");
  /* Executes the query*/ 
  SQLEXECDIRECT(hstmt,sql,SQL_NTS, "db_query");
  
  Int arity;
  Int i;
  
  if (IsNonVarTerm(arg_arity)){
    arity = IntegerOfTerm(arg_arity);
  
    
    char *bind_space=NULL;

    //const Int functor_arity=3;
    const Short functor_arity=3;
    Functor functor = Yap_MkFunctor(Yap_LookupAtom("bind"),functor_arity);
    Term properties[functor_arity];
  
    Term head,list=arg_bind_list;

    SQLUINTEGER ColumnSizePtr;
    SQLINTEGER *data_info=NULL;

    for (i=1;i<=arity;i++)
      {
	head = HeadOfTerm(list);
	list = TailOfTerm(list);
        
	SQLDESCRIBECOL(hstmt,i,NULL,0,NULL,&type,&ColumnSizePtr,NULL,NULL,"db_query");
	
	/* +1 because of '\0' */
	bind_space = malloc(sizeof(char)*(ColumnSizePtr+1));
	data_info = malloc(sizeof(SQLINTEGER));
	SQLBINDCOL(hstmt,i,SQL_C_CHAR,bind_space,(ColumnSizePtr+1),data_info,"db_query");
	
	properties[0] = MkIntegerTerm((Int)bind_space);
	properties[2] = MkIntegerTerm((Int)data_info);
	
	if (IS_SQL_INT(type))
	  properties[1]=MkAtomTerm(Yap_LookupAtom("integer"));
	else if (IS_SQL_FLOAT(type))
	  properties[1]=MkAtomTerm(Yap_LookupAtom("real"));
	else
	  properties[1]=MkAtomTerm(Yap_LookupAtom("string"));
	
	Yap_unify(head,Yap_MkApplTerm(functor,functor_arity,properties));
	continue;
	
      }
  }
  
  if (!Yap_unify(arg_result_set, MkIntegerTerm((Int) hstmt)))
    {
      SQLCLOSECURSOR(hstmt,"db_query");
      SQLFREESTMT(hstmt,SQL_CLOSE,"db_query");
      return FALSE;
    }
  return TRUE;
}

static Int
c_db_odbc_number_of_fields(void) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);


  char *relation = AtomName(AtomOfTerm(arg_relation));

  SQLHDBC     hdbc =(SQLHDBC) (IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;
  
  char sql[256];
  SQLSMALLINT number_fields;

  sprintf(sql,"DESCRIBE %s",relation);
  
  SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_number_of_fields");
  SQLEXECDIRECT(hstmt,sql,SQL_NTS, "db_number_of_fields");  
  
  /* Calcula o numero de campos*/
  number_fields=0;
  while(TRUE) {
    SQLFETCH(hstmt,"db_number_of_fields");
    number_fields++;
  }
  
  SQLCLOSECURSOR(hstmt,"db_number_of_fields");
  SQLFREESTMT(hstmt,SQL_CLOSE,"db_number_of_fields");
  
  if (!Yap_unify(arg_fields, MkIntegerTerm(number_fields)))
    return FALSE;
  return TRUE;
}


/* db_get_attributes_types: RelName x Connection -> TypesList */
static Int
c_db_odbc_get_attributes_types(void) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_types_list = Deref(ARG3);

  char *relation = AtomName(AtomOfTerm(arg_relation));
  SQLHDBC     hdbc =(SQLHDBC) (IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;
  
  char sql[256];
  Term head, list;
  list = arg_types_list;

  sprintf(sql,"DESCRIBE %s",relation);

  SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_get_attributes_types");
  SQLEXECDIRECT(hstmt,sql,SQL_NTS, "db_get_attributes_types");
  
  while (TRUE) 
    {
      SQLFETCH(hstmt, "db_get_attributes_types");
           
      /* Tentar fazer de uma maneira que a gente consiga calcular o tamanho que o 
       nome do campo vai ocupar, assim podemos alocar memoria dinamicamente*/
      sql[0]='\0';
      SQLGETDATA(hstmt, 1, SQL_C_CHAR, sql, 256, NULL, "db_get_attributes_types");
      
      head = HeadOfTerm(list);
      Yap_unify(head, MkAtomTerm(Yap_LookupAtom(sql)));
      list = TailOfTerm(list);
      head = HeadOfTerm(list);
      list = TailOfTerm(list);
  
      sql[0]='\0';
      SQLGETDATA(hstmt, 2, SQL_C_CHAR, sql, 256, NULL, "db_get_attributes_types");
      
      if (strncmp(sql, "smallint",8) == 0 || strncmp(sql,"int",3) == 0 ||
	  strncmp(sql, "mediumint",9) == 0 || strncmp(sql, "tinyint",7) == 0 ||
	  strncmp(sql, "bigint",6) == 0 || strcmp(sql, "year") == 0)
	Yap_unify(head, MkAtomTerm(Yap_LookupAtom("integer")));
      else 
	if (strcmp(sql, "float") == 0 || strncmp(sql, "double",6) == 0
	    || strcmp(sql, "real") == 0)
	  Yap_unify(head, MkAtomTerm(Yap_LookupAtom("real")));
	else 
	  Yap_unify(head, MkAtomTerm(Yap_LookupAtom("string")));
    }
  
  SQLCLOSECURSOR(hstmt,"db_get_attributes_types");
  SQLFREESTMT(hstmt,SQL_CLOSE, "db_get_attributes_types");
  return TRUE;
}

/* db_disconnect */
static Int
c_db_odbc_disconnect(void) {
  Term arg_conn = Deref(ARG1);

  SQLHDBC conn = (SQLHDBC) (IntegerOfTerm(arg_conn));
  SQLHENV henv = myddas_util_get_odbc_enviromment(conn);
  
  if ((myddas_util_search_connection(conn)) != NULL)
    {
      myddas_util_delete_connection(conn);
      /* More information about this process on
	 msdn.microsoft.com*/
      SQLDISCONNECT(conn,"db_disconnect");
      SQLFREEHANDLE(SQL_HANDLE_DBC,conn,"db_disconnect");
      SQLFREEHANDLE(SQL_HANDLE_ENV,henv,"db_disconnect");
      
      return TRUE;
    }
  else
    return FALSE;
}

static Int
c_db_odbc_row_cut(void) {
    
  SQLHSTMT hstmt = (SQLHSTMT) IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,1)); 
  
  SQLCLOSECURSOR(hstmt,"db_row_cut"); 
  SQLFREESTMT(hstmt,SQL_CLOSE,"db_row_cut"); 
   
  return TRUE;
}

/* db_row: ResultSet x BindList x ListOfArgs -> */
static Int
c_db_odbc_row(void) {
  Term arg_result_set = Deref(ARG1);
  Term arg_bind_list = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);

  SQLHSTMT hstmt = (SQLHSTMT) IntegerOfTerm(arg_result_set);
    
  /* EXTRA_CBACK_ARG(ARIDADE,LOCAL_ONDE_COLOCAR_VALOR)*/
  EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((Int)hstmt);

  Term head, list, null_atom[1];
  Term head_bind, list_bind;
   
  SQLRETURN retcode = SQLFetch(hstmt);
  if (retcode == SQL_NO_DATA)
    {
      SQLCLOSECURSOR(hstmt,"db_row"); 
      SQLFREESTMT(hstmt,SQL_CLOSE,"db_row"); 
  
      cut_fail(); 
      return FALSE;
    }
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    {
      printf("erro no SQLFETCH number of fields\n");
      return FALSE;
    }
    
  char *bind_value=NULL;
  Term type;

  list = arg_list_args;
  list_bind = arg_bind_list;
  SQLINTEGER *data_info=NULL;
  
  while (IsPairTerm(list_bind))
    {
      head = HeadOfTerm(list);
      list = TailOfTerm(list);
      
      head_bind = HeadOfTerm(list_bind);
      list_bind = TailOfTerm(list_bind);
      
      bind_value = (char *)IntegerOfTerm(ArgOfTerm(1,head_bind));
      type = ArgOfTerm(2,head_bind);
      data_info = (SQLINTEGER *)IntegerOfTerm(ArgOfTerm(3,head_bind));

      if ((*data_info) == SQL_NULL_DATA){
	null_atom[0] = MkIntegerTerm(null_id++);
	if (!Yap_unify(head, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom)))
	  continue;
      }
      else
	{
	  
	  if (!strcmp(AtomName(AtomOfTerm(type)),"integer"))
	    {
	      if (!Yap_unify(head, MkIntegerTerm(atoi(bind_value))))
		continue;
	    }
	  else if (!strcmp(AtomName(AtomOfTerm(type)),"real"))
	    {
	      if (!Yap_unify(head, MkFloatTerm(atof(bind_value))))
		continue;
	    }
	  else  if (!strcmp(AtomName(AtomOfTerm(type)),"string"))
	    {
	      if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(bind_value))))
		continue;
	    }
	}
    }
  return TRUE;
}


/* Mudar esta funcao de forma a nao fazer a consulta, pois 
 no predicate db_sql_selet vai fazer duas vezes a mesma consutla*/ 
static Int
c_db_odbc_number_of_fields_in_query(void) {
  Term arg_query = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);

  char *sql = AtomName(AtomOfTerm(arg_query));
  
  SQLHDBC hdbc =(SQLHDBC) (IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;
  SQLSMALLINT number_cols=0;

  SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, 
		 "db_number_of_fields_in_query");
  SQLEXECDIRECT(hstmt,sql,SQL_NTS, 
		"db_number_of_fields_in_query");
  
  SQLNUMRESULTCOLS(hstmt,&number_cols,
		   "db_number_of_fields_in_query");
  
  if (!Yap_unify(arg_fields, MkIntegerTerm(number_cols))){
    return FALSE;
  }
  
  SQLCLOSECURSOR(hstmt,"db_number_of_fields_in_query");
  SQLFREESTMT(hstmt,SQL_CLOSE, "db_number_of_fields_in_query");
 
  return TRUE;
}

static Int
c_db_odbc_get_fields_properties(void) {
  Term nome_relacao = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term fields_properties_list = Deref(ARG3);
  Term head, list;

  char *relacao = AtomName(AtomOfTerm(nome_relacao));
  char sql[256];
  char name[200];
  Int i;
  
  
  SQLSMALLINT num_fields=0;
  SQLSMALLINT NullablePtr=0;
  SQLSMALLINT AutoIncrementPointer=0;
  SQLHSTMT hstmt,hstmt2;
  SQLHDBC  hdbc =(SQLHDBC) (IntegerOfTerm(arg_conn));


  /* LIMIT 0 -> We don't need the results of the query, 
     only the information about the fields of the relation*/
  sprintf (sql,"SELECT * FROM `%s` LIMIT 0",relacao);

  /*Allocate an handle for the query*/ 
  SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_get_fields_properties");
  /* Executes the query*/ 
  SQLEXECDIRECT(hstmt,sql,SQL_NTS, "db_get_fields_properties");
      
  Functor functor = Yap_MkFunctor(Yap_LookupAtom("property"),4);
  Term properties[4];
  
  SQLNUMRESULTCOLS(hstmt,&num_fields,
		   "db_get_fields_properties");
  
  list = fields_properties_list;
  
  SQLSMALLINT bind_prim_key;
  //por causa de as rows em odbc começam em 1 :)
  Short *null=malloc(sizeof(Short)*(1+num_fields));
  
  SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt2, "db_get_fields_properties");
  /* Executes the query*/ 
  SQLPRIMARYKEYS(hstmt2,NULL,0,NULL,0,relacao,SQL_NTS, "db_get_fields_properties");
  /* Associates bind value for the 5 column*/
  SQLBINDCOL(hstmt2,5,SQL_C_SSHORT,&bind_prim_key,sizeof(SQLSMALLINT),NULL,
	     "db_get_fields_properties");
  
  while(1)
    {
      SQLFETCH(hstmt2,"db_get_fields_properties");
      null[bind_prim_key]=1;
    }

  SQLCLOSECURSOR(hstmt2,"db_get_fields_properties");
  SQLFREESTMT(hstmt2,SQL_CLOSE,"db_get_fields_properties");
  
  for (i=1;i<=num_fields;i++)
    {
      head = HeadOfTerm(list);
      name[0]='\0';
      SQLDESCRIBECOL(hstmt,i,name,200,NULL,NULL,NULL,NULL,&NullablePtr,
		     "db_get_fields_properties");

      SQLCOLATTRIBUTE(hstmt,i,SQL_DESC_AUTO_UNIQUE_VALUE,NULL,0,NULL,&AutoIncrementPointer,
		      "db_get_fields_properties");
      
      properties[0] = MkAtomTerm(Yap_LookupAtom(name));
       
     
      if (NullablePtr & SQL_NULLABLE)
      	properties[1] = MkIntegerTerm(1); //Can't be NULL
      else
	properties[1] = MkIntegerTerm(0);
      
      if (null[i] == 1)
	properties[2] = MkIntegerTerm(1); //It''s a primary key
      else
	properties[2] = MkIntegerTerm(0);
      
      if (AutoIncrementPointer & SQL_TRUE)
	properties[3] = MkIntegerTerm(1); //It's auto_incremented field
      else
	properties[3] = MkIntegerTerm(0);
      
            
      list = TailOfTerm(list);
      if (!Yap_unify(head, Yap_MkApplTerm(functor,4,properties))){
      	return FALSE;
      }
    }
  
  SQLCLOSECURSOR(hstmt,"db_get_fields_properties");
  SQLFREESTMT(hstmt,SQL_CLOSE,"db_get_fields_properties");
    
  return TRUE;
}



void Yap_InitMYDDAS_ODBCPreds(void)
{
  /* db_connect: Host x User x Passwd x Database x Connection */
  Yap_InitCPred("c_db_odbc_connect", 4, c_db_odbc_connect, 0);
  
  /* db_number_of_fields: Relation x Connection x NumberOfFields */
  Yap_InitCPred("c_db_odbc_number_of_fields",3, c_db_odbc_number_of_fields, 0);

  /* db_number_of_fields_in_query: SQLQuery x Connection x NumberOfFields */
  Yap_InitCPred("c_db_odbc_number_of_fields_in_query",3, c_db_odbc_number_of_fields_in_query, 0);
  
  /* db_get_attributes_types: Relation x TypesList */
  Yap_InitCPred("c_db_odbc_get_attributes_types", 3, c_db_odbc_get_attributes_types,  0);
  
  /* db_query: SQLQuery x ResultSet x Connection */
  Yap_InitCPred("c_db_odbc_query", 5, c_db_odbc_query, 0);
  
  /* db_disconnect: Connection */
  Yap_InitCPred("c_db_odbc_disconnect", 1,c_db_odbc_disconnect, 0);
  
  /* db_get_fields_properties: PredName x Connnection x PropertiesList */
  Yap_InitCPred("c_db_odbc_get_fields_properties",3,c_db_odbc_get_fields_properties,0);

}


void Yap_InitBackMYDDAS_ODBCPreds(void)
{
  
  /*  db_row: ResultSet x ListOfArgs */
  Yap_InitCPredBackCut("c_db_odbc_row", 3, sizeof(Int),
		       c_db_odbc_row,
		       c_db_odbc_row,
		       c_db_odbc_row_cut, 0);

}

#endif /*MYDDAS_ODBC*/
