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

#if MYDDAS_ODBC

#if !defined(ODBCVER)
typedef void *SQLHDBC;
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Yap.h"
#include "Yatom.h"
#include "myddas.h"
#include "../myddas_util.h"
#include "cut_c.h"
#include <sql.h>
#include <sqlucode.h>

/* Return enviromment identifier*/
SQLHENV myddas_util_get_odbc_enviromment(SQLHDBC);

static Int null_id = 0;

static Int c_db_odbc_connect(USES_REGS1);
static Int c_db_odbc_disconnect(USES_REGS1);
static Int c_db_odbc_number_of_fields(USES_REGS1);
static Int c_db_odbc_get_attributes_types(USES_REGS1);
static Int c_db_odbc_query(USES_REGS1);
static Int c_db_odbc_row(USES_REGS1);
static Int c_db_odbc_row_cut(USES_REGS1);
static Int c_db_odbc_get_fields_properties(USES_REGS1);
static Int c_db_odbc_number_of_fields_in_query(USES_REGS1);

static int odbc_error(SQLSMALLINT type, SQLHANDLE hdbc, char *msg,
                      char *print) {
  SQLCHAR SqlState[6], Msg[SQL_MAX_MESSAGE_LENGTH];
  SQLINTEGER NativeError;
  SQLSMALLINT i = 1, MsgLen;

  SQLGetDiagRec(type, hdbc, i, SqlState, &NativeError, Msg, sizeof(Msg),
                &MsgLen);
  fprintf(stderr,
          "%% error in SQLConnect: %s got error code %s\n%% SQL Message: %s\n",
          print, SqlState, Msg);
  return FALSE;
}

static int SQLALLOCHANDLE(SQLSMALLINT HandleType, SQLHANDLE hdbc,
                          SQLHANDLE *outHandle, char *print) {
  SQLRETURN retcode;

  retcode = SQLAllocHandle(HandleType, hdbc, outHandle);
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
    return odbc_error(HandleType, hdbc, "SQLAllocHandle(ENV)", print);
  }
  return TRUE;
}

static int SQLSETENVATTR(SQLHENV henv, SQLINTEGER att, SQLPOINTER p,
                         SQLINTEGER len, char *print) {
  SQLRETURN retcode;

  retcode = SQLSetEnvAttr(henv, att, p, len);
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
    return odbc_error(SQL_HANDLE_ENV, henv, "SQLSetEnvAttr", print);
  }
  return TRUE;
}

static int SQLCONNECT(SQLHDBC hdbc, SQLCHAR *driver, SQLCHAR *user,
                      SQLCHAR *password, char *print) {
  SQLRETURN retcode;

  retcode = SQLConnect(hdbc, driver, SQL_NTS, user, SQL_NTS, password, SQL_NTS);
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_DBC, hdbc, "SQLConnect", print);
  return TRUE;
}

static int SQLEXECDIRECT(SQLHSTMT StatementHandle, SQLCHAR *StatementText,
                         char *print) {
  SQLRETURN retcode;
  retcode = SQLExecDirect(StatementHandle, StatementText, SQL_NTS);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, StatementHandle, "SQLExecDirect", print);
  return TRUE;
}

static int SQLDESCRIBECOL(SQLHSTMT sth, SQLSMALLINT colno, SQLCHAR *colname,
                          SQLSMALLINT bflength, SQLSMALLINT *nmlengthp,
                          SQLSMALLINT *dtptr, SQLULEN *colszptr,
                          SQLSMALLINT *ddptr, SQLSMALLINT *nullableptr,
                          char *print) {
  SQLRETURN retcode;
  retcode = SQLDescribeCol(sth, colno, colname, bflength, nmlengthp, dtptr,
                           colszptr, ddptr, nullableptr);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, sth, "SQLDescribeCol", print);
  return TRUE;
}

static int SQLSETCONNECTATTR(SQLHDBC hdbc, SQLINTEGER attr, SQLPOINTER vptr,
                             SQLINTEGER slen, char *print) {
  SQLRETURN retcode;
  retcode = SQLSetConnectAttr(hdbc, attr, vptr, slen);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, hdbc, "SQLSetConnectAttr", print);
  return TRUE;
}

static int SQLBINDCOL(SQLHSTMT sthandle, SQLUSMALLINT colno, SQLSMALLINT tt,
                      SQLPOINTER tvptr, SQLLEN blen, SQLLEN *strl,
                      char *print) {
  SQLRETURN retcode;
  retcode = SQLBindCol(sthandle, colno, tt, tvptr, blen, strl);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, sthandle, "SQLBindCol", print);
  return TRUE;
}

static int SQLNUMRESULTCOLS(SQLHSTMT sthandle, SQLSMALLINT *ncols,
                            char *print) {
  SQLRETURN retcode;
  retcode = SQLNumResultCols(sthandle, ncols);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, sthandle, "SQLNumResultCols", print);
  return TRUE;
}

static int SQLCLOSECURSOR(SQLHSTMT sthandle, char *print) {
  SQLRETURN retcode;
  retcode = SQLCloseCursor(sthandle);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, sthandle, "SQLCloseCursor", print);
  return TRUE;
}

#define SQLFETCH(A, print)                                                     \
  {                                                                            \
    SQLRETURN retcode;                                                         \
    retcode = SQLFetch(A);                                                     \
    if (retcode == SQL_NO_DATA)                                                \
      break;                                                                   \
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {          \
      printf("Error in SQLFETCH: %s\n", print);                                \
      return FALSE;                                                            \
    }                                                                          \
  }

static int SQLGETDATA(SQLHSTMT sthandle, SQLUSMALLINT Col_or_Param_Num,
                      SQLSMALLINT TargetType, SQLPOINTER TargetValuePtr,
                      SQLLEN BufferLength, SQLLEN *StrLen_or_IndPtr,
                      char *print) {
  SQLRETURN retcode;
  retcode = SQLGetData(sthandle, Col_or_Param_Num, TargetType, TargetValuePtr,
                       BufferLength, StrLen_or_IndPtr);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, sthandle, "SQLGetData", print);
  return TRUE;
}

static int SQLDISCONNECT(SQLHSTMT sthandle, char *print) {
  SQLRETURN retcode;
  retcode = SQLDisconnect(sthandle);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_DBC, sthandle, "SQLDisconnect", print);
  return TRUE;
}

static int SQLFREEHANDLE(SQLSMALLINT HandleType, SQLHANDLE Handle,
                         char *print) {
  SQLRETURN retcode;
  retcode = SQLFreeHandle(HandleType, Handle);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(HandleType, Handle, "SQLDisconnect", print);
  return TRUE;
}

static int SQLPRIMARYKEYS(SQLHSTMT StatementHandle, SQLCHAR *CatalogName,
                          SQLSMALLINT NameLength1, SQLCHAR *SchemaName,
                          SQLSMALLINT NameLength2, SQLCHAR *TableName,
                          SQLSMALLINT NameLength3, char *print) {
  SQLRETURN retcode;
  retcode = SQLPrimaryKeys(StatementHandle, CatalogName, NameLength1,
                           SchemaName, NameLength2, TableName, NameLength3);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, StatementHandle, "SQLPrimaryKeys",
                      print);
  return TRUE;
}

/********************************************
 NOT IN USE
static int SQLGETTYPEINFO(SQLHSTMT      StatementHandle,
                          SQLSMALLINT   DataType,
                          char *          print)
{
  SQLRETURN retcode;
  retcode = SQLGetTypeInfo(StatementHandle, DataType);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, StatementHandle, "SQLGetTypeInfo",
print);
  return TRUE;
}
********************************************/

static int SQLCOLATTRIBUTE(SQLHSTMT StatementHandle, SQLUSMALLINT ColumnNumber,
                           SQLUSMALLINT FieldIdentifier,
                           SQLPOINTER CharacterAttributePtr,
                           SQLSMALLINT BufferLength,
                           SQLSMALLINT *StringLengthPtr,
                           SQLLEN *NumericAttributePtr, char *print) {
  SQLRETURN retcode;
  retcode = SQLColAttribute(StatementHandle, ColumnNumber, FieldIdentifier,
                            CharacterAttributePtr, BufferLength,
                            StringLengthPtr, NumericAttributePtr);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return odbc_error(SQL_HANDLE_STMT, StatementHandle, "SQLColAttribute",
                      print);
  return TRUE;
}

/* Verificar tipo de dados*/
#define IS_SQL_INT(FIELD)                                                      \
  FIELD == SQL_DECIMAL || FIELD == SQL_NUMERIC || FIELD == SQL_SMALLINT ||     \
      FIELD == SQL_INTEGER || FIELD == SQL_TINYINT || FIELD == SQL_BIGINT

#define IS_SQL_FLOAT(FIELD)                                                    \
  FIELD == SQL_FLOAT || FIELD == SQL_DOUBLE || FIELD == SQL_REAL

static Int c_db_odbc_connect(USES_REGS1) {
  Term arg_driver = Deref(ARG1);
  Term arg_user = Deref(ARG2);
  Term arg_passwd = Deref(ARG3);
  Term arg_conn = Deref(ARG4);

  MYDDAS_UTIL_CONNECTION new = NULL;

  const char *driver = AtomName(AtomOfTerm(arg_driver));
  const char *user = AtomName(AtomOfTerm(arg_user));
  const char *passwd = AtomName(AtomOfTerm(arg_passwd));

  SQLHENV henv;
  SQLHDBC hdbc;

  /*Allocate environment handle */
  if (!SQLALLOCHANDLE(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv, "connect"))
    return FALSE;
  /* Set the ODBC version environment attribute */
  if (!SQLSETENVATTR(henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, 0,
                     "connect"))
    return FALSE;
  /* Allocate connection handle */
  if (!SQLALLOCHANDLE(SQL_HANDLE_DBC, henv, &hdbc, "connect"))
    return FALSE;
  /* Set login timeout to 6 seconds. */
  if (!SQLSETCONNECTATTR(hdbc, SQL_LOGIN_TIMEOUT, (SQLPOINTER)6, 0, "connect"))
    return FALSE;
  /* Connect to data source */
  if (!SQLCONNECT(hdbc, (SQLCHAR *)driver, (SQLCHAR *)user, (SQLCHAR *)passwd,
                  "connect"))
    return FALSE;
  if (!Yap_unify(arg_conn, MkIntegerTerm((Int)(hdbc))))
    return FALSE;
  else {
    /* Criar um novo no na lista de ligacoes*/
    // new = add_connection(&TOP,hdbc,henv);
    new = myddas_util_add_connection(hdbc, henv, MYDDAS_ODBC);
    if (new == NULL) {
      fprintf(stderr, "Error: could not allocate list memory\n");
      return FALSE;
    }
    return TRUE;
  }
}

/* db_query: SQLQuery x ResultSet x Arity x BindList x Connection */
static Int c_db_odbc_query(USES_REGS1) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_arity = Deref(ARG3);
  Term arg_bind_list = Deref(ARG4);
  Term arg_conn = Deref(ARG5);

  SQLCHAR *sql = (SQLCHAR *)AtomName(AtomOfTerm(arg_sql_query));

  SQLHDBC hdbc = (SQLHDBC)(IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;
  SQLSMALLINT type;
  Int arity;
  Int i;

  /*Allocate an handle for the query*/
  if (!SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_query"))
    return FALSE;
  /* Executes the query*/
  if (!SQLEXECDIRECT(hstmt, sql, "db_query"))
    return FALSE;

  if (IsNonVarTerm(arg_arity)) {
    arity = IntegerOfTerm(arg_arity);

    char *bind_space = NULL;

    // const Int functor_arity=3;
    const Short functor_arity = 3;
    Functor functor = Yap_MkFunctor(Yap_LookupAtom("bind"), functor_arity);
    Term properties[functor_arity];

    Term head, list = arg_bind_list;

    SQLULEN ColumnSizePtr;
    SQLLEN *data_info = NULL;

    for (i = 1; i <= arity; i++) {
      head = HeadOfTerm(list);
      list = TailOfTerm(list);

      if (!SQLDESCRIBECOL(hstmt, i, NULL, 0, NULL, &type, &ColumnSizePtr, NULL,
                          NULL, "db_query"))
        return FALSE;

      /* +1 because of '\0' */
      bind_space = malloc(sizeof(char) * (ColumnSizePtr + 1));
      data_info = malloc(sizeof(SQLINTEGER));
      if (!SQLBINDCOL(hstmt, i, SQL_C_CHAR, bind_space, (ColumnSizePtr + 1),
                      data_info, "db_query")) {
        return FALSE;
      }

      properties[0] = MkIntegerTerm((Int)bind_space);
      properties[2] = MkIntegerTerm((Int)data_info);

      if (IS_SQL_INT(type))
        properties[1] = MkAtomTerm(Yap_LookupAtom("integer"));
      else if (IS_SQL_FLOAT(type))
        properties[1] = MkAtomTerm(Yap_LookupAtom("real"));
      else
        properties[1] = MkAtomTerm(Yap_LookupAtom("string"));

      Yap_unify(head, Yap_MkApplTerm(functor, functor_arity, properties));
      continue;
    }
  }

  if (!Yap_unify(arg_result_set, MkIntegerTerm((Int)hstmt))) {
    if (!SQLCLOSECURSOR(hstmt, "db_query"))
      return FALSE;
    if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_query"))
      return FALSE;
    return FALSE;
  }
  return TRUE;
}

static Int c_db_odbc_number_of_fields(USES_REGS1) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);

  const char *relation = AtomName(AtomOfTerm(arg_relation));

  SQLHDBC hdbc = (SQLHDBC)(IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;

  char sql[256];
  SQLSMALLINT number_fields;

  sprintf(sql, "SELECT column_name from INFORMATION_SCHEMA.COLUMNS where "
               "table_name = \'%s\' GROUP BY column_name, dtd_identifier ORDER "
               "BY CAST(dtd_identifier AS INTEGER)",
          relation);

  if (!SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_number_of_fields"))
    return FALSE;
  if (!SQLEXECDIRECT(hstmt, (SQLCHAR *)sql, "db_number_of_fields"))
    return FALSE;

  /* Calcula o numero de campos*/
  number_fields = 0;
  while (TRUE) {
    SQLFETCH(hstmt, "db_number_of_fields");
    number_fields++;
  }

  if (!SQLCLOSECURSOR(hstmt, "db_number_of_fields"))
    return FALSE;
  if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_number_of_fields"))
    return FALSE;

  if (!Yap_unify(arg_fields, MkIntegerTerm(number_fields)))
    return FALSE;
  return TRUE;
}

/* db_get_attributes_types: RelName x Connection -> TypesList */
static Int c_db_odbc_get_attributes_types(USES_REGS1) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_types_list = Deref(ARG3);

  const char *relation = AtomName(AtomOfTerm(arg_relation));
  SQLHDBC hdbc = (SQLHDBC)(IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;

  char sql[256];
  Term head, list;
  list = arg_types_list;

  sprintf(sql, "SELECT column_name,data_type from INFORMATION_SCHEMA.COLUMNS "
               "WHERE table_name = \'%s\' GROUP BY column_name, dtd_identifier "
               "ORDER BY CAST(dtd_identifier AS INTEGER)",
          relation);

  if (!SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt, "db_get_attributes_types"))
    return FALSE;
  if (!SQLEXECDIRECT(hstmt, (SQLCHAR *)sql, "db_get_attributes_types"))
    return FALSE;

  while (TRUE) {
    SQLFETCH(hstmt, "db_get_attributes_types");

    /* Tentar fazer de uma maneira que a gente consiga calcular o tamanho que o
     nome do campo vai ocupar, assim podemos alocar memoria dinamicamente*/
    sql[0] = '\0';
    if (!SQLGETDATA(hstmt, 1, SQL_C_CHAR, sql, 256, NULL,
                    "db_get_attributes_types"))
      return FALSE;

    head = HeadOfTerm(list);
    Yap_unify(head, MkAtomTerm(Yap_LookupAtom(sql)));
    list = TailOfTerm(list);
    head = HeadOfTerm(list);
    list = TailOfTerm(list);

    sql[0] = '\0';
    if (!SQLGETDATA(hstmt, 2, SQL_C_CHAR, sql, 256, NULL,
                    "db_get_attributes_types"))
      return FALSE;

    if (strncmp(sql, "smallint", 8) == 0 || strncmp(sql, "int", 3) == 0 ||
        strncmp(sql, "mediumint", 9) == 0 || strncmp(sql, "tinyint", 7) == 0 ||
        strncmp(sql, "bigint", 6) == 0 || strcmp(sql, "year") == 0)
      Yap_unify(head, MkAtomTerm(Yap_LookupAtom("integer")));
    else if (strcmp(sql, "float") == 0 || strncmp(sql, "double", 6) == 0 ||
             strcmp(sql, "real") == 0)
      Yap_unify(head, MkAtomTerm(Yap_LookupAtom("real")));
    else
      Yap_unify(head, MkAtomTerm(Yap_LookupAtom("string")));
  }

  if (!SQLCLOSECURSOR(hstmt, "db_get_attributes_types"))
    return FALSE;
  if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_get_attributes_types"))
    return FALSE;
  return TRUE;
}

/* db_disconnect */
static Int c_db_odbc_disconnect(USES_REGS1) {
  Term arg_conn = Deref(ARG1);

  SQLHDBC conn = (SQLHDBC)(IntegerOfTerm(arg_conn));
  SQLHENV henv = myddas_util_get_odbc_enviromment(conn);

  if ((myddas_util_search_connection(conn)) != NULL) {
    myddas_util_delete_connection(conn);
    /* More information about this process on
       msdn.microsoft.com*/
    if (!SQLDISCONNECT(conn, "db_disconnect"))
      return FALSE;
    if (!SQLFREEHANDLE(SQL_HANDLE_DBC, conn, "db_disconnect"))
      return FALSE;
    if (!SQLFREEHANDLE(SQL_HANDLE_ENV, henv, "db_disconnect"))
      return FALSE;

    return TRUE;
  } else
    return FALSE;
}

static Int c_db_odbc_row_cut(USES_REGS1) {

  SQLHSTMT hstmt = (SQLHSTMT)IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term, 1));

  if (!SQLCLOSECURSOR(hstmt, "db_row_cut"))
    return FALSE;
  if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_row_cut"))
    return FALSE;

  return TRUE;
}

static int release_list_args(Term arg_list_args, Term arg_bind_list,
                             const char *error_msg) {
  Term list = arg_list_args;
  Term list_bind = arg_bind_list;

  while (IsPairTerm(list_bind)) {
    Term head_bind = HeadOfTerm(list_bind);

    list = TailOfTerm(list);
    list_bind = TailOfTerm(list_bind);

    free((char *)IntegerOfTerm(ArgOfTerm(1, head_bind)));
    free((SQLINTEGER *)IntegerOfTerm(ArgOfTerm(3, head_bind)));
  }
  return TRUE;
}

/* db_row: ResultSet x BindList x ListOfArgs -> */
static Int c_db_odbc_row(USES_REGS1) {
  Term arg_result_set = Deref(ARG1);
  Term arg_bind_list = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);

  SQLHSTMT hstmt = (SQLHSTMT)IntegerOfTerm(arg_result_set);

  /* EXTRA_CBACK_ARG(ARIDADE,LOCAL_ONDE_COLOCAR_VALOR)*/
  EXTRA_CBACK_ARG(3, 1) = (CELL)MkIntegerTerm((Int)hstmt);

  Term head, list, null_atom[1];
  Term head_bind, list_bind;

  SQLRETURN retcode = SQLFetch(hstmt);
  if (retcode == SQL_NO_DATA) {
    if (!SQLCLOSECURSOR(hstmt, "db_row"))
      return FALSE;
    if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_row"))
      return FALSE;
    if (!release_list_args(arg_list_args, arg_bind_list, "db_row")) {
      return FALSE;
    }

    cut_fail();
    return FALSE;
  }
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
    printf("erro no SQLFETCH number of fields\n");
    return FALSE;
  }

  char *bind_value = NULL;
  Term type;

  list = arg_list_args;
  list_bind = arg_bind_list;
  SQLINTEGER *data_info = NULL;

  while (IsPairTerm(list_bind)) {
    head = HeadOfTerm(list);
    list = TailOfTerm(list);

    head_bind = HeadOfTerm(list_bind);
    list_bind = TailOfTerm(list_bind);

    bind_value = (char *)IntegerOfTerm(ArgOfTerm(1, head_bind));
    type = ArgOfTerm(2, head_bind);
    data_info = (SQLINTEGER *)IntegerOfTerm(ArgOfTerm(3, head_bind));

    if ((*data_info) == SQL_NULL_DATA) {
      null_atom[0] = MkIntegerTerm(null_id++);
      if (!Yap_unify(head,
                     Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"), 1), 1,
                                    null_atom)))
        continue;
    } else {

      if (!strcmp(AtomName(AtomOfTerm(type)), "integer")) {
        if (!Yap_unify(head, MkIntegerTerm(atol(bind_value))))
          continue;
      } else if (!strcmp(AtomName(AtomOfTerm(type)), "real")) {
        if (!Yap_unify(head, MkFloatTerm(atof(bind_value))))
          continue;
      } else if (!strcmp(AtomName(AtomOfTerm(type)), "string")) {
        if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(bind_value))))
          continue;
      }
    }
  }
  return TRUE;
}

/* Mudar esta funcao de forma a nao fazer a consulta, pois
 no predicate db_sql_selet vai fazer duas vezes a mesma consutla*/
static Int c_db_odbc_number_of_fields_in_query(USES_REGS1) {
  Term arg_query = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);

  const char *sql = AtomName(AtomOfTerm(arg_query));

  SQLHDBC hdbc = (SQLHDBC)(IntegerOfTerm(arg_conn));
  SQLHSTMT hstmt;
  SQLSMALLINT number_cols = 0;

  if (!SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt,
                      "db_number_of_fields_in_query"))
    return FALSE;
  if (!SQLEXECDIRECT(hstmt, (SQLCHAR *)sql, "db_number_of_fields_in_query"))
    return FALSE;

  if (!SQLNUMRESULTCOLS(hstmt, &number_cols, "db_number_of_fields_in_query"))
    return FALSE;

  if (!Yap_unify(arg_fields, MkIntegerTerm(number_cols))) {
    if (!SQLCLOSECURSOR(hstmt, "db_number_of_fields_in_query"))
      return FALSE;
    if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_number_of_fields_in_query"))
      return FALSE;

    return FALSE;
  }

  if (!SQLCLOSECURSOR(hstmt, "db_number_of_fields_in_query"))
    return FALSE;
  if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt, "db_number_of_fields_in_query"))
    return FALSE;

  return TRUE;
}

#ifdef MYDDAS_ODBC
/* This function searches the MYDDAS list for odbc connections
 If there isn't any, it returns NULL. This is a nice way to know
 if there is any odbc connections left on the list*/
SQLHENV
myddas_util_get_odbc_enviromment(SQLHDBC connection) {
  CACHE_REGS
  MYDDAS_UTIL_CONNECTION top =
      Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_top_connections;

  for (; top != NULL; top = top->next)
    if (top->connection == ((void *)connection))
      return top->odbc_enviromment;

  return NULL;
}
#endif

static Int c_db_odbc_get_fields_properties(USES_REGS1) {
  Term nome_relacao = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term fields_properties_list = Deref(ARG3);
  Term head, list;

  SQLCHAR *relacao = (SQLCHAR *)AtomName(AtomOfTerm(nome_relacao));
  char sql[256];
  char name[200];
  Int i;

  SQLSMALLINT num_fields = 0;
  SQLSMALLINT NullablePtr = 0;
  SQLLEN AutoIncrementPointer = 0;
  SQLHSTMT hstmt, hstmt2;
  SQLHDBC hdbc = (SQLHDBC)(IntegerOfTerm(arg_conn));

  /* LIMIT 0 -> We don't need the results of the query,
     only the information about the fields of the relation*/
  sprintf(sql, "SELECT * FROM `%s` LIMIT 0", relacao);

  /*Allocate an handle for the query*/
  if (!SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt,
                      "db_get_fields_properties"))
    return FALSE;
  /* Executes the query*/
  if (!SQLEXECDIRECT(hstmt, (SQLCHAR *)sql, "db_get_fields_properties"))
    return FALSE;

  Functor functor = Yap_MkFunctor(Yap_LookupAtom("property"), 4);
  Term properties[4];

  if (!SQLNUMRESULTCOLS(hstmt, &num_fields, "db_get_fields_properties"))
    return FALSE;

  list = fields_properties_list;

  SQLSMALLINT bind_prim_key;
  // rows in odbc start at 1 :)
  Short *null = (Short *)malloc(sizeof(Short) * (1 + num_fields));

  if (!SQLALLOCHANDLE(SQL_HANDLE_STMT, hdbc, &hstmt2,
                      "db_get_fields_properties"))
    return FALSE;
  /* Executes the query*/
  if (!SQLPRIMARYKEYS(hstmt2, NULL, 0, NULL, 0, relacao, SQL_NTS,
                      "db_get_fields_properties"))
    return FALSE;
  /* Associates bind value for the 5 column*/
  if (!SQLBINDCOL(hstmt2, 5, SQL_C_SSHORT, &bind_prim_key, sizeof(SQLSMALLINT),
                  NULL, "db_get_fields_properties"))
    return FALSE;

  while (1) {
    SQLFETCH(hstmt2, "db_get_fields_properties");
    null[bind_prim_key] = 1;
  }

  if (!SQLCLOSECURSOR(hstmt2, "db_get_fields_properties"))
    return FALSE;
  if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt2, "db_get_fields_properties"))
    return FALSE;

  for (i = 1; i <= num_fields; i++) {
    head = HeadOfTerm(list);
    name[0] = '\0';
    SQLDESCRIBECOL(hstmt, i, (SQLCHAR *)name, 200, NULL, NULL, NULL, NULL,
                   &NullablePtr, "db_get_fields_properties");

    if (!SQLCOLATTRIBUTE(hstmt, i, SQL_DESC_AUTO_UNIQUE_VALUE, NULL, 0, NULL,
                         &AutoIncrementPointer, "db_get_fields_properties"))
      return FALSE;

    properties[0] = MkAtomTerm(Yap_LookupAtom(name));

    if (NullablePtr & SQL_NULLABLE)
      properties[1] = MkIntegerTerm(1); // Can't be NULL
    else
      properties[1] = MkIntegerTerm(0);

    if (null[i] == 1)
      properties[2] = MkIntegerTerm(1); // It''s a primary key
    else
      properties[2] = MkIntegerTerm(0);

    if (AutoIncrementPointer & SQL_TRUE)
      properties[3] = MkIntegerTerm(1); // It's auto_incremented field
    else
      properties[3] = MkIntegerTerm(0);

    list = TailOfTerm(list);
    if (!Yap_unify(head, Yap_MkApplTerm(functor, 4, properties))) {
      return FALSE;
    }
  }

  if (!SQLCLOSECURSOR(hstmt, "db_get_fields_properties"))
    return FALSE;
  if (!SQLFREEHANDLE(SQL_HANDLE_STMT, hstmt2, "db_get_fields_properties"))
    return FALSE;
  return TRUE;
}

void Yap_InitMYDDAS_ODBCPreds(void) {
  /* db_connect: Host x User x Passwd x Database x Connection */
  Yap_InitCPred("c_db_odbc_connect", 4, c_db_odbc_connect, 0);

  /* db_number_of_fields: Relation x Connection x NumberOfFields */
  Yap_InitCPred("c_db_odbc_number_of_fields", 3, c_db_odbc_number_of_fields, 0);

  /* db_number_of_fields_in_query: SQLQuery x Connection x NumberOfFields */
  Yap_InitCPred("c_db_odbc_number_of_fields_in_query", 3,
                c_db_odbc_number_of_fields_in_query, 0);

  /* db_get_attributes_types: Relation x TypesList */
  Yap_InitCPred("c_db_odbc_get_attributes_types", 3,
                c_db_odbc_get_attributes_types, 0);

  /* db_query: SQLQuery x ResultSet x Connection */
  Yap_InitCPred("c_db_odbc_query", 5, c_db_odbc_query, 0);

  /* db_disconnect: Connection */
  Yap_InitCPred("c_db_odbc_disconnect", 1, c_db_odbc_disconnect, 0);

  /* db_get_fields_properties: PredName x Connnection x PropertiesList */
  Yap_InitCPred("c_db_odbc_get_fields_properties", 3,
                c_db_odbc_get_fields_properties, 0);
}

void Yap_InitBackMYDDAS_ODBCPreds(void) {

  /*  db_row: ResultSet x ListOfArgs */
  Yap_InitCPredBackCut("c_db_odbc_row", 3, sizeof(Int), c_db_odbc_row,
                       c_db_odbc_row, c_db_odbc_row_cut, 0);
}
#else

void Yap_InitMYDDAS_ODBCPreds(void) {}
void Yap_InitBackMYDDAS_ODBCPreds(void) {}

#endif

void init_odbc( void )
{
    Yap_InitMYDDAS_ODBCPreds();
    Yap_InitBackMYDDAS_ODBCPreds();
}


#ifdef _WIN32

#include <windows.h>

int WINAPI win_odbc(HANDLE hinst, DWORD reason, LPVOID reserved);

int WINAPI win_odbc(HANDLE hinst, DWORD reason, LPVOID reserved) {
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    break;
  case DLL_PROCESS_DETACH:
    break;
  case DLL_THREAD_ATTACH:
    break;
  case DLL_THREAD_DETACH:
    break;
  }
  return 1;
}
#endif
/*MYDDAS_ODBC*/
