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
* File:		myddas_mysql.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for comunicating with a mysql database system *
*									 *
*************************************************************************/

#include "Yap.h"
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include <myddas_util.h>

#ifdef MYDDAS_MYSQL
/* Auxilary function to table_write*/
static void n_print(Int, char);
#endif

/* Auxilary function to table_write*/
static void n_print(Int n, char c) {
  for (; n > 0; n--)
    printf("%c", c);
}

void myddas_util_table_write(MYSQL_RES *res_set) {

  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  Int i, f;

  if (mysql_num_rows(res_set) == 0) {
    printf("Empty Set\n");
    return;
  }

  f = mysql_num_fields(res_set);

  fields = mysql_fetch_field(res_set);
  for (i = 0; i < f; i++) {
    printf("+");
    if (strlen(fields[i].name) > fields[i].max_length)
      fields[i].max_length = strlen(fields[i].name);
    n_print(fields[i].max_length + 2, '-');
  }
  printf("+\n");

  for (i = 0; i < f; i++) {
    printf("|");
    printf(" %s ", fields[i].name);
    n_print(fields[i].max_length - strlen(fields[i].name), ' ');
  }
  printf("|\n");

  for (i = 0; i < f; i++) {
    printf("+");
    n_print(fields[i].max_length + 2, '-');
  }
  printf("+\n");

  while ((row = mysql_fetch_row(res_set)) != NULL) {
    for (i = 0; i < f; i++) {
      printf("|");
      if (row[i] != NULL) {
        printf(" %s ", row[i]);
        n_print(fields[i].max_length - strlen(row[i]), ' ');
      } else {
        printf(" NULL ");
        n_print(fields[i].max_length - 4, ' ');
      }
    }
    printf("|\n");
  }

  for (i = 0; i < f; i++) {
    printf("+");
    n_print(fields[i].max_length + 2, '-');
  }
  printf("+\n");
}
