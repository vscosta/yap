#ifndef _DBIO_H_
#define _DBIO_H_

#include "pred.h"
#ifdef TUFFY
#include <libpq-fe.h>
#endif
#ifdef ROCKIT
#include <mysql/mysql.h>
#endif
#include <vector>
#include "lista.h"

using namespace std;

#ifdef TUFFY
void postgresRead(PGconn **ret, vector<gpunode> *L, int *inpquery, char *names, int finalDR);
void postgresWrite(int *inpquery, int ninpf, vector<rulenode>::iterator rul_str, vector<rulenode>::iterator fin, vector<gpunode> *L, PGconn *conn, int finalDR);
#endif
#ifdef ROCKIT
void mysqlRead(MYSQL **ret, int *qrs, vector<gpunode> *L, int ninpf, char *names, int finalDR);
void mysqlWrite(vector<rulenode>::iterator rul_str, vector<rulenode>::iterator fin, vector<gpunode> *L, MYSQL *con);
#endif
#ifdef DATALOG
void datalogWrite(int query, vector<rulenode>::iterator rul_str, vector<rulenode>::iterator fin, int finalDR, int **result);
#endif

#endif
