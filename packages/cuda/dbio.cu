#include <iostream>
#include <algorithm>
#include <stdio.h>
#include "memory.h"
#include "union2.h"
#include "dbio.h"

#ifdef DATALOG
//template<class InputIterator>
//void datalogWrite(int query, InputIterator rul_str, InputIterator fin, int finalDR, int **result)
void datalogWrite(int query, vector<rulenode>::iterator rul_str, vector<rulenode>::iterator fin, int finalDR, int **result)
{
	rulenode tmprule;
	vector<rulenode>::iterator qposr;
	int *dop1, *hres;
	int cols1, res_rows, tipo;
	tmprule.name = query;
	qposr = lower_bound(rul_str, fin, tmprule, comparer);
	cols1 = qposr->num_columns;
	res_rows = cargafinal(query, cols1, &dop1);

	if(res_rows != 0)
	{	
		if(res_rows > 0)
		{
			if(finalDR)
				res_rows = unir(dop1, res_rows, cols1, &dop1, 0);
			tipo = res_rows * cols1 * sizeof(int);
			hres = (int *)malloc(tipo);
			hipMemcpy(hres, dop1, tipo, hipMemcpyDeviceToHost);
			hipFree(dop1);
			*result = hres;
		}
		else
		{
			res_rows *= -1;
			if(finalDR)
			{
				int *dop2;
				tipo = res_rows * cols1 * sizeof(int);
				reservar(&dop2, tipo); 
				hipMemcpy(dop2, dop1, tipo, hipMemcpyHostToDevice);
				free(dop1);
				res_rows = unir(dop2, res_rows, cols1, &dop2, 0);
				tipo = res_rows * cols1 * sizeof(int);
				hres = (int *)malloc(tipo);
				hipMemcpy(hres, dop2, tipo, hipMemcpyDeviceToHost);
				hipFree(dop2);
				*result = hres;
			}
			else
				*result = dop1;
		}
	}
}
#endif

#ifdef TUFFY
void postgresRead(PGconn **ret, vector<gpunode> *L, int *inpquery, char *names, int finalDR)
{
	PGresult *pgr;
	int x, y;
	int *mat, *mat2;
	char *tok, sel[1024], **qrs;
	int w, z = 0, numt, numc, numc2, start = 0, start2, val;
	PGconn *conn = PQconnectdb("host=localhost port=5432 dbname = prueba user=tuffer password=root");
	if(PQstatus(conn) != CONNECTION_OK)
    	{
        	fprintf(stderr, "Connection to database failed: %s", PQerrorMessage(conn));
		exit(1);
        }

	pgr = PQexec(conn, "Select nspname from pg_catalog.pg_namespace where oid = (select max(oid) from pg_catalog.pg_namespace)");
	sprintf(sel, "SET search_path = %s", PQgetvalue(pgr, 0, 0)); 
	PQclear(pgr);
	PQexec(conn, sel);
	tok = strtok(names, " ");	
	if(finalDR)
	{
		qrs = (char **)malloc(100 * sizeof(char *));
		while(tok != NULL)
		{
			sprintf(sel, "Select * from %s limit 0", tok);
			pgr = PQexec(conn, sel);
			numc = L->at(z).num_columns;
			if(tok[0] == 'c')
			{
				sprintf(sel, "Select ");
				numt = numc + 1;
				for(x = 1; x < numt; x++)
				{
					strcat(sel, PQfname(pgr, x));
					strcat(sel, ", ");
				}
				sel[strlen(sel)-2] = '\0';
				sprintf(sel, "%s from %s", sel, tok);
			}
			else
			{
				sprintf(sel, "Select id, Club, ");
				numt = numc + 6;
				for(x = 8; x < numt; x++)
				{
					strcat(sel, PQfname(pgr, x));
					strcat(sel, ", ");
				}
				sel[strlen(sel)-2] = '\0';
				sprintf(sel, "%s from %s", sel, tok);
			}
			PQclear(pgr);
			pgr = PQexec(conn, sel);
			numt = PQntuples(pgr);
			mat = (int *)malloc(numt * numc * sizeof(int));
			if(tok[0] == 'c')
			{
				for(x = 0; x < numt; x++)
				{
					start = x * numc;
					for(y = 0; y < numc; y++)
						mat[start + y] = atoi(PQgetvalue(pgr, x, y));
				}
			}
			else
			{
				numc2 = numc - 2;
				mat2 = (int *)malloc(numt * numc2 * sizeof(int));
				start = 0;
				start2 = 0;
				for(x = 0; x < numt; x++)
				{
					w = atoi(PQgetvalue(pgr, x, 1));
					if(w < 2)
					{
						mat[start] = atoi(PQgetvalue(pgr, x, 0));
						start++;
						mat[start] = w;
						start++;
						if(w > 0)
						{
							for(y = 2; y < numc; y++)
							{
								val = atoi(PQgetvalue(pgr, x, y));
								mat[start] = val;
								mat2[start2] = val;
								start++;
								start2++;
							}
						}
						else
						{
							for(y = 2; y < numc; y++)
							{
								val = atoi(PQgetvalue(pgr, x, y));
								mat[start] = val;
								start++;
							}
						}
					}
					else
					{
						for(y = 2; y < numc; y++)
						{
							val = atoi(PQgetvalue(pgr, x, y));
							mat2[start2] = val;
							start2++;
						}
					}
				}
				L->at(z+1).address_host_table = mat2;
				L->at(z+1).num_rows = start2 / numc2;
			}
			L->at(z).address_host_table = mat;
			L->at(z).num_rows = start / numc;
			PQclear(pgr);
			
			x = 1;
			while(inpquery[x] != -1)
			{
				if(L->at(z).name == inpquery[x])
				{
					numt = (strlen(tok) + 1) * sizeof(char);
					qrs[x] = (char *)malloc(numt);
					memmove(qrs[x], tok, numt);
				}
				x += 2;
			}
			if(tok[0] == 'c')
			{
				tok = strtok(NULL, " ");
				z++;
			}
			else
			{
				strtok(NULL, " ");	
				tok = strtok(NULL, " ");
				z += 2;
			}
		}
	}
	else
	{
		while(tok != NULL)
		{
			sprintf(sel, "Select * from %s limit 0", tok);
			pgr = PQexec(conn, sel);
			numc = L->at(z).num_columns;
			if(tok[0] == 'c')
			{
				sprintf(sel, "Select weight, myid, ");
				start = 1;
				numt = numc + 1;
			}
			else
			{
				sprintf(sel, "Select truth, Club, atomID, ");
				start = 8;
				numt = numc + 5;
			}
			for(x = start; x < numt; x++)
			{
				strcat(sel, PQfname(pgr, x));
				strcat(sel, ", ");
			}
			sel[strlen(sel)-2] = '\0';
			sprintf(sel, "%s from %s", sel, tok);
			PQclear(pgr);
			pgr = PQexec(conn, sel);
			numt = PQntuples(pgr);
			mat = (int *)malloc(numt * numc * sizeof(int)); 
			L->at(z).weight = (double *)malloc(numt * sizeof(double));
			L->at(z).num_rows = numt;

			for(x = 0; x < numt; x++)
			{	
				start = x * numc;
				for(y = 1; y < numc; y++)
					mat[start + y] = atoi(PQgetvalue(pgr, x, y));
			}

			numt *= numc;
			double flo;
			if(tok[0] == 'c')
			{
				for(x = 0, y = 0; x < numt; x+=numc, y++)
				{
					flo = atof(PQgetvalue(pgr, y, 0));
					L->at(z).weight[y] = flo;
					if(flo > 0)
						mat[x] = y + 1;
					else
						mat[x] = -y - 1;
				}
			}
			else
			{
				for(x = 0, y = 0; x < numt; x+=numc, y++)
				{
					if(PQgetvalue(pgr, y, 0)[0] == 't')
						mat[x] = 2;
					else
						mat[x] = 1;
				}				
			}
			L->at(z).address_host_table = mat;
			numc = (strlen(tok) + 1) * sizeof(char);
			L->at(z).predname = (char *)malloc(numc);
			memmove(L->at(z).predname, tok, numc);
			PQclear(pgr);
			tok = strtok(NULL, " ");
			z++;
		}
	}
	*ret = conn;
}

void postgresWrite(int *inpquery, int ninpf, vector<rulenode>::iterator rul_str, vector<rulenode>::iterator fin, vector<gpunode> *L, PGconn *conn, int finalDR)
{
	char sel[1024];
	double *matw = NULL;
	int qname, cols1, res_rows, tipo, *dop1;
	int x, w, z, y, *hres;
	rulenode tmprule;
	vector<rulenode>::iterator qposr;
	if(finalDR)
	{
		char file[] = "/dev/shm/mln0_atoms.csv";
		z = 0;
		int seqid = 1;
		FILE *fp;
		fp = fopen(file, "w");
		if(fp == NULL)
		{
			cerr << "Failed to create main memory temporary file, attempting to use hardrive" << endl;
			sprintf(file, "./temp/mln0_atoms.csv");
			fp = fopen(file, "w");
			if(fp == NULL)
			{
				cerr << "Failed to create main memory temporary file" << endl;
				exit(1);
			}
		}
		while((qname = inpquery[z]) != -1)
		{
			tmprule.name = qname;
			qposr = lower_bound(rul_str, fin, tmprule, comparer);
			cols1 = qposr->num_columns;
			res_rows = cargafinal(qname, cols1, &dop1);

			if(res_rows != 0)
			{
				if(res_rows < 0)
					res_rows = unir(dop1, -res_rows, cols1, &dop1, 0);  /*duplicate elimination on result*/
				else
					res_rows = unir(dop1, res_rows, cols1, &dop1, finalDR);

				tipo = res_rows * cols1 * sizeof(int);
				hres = (int *)malloc(tipo);
				hipMemcpy(hres, dop1, tipo, hipMemcpyDeviceToHost);
				hipFree(dop1);
				w = z + 1;

				strtok(qposr->rulename, "_");
				strtok(NULL, "_");
				int prid = atoi(strtok(NULL, "_"));

				for(x = 0, w = 0; x < res_rows; x++, w+=2)
				{
					if(hres[w+1])
						fprintf(fp, "%d,%d,%d,true\n", seqid, hres[w], prid);
					else
						fprintf(fp, "%d,%d,%d,false\n", seqid, hres[w], prid);
					seqid++;
				}
				free(hres);
			}
			z += 2;
		}
		fclose(fp);
		sprintf(sel, "Copy mln0_atoms(atomid,tupleID,predID,isquery) from '%s' CSV", file);
		PQexec(conn, sel);
	}
	else
	{
		while(rul_str != fin)
		{
			cols1 = rul_str->num_columns;
			res_rows = cargafinal(rul_str->name, cols1, &dop1);
			if(res_rows == 0)
			{
				rul_str++;
				continue;
			}
			res_rows = abs(res_rows);
			tipo = res_rows * cols1 * sizeof(int);
			hres = (int *)malloc(tipo);
			hipMemcpy(hres, dop1, tipo, hipMemcpyDeviceToHost);
			hipFree(dop1);

			char file[] = "/dev/shm/buffer.csv";
			FILE *fp;
			fp = fopen(file, "w");
			if(fp == NULL)
			{
				cerr << "Failed to create main memory temporary file, attempting to use hardrive" << endl;
				sprintf(file, "./temp/buffer.csv");
				fp = fopen(file, "w");
				if(fp == NULL)
				{
					cerr << "Failed to create main memory temporary file" << endl;
					exit(1);
				}
			}

			if(rul_str->rulename[0] == 'z')
			{
				char *name = rul_str->rulename + 1;
				for(x = 0; x < ninpf; x++)
				{
					if(strncmp(L->at(x).predname, name, strlen(name)) == 0)
					{
						matw = L->at(x).weight;
						break;
					}
				}

				cols1 -= 3;
				for(x = 0, z = 0; x < res_rows; x++, z+=3)
				{
					for(y = 0; y < cols1; y++, z++)
						fprintf(fp, "%d,", hres[z]);
					fprintf(fp, "%d,%lf,%d\n", hres[z], matw[abs(hres[z+1])-1], hres[z+2]);
				}
				fclose(fp);
				sprintf(sel, "Copy %s from '%s' CSV", name, file);
				PQexec(conn, sel);
			}
			else
			{
				cols1--;
				for(x = 0, z = 0; x < res_rows; x++, z++)
				{
					for(y = 0; y < cols1; y++, z++)
						fprintf(fp, "%d,", hres[z]);
					fprintf(fp, "%d\n", hres[z]);
				}
				fclose(fp);
				sprintf(sel, "Copy %s from '%s' CSV", rul_str->rulename, file);
				PQexec(conn, sel);
			}
			free(hres);
			rul_str++;
		}
	}
	PQfinish(conn);
	if(finalDR)
		clear_memory_all();
}
#endif

#ifdef ROCKIT
void mysqlRead(MYSQL **ret, int *qrs, vector<gpunode> *L, int ninpf, char *names, int finalDR)
{
	char *tok, sel[1024];
	int w, x, y, z = 0, numt, numc;
	int *mat;
	MYSQL *con = mysql_init(NULL);
	if(con == NULL)
	{
		fprintf(stderr, "mysql_init() failed\n");
      		exit(1);
	}
	mysql_options(con, MYSQL_OPT_LOCAL_INFILE, NULL);
	mysql_real_connect(con, "localhost", "root", "root", "rockit", 0, NULL, 0);
	if(finalDR)
	{
		y = 0;
		while(qrs[y] != 0)
		{
			for(z = 0; z < ninpf; z++)
			{
				if(qrs[y] == L->at(z).name)
				{
					MYSQL_ROW row;
					sprintf(sel, "Select count(*) from %s", L->at(z).predname);
					mysql_query(con, sel);
					MYSQL_RES *result = mysql_store_result(con);
					row = mysql_fetch_row(result);
					numt = atoi(row[0]);
					mysql_free_result(result);

					if(numt != L->at(z).num_rows)
					{
						liberar(L->at(z).name);
						numc = L->at(z).num_columns;
						sprintf(sel, "Select * from %s", L->at(z).predname);
						mysql_query(con, sel);
						MYSQL_RES *result = mysql_store_result(con);
						mat = (int *)malloc(numt * numc * sizeof(int));
						w = 0;
						while ((row = mysql_fetch_row(result))) 
						{
							for(x = 0; x < numc; x++, w++)
								mat[w] = atoi(row[x]);
						}

						mysql_free_result(result);
						if(L->at(z).address_host_table != NULL)
							free(L->at(z).address_host_table);
						L->at(z).address_host_table = mat;
						L->at(z).num_rows = numt;
					}
				}
			}
			y++;
		}
	}
	else
	{
		tok = strtok(names, " ");
		while(tok != NULL)
		{
			numc = L->at(z).num_columns;
			sprintf(sel, "Select * from %s", tok);
			mysql_query(con, sel);
			MYSQL_RES *result = mysql_store_result(con);
			numt = mysql_num_rows(result);

			MYSQL_ROW row;
			mat = (int *)malloc(numt * numc * sizeof(int));
			w = 0;
			if(tok[0] == 'f' && tok[1] >= '0' && tok[1] <= '9')
			{
				while ((row = mysql_fetch_row(result))) 
				{
					for(x = 1; x <= numc; x++, w++)
						mat[w] = atoi(row[x]);
				}
			}
			else
			{
				while ((row = mysql_fetch_row(result))) 
				{
					for(x = 0; x < numc; x++, w++)
						mat[w] = atoi(row[x]);
				}
			}
			mysql_free_result(result);
			L->at(z).address_host_table = mat;
			L->at(z).num_rows = numt;

			numc = (strlen(tok) + 1) * sizeof(char);
			L->at(z).predname = (char *)malloc(numc);
			strcpy(L->at(z).predname, tok);
			tok = strtok(NULL, " ");
			z++;
		}
	}
	*ret = con;
}

void mysqlWrite(vector<rulenode>::iterator rul_str, vector<rulenode>::iterator fin, vector<gpunode> *L, MYSQL *con)
{
	int x, y, z, cols1, cols2, res_rows, tipo;
	int *hres, *dop1;
	char *id, *sign, *q1, *q2;
	char sel[1024], weight[1024];
	gpunode tmpfact;
	while(rul_str != fin)
	{
		cols1 = rul_str->num_columns;
		res_rows = cargafinal(rul_str->name, cols1, &dop1);
		id = strtok(rul_str->rulename, "_");
		sprintf(sel, "create table if not exists %s(weight double, ", id);
		for(x = 0; x < cols1; x++)
		{
			sprintf(weight, "a%d char(10), ", x);
			strcat(sel, weight);
		}
		sel[strlen(sel)-2] = ')';
		strcat(sel, "ENGINE = MEMORY DEFAULT CHARSET=latin1");
		mysql_query(con, sel);
		sprintf(sel, "truncate %s", id);
		mysql_query(con, sel);

		if(res_rows == 0)
		{
			rul_str++;
			continue;
		}

		if(res_rows > 0)
		{
			tmpfact = L->at(-rul_str->referencias[rul_str->num_rows - 2] - 1);
			sign = tmpfact.predname;
			tipo = res_rows * cols1 * sizeof(int);
			hres = (int *)malloc(tipo);
			hipMemcpy(hres, dop1, tipo, hipMemcpyDeviceToHost);
			if(sign[0] == 'f' && sign[1] >= '0' && sign[1] <= '9')
				sumar(tmpfact.name, dop1, cols1, res_rows);
		}
		else
		{
			hres = dop1;
			res_rows = -res_rows;
		}

		sign = strtok(NULL, "_");
		q1 = strtok(NULL, "_");
		q2 = strtok(NULL, "_");
		if(sign[0] == '0')
			sprintf(weight, "%s.%s", q1, q2);
		else
			sprintf(weight, "-%s.%s", q1, q2);

		FILE *fp;
		char file[512];
		sprintf(file, "/dev/shm/%s.tsv", id);
		fp = fopen(file, "w");
		if(fp == NULL)
		{
			cerr << "Failed to create main memory temporary file, attempting to use hardrive" << endl;
			sprintf(file, "./temp/%s.tsv", id);
			fp = fopen(file, "w");
		}
		
		cols2 = cols1 - 1;
		for(x = 0, z = 0; x < res_rows; x++, z++)
		{
			fprintf(fp, "%s\t", weight);
			for(y = 0; y < cols2; y++, z++)
				fprintf(fp, "%d\t", hres[z]);
			fprintf(fp, "%d\n", hres[z]);
		}
		fclose(fp);

		sprintf(sel, "LOAD DATA LOCAL INFILE '%s' INTO TABLE %s", file, id);
		mysql_query(con, sel);
		rul_str++;
	}
	mysql_close(con);
}
#endif

