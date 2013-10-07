#include <iostream>
#include <vector>
#include <algorithm>
#include "lista.h"
#include "memory.h"
extern "C" {
#include "pred.h"
}
#include "selectproyect.cu"
#include "treeb.cu"
#include "union2.cu"
#include "bpreds.cu"

#define MAXVALS 200

bool compare(const gpunode &r1, const gpunode &r2)
{
	return (r1.name > r2.name); 
}

bool comparecompleted(const compnode &r1, const compnode &r2)
{
	return (r1.name > r2.name); 
}

bool comparef(const gpunode &r1, const gpunode &r2)
{
	return (r1.is_fact > r2.is_fact); 
}

bool comparer(const rulenode &r1, const rulenode &r2)
{
	return (r1.name > r2.name); 
}

template<class InputIterator>
InputIterator buscar(InputIterator inicio, InputIterator rul_str, InputIterator fin, int name)
{
	vector<gpunode>::iterator i;
	gpunode temp;
	temp.name = name;
	i = lower_bound(inicio, rul_str, temp, compare);
	if (i != rul_str && i->name == name)
        	return i;
	i = lower_bound(rul_str, fin, temp, compare);
	if (i != rul_str && i->name == name)
        	return i;
	return fin;
}

void buscarreglas(vector<gpunode> *facts, vector<rulenode> *rules)
{
	vector<gpunode>::iterator first = facts->begin();
	rulenode temp;
	while(first != facts->end())
	{
		if(!first->is_fact)
		{
			temp.name = first->name;
			temp.num_rows = first->num_rows;
			temp.address_host_table = first->address_host_table;
			rules->push_back(temp);
			first = facts->erase(first);
		}
		else
			first++;
	}
}

template<class InputIterator>
void nombres(InputIterator rules, InputIterator end)
{
	int x, pos;
	while(rules != end)
	{
		pos = 1;
		rules->rule_names = (int *)malloc(sizeof(int) * (rules->num_rows + 1));
		rules->rule_names[0] = 0;
		rules->num_bpreds.x = 0;
		for(x = 1; x <= rules->num_rows; x++)
		{
			while(rules->address_host_table[pos] != 0)
				pos++;
			pos++;
			rules->rule_names[x] = pos;
			if(rules->address_host_table[pos] < 0 && x < rules->num_rows)
				rules->num_bpreds.x++;
		}
		rules->num_columns = rules->rule_names[1] - 2;
		rules->num_rows -= rules->num_bpreds.x; /*quita los builtin del total*/
		rules++;
	}
}

template<class InputIterator, class RulesIterator>
void referencias(InputIterator facts, InputIterator fend, RulesIterator rules, RulesIterator end)
{
	vector<gpunode>::iterator res1;
	gpunode n1;
	vector<rulenode>::iterator actual = rules, res2;
	rulenode n2;
	int x, temp, cont;
	while(actual != end)
	{
		actual->referencias = (int *)malloc((actual->num_rows - 1) * sizeof(int));
		cont = 0;
		for(x = 1; x < actual->num_rows; x++)
		{
			temp = actual->address_host_table[actual->rule_names[x]];
			if(temp < 0) /*puede ser reemplazado una vez que se reacomoden las reglas*/
				continue;
			if(temp == actual->name)
				actual->referencias[x-1] = actual - rules;
			else
			{
				n1.name = temp;
				res1 = lower_bound(facts, fend, n1, compare);
				if (res1 != fend && res1->name == temp)
				{
					actual->referencias[x-1] = -(res1 - facts) - 1;
					cont++;
				}
				else
				{
					n2.name = temp;
					res2 = lower_bound(rules, end, n2, comparer);
					actual->referencias[x-1] = res2 - rules;
				}
			}
		}
		if(cont == (x - 1))
			actual->gen_ant = 0;
		else
			actual->gen_ant = -1;
		actual->gen_act = 0;
		actual++;
	}
}

template<class InputIterator>
void seleccion(InputIterator actual, InputIterator end)
{
	int x, y, ini, temp, sl, tam, cont;
	int pv[MAXVALS];
	while(actual != end)
	{
		temp = actual->num_rows - 1;
		actual->select = (int **)malloc(temp * sizeof(int *));
		actual->numsel = (int *)malloc(temp * sizeof(int));
		for(x = 1; x < actual->num_rows; x++)
		{
			ini = actual->rule_names[x];
			if(actual->address_host_table[ini] < 0)
				continue;
			ini++;
			cont = 0;
			for(y = ini; y < (actual->rule_names[x+1] - 1); y++)
			{
				temp = actual->address_host_table[y];
				if(temp < 0)
				{
					pv[cont] = y - ini;
					cont++;
					pv[cont] = -temp;
					cont++;
				}
			}
			sl = x - 1;
			tam = cont * sizeof(int);
			actual->select[sl] = (int *)malloc(tam);
			memcpy(actual->select[sl], pv, tam);
			actual->numsel[sl] = cont;
		}
		actual++;
	}
}

int notin(int bus, int *array, int size)
{
	int x;
	for(x = 0; x < size; x++)
	{
		if(array[x] == bus)
			return 0;
	}
	return 1;
}

int2 columnsproject(int *first, int tam, int *rule, int ini, int fin, int sini, int sfin, int **res, int **newrule)
{
	int x, y, temp;
	int pv[MAXVALS], pv2[MAXVALS];
	int2 ret = make_int2(0, 0);
	for(x = 0; x < tam; x++)
	{
		y = 0;
		temp = first[x];
		for(y = 0; y < ini; y++)
		{
			if(temp == rule[y])
			{
				if(notin(temp, pv, ret.y))
				{
					pv[ret.y] = temp;
					pv2[ret.y] = x;
					ret.y++;
				}
				break;
			}
		}
		if(y != ini)
			continue;
		for(y = sfin + 1; y < fin; y++)
		{
			if(temp == rule[y])
			{
				if(notin(temp, pv, ret.y))
				{
					pv[ret.y] = temp;
					pv2[ret.y] = x;
					ret.y++;
				}
				break;
			}
		}
	}
	ret.x = ret.y;
	for(x = sini; x < sfin; x++)
	{
		y = 0;
		temp = rule[x];
		for(y = 0; y < ini; y++)
		{
			if(temp == rule[y])
			{
				if(notin(temp, pv, ret.y))
				{
					pv[ret.y] = temp;
					pv2[ret.y] = x - sini;
					ret.y++;
				}
				break;
			}
		}
		if(y != ini)
			continue;
		for(y = sfin + 1; y < fin; y++)
		{
			if(temp == rule[y])
			{
				if(notin(temp, pv, ret.y))
				{
					pv[ret.y] = temp;
					pv2[ret.y] = x - sini;
					ret.y++;
				}
				break;
			}
		}
	}
	temp = ret.y * sizeof(int);
	free(*newrule);
	*newrule = (int *)malloc(temp);
	memcpy(*newrule, pv, temp);
	*res = (int *)malloc(temp);
	memcpy(*res, pv2, temp);	
	return ret;
}

int wherejoin(int *tmprule, int tmplen, int *rule, int tam2, int **res)
{
	int x, y, temp;
	int cont = 0;
	int joins[MAXVALS];
	for(x = 0; x < tmplen; x++)
	{
		for(y = 0; y < tam2; y++)
		{
			if(rule[y] > 0 && tmprule[x] == rule[y])
			{
				joins[cont] = x;
				cont++;
				joins[cont] = y;
				cont++;
				break;
			}
		}
	}

	temp = cont * sizeof(int);
	*res = (int *)malloc(temp);
	memcpy(*res, joins, temp);
	return cont;
}

int builtinpredicates(int *tmprule, int tmplen, int *rule, int ini, int fin, int **res)
{
	int x, y, temp;
	int cont = 0, cont2 = 0;
	int joins[MAXVALS];
	
	for(x = ini; x < fin; x++)
	{
		joins[cont] = rule[x];
		cont++;
		x++;
		if(rule[x] < 0)
		{
			joins[cont] = rule[x];
			cont++;
			x++;
		}
		for(y = 0; y < tmplen; y++)
		{
			if(tmprule[y] == rule[x])
			{
				joins[cont] = y;
				cont++;
				x++;
				break;
			}
		}
		if(rule[x] == 0)
			continue;
		if(rule[x] < 0)
		{
			joins[cont] = rule[x];
			cont++;
			x++;
			continue;
		}
		for(y = 0; y < tmplen; y++)
		{
			if(tmprule[y] == rule[x])
			{
				joins[cont] = y;
				cont++;
				x++;
				break;
			}
		}
	}
	x = 1;
	while(rule[x] != 0)
	{
		for(y = 0; y < tmplen; y++)
		{
			if(rule[x] == tmprule[y])
			{
				joins[cont] = y;
				cont++;
				cont2++;
				break;
			}
		}
		x++;
	}
	temp = cont * sizeof(int);
	*res = (int *)malloc(temp);
	memcpy(*res, joins, temp);
	return cont2;
}

template<class InputIterator>
void proyeccion(InputIterator actual, InputIterator end)
{
	int x, y, ini, fin, total, numjoins, temp, rulestart, ruleend, malptr;
	int fjoin[MAXVALS];
	int *pv, *res;
	int2 pos;
	while(actual != end)
	{
		numjoins = actual->num_rows - 2;
		if(numjoins < 1)
		{
			actual->projpos = (int2 *)malloc(sizeof(int2));
			ini = actual->rule_names[1] + 1;
			fin = actual->rule_names[2] - 1;

			if(actual->numsel[0] == 0 && actual->numselfj[0] == 0 && actual->num_columns == (fin - ini))
			{
				for(x = 1, y = actual->num_columns + 3; x <= actual->num_columns; x++, y++)
				{
					if(actual->address_host_table[x] != actual->address_host_table[y])
						break;
				}
				if(x > actual->num_columns)
				{
					pos.x = -1;
					pos.y = -1;
					actual->projpos[0] = pos;
					actual++;
					continue;
				}
			}
			actual->project = (int **)malloc(sizeof(int *));
			pos.x = 0;
			for(x = 1; x <= actual->num_columns; x++)
			{
				temp = actual->address_host_table[x];
				for(y = ini; y < fin; y++)
				{
					if(temp == actual->address_host_table[y])
					{
						fjoin[pos.x] = y - ini;
						pos.x++;
						break;
					}
				}
			}
			temp = pos.x * sizeof(int);
			actual->project[0] = (int *)malloc(temp);
			memcpy(actual->project[0], fjoin, temp);
			pos.y = pos.x;
			actual->projpos[0] = pos;
			actual++;
			continue;
		}
		malptr = numjoins * sizeof(int *);
		actual->project = (int **)malloc(malptr);
		actual->projpos = (int2 *)malloc(numjoins * sizeof(int2));
		actual->wherejoin = (int **)malloc(malptr);
		actual->numjoin = (int *)malloc(numjoins * sizeof(int));
		ini = actual->rule_names[1] + 1;
		total = actual->num_rows + actual->num_bpreds.x;
		fin = actual->rule_names[total] - 1;
		pos.y = actual->rule_names[2] - actual->rule_names[1] - 2;
		temp = pos.y * sizeof(int);
		pv = (int *)malloc(temp);
		memcpy(pv, actual->address_host_table + actual->rule_names[1] + 1, temp);

		for(x = 2, y = 0; x <= numjoins; x++, y++)
		{
			rulestart = actual->rule_names[x] + 1;
			ruleend = actual->rule_names[x+1] - 1;
			temp = wherejoin(pv, pos.y, actual->address_host_table + rulestart, ruleend - rulestart, &res);
			actual->wherejoin[y] = res;
			actual->numjoin[y] = temp;
			pos = columnsproject(pv, pos.y, actual->address_host_table, ini, fin, rulestart, ruleend, &res, &pv);
			actual->project[y] = res;
			actual->projpos[y] = pos;
		}

		rulestart = actual->rule_names[actual->num_rows-1] + 1;
		ruleend = actual->rule_names[actual->num_rows] - 1; 

		temp = wherejoin(pv, pos.y, actual->address_host_table + rulestart, ruleend - rulestart, &res);
		actual->wherejoin[y] = res;
		actual->numjoin[y] = temp;
		numjoins--;
		
		if(actual->num_bpreds.x > 0)
		{
			pos = columnsproject(pv, pos.y, actual->address_host_table, ini, fin, rulestart, ruleend, &res, &pv);
			actual->project[numjoins] = res;
			actual->projpos[numjoins] = pos;
			actual->num_bpreds.y = pos.y; /*para guardar el tamaño de la union final*/
			actual->num_bpreds.z = builtinpredicates(pv, pos.y, actual->address_host_table, ruleend + 1, actual->rule_names[total] - 1, &res);
			actual->builtin = res;
		}
		else
		{
			pos.x = 0;
			for(x = 1; x <= actual->num_columns; x++)
			{
				temp = actual->address_host_table[x];
				for(y = 0; y < pos.y; y++)
				{
					if(temp == pv[y])
					{
						fjoin[pos.x] = y + 1;
						pos.x++;
						break;
					}
				}
				if(y != pos.y)
					continue;
				for(y = rulestart; y < ruleend; y++)
				{
					if(temp == actual->address_host_table[y])
					{
						fjoin[pos.x] = -(y - rulestart + 1);
						pos.x++;
						break;
					}
				}
			}

			temp = pos.x * sizeof(int);	
			actual->project[numjoins] = (int *)malloc(temp);
			memcpy(actual->project[numjoins], fjoin, temp);
			pos.y = pos.x;
			actual->projpos[numjoins] = pos;
		}
		actual++;
	}
}

template<class InputIterator>
void selfjoin(InputIterator actual, InputIterator end)
{
	int x, y, z;
	int cont, tam, temp, size, pos;
	int fjoin[MAXVALS], rulecpy[MAXVALS];
	while(actual != end)
	{
		size = actual->num_rows - 1;
		actual->selfjoin = (int **)malloc(size * sizeof(int *));
		actual->numselfj = (int *)malloc(size * sizeof(int));
		for(x = 1; x <= size; x++)
		{
			pos = actual->rule_names[x];
			if(actual->address_host_table[pos] < 0)
				continue;
			tam = actual->rule_names[x+1] - actual->rule_names[x] - 2;
			memcpy(rulecpy, actual->address_host_table + pos + 1, tam * sizeof(int));
			cont = 0;
			for(y = 0; y < tam; y++)
			{
				temp = rulecpy[y];
				if(temp > -1)
				{
					for(z = y + 1; z < tam; z++)
					{
						if(temp == rulecpy[z])
						{
							fjoin[cont] = y;
							cont++;
							fjoin[cont] = z;
							cont++;
							rulecpy[z] = -1;
							for(z++; z < tam; z++)
							{
								if(temp == rulecpy[z])
								{
									fjoin[cont] = z;
									cont++;
									rulecpy[z] = -1;
								}
							}
							fjoin[cont] = -1;
							cont++;
						}
					}
				}
			}
			temp = x - 1;
			tam = cont * sizeof(int);
			actual->selfjoin[temp] = (int *)malloc(tam);
			memcpy(actual->selfjoin[temp], fjoin, tam);
			actual->numselfj[temp] = cont;
		}
		actual++;
	}
}

template<class InputIterator>
int linears(InputIterator first, InputIterator last, int name)
{
	while(first != last) 
	{
    		if(first->name == name) 
			return 0;
    		first++;
  	}
  	return 1;
}

template<class InputIterator>
void tempointer(InputIterator rules, InputIterator end, vector<rulenode>::iterator aux)
{
	while(rules != end)
	{
		rules->temp = &(*aux);
		rules++;
		aux++;
	}
}

void cargareglas(vector<rulenode> *rules, int name, list<rulenode> *res) /*This is the function to create the rule queue based on the query*/
{
	rulenode searched;
	vector<rulenode>::iterator ini = rules->begin(), fin = rules->end(), actual;
	list<rulenode>::iterator start;
	unsigned int numrules;
	int x, pos;
	searched.name = name;
	actual = lower_bound(ini, fin, searched, comparer);
	if(actual == fin)
		return;
	while(actual != fin && actual->name == name)
	{
		res->push_back(*actual);
		actual++;
	}
	numrules = rules->size();
	start = res->begin();
	while(res->size() < numrules && start != res->end())
	{
		for(x = 0; x < start->num_rows - 1; x++)
		{
			pos = start->referencias[x];
			if(pos > -1)
			{
				searched = rules->at(pos);
				if(linears(res->begin(), res->end(), searched.name))
					res->push_back(searched);
			}
		}
		start++;
	}
	res->sort(comparer);
}

void consulta(int *query, int qsize, int qname, rulenode *res)
{
	int sel[MAXVALS], pro[MAXVALS];
	int temp, cont1 = 0, cont2 = 0, size;
	int x, y;
	res->numsel = (int *)malloc(sizeof(int));
	res->numselfj = (int *)malloc(sizeof(int));
	for(x = 0; x < qsize; x++)
	{
		temp = query[x];
		if(temp < 0)
		{
			sel[cont1] = x;
			cont1++;
			sel[cont1] = -temp;
			cont1++;
		}
		else
		{
			pro[cont2] = x;
			cont2++;
		}
	}
	res->numsel[0] = cont1;
	res->num_columns = cont2;
	if(cont1 > 0)
	{
		size = cont1 * sizeof(int);
		res->select = (int **)malloc(sizeof(int *));
		res->select[0] = (int *)malloc(size);
		memcpy(res->select[0], sel, size);
		cont1 = 0;
	}
	if(cont2 > 0)
	{
		size = cont2 * sizeof(int);
		res->project = (int **)malloc(sizeof(int *));
		res->project[0] = (int *)malloc(size);
		memcpy(res->project[0], pro, size);
	}
	for(x = 0; x < qsize; x++)
	{
		temp = query[x];
		if(temp > -1)
		{
			for(y = x + 1; y < qsize; y++)
			{
				if(temp == query[y])
				{
					sel[cont1] = x;
					cont1++;
					sel[cont1] = y;
					cont1++;
					query[y] = -1;
					for(y++; y < qsize; y++)
					{
						if(temp == query[y])
						{
							sel[cont1] = y;
							cont1++;
							query[y] = -1;
						}
					}
					sel[cont1] = -1;
					cont1++;
				}
			}
		}
	}
	res->numselfj[0] = cont1;
	if(cont1 > 0)
	{
		size = cont1 * sizeof(int);
		res->selfjoin = (int **)malloc(sizeof(int *));
		res->selfjoin[0] = (int *)malloc(size);
		memcpy(res->selfjoin[0], sel, size);
	}
}

template<class InputIterator>
void completitud(InputIterator actual, InputIterator end, vector<compnode> *rulcomp)
{
	vector<compnode>::iterator bus;
	compnode searched;
	while(actual != end)
	{
		searched.name = actual->name;
		bus = lower_bound(rulcomp->begin(), rulcomp->end(), searched, comparecompleted);
		if(bus == rulcomp->end() || bus->name != searched.name)
		{
			searched.numrules = 1;
			searched.reduce = 0;
			rulcomp->push_back(searched);
		}
		else
			bus->numrules++;
		actual++;
	}
	bus = rulcomp->begin();
	while(bus != rulcomp->end())
	{
		bus->reset = bus->numrules;
		bus++;
	}
}

template<class InputIterator>
void mostrarcontenido(InputIterator actual, InputIterator end)
{
	int y, z, num;
	cout << "AUX INICIO" << endl;
	cout << "tamanio = " << end - actual << endl;
	while(actual != end)
	{
		cout << "name = " << actual->name << endl;
		cout << "rule_names = ";
		for(y = 0; y <= actual->num_rows; y++)
			cout << actual->rule_names[y] << " ";
		cout << endl << "referencias = ";
		num = actual->num_rows - 1;
		for(y = 0; y < num; y++)
			cout << actual->referencias[y] << " ";
		cout << endl << "select = " << endl;
		for(y = 0; y < num; y++)
		{	
			cout << actual->numsel[y] << ": ";
			for(z = 0; z < actual->numsel[y]; z++)
				cout << actual->select[y][z] << " ";
			cout << endl;
		}
		cout << "selfjoins = " << endl;
		for(y = 0; y < num; y++)
		{	
			cout << actual->numselfj[y] << ": ";
			for(z = 0; z < actual->numselfj[y]; z++)
				cout << actual->selfjoin[y][z] << " ";
			cout << endl;
		}
		cout << "project = " << endl;
		for(y = 0; y < num; y++)
		{	
			cout << actual->projpos[y].x << " " << actual->projpos[y].y << ": ";
			for(z = 0; z < actual->projpos[y].y; z++)
				cout << actual->project[y][z] << " ";
			cout << endl;
		}
		num--;
		cout << "wherejoin = " << endl;
		for(y = 0; y < num; y++)
		{	
			cout << actual->numjoin[y] << ": ";
			for(z = 0; z < actual->numjoin[y]; z++)
				cout << actual->wherejoin[y][z] << " ";
			cout << endl;
		}
		actual++;
	}	
	cout << "AUX FIN" << endl;
}

void mostrareglas(list<rulenode> aux)
{
	list<rulenode>::iterator actual = aux.begin();
	cout << "Rules to eval = ";
	while(actual != aux.end())
	{
		cout << actual->name << " ";
		actual++;
	}
	cout << endl;
}

extern "C"
int Cuda_Eval(predicate **inpfacts, int ninpf, predicate **inprules, int ninpr, predicate *inpquery, int **result)
{
	vector<gpunode> L;
	int showr = 0; /*1 show results; 0 don't show results*/
	int x, y;
	int qsize, *query, qname;

	for(x = 0; x < ninpf; x++)
		L.push_back(*inpfacts[x]);
	for(x = 0; x < ninpr; x++)
		L.push_back(*inprules[x]);

	qname = inpquery->name;
	query = inpquery->address_host_table;
	qsize = inpquery->num_columns;

	/*cout << qname << " " << qsize << endl;
	for(x = 0; x < q->symbols_num; x++)
		cout << q->symbols_id[x] << " ";
	cout << endl;*/

	vector<gpunode>::iterator i;
	i = L.begin();

	calcular_mem(0);
	int res_rows, rows1, rows2;
	int tipo;
	int *dop1, *dop2, *res;
	
	vector<rulenode> rules;
	vector<rulenode>::iterator rul_str, fin;
	buscarreglas(&L, &rules);
	sort(L.begin(), L.end(), compare);
	sort(rules.begin(), rules.end(), comparer);
	rul_str = rules.begin();
	fin = rules.end();

	nombres(rul_str, fin);
	referencias(L.begin(), L.end(), rul_str, fin);
	seleccion(rul_str, fin);
	selfjoin(rul_str, fin);
	proyeccion(rul_str, fin);
	
	//mostrarcontenido(rul_str, fin);

	list<rulenode> reglas;
	list<rulenode>::iterator rul_act, busqueda;
	rulenode completed;
	cargareglas(&rules, qname, &reglas);
	//mostrareglas(reglas);

	gpunode tmpfact;
	rulenode tmprule;
	int name1, filas1, cols1, isfact1, name2, filas2, cols2, isfact2;
	int *table1, *table2, *hres;
	int num_refs, itr = 0;
	vector<gpunode>::iterator qposf;
	vector<rulenode>::iterator qposr;

	cudaEvent_t start, stop;
	float time;
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);

	while(reglas.size()) /*Here's the main loop*/
	{
		rul_act = reglas.begin();

		while(rul_act != reglas.end()) /*Here's the loop that evaluates each rule*/
		{
			tipo = rul_act->referencias[0];
			if(tipo)
			{
				tmpfact = L.at(-tipo - 1);
				name1 = tmpfact.name;
				filas1 = tmpfact.num_rows;
				cols1 = tmpfact.num_columns;
				isfact1 = 1;
				table1 = tmpfact.address_host_table;
			}
			else
			{
				tmprule = rules.at(tipo);
				name1 = tmprule.name;
				filas1 = tmprule.num_rows;
				cols1 = tmprule.num_columns;
				isfact1 = 0;
				table1 = NULL;
			}

			rows1 = cargar(name1, filas1, cols1, isfact1, table1, &dop1, itr);

			//cout << "rows1 = " << rows1 << endl;

			if(rows1 == 0)
			{
				//rul_act->gen_ant = rul_act->gen_act;
				rul_act->gen_act = 0;
				rul_act++;
				continue;
			}
			
			if(rul_act->num_rows < 3)
			{	
				if(rul_act->projpos[0].x == -1)
				{
					num_refs = rows1 * cols1 * sizeof(int);
					reservar(&res, num_refs);
					cudaMemcpyAsync(res, dop1, num_refs, cudaMemcpyDeviceToDevice);
					registrar(rul_act->name, cols1, res, rows1, itr, 1);
					rul_act->gen_ant = rul_act->gen_act;
					rul_act->gen_act = rows1;
				}
				else
				{

					/*int x, y;
					cout << "antes = " << cols1 << " " << rows1 << endl;
					int *hop1 = (int *)malloc(cols1 * rows1 * sizeof(int));
					cudaMemcpy(hop1, dop1, cols1 * rows1 * sizeof(int), cudaMemcpyDeviceToHost);
					for(x = 0; x < rows1; x++)
					{
						for(y = 0; y < cols1; y++)
							cout << hop1[x * cols1 + y] << " ";
						cout << endl;
					}
					free(hop1);*/

					res_rows = selectproyect(dop1, rows1, cols1, rul_act->num_columns, rul_act->select[0], rul_act->numsel[0], rul_act->selfjoin[0], rul_act->numselfj[0], rul_act->project[0], &res);
					if(res_rows > 0)
					{
						registrar(rul_act->name, rul_act->num_columns, res, res_rows, itr, 1);
						rul_act->gen_ant = rul_act->gen_act;
						rul_act->gen_act = res_rows;
					}
					else
					{
						//rul_act->gen_ant = rul_act->gen_act;
						rul_act->gen_act = 0;
					}
				}
				rul_act++;
				continue;
			}

			tipo = rul_act->referencias[1];
			if(tipo < 0)
			{
				tmpfact = L.at(-tipo - 1);
				name2 = tmpfact.name;
				filas2 = tmpfact.num_rows;
				cols2 = tmpfact.num_columns;
				isfact2 = 1;
				table2 = tmpfact.address_host_table;
			}
			else
			{
				tmprule = rules.at(tipo);
				name2 = tmprule.name;
				filas2 = tmprule.num_rows;
				cols2 = tmprule.num_columns;
				isfact2 = 0;
				table2 = NULL;
			}

			rows2 = cargar(name2, filas2, cols2, isfact2, table2, &dop2, itr);
			
			//cout << "rows2 = " << rows2 << endl;
	
			if(rows2 == 0)
			{
				//rul_act->gen_ant = rul_act->gen_act;
				rul_act->gen_act = 0;
				rul_act++;
				continue;
			}

			res = NULL;
			res_rows = join(dop1, dop2, rows1, rows2, cols1, cols2, rul_act, 0, 1, &res);
	
			if(res_rows == 0)
			{
				//rul_act->gen_ant = rul_act->gen_act;
				rul_act->gen_act = 0;
				rul_act++;
				continue;
			}

			num_refs = rul_act->num_rows - 1;
			for(x = 2; x < num_refs; x++)
			{
				tipo = rul_act->referencias[x];
				if(tipo < 0)
				{
					tmpfact = L.at(-tipo - 1);
					name2 = tmpfact.name;
					filas2 = tmpfact.num_rows;
					cols2 = tmpfact.num_columns;
					isfact2 = 1;
					table2 = tmpfact.address_host_table;
				}
				else
				{
					tmprule = rules.at(tipo);
					name2 = tmprule.name;
					filas2 = tmprule.num_rows;
					cols2 = tmprule.num_columns;
					isfact2 = 0;
					table2 = NULL;
				}

				rows2 = cargar(name2, filas2, cols2, isfact2, table2, &dop2, itr);

				//cout << "rows = " << x << " " << rows2 << endl;

				if(rows2 == 0)
					break;
				res_rows = join(res, dop2, res_rows, rows2, rul_act->projpos[x-2].y, cols2, rul_act, x-1, 0, &res);
				if(res_rows == 0)
					break;
				
				//cout << "resrows = " << res_rows << endl;

			}
			if(x == num_refs)
			{
				if(rul_act->num_bpreds.x > 0) /*Built-in predicates*/
				{
					#ifdef TIMER
					cudaEvent_t start3, stop3;
					cudaEventCreate(&start3);
					cudaEventCreate(&stop3);
					cudaEventRecord(start3, 0);
					#endif					
				
					res_rows = bpreds(res, res_rows, rul_act->builtin, rul_act->num_bpreds, &res);

					#ifdef TIMER
					cudaEventRecord(stop3, 0);
					cudaEventSynchronize(stop3);
					cudaEventElapsedTime(&time, start3, stop3);
					cout << "Predicados = " << time << endl;
					#endif
				}

				//cout << "antes de unir = " << res_rows << endl;

				#ifdef TIMER
				cudaEvent_t start2, stop2;
				cudaEventCreate(&start2);
				cudaEventCreate(&stop2);
				cudaEventRecord(start2, 0);
				#endif

				res_rows = unir(res, res_rows, rul_act->num_columns, &res); /*Duplicate Elimination*/

				#ifdef TIMER
				cudaEventRecord(stop2, 0);
				cudaEventSynchronize(stop2);
				cudaEventElapsedTime(&time, start2, stop2);
				cout << "Union = " << time << endl;
				#endif					
	
				//cout << "despues de unir = " << res_rows << endl;

				registrar(rul_act->name, rul_act->num_columns, res, res_rows, itr, 1);	
				rul_act->gen_ant = rul_act->gen_act;
				rul_act->gen_act = res_rows;
			}
			else
			{
				//rul_act->gen_ant = rul_act->gen_act;
				rul_act->gen_act = 0;
			}
			rul_act++;
		}

		rul_act = reglas.begin();

		/*while(rul_act != reglas.end())
		{
			cout << rul_act->gen_act << " " << rul_act->gen_ant << endl;
			rul_act++;
		}
		return 0;*/

		//cout << rul_act->gen_act << " " << rul_act->gen_ant << endl;

		while(rul_act != reglas.end()) /*Here's the loop that discards finished rules*/
		{
			if(rul_act->gen_act == -1 || rul_act->gen_ant == -1) //&& rul_act->gen_act == 0))
			{
				rul_act++;
				continue;
			}
			if(rul_act->gen_act == 0)
			{
				rul_act->gen_act = -1;
				rul_act++;
				continue;
			}
			num_refs = rul_act->num_rows - 1;
			for(x = 0; x < num_refs; x++)
			{
				tipo = rul_act->referencias[x];
				if(tipo < 0)
					continue;
				completed.name = rul_act->address_host_table[rul_act->rule_names[x+1]];
				if(!binary_search(reglas.begin(), reglas.end(), completed, comparer))
				{
					rul_act->gen_act = -1;
					break;
				}

				//cout << rul_act->gen_act << " " << rul_act->gen_ant << endl;			

				/*if(rul_act->gen_act == rul_act->gen_ant)
				{			
					tipo = rul_act->name;
					busqueda = rul_act;
					busqueda++;
					while(busqueda != reglas.end() && busqueda->name == tipo)
					{
						if(busqueda->gen_act != busqueda->gen_ant)
							break;
						busqueda++;
					}
					if(busqueda != reglas.end() && busqueda->name == tipo)
						break;*/
				tipo = rul_act->name;
				if(generadas(tipo, rul_act->num_rows, rul_act->num_columns, itr))
				{
					rul_act->gen_act = -1;
					busqueda = rul_act;
					busqueda++;
					while(busqueda != reglas.end() && busqueda->name == tipo)
					{
						busqueda->gen_act = -1;
						busqueda++;
					}
				}
				break;
			}
			if(x == num_refs)
				rul_act->gen_act = -1;
			rul_act++;
		}
		rul_act = reglas.begin();
		while(rul_act != reglas.end())
		{
			if(rul_act->gen_act == -1)
				rul_act = reglas.erase(rul_act);
			else
				rul_act++;
		}
		
		//cout << "ITR = " << itr << endl;

		itr++;
	}
	tmprule.name = qname;
	qposr = lower_bound(rul_str, fin, tmprule, comparer);
	if(qposr != fin && qposr->name == qname) 
	{
		cols1 = qposr->num_columns;
		rows1 = cargafinal(qname, cols1, &dop1);
	}
	else
	{
		tmpfact.name = qname;
		qposf = lower_bound(L.begin(), L.end(), tmpfact, compare);
		cols1 = qposf->num_columns;
		rows1 =  cargar(qname, qposf->num_rows, cols1, 1, qposf->address_host_table, &dop1, 0);
	}
	if(rows1 > 0) /*Query consideration*/
	{
		consulta(query + 1, qsize, qname, &tmprule);
		if(tmprule.numsel[0] == 0 && tmprule.numselfj[0] == 0)
		{
			res = dop1;
			res_rows = rows1;
		}
		else		
			res_rows = selectproyect(dop1, rows1, cols1, tmprule.num_columns, tmprule.select[0], tmprule.numsel[0], tmprule.selfjoin[0], tmprule.numselfj[0], tmprule.project[0], &res);

		cols1 = tmprule.num_columns;
		tipo = res_rows * cols1 * sizeof(int);
		hres = (int *)malloc(tipo);
		cudaMemcpy(hres, res, tipo, cudaMemcpyDeviceToHost);
	}
	else
		res_rows = 0;

	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&time, start, stop);

	if(showr == 1)
	{
		for(x = 0; x < res_rows; x++)
		{
			cols2 = x * cols1 + cols1;
			for(y = x * cols1; y < cols2; y++)
				cout << hres[y] << " ";
			cout << endl;
		}
	}
	//free(hres);

	cout << "Elapsed = " << time << endl;
	cout << "Size = " << res_rows << endl;
	cout << "Iterations = " << itr << endl;

	*result = hres;

	return res_rows;
}

	/*gpunode k;
	k.name = 666;
	L.push_back(k);
	k.name = 777;
	L.push_back(k);
	int a = 666;
	vector<gpunode>::iterator i;
   	for(i=L.begin(); i != L.end(); ++i)
		cout << i->name << " ";
	i = buscar(L.begin(), L.end(), a);
	cout << endl << i->name;
	int *pred = (int *)malloc(sizeof(int) * 4);
	pred[0] = 2;
	pred[3] = 6;
	Cuda_newPred(888, 2, 2, pred, &L);
	for(i=L.begin(); i != L.end(); ++i)
		cout << i->name << " ";*/
