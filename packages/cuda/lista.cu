#include <iostream>
#include <vector>
#include <algorithm>
#include "lista.h"
#include "memory.h"
extern "C" {
#include "pred.h"
}
#include "selectproyect.cu"
#include "selectproyectcpu.cpp"
#include "treeb.cu"
#include "joincpu.cpp"
#include "union2.h"
#include "unioncpu2.cpp"
#include "bpreds.h"
#include "bpredscpu.cpp"
#include "dbio.h"

#define MAXVALS 2000

#if TIMER
statinfo cuda_stats;
#endif

/*Auxiliary function to sort fact list*/
bool compare(const gpunode &r1, const gpunode &r2)
{
	return (r1.name > r2.name); 
}

/*Creates and stores the rule nodes in the rule list*/ 
void meter(vector<rulenode> *rules, predicate **p, int size)
{
	int x;
	rulenode temp;
	for(x = 0; x < size; x++)
	{
		temp.name = (*p[x]).name;
		temp.num_rows = (*p[x]).num_rows;
		temp.address_host_table = (*p[x]).address_host_table;
		temp.negatives = (*p[x]).negatives;
		temp.rulename = (*p[x]).predname;
		rules->push_back(temp);
	}
}

/*Find the first predicate that has a certain variable val*/
int closestpred(int *arr, int size, int val)
{
	int x;
	for(x = 1; x < size; x++)
	{
		if(arr[x] > val)
			break;
	}
	return x - 2;
}

/*template<class InputIterator>
void checkjoins(InputIterator rules, InputIterator end)
{
			x = 1;
			while(rules->rule_name[x] < 0)
				x++;
			pos = rules->rule_names[x] + 1;

			for(y = x + 1; y < total; y++)
			{
				bs = rules->address_host_table[pos];
				pos2 = rules->rule_names[x+1] + 1;
				while((bs2 = rules->address_host_table[pos2]) > 0)
				{
					if(bs == bs2)
					{
						flag = 1;
						break;
					}
					pos2++;
				}
			}
}*/

/*Move comparison predicates based on their variables. They are moved 
after the first normal predicate that has one (in case of variable vs constant comparison)
or both (variable vs variable) variables found in the comparison. */
template<class InputIterator>
void movebpreds(InputIterator rules, InputIterator end)
{
	int x, y, subs, total, pos, fin;
	int cont, cont2, cont3;
	int p1 = -1, p2 = -1;
	int move[MAXVALS], move2[MAXVALS], rest[MAXVALS];
	
	while(rules != end)
	{
		total = rules->num_rows - 1;
		cont = total * sizeof(int2);
		rules->preds = (int **)malloc(total * sizeof(int *));
		rules->numpreds = (int2 *)malloc(cont);
		memset(rules->numpreds, 0x0, cont);
	
		if(rules->totalpreds > 0)
		{
			cont2 = MAXVALS * sizeof(int);
			for(x = 0; x < total; x++)
				rules->preds[x] = (int *)malloc(cont2);

			total = rules->num_rows + rules->totalpreds;
			cont = 0;
			cont2 = 0;
			cont3 = 0;
			for(x = 0; x < total; x++)
			{
				subs = rules->rule_names[x+1] - rules->rule_names[x];
				if(rules->address_host_table[rules->rule_names[x]] > 0)
				{
					memmove(rest + cont, rules->address_host_table + rules->rule_names[x], subs * sizeof(int));
					cont += subs;
				}
				else
				{
					pos = rules->rule_names[x] + 1;
					subs--;
					if(rules->address_host_table[pos] > 0 && rules->address_host_table[pos+1] > 0)
					{
						memmove(move2 + cont3, rules->address_host_table + rules->rule_names[x], subs * sizeof(int));
						cont3 += subs;
					}
					else
					{
						memmove(move + cont2, rules->address_host_table + rules->rule_names[x], subs * sizeof(int));
						cont2 += subs;
					}
				}
			}
			memmove(rules->address_host_table, rest, cont * sizeof(int));

			pos = 1;
			for(x = 1; x <= total; x++)
			{
				while(rest[pos] != 0)
					pos++;
				pos++;
				rules->rule_names[x] = pos;
			}

			pos = 1;
			total = 3 * sizeof(int);
			fin = cont2 / 3;
			cont2 = 0;
			for(x = 0; x < fin; x++)
			{
				subs = move[pos];
				if(subs > 0)
				{
					for(y = rules->rule_names[1] + 1; y < rules->rule_names[rules->num_rows]; y++)
					{
						if(rules->address_host_table[y] == subs)
						{
							p1 = y;
							break;
						}
					}
				}
				else
				{
					pos++;
					subs = move[pos];
					for(y = rules->rule_names[1] + 1; y < rules->rule_names[rules->num_rows]; y++)
					{
						if(rules->address_host_table[y] == subs)
						{
							p1 = y;
							break;
						}
					}
				}
				pos += 2;
				cont = closestpred(rules->rule_names, rules->num_rows, p1);

				memmove(rules->preds[cont] + rules->numpreds[cont].x, move + cont2, total);
				rules->numpreds[cont].x += 3;
				cont2 += 3;
			}

			pos = 1;
			fin = cont3 / 3;
			cont3 = 0;
			for(x = 0; x < fin; x++)
			{
				subs = move2[pos];
				for(y = rules->rule_names[1] + 1; y < rules->rule_names[rules->num_rows]; y++)
				{
					if(rules->address_host_table[y] == subs)
					{
						p1 = y;
						break;
					}
				}
				pos++;
				subs = move2[pos];
				for(y = rules->rule_names[1] + 1; y < rules->rule_names[rules->num_rows]; y++)
				{
					if(rules->address_host_table[y] == subs)
					{
						p2 = y;
						break;
					}
				}
				pos += 2;
				cont = closestpred(rules->rule_names, rules->num_rows, p1);
				cont2 = closestpred(rules->rule_names, rules->num_rows, p2);
				if(cont == cont2)
				{
					memmove(rules->preds[cont] + rules->numpreds[cont].x, move2 + cont3, total);
					rules->numpreds[cont].x += 3;
				}
				else
				{
					if(cont > cont2)
					{
						memmove(rules->preds[cont] + rules->numpreds[cont].x + rules->numpreds[cont].y, move2 + cont3, total);
						rules->numpreds[cont].y += 3;
					}
					else
					{
						memmove(rules->preds[cont2] + rules->numpreds[cont2].x + rules->numpreds[cont2].y, move2 + cont3, total);
						rules->numpreds[cont2].y += 3;
					}
				}
				cont3 += 3;
			}
		}
		rules++;
	}
}

/*Mark the location of predicate names in each rule*/
template<class InputIterator>
void nombres(InputIterator rules, InputIterator end)
{
	int x, pos;
	while(rules != end)
	{
		pos = 1;
		rules->rule_names = (int *)malloc(sizeof(int) * (rules->num_rows + 1));
		rules->rule_names[0] = 0;
		rules->totalpreds = 0;
		for(x = 1; x <= rules->num_rows; x++)
		{
			while(rules->address_host_table[pos] != 0)
				pos++;
			pos++;
			rules->rule_names[x] = pos;
			if(x < rules->num_rows && rules->address_host_table[pos] < 0)
				rules->totalpreds++;
		}
		rules->num_columns = rules->rule_names[1] - 2;
		rules->num_rows -= rules->totalpreds; /*quita los builtin del total*/
		rules++;
	}
}

/*Determine if a predicate refers to a fact or a rule*/
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
			if(temp == actual->name)
			{
				n1.name = temp;
				res1 = lower_bound(facts, fend, n1, compare);
				if (res1 != fend && res1->name == temp)
				{			
					actual->referencias[x-1] = -(res1 - facts) - 1;
					cont++;
				}
				else
					actual->referencias[x-1] = actual - rules;
			}			
			else
			{
				n2.name = temp;
				res2 = lower_bound(rules, end, n2, comparer);
				if(res2 != end && res2->name == temp)
					actual->referencias[x-1] = res2 - rules;
				else
				{
					n1.name = temp;
					res1 = lower_bound(facts, fend, n1, compare);
					actual->referencias[x-1] = -(res1 - facts) - 1;
					cont++;
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

/*Creates an array for each selections found in a predicate for all rules*/
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
			memmove(actual->select[sl], pv, tam);
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

int2 columnsproject(int *first, int tam, int *rule, int ini, int fin, int sini, int sfin, int **res, int **newrule, int **preds, int2 *nump, int pact, int ptot)
{
	int x, y, z, temp, size;
	int pv[MAXVALS], pv2[MAXVALS];
	int2 ret = make_int2(0, 0);
	for(x = 0; x < tam; x++)
	{
		y = 0;
		temp = first[x];
		for(y = 0; y < ini; y++)
		{
			if(temp == rule[y] && temp > 0) /*added condition to avoid constants*/
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
			if(temp == rule[y] && temp > 0)
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
		if(y != fin)
			continue;
		for(y = pact; y < ptot; y++)
		{
			size = nump[y].x + nump[y].y;
			for(z = 1; z < size; z+=3)
			{
				if((temp == preds[y][z] || temp == preds[y][z+1]) && temp > 0)
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
			if(z != size)
				break;
		}

	}
	ret.x = ret.y;
	for(x = sini; x < sfin; x++)
	{
		y = 0;
		temp = rule[x];
		for(y = 0; y < ini; y++)
		{
			if(temp == rule[y] && temp > 0)
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
			if(temp == rule[y] && temp > 0)
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
		for(y = pact; y < ptot; y++)
		{
			size = nump[y].x + nump[y].y;
			for(z = 1; z < size; z+=3)
			{
				if((temp == preds[y][z] || temp == preds[y][z+1]) && temp > 0)
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
			if(z != size)
				break;
		}
	}

	temp = ret.y * sizeof(int);
	free(*newrule);
	*newrule = (int *)malloc(temp);
	memmove(*newrule, pv, temp);
	*res = (int *)malloc(temp);
	memmove(*res, pv2, temp);
	return ret;
}

/*Determines all possible joins between two adjacent predicates*/
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
	memmove(*res, joins, temp);
	return cont;
}

template<class InputIterator>
void builtinmark(InputIterator actual, InputIterator end)
{
	int x, y, z;
	int ini, fin, bus;

	while(actual != end)
	{
		for(x = 0; x < (actual->num_rows - 1); x++)
		{
			ini = actual->rule_names[x+1] + 1;
			fin = actual->rule_names[x+2] - 1;

			for(y = 1; y < actual->numpreds[x].x; y += 2)
			{
				bus = actual->preds[x][y];
				if(bus > 0)
				{
					for(z = ini; z < fin; z++)
					{
						if(actual->address_host_table[z] == bus)
						{
							actual->preds[x][y] = z - ini;
							break;
						}
					}
				}
				y++;
				bus = actual->preds[x][y];
				if(bus > 0)
				{
					for(z = ini; z < fin; z++)
					{
						if(actual->address_host_table[z] == bus)
						{
							actual->preds[x][y] = z - ini;
							break;
						}
					}
				}
			}
		}
		actual++;
	}
}

/*Creates arrays for each predicate with the positions of join and projection columns*/
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

			if(actual->numsel[0] == 0 && actual->numselfj[0] == 0 && actual->num_columns == (fin - ini) && actual->numpreds[0].x == 0)
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
			memmove(actual->project[0], fjoin, temp);
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
		total = actual->num_rows + actual->totalpreds;
		fin = actual->rule_names[total] - 1;
		pos.y = actual->rule_names[2] - actual->rule_names[1] - 2;
		temp = pos.y * sizeof(int);
		pv = (int *)malloc(temp);
		memmove(pv, actual->address_host_table + actual->rule_names[1] + 1, temp);

		for(x = 2, y = 0; x <= numjoins; x++, y++)
		{
			rulestart = actual->rule_names[x] + 1;
			ruleend = actual->rule_names[x+1] - 1;
			temp = wherejoin(pv, pos.y, actual->address_host_table + rulestart, ruleend - rulestart, &res);
			actual->wherejoin[y] = res;
			actual->numjoin[y] = temp;
			pos = columnsproject(pv, pos.y, actual->address_host_table, ini, fin, rulestart, ruleend, &res, &pv, actual->preds, actual->numpreds, y + 2, actual->num_rows);
			actual->project[y] = res;
			actual->projpos[y] = pos;
		}

		rulestart = actual->rule_names[actual->num_rows-1] + 1;
		ruleend = actual->rule_names[actual->num_rows] - 1; 
		temp = wherejoin(pv, pos.y, actual->address_host_table + rulestart, ruleend - rulestart, &res);
		actual->wherejoin[y] = res;
		actual->numjoin[y] = temp;
		numjoins--;

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
		memmove(actual->project[numjoins], fjoin, temp);
		pos.y = pos.x;
		actual->projpos[numjoins] = pos;
		actual++;
	}
}

/*Creates an array for each predicate for all rules where selfjoin positions are stored*/
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
			memmove(rulecpy, actual->address_host_table + pos + 1, tam * sizeof(int));
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
			memmove(actual->selfjoin[temp], fjoin, tam);
			actual->numselfj[temp] = cont;
		}
		actual++;
	}
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
		memmove(res->select[0], sel, size);
		cont1 = 0;
	}
	if(cont2 > 0)
	{
		size = cont2 * sizeof(int);
		res->project = (int **)malloc(sizeof(int *));
		res->project[0] = (int *)malloc(size);
		memmove(res->project[0], pro, size);
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
		memmove(res->selfjoin[0], sel, size);
	}
}

/*Discards finished rules based on their predicates. Rules with only facts finish after one iteration. Rules with other rules as predicates finish when at least one of the rules finishes.*/
void discardRules(list<rulenode> *reglas, int itr)
{
	list<rulenode>::iterator begin = reglas->begin();
	list<rulenode>::iterator end = reglas->end();
	list<rulenode>::iterator busqueda, rul_act = begin;
	int x, num_refs, tipo;
	rulenode completed;
	while(rul_act != end)
	{
		if(rul_act->gen_act == -1 || rul_act->gen_ant == -1)
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
			if(!binary_search(begin, end, completed, comparer))
			{
				rul_act->gen_act = -1;
				break;
			}

			tipo = rul_act->name;
			if(generadas(tipo, rul_act->num_rows, rul_act->num_columns, itr))
			{
				rul_act->gen_act = -1;
				busqueda = rul_act;
				busqueda++;
				while(busqueda != end && busqueda->name == tipo)
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
	rul_act = begin;
	while(rul_act != end)
	{
		if(rul_act->gen_act == -1)
			rul_act = reglas->erase(rul_act);
		else
			rul_act++;
	}
}

/*Display information about each rule*/
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
		num--;
		cout << "project = " << endl;
		for(y = 0; y < num; y++)
		{	
			cout << actual->projpos[y].x << " " << actual->projpos[y].y << ": ";
			for(z = 0; z < actual->projpos[y].y; z++)
				cout << actual->project[y][z] << " ";
			cout << endl;
		}
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

extern "C"
 void Cuda_Statistics(void)
{
  cerr << "GPU Statistics" << endl;
#if TIMER
  cerr << "Called " << cuda_stats.calls << "times." << endl;
  cerr << "GPU time " << cuda_stats.total_time << "msec." << endl;
  cerr << "Longest call " << cuda_stats.max_time << "msec." << endl;
  cerr << "Fastest call " << cuda_stats.min_time << "msec." << endl << endl;
  cerr << "Steps" << endl;
  cerr << "    Select First: " << cuda_stats.select1_time << " msec." << endl;
  cerr << "    Select Second: " << cuda_stats.select2_time << " msec." << endl;
  cerr << "    Sort: " << cuda_stats.sort_time << " msec." << endl;
  cerr << "    Join: " << cuda_stats.join_time << " msec." << endl;
  cerr << "    Union: " << cuda_stats.union_time << " msec." << endl;
  cerr << "    Built-in: " << cuda_stats.pred_time << " msec." << endl << endl;
  cerr << "Operations" << endl;
  cerr << "    Joins: " << cuda_stats.joins << "." << endl;
  cerr << "    Selects/Projects: " << cuda_stats.selects << "." << endl;
  cerr << "    Unions: " << cuda_stats.unions << "." << endl;
  cerr << "    Built-ins: " << cuda_stats.builtins << "." << endl << endl;
#endif
}

vector<gpunode> L;

extern "C"
int Cuda_Eval(predicate **inpfacts, int ninpf, predicate **inprules, int ninpr, int *inpquery, int **result, char *names, int finalDR)
{
	hipSetDevice(0);
	vector<rulenode> rules;
	int x;

#if TIMER
	cuda_stats.calls++;
#endif

	#ifdef ROCKIT
	if(finalDR)
		ninpf *= -1;
	else //this else makes the 'for' conditional
	#else
	L.clear();
	#endif
	for(x = 0; x < ninpf; x++)
		L.push_back(*inpfacts[x]);
	meter(&rules, inprules, ninpr);

	#ifdef TUFFY
	PGconn *conn = NULL;
	postgresRead(&conn, &L, inpquery, names, finalDR);
	#endif
	#ifdef ROCKIT
	MYSQL *con = NULL;
	mysqlRead(&con, inpquery, &L, ninpf, names, finalDR);
	#endif

	int res_rows = 0, rows1, rows2;
	int tipo;
	int *dop1, *dop2, *res;
	
	vector<rulenode>::iterator rul_str, fin;
	sort(L.begin(), L.end(), compare);
	sort(rules.begin(), rules.end(), comparer);
	rul_str = rules.begin();
	fin = rules.end();

	nombres(rul_str, fin); /*preprocessing*/
	movebpreds(rul_str, fin);
	referencias(L.begin(), L.end(), rul_str, fin);
	seleccion(rul_str, fin);
	selfjoin(rul_str, fin);
	proyeccion(rul_str, fin);
	builtinmark(rul_str, fin);
	//mostrarcontenido(rul_str, fin);

	list<rulenode>::iterator rul_act, busqueda;
	list<rulenode> reglas(rul_str, fin);
	
	//cargareglas(&rules, qname, &reglas);
	//mostrareglas(reglas);

	gpunode tmpfact;
	rulenode tmprule;
	int name1, filas1, cols1, isfact1, name2, filas2, cols2, isfact2;
	int *table1, *table2;
	int num_refs, itr = 0, genflag = 0;
	vector<gpunode>::iterator qposf;
	vector<rulenode>::iterator qposr;

#if TIMER
	hipEvent_t start, stop;
	float time;
	hipEventCreate(&start);
	hipEventCreate(&stop);
	hipEventRecord(start, 0);
#endif

	while(reglas.size()) /*Here's the main loop*/
	{
		rul_act = reglas.begin();

		while(rul_act != reglas.end()) /*Here's the loop that evaluates each rule*/
		{
			tipo = rul_act->referencias[0];

			//cout << "tipo top = " << tipo << endl;

			if(tipo < 0)
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

			//cout << "inicio " << rul_act->name << endl << "name1 = " << name1 << " filas1 = " << filas1 << " cols1 = " << cols1 << " isfact1 = " << isfact1 << endl;

			rows1 = cargar(name1, filas1, cols1, isfact1, table1, &dop1, itr);

			//cout << "rows1 = " << rows1 << endl;

			if(rows1 == 0)
			{
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
					hipMemcpyAsync(res, dop1, num_refs, hipMemcpyDeviceToDevice);
					registrar(rul_act->name, cols1, res, rows1, itr, 1);
					genflag = 1;
					rul_act->gen_ant = rul_act->gen_act;
					rul_act->gen_act = rows1;

					if(isfact1)
					{
						tmprule.name = name1;
						qposr = lower_bound(rul_str, fin, tmprule, comparer);
						if (qposr != fin && qposr->name == name1)
							rul_act->referencias[0] =  qposr - rul_str;
					}
				}
				else
				{
					res_rows = selectproyect(dop1, rows1, cols1, rul_act->num_columns, rul_act->select[0], rul_act->numsel[0], rul_act->selfjoin[0], rul_act->numselfj[0], rul_act->preds[0], rul_act->numpreds[0].x, rul_act->project[0], &res, finalDR);

					//cout << "name = " << rul_act->name << " res_rows = " << res_rows << endl; 

					if(res_rows > 0)
					{
						registrar(rul_act->name, rul_act->num_columns, res, res_rows, itr, 1);
						genflag = 1;
						rul_act->gen_ant = rul_act->gen_act;
						rul_act->gen_act = res_rows;
						
						if(isfact1)
						{
							tmprule.name = name1;
							qposr = lower_bound(rul_str, fin, tmprule, comparer);
							if (qposr != fin && qposr->name == name1)
								rul_act->referencias[0] =  qposr - rul_str;
						}
					}
					else
						rul_act->gen_act = 0;
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

			//cout << "name2 = " << name2 << " filas2 = " << filas2 << " cols2 = " << cols2 << " isfact2 = " << isfact2 << " itr = " << itr << endl;

			rows2 = cargar(name2, filas2, cols2, isfact2, table2, &dop2, itr);
			
			//cout << "rows2 = " << rows2 << endl;

			res = NULL;
			if(rows2 == 0)
			{
				if(rul_act->negatives[2])
				{
					if(rul_act->num_rows == 3)
						project(dop1, rows1, cols1, rul_act->num_columns, rul_act->project[0], &res, 1);
					else
						project(dop1, rows1, cols1, rul_act->projpos[0].x, rul_act->project[0], &res, 0);
					res_rows = rows1;
				}
				else
				{
					rul_act->gen_act = 0;
					rul_act++;
					continue;
				}
			}
			else
			{
				#ifdef ROCKIT
				if(rul_act->numjoin[0] == 0)
				{
					juntar(dop1, dop2, rows1, rows2, cols1, cols2, rul_act->project[0], rul_act->num_columns, &res);
					registrar(rul_act->name, rul_act->num_columns, res, rows1 * rows2, itr, 1);
					rul_act++;
					continue;
				}
				#endif
				res_rows = join(dop1, dop2, rows1, rows2, cols1, cols2, rul_act, 0, 1, &res, finalDR);
			}

			//cout << "res_rows = " << res_rows << " num_refs = " << rul_act->num_rows << " numcols = " << rul_act->projpos[0].y << endl;

			if(res_rows == 0)
			{
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

				//cout << "name2 = " << name2 << " filas2 = " << filas2 << " cols2 = " << cols2 << " isfact2 = " << isfact2 << " itr = " << itr << endl;

				rows2 = cargar(name2, filas2, cols2, isfact2, table2, &dop2, itr);

				//cout << "rows = " << x << " " << rows2 << endl;
				
				if(rows2 == 0)
				{
					if(rul_act->negatives[x+1])
					{
						if(x == rul_act->num_rows - 2)
							project(res, res_rows, rul_act->projpos[x-2].y, rul_act->num_columns, rul_act->project[x-1], &res, 1);
						else
							project(res, res_rows, rul_act->projpos[x-2].y, rul_act->projpos[x-1].x, rul_act->project[x-1], &res, 0);
						continue;
					}
					break;
				}

				res_rows = join(res, dop2, res_rows, rows2, rul_act->projpos[x-2].y, cols2, rul_act, x-1, 0, &res, finalDR);

				//cout << "resrows = " << res_rows << endl;
				
			}
#ifdef ROCKIT
			if(x == num_refs)
				registrar(rul_act->name, rul_act->num_columns, res, res_rows, itr, 1);
			rul_act++;
		}
		reglas.clear();
	}
#else
			if(x == num_refs)
			{
				#ifdef TIMER
				hipEvent_t start2, stop2;
				hipEventCreate(&start2);
				hipEventCreate(&stop2);
				hipEventRecord(start2, 0);
				#endif

				//cout << rul_act->name << " res_rows = " << res_rows << endl;

				if(finalDR)
					res_rows = unir(res, res_rows, rul_act->num_columns, &res, 0);

				#ifdef TIMER
				hipEventRecord(stop2, 0);
				hipEventSynchronize(stop2);
				hipEventElapsedTime(&time, start2, stop2);
				hipEventDestroy(start2);
				hipEventDestroy(stop2);
				//cout << "Union = " << time << endl;
				cuda_stats.union_time += time;
				#endif					
	
				//cout << "despues de unir = " << res_rows << endl;

				registrar(rul_act->name, rul_act->num_columns, res, res_rows, itr, 1);
				genflag = 1;
				rul_act->gen_ant = rul_act->gen_act;
				rul_act->gen_act = res_rows;

				for(x = 0; x < num_refs; x++)
				{
					if(rul_act->referencias[x] < 0)
					{
						tmprule.name = rul_act->address_host_table[rul_act->rule_names[x+1]];
						qposr = lower_bound(rul_str, fin, tmprule, comparer);
						if (qposr != fin && qposr->name == tmprule.name)
							rul_act->referencias[x] =  qposr - rul_str;
					}
				}
			}
			else
				rul_act->gen_act = 0;
			rul_act++;
		}
		if(genflag != 1)
			break;
		else
			genflag = 0;
		discardRules(&reglas, itr);

		//cout << "itr " << itr << endl;

		itr++;
	}
#endif

	#ifdef DATALOG
	datalogWrite(inpquery[0], rul_str, fin, finalDR, result);
	#else
	res_rows = 0;
	#endif
	#ifdef TUFFY
	postgresWrite(inpquery, ninpf, rul_str, fin, &L, conn, finalDR);
	#endif
	#ifdef ROCKIT
	mysqlWrite(rul_str, fin, &L, con);
	#endif

#if TIMER
	hipEventRecord(stop, 0);
	hipEventSynchronize(stop);
	hipEventElapsedTime(&time, start, stop);
	cuda_stats.total_time += time;
	if (time > cuda_stats.max_time) 
	  cuda_stats.max_time = time;
	if (time < cuda_stats.min_time || cuda_stats.calls == 1) 
	  cuda_stats.min_time = time;
	hipEventDestroy(start);
	hipEventDestroy(stop);
	Cuda_Statistics();
#endif

	return res_rows;
}

