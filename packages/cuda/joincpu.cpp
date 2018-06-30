#include "CC_CSSTree.h"
#include <vector>
#include <thrust/sort.h>
#include <thrust/system/omp/execution_policy.h>
#include "pred.h"

void partInlj(Record *R, int rLen, CC_CSSTree *tree, Record *S, int startS, int endS, int of1, int of2, vector<int> *res, int *p1, int *p2, int *perm, int *proj, int wj, int halfrul, int lenrul)
{
	//set_thread_affinity(cpuid,NUM_T);
	int i=0;
	int k=0;
	int curIndex=0;
	int keyForSearch;
	int y, posS, posR;
	for(k=startS; k<endS; k++)
	{
		if(S == NULL)
			posS = k * of2;
		else
			posS = S[k] * of2;
		keyForSearch=p2[posS + wj];
		curIndex=tree->search(keyForSearch);
		for(i=curIndex-1;i>0;i--)
		{
			if(keyForSearch == R[i])
			{

				//cout << keyForSearch << endl;

				posR = perm[i] * of1;
				for(y = 0; y < halfrul; y++)
					res->push_back(p1[posR + proj[y]]);
				for(; y < lenrul; y++)
					res->push_back(p2[posS + proj[y]]);
			}
			else
				if(R[i]<keyForSearch)
				break;
		}
		for(i=curIndex;i<rLen;i++)
		{	
			if(keyForSearch == R[i])
			{

				//cout << -i << " " << keyForSearch << endl;

				posR = perm[i] * of1;
				for(y = 0; y < halfrul; y++)
					res->push_back(p1[posR + proj[y]]);
				for(; y < lenrul; y++)
					res->push_back(p2[posS + proj[y]]);
			}
			else
				if(R[i]>keyForSearch)
				break;
		}
	}
}

void partInlj2(Record *R, int rLen, CC_CSSTree *tree, Record *S, int startS, int endS, int of1, int of2, vector<int> *res, int *p1, int *p2, int *perm, int *proj, int cols, int wj)
{
	//set_thread_affinity(cpuid,NUM_T);
	int i=0;
	int k=0;
	int curIndex=0;
	int keyForSearch;
	int y, cond, posS, posR;
	for(k=startS; k<endS; k++)
	{
		if(S == NULL)
			posS = k * of2;
		else
			posS = S[k] * of2;
		keyForSearch=p2[posS + wj];
		curIndex=tree->search(keyForSearch);
		for(i=curIndex-1;i>0;i--)
		{
			if(keyForSearch == R[i])
			{

				//cout << keyForSearch << endl;

				posR = perm[i] * of1 - 1;
				for(y = 0; y < cols; y++)
				{
					cond = proj[y];
					if(cond > 0)
						res->push_back(p1[posR + cond]);
					else
						res->push_back(p2[posS - cond - 1]);
				}
			}
			else
				if(R[i]<keyForSearch)
				break;
		}
		for(i=curIndex;i<rLen;i++)
		{	
			if(keyForSearch == R[i])
			{

				//cout << -i << " " << keyForSearch << endl;

				posR = perm[i] * of1 - 1;
				for(y = 0; y < cols; y++)
				{
					cond = proj[y];
					if(cond > 0)
						res->push_back(p1[posR + cond]);
					else
						res->push_back(p2[posS - cond - 1]);
				}
			}
			else
				if(R[i]>keyForSearch)
				break;
		}
	}
}

void multipartInlj(Record *R, int rLen, CC_CSSTree *tree, Record *S, int startS, int endS, int of1, int of2, vector<int> *res, int *p1, int *p2, int *perm, int *proj, int *wj, int numj, int halfrul, int lenrul)
{
	//set_thread_affinity(cpuid,NUM_T);
	int i=0;
	int k=0;
	int curIndex=0;
	int keyForSearch;
	int y, posS, posR;
	for(k=startS; k<endS; k++)
	{
		if(S == NULL)
			posS = k * of2;
		else
			posS = S[k] * of2;
		keyForSearch=p2[posS + wj[1]];
		curIndex=tree->search(keyForSearch);
		for(i=curIndex-1;i>0;i--)
		{
			if(keyForSearch == R[i])
			{
				posR = perm[i] * of1;
				for(y = 2; y < numj; y += 2)
				{
					if(p1[posR + wj[y]] != p2[posS + wj[y+1]])
						break;
				}
				if(y < numj)
					continue;
				for(y = 0; y < halfrul; y++)
					res->push_back(p1[posR + proj[y]]);
				for(; y < lenrul; y++)
					res->push_back(p2[posS + proj[y]]);
			}
			else
				if(R[i]<keyForSearch)
				break;
		}
		for(i=curIndex;i<rLen;i++)
		{	
			if(keyForSearch == R[i])
			{
				posR = perm[i] * of1;
				for(y = 2; y < numj; y += 2)
				{
					if(p1[posR + wj[y]] != p2[posS + wj[y+1]])
						break;
				}
				if(y < numj)
					continue;
				for(y = 0; y < halfrul; y++)
					res->push_back(p1[posR + proj[y]]);
				for(; y < lenrul; y++)
					res->push_back(p2[posS + proj[y]]);
			}
			else
				if(R[i]>keyForSearch)
				break;
		}
	}
}

void multipartInlj2(Record *R, int rLen, CC_CSSTree *tree, Record *S, int startS, int endS, int of1, int of2, vector<int> *res, int *p1, int *p2, int *perm, int *proj, int cols, int *wj, int numj)
{
	//set_thread_affinity(cpuid,NUM_T);
	int i=0;
	int k=0;
	int curIndex=0;
	int keyForSearch;
	int y, cond, posS, posR;
	for(k=startS; k<endS; k++)
	{
		if(S == NULL)
			posS = k * of2;
		else
			posS = S[k] * of2;
		keyForSearch=p2[posS + wj[1]];
		curIndex=tree->search(keyForSearch);
		for(i=curIndex-1;i>0;i--)
		{
			if(keyForSearch == R[i])
			{
				posR = perm[i] * of1;
				for(y = 2; y < numj; y += 2)
				{
					if(p1[posR + wj[y]] != p2[posS + wj[y+1]])
						break;
				}
				if(y < numj)
					continue;
				for(y = 0; y < cols; y++)
				{
					cond = proj[y];
					if(cond > 0)
						res->push_back(p1[posR + cond - 1]);
					else
						res->push_back(p2[posS - cond - 1]);
				}
			}
			else
				if(R[i]<keyForSearch)
				break;
		}
		for(i=curIndex;i<rLen;i++)
		{	
			if(keyForSearch == R[i])
			{
				posR = perm[i] * of1;
				for(y = 2; y < numj; y += 2)
				{
					if(p1[posR + wj[y]] != p2[posS + wj[y+1]])
						break;
				}
				if(y < numj)
					continue;
				for(y = 0; y < cols; y++)
				{
					cond = proj[y];
					if(cond > 0)
						res->push_back(p1[posR + cond - 1]);
					else
						res->push_back(p2[posS - cond - 1]);
				}
			}
			else
				if(R[i]>keyForSearch)
				break;
		}
	}
}

void inlj_omp(Record *R, int rLen, CC_CSSTree *tree, Record *S, int sLen, int of1, int of2, vector<int> *res, int *p1, int *p2, int *perm, int *proj, int2 projp, int cols, int* wj, int numj, int tipo)
{
	int i=0;
	int j=0;
	int *startS=new int[NUM_T];
	int *endS=new int[NUM_T];
	int chunkSize=sLen/NUM_T;
	for(i=0;i<NUM_T;i++)
	{
		startS[i]=i*chunkSize;
		if(i==(NUM_T-1))
			endS[i]=sLen;
		else
			endS[i]=(i+1)*chunkSize;

		//cout<<"T"<<i<<", "<<endS[i]-startS[i]<<"; ";

	}

	//cout<<endl;

	//omp_set_num_threads(NUM_T);

	//cout << "inicio" << endl;

	#pragma omp parallel for
	for(j=0;j<NUM_T;j++)
	{
		if(tipo)
		{
			if(numj > 2)
				multipartInlj2(R, rLen, tree, S, startS[j], endS[j], of1, of2, &res[j], p1, p2, perm, proj, cols, wj, numj);
			else
				partInlj2(R, rLen, tree, S, startS[j], endS[j], of1, of2, &res[j], p1, p2, perm, proj, cols, wj[1]);
		}
		else
		{
			if(numj > 2)
				multipartInlj(R, rLen, tree, S, startS[j], endS[j], of1, of2, &res[j], p1, p2, perm, proj, wj, numj, projp.x, projp.y);
			else
				partInlj(R, rLen, tree, S, startS[j], endS[j], of1, of2, &res[j], p1, p2, perm, proj, wj[1], projp.x, projp.y);
		}
	}
	
	//cout << "fin" << endl;
	
	delete startS;
	delete endS;
}

int joincpu(int *p1, int *p2, int rLen, int sLen, int of1, int of2, list<rulenode>::iterator rule, int pos, int bothops, int **ret)
{
	int pos2 = pos + 1;
	int *sel1, nsel1 = 0;
	int *sel2 = rule->select[pos2];
	int nsel2 = rule->numsel[pos2];
	int *proj = rule->project[pos];
	int2 projp = rule->projpos[pos];
	int *sjoin1, nsj1 = 0;
	int *sjoin2 = rule->selfjoin[pos2];
	int nsj2 = rule->numselfj[pos2];
	int *wherej = rule->wherejoin[pos];
	int numj = rule->numjoin[pos];
	int size, *fres, ini[NUM_T], *temp;
	int x, tipo = 0;
	int *Sres = NULL, *Rres, Snl, Rnl, *permutation;

	if(bothops)
	{
		sel1 = rule->select[pos];
		nsel1 = rule->numsel[pos];
		sjoin1 = rule->selfjoin[pos];
		nsj1 = rule->numselfj[pos];
	}

	#ifdef TIMER
	hipEvent_t start, stop;
	float time;
	hipEventCreate(&start);
	hipEventCreate(&stop);
	hipEventRecord(start, 0);
	#endif

	if(nsel1 > 0 || nsj1 > 0)
		Rnl = selectproyectcpu2(p1, rLen, of1, sel1, nsel1, sjoin1, nsj1, wherej[0], &Rres, &permutation);
	else
	{

		/*cout << "sin sel" << endl;
		cout << "valores = " << rLen << " " << of1 << " " << wherej[0] << endl;
		for(x = 0; x < 100; x++)
			cout << p1[x] << " ";
		cout << endl;
		cout << "ultimo = " << p1[of1 * rLen - 1] << endl;*/ 

		Rnl = rLen;
		size = Rnl * sizeof(int);
		permutation = (int *)malloc(size);
		Rres = (int *)malloc(size);
		#pragma omp parallel for firstprivate(of1)
		for(x = 0; x < Rnl; x++)
		{
			permutation[x] = x;
			Rres[x] = p1[of1 * x + wherej[0]];
		}

		//cout << "sin sel fin" << endl;

	}

	#ifdef TIMER
	hipEventRecord(stop, 0);
	hipEventSynchronize(stop);
	hipEventElapsedTime(&time, start, stop);
	cuda_stats.select1_time += time;

	hipEventDestroy(start);
	hipEventDestroy(stop);
	hipEventCreate(&start);
	hipEventCreate(&stop);
	hipEventRecord(start, 0);
	#endif

	if(nsel2 > 0 || nsj2 > 0)
	{
		//cout << "con sel S" << endl;

		Snl = selectproyectcpu2(p2, sLen, of2, sel2, nsel2, sjoin2, nsj2, wherej[1], &Sres, NULL);
	}
	else
		Snl = sLen;

	#ifdef TIMER
	hipEventRecord(stop, 0);
	hipEventSynchronize(stop);
	hipEventElapsedTime(&time, start, stop);
	cuda_stats.select2_time += time;

	hipEventDestroy(start);
	hipEventDestroy(stop);
	hipEventCreate(&start);
	hipEventCreate(&stop);
	hipEventRecord(start, 0);
	#endif

	//cout << "antes" << endl;

	/*cout << "antes" << endl;
	for(x = 0; x < Rnl; x++)
		cout << permutation[x] << " ";
	cout << endl;
	for(x = 0; x < 100; x++)
		cout << Rres[x] << " ";
	cout << endl;*/

	thrust::stable_sort_by_key(thrust::omp::par, Rres, Rres + Rnl, permutation);

	#ifdef TIMER
	hipEventRecord(stop, 0);
	hipEventSynchronize(stop);
	hipEventElapsedTime(&time, start, stop);
	cuda_stats.sort_time += time;
	
	hipEventDestroy(start);
	hipEventDestroy(stop);
	hipEventCreate(&start);
	hipEventCreate(&stop);
	hipEventRecord(start, 0);
	#endif

	/*cout << "despues" << endl;
	for(x = 0; x < Rnl; x++)
		cout << permutation[x] << " ";
	cout << endl;
	for(x = 0; x < Rnl; x++)
		cout << Rres[x] << " ";
	cout << endl;*/

	//cout << "despues sort" << endl;

	vector<int> *res = new vector<int>[NUM_T];
	for(x = 0; x < NUM_T; x++)
		res[x].reserve(INISIZE);
	CC_CSSTree *tree = new CC_CSSTree(Rres, Rnl, CSS_TREE_FANOUT);
	if(pos == (rule->num_rows - 3)) // && rule->num_bpreds.x == 0)
		tipo = 1;
	inlj_omp(Rres, Rnl, tree, Sres, Snl, of1, of2, res, p1, p2, permutation, proj, projp, rule->num_columns, wherej, numj, tipo);

	/*cout << "proj = ";
	for(x = 0; x < rule->num_columns; x++)
		cout << proj[x] << " ";
	cout << endl;
	int y,z;
	for(x = 0; x < NUM_T; x++)
	{
		cout << "Thread " << x << endl;
		for(y = 0; y < res[x].size() / projp.y; y++)
		{	
			for(z = 0; z < projp.y; z++)
				cout << res[x][y * projp.y + z] << " ";
			cout << endl;
		}
	}
	cout << "Tamanios" << endl;*/

	size = 0;
	for(x = 0; x < NUM_T; x++)
	{
		ini[x] = res[x].size();
		size += ini[x];
		
		//cout << ini[x] << " " << size << endl;

	}
	fres = (int *)malloc(size * sizeof(int));
	temp = fres;
	for(x = 0; x < NUM_T; x++)
	{
		memmove(temp, res[x].data(), ini[x] * sizeof(int));
		temp += ini[x];
	}

	if(*ret != NULL)
		free(*ret);
	free(Rres);
	free(permutation);
	if(Sres != NULL)
		free(Sres);
	delete tree;
	delete [] res;

	*ret = fres;

	#ifdef TIMER
	hipEventRecord(stop, 0);
	hipEventSynchronize(stop);
	hipEventElapsedTime(&time, start, stop);
	cuda_stats.join_time += time;
	#endif

	//cout << "Projp.x = " << projp.x << " projp.y = " << projp.y << endl;

	/*if(numj > 2)
	{
		cout << "total = " << rLen << " " << size / projp.y << " " << projp.y << " " << rule->num_columns << endl;
		exit(1);
	}*/

	return size / projp.y;
}
