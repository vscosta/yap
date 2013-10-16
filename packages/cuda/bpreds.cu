__global__ void predicates(int *dop1, int rows, int cols, int *cons, int numc, int *res)
{
 	extern __shared__ int shared[];
	int id = blockIdx.x * blockDim.x + threadIdx.x;
	int x, rowact, op1, op2;
	if(threadIdx.x < numc)
		shared[threadIdx.x] = cons[threadIdx.x];
	__syncthreads();
	if(id < rows)
	{
		rowact = id * cols;
		for(x = 0; x < numc; x += 3)
		{
			op1 = shared[x+1];
			if(op1 < 0)
				op1 *= -1;
			else
				op1 = dop1[rowact + op1];
			op2 = shared[x+2];
			if(op2 < 0)
				op2 *= -1;
			else
				op2 = dop1[rowact + op2];
			switch(shared[x])
			{
				case SBG_EQ:  if(op1 != op2)
						return;
				  break;
				case SBG_GT: if(op1 <= op2)
						return;
				  break;
				case SBG_LT: if(op1 >= op2)
						return;
				  break;
				case SBG_GE: if(op1 < op2)
						return;
				  break;
				case SBG_LE: if(op1 > op2)
						return;
				  break;
				case SBG_DF: if(op1 == op2)
						return;
			}
		}
		res[id] = 1;
	}
}

int bpreds(int *dop1, int rows, int cols, int *bin, int3 numpreds, int **ret)
{
	int *temp;
	int tmplen = rows + 1;
	int size = tmplen * sizeof(int);
	reservar(&temp, size);
	// DEBUG_MEM cerr << "+ " << temp << " temp bpreds " << size << endl;
	cudaMemset(temp, 0, size);

#if TIMER
	cuda_stats.builtins++;
#endif
	int *dhead;
	int predn = numpreds.x * 3;
	int spredn = predn * sizeof(int);
	int sproj = numpreds.z * sizeof(int);
	int hsize;
	if(predn > numpreds.z)
		hsize = spredn;
	else
		hsize = sproj;
	reservar(&dhead, hsize);
	// DEBUG_MEM cerr << "+ " << dhead << " dhead  " << hsize << endl;
	cudaMemcpy(dhead, bin, spredn, cudaMemcpyHostToDevice);

	int blockllen = rows / 1024 + 1;
	int numthreads = 1024;

	/*int x;
	cout << "arraypreds = ";
	for(x = 0; x < predn; x++)
		cout << bin[x] << " ";
	cout << endl;
	cout << "temptable = ";
	for(x = 0; x < numpreds.z; x++)
		cout << bin[x+predn] << " ";
	cout << endl; 
	int y;
	int *hop1 = (int *)malloc(numpreds.y * rows * sizeof(int));
	cudaMemcpy(hop1, dop1, numpreds.y * rows * sizeof(int), cudaMemcpyDeviceToHost);
	for(x = 0; x < rows; x++)
	{
		for(y = 0; y < numpreds.y; y++)
			cout << hop1[x * numpreds.y + y] << " ";
		cout << endl;
	}
	free(hop1);*/

	predicates<<<blockllen, numthreads, spredn>>>(dop1, rows, numpreds.y, dhead, predn, temp + 1);

	/*int y;
	int *hop1 = (int *)malloc((rows + 1) * sizeof(int));
	cudaMemcpy(hop1, temp, (rows + 1) * sizeof(int), cudaMemcpyDeviceToHost);
	for(x = 0; x < (rows + 1); x++)
		cout << hop1[x] << " ";
	cout << endl;
	free(hop1);*/

	thrust::device_ptr<int> res;
	res = thrust::device_pointer_cast(temp);
	thrust::inclusive_scan(res + 1, res + tmplen, res + 1);
	int num = res[rows];
	if(num == 0)
		return 0;

	int *fres;
	reservar(&fres, num * sproj);
	// DEBUG_MEM cerr << "+ " << fres << " fres  " << num * sproj << endl;
	cudaMemcpy(dhead, bin + predn, sproj, cudaMemcpyHostToDevice);
	llenarproyectar<<<blockllen, numthreads, sproj>>>(dop1, rows, numpreds.y, temp, dhead, numpreds.z, fres);

	/*int y;
	int *hop1 = (int *)malloc(numpreds.z * num * sizeof(int));
	cudaMemcpy(hop1, fres, numpreds.z * num * sizeof(int), cudaMemcpyDeviceToHost);
	for(x = 0; x < num; x++)
	{
		for(y = 0; y < numpreds.z; y++)
			cout << hop1[x * numpreds.z + y] << " ";
		cout << endl;
	}
	free(hop1);*/

	liberar(dhead, hsize);
	liberar(temp, size);
	liberar(dop1, rows * cols * sizeof(int));

	*ret = fres;
	return num;
}
