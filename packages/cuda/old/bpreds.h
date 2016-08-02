#ifndef _BPREDS_H_
#define _BPREDS_H_

int maximo(int, ...);
__global__ void bpreds(int*, int*, int, int, int, int*, int, int, int*, int*);
__global__ void bpredsnormal(int*, int, int, int*, int, int*);
__global__ void bpredsnormal2(int*, int, int, int*, int, int*);
__global__ void bpredsOR(int*, int*, int, int, int, int*, int, int, int*, int*);
__global__ void bpredsorlogic(int*, int, int, int*, int, int*);
__global__ void bpredsorlogic2(int*, int, int, int*, int, int*);

#endif
