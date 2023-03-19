__global__ void marcar2(int *dop1, int rows, int cols, int *cons, int numc, int *res);
extern __global__ void marcar(int *dop1, int rows, int cols, int *cons, int numc, int *res);
extern __global__ void samejoin(int *dop1, int rows, int cols, int *dhead, int cont, int *res);
extern __global__ void samejoin2(int *dop1, int rows, int cols, int *dhead, int cont, int *res);
extern __global__ void proyectar(int *dop1, int rows, int cols, int *dhead, int hsize, int *res);
extern __global__ void llenarproyectar(int *dop1, int rows, int cols, int *temp, int *dhead, int hsize, int *res);

extern int selectproyect(int *dop1, int rows, int cols, int head_size, int *select, int numselect, int *selfjoin, int numselfj, int *preds, int numpreds, int *project, int **ret, int ANDlogic);

extern void project(int *res, int resrows, int numcols1, int numcols2, int *proj, int **ret, int type);
extern int join(int *p1, int *p2, int rLen, int sLen, int of1, int of2, list<rulenode>::iterator rule, int pos, int bothops, int **ret, int ANDlogic);
