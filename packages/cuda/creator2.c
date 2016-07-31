#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/*Program used to generate union2.cu and union2.h. A new pointer and all its operations are generated for each set (pairs, triplets, etc.).
 Arguments are the cardinality of the biggest set and the name of the cuda file. For example, executing "creator2 20 union2" will generate 
 all pointers and operations for all sets from 1 to 20 in the files union2.cu and union2.h.*/

int main(int argc, char *argv[])
{
	int num = atoi(argv[1]);
	int x;
	char *str = (char *)malloc((strlen(argv[2]) + 4) * sizeof(char));
	sprintf(str, "%s.cu", argv[2]);
	FILE *cuda = fopen(str, "w");

	fprintf(cuda, "/*Computer generated file to remove duplicates. Since Thrust's unique and sort, unlike their std's counterparts, don't have a way to specify the size of each element in\n");
	fprintf(cuda, "the array, comparing pairs, triplets and other sets is not possible without defining a new pointer and all related operations for each set. If you have a better idea to do\n");
	fprintf(cuda, "this, please don't hesitate to email us.*/\n\n");
	fprintf(cuda, "#include <thrust/device_vector.h>\n");
	fprintf(cuda, "#include <thrust/unique.h>\n");
	fprintf(cuda, "#include <thrust/distance.h>\n");
	fprintf(cuda, "#include <thrust/sort.h>\n");
	fprintf(cuda, "#include <iostream>\n");
	fprintf(cuda, "#include \"memory.h\"\n");
	fprintf(cuda, "#include \"%s.h\"\n\n", argv[2]);
	fprintf(cuda, "int unir(int *res, int rows, int tipo, int **ret, int final)\n");
	fprintf(cuda, "{\n");
	fprintf(cuda, "\tthrust::device_ptr<int> pt, re;\n");
	for(x = 2; x <= num; x++)
		fprintf(cuda, "\tthrust::device_ptr<s%d> pt%d, re%d;\n", x, x ,x);
	for(x = 2; x <= num; x++)
		fprintf(cuda, "\ts%d *t%d;\n", x, x);
	fprintf(cuda, "\tint flag, nrows, *nres, size;\n\n");
	fprintf(cuda, "#if TIMER\n");
	fprintf(cuda, "\tcuda_stats.unions++;\n");
	fprintf(cuda, "#endif\n\n");
	fprintf(cuda, "\tswitch(tipo)\n");
	fprintf(cuda, "\t{\n");
	fprintf(cuda, "\t\tcase 1:\n");
	fprintf(cuda, "\t\t{\n");
	fprintf(cuda, "\t\t\tpt = thrust::device_pointer_cast(res);\n");
	fprintf(cuda, "\t\t\tflag = 0;\n");
	fprintf(cuda, "\t\t\twhile(flag != 1)\n");
	fprintf(cuda, "\t\t\t{\n");
	fprintf(cuda, "\t\t\t\ttry\n");
	fprintf(cuda, "\t\t\t\t{\n");
	fprintf(cuda, "\t\t\t\t\tthrust::sort(pt, pt + rows);\n");
	fprintf(cuda, "\t\t\t\t\tif(final)\n");
	fprintf(cuda, "\t\t\t\t\t{\n");
	fprintf(cuda, "\t\t\t\t\t\tre = thrust::unique(pt, pt + rows, q1());\n");
	fprintf(cuda, "\t\t\t\t\t\tre = thrust::unique(pt, re);\n");
	fprintf(cuda, "\t\t\t\t\t}\n");
	fprintf(cuda, "\t\t\t\t\telse\n");
	fprintf(cuda, "\t\t\t\t\t\tre = thrust::unique(pt, pt + rows);\n");
	fprintf(cuda, "\t\t\t\t\tflag = 1;\n");
	fprintf(cuda, "\t\t\t\t}\n");
	fprintf(cuda, "\t\t\t\tcatch(std::bad_alloc &e)\n");
	fprintf(cuda, "\t\t\t\t{\n");
	fprintf(cuda, "\t\t\t\t\tlimpiar(\"sort/unique in unir\", 0);\n");
	fprintf(cuda, "\t\t\t\t}\n");
	fprintf(cuda, "\t\t\t}\n");
	fprintf(cuda, "\t\t\tnrows = thrust::distance(pt, re);\n");
	fprintf(cuda, "\t\t\tif(nrows < rows / 2)\n");
	fprintf(cuda, "\t\t\t{\n");
	fprintf(cuda, "\t\t\t\tsize = nrows * tipo * sizeof(int);\n");
	fprintf(cuda, "\t\t\t\treservar(&nres, size);\n");
	fprintf(cuda, "\t\t\t\tcudaMemcpyAsync(nres, res, size, hipMemcpyDeviceToDevice);\n");
	fprintf(cuda, "\t\t\t\tcudaFree(*ret);\n");
	fprintf(cuda, "\t\t\t\t*ret = nres;\n");
	fprintf(cuda, "\t\t\t}\n");
	fprintf(cuda, "\t\t\treturn nrows;\n");
	fprintf(cuda, "\t\t}\n");
	for(x = 2; x <= num; x++)
	{
		fprintf(cuda, "\t\tcase %d:\n", x);
		fprintf(cuda, "\t\t{\n");
		fprintf(cuda, "\t\t\tt%d = (s%d*)res;\n", x, x);
		fprintf(cuda, "\t\t\tpt%d = thrust::device_pointer_cast(t%d);\n", x, x);
		fprintf(cuda, "\t\t\tflag = 0;\n");
		fprintf(cuda, "\t\t\twhile(flag != 1)\n");
		fprintf(cuda, "\t\t\t{\n");
		fprintf(cuda, "\t\t\t\ttry\n");
		fprintf(cuda, "\t\t\t\t{\n");
		fprintf(cuda, "\t\t\t\t\tthrust::sort(pt%d, pt%d + rows, o%d());\n", x, x, x);
		fprintf(cuda, "\t\t\t\t\tif(final)\n");
		fprintf(cuda, "\t\t\t\t\t{\n");
		fprintf(cuda, "\t\t\t\t\t\tre%d = thrust::unique(pt%d, pt%d + rows, q%d());\n", x, x, x, x);
		fprintf(cuda, "\t\t\t\t\t\tre%d = thrust::unique(pt%d, re%d, p%d());\n", x, x, x, x);
		fprintf(cuda, "\t\t\t\t\t}\n");
		fprintf(cuda, "\t\t\t\t\telse\n");
		fprintf(cuda, "\t\t\t\t\t\tre%d = thrust::unique(pt%d, pt%d + rows, p%d());\n", x, x, x, x);
		fprintf(cuda, "\t\t\t\t\tflag = 1;\n");
		fprintf(cuda, "\t\t\t\t}\n");
		fprintf(cuda, "\t\t\t\tcatch(std::bad_alloc &e)\n");
		fprintf(cuda, "\t\t\t\t{\n");
		fprintf(cuda, "\t\t\t\t\tlimpiar(\"sort/unique in unir\", 0);\n");
		fprintf(cuda, "\t\t\t\t}\n");
		fprintf(cuda, "\t\t\t}\n");
		fprintf(cuda, "\t\t\tnrows = thrust::distance(pt%d, re%d);\n", x, x);
		fprintf(cuda, "\t\t\tif(nrows < rows / 2)\n");
		fprintf(cuda, "\t\t\t{\n");
		fprintf(cuda, "\t\t\t\tsize = nrows * tipo * sizeof(int);\n");
		fprintf(cuda, "\t\t\t\treservar(&nres, size);\n");
		fprintf(cuda, "\t\t\t\tcudaMemcpyAsync(nres, res, size, hipMemcpyDeviceToDevice);\n");
		fprintf(cuda, "\t\t\t\tcudaFree(*ret);\n");
		fprintf(cuda, "\t\t\t\t*ret = nres;\n");
		fprintf(cuda, "\t\t\t}\n");
		fprintf(cuda, "\t\t\treturn nrows;\n");
		fprintf(cuda, "\t\t}\n");
	}
	fprintf(cuda, "\t}\n");
	fprintf(cuda, "\treturn 0;\n");
	fprintf(cuda, "}\n");

	fclose(cuda);
	sprintf(str, "%s.h", argv[2]);
	cuda = fopen(str, "w"); /*tipo de archivo cambiar*/

	fprintf(cuda, "#ifndef _");
	for(x = 0; x < strlen(argv[2]); x++)
		fprintf(cuda, "%c", toupper(argv[2][x]));
	fprintf(cuda, "_H_\n");
	fprintf(cuda, "#define _");
	for(x = 0; x < strlen(argv[2]); x++)
		fprintf(cuda, "%c", toupper(argv[2][x]));
	fprintf(cuda, "_H_\n\n");
	fprintf(cuda, "int unir(int *res, int rows, int tipo, int **ret, int final);\n\n");
	for(x = 2; x <= num; x++)
	{
		fprintf(cuda, "typedef struct n%d\n", x);
		fprintf(cuda, "{\n");
		fprintf(cuda, "\tint v[%d];\n", x);
		fprintf(cuda, "}s%d;\n\n", x);
	}
	fprintf(cuda, "struct q1\n");
	fprintf(cuda, "{\n");
	fprintf(cuda, "\t__host__ __device__\n");
	fprintf(cuda, "\tbool operator()(const int &r1, const int &r2)\n");
	fprintf(cuda, "\t{\n");
	fprintf(cuda, "\t\tif(r1 != r2)\n");
	fprintf(cuda, "\t\t\treturn true;\n");
	fprintf(cuda, "\t\treturn false;\n");
	fprintf(cuda, "\t}\n");
	fprintf(cuda, "};\n\n");
	for(x = 2; x <= num; x++)
	{
		fprintf(cuda, "struct p%d\n", x);
		fprintf(cuda, "{\n");
		fprintf(cuda, "\t__host__ __device__\n");
		fprintf(cuda, "\tbool operator()(const s%d &r1, const s%d &r2)\n", x, x);
		fprintf(cuda, "\t{\n");
     		fprintf(cuda, "\t\tint x;\n");
		fprintf(cuda, "\t\tfor(x = 0; x < %d; x++)\n", x);
		fprintf(cuda, "\t\t{\n");
		fprintf(cuda, "\t\t\tif(r1.v[x] != r2.v[x])\n");
		fprintf(cuda, "\t\t\t\treturn false;\n");
		fprintf(cuda, "\t\t}\n");
		fprintf(cuda, "\t\treturn true;\n");
    		fprintf(cuda, "\t}\n");
		fprintf(cuda, "};\n\n");
		fprintf(cuda, "struct q%d\n", x);
		fprintf(cuda, "{\n");
		fprintf(cuda, "\t__host__ __device__\n");
		fprintf(cuda, "\tbool operator()(const s%d &r1, const s%d &r2)\n", x, x);
		fprintf(cuda, "\t{\n");
     		fprintf(cuda, "\t\tint x;\n");
		fprintf(cuda, "\t\tfor(x = 0; x < %d; x++)\n", x);
		fprintf(cuda, "\t\t{\n");
		fprintf(cuda, "\t\t\tif(r1.v[x] != r2.v[x])\n");
		fprintf(cuda, "\t\t\t\treturn true;\n");
		fprintf(cuda, "\t\t}\n");
		fprintf(cuda, "\t\treturn false;\n");
    		fprintf(cuda, "\t}\n");
		fprintf(cuda, "};\n\n");
		fprintf(cuda, "struct o%d\n", x);
		fprintf(cuda, "{\n");
		fprintf(cuda, "\t__host__ __device__\n");
		fprintf(cuda, "\tbool operator()(const s%d &r1, const s%d &r2)\n", x, x);
		fprintf(cuda, "\t{\n");
     		fprintf(cuda, "\t\tint x;\n");
		fprintf(cuda, "\t\tfor(x = 0; x < %d; x++)\n", x);
		fprintf(cuda, "\t\t{\n");
		fprintf(cuda, "\t\t\tif(r1.v[x] > r2.v[x])\n");
		fprintf(cuda, "\t\t\t\treturn true;\n");
		fprintf(cuda, "\t\t\tif(r1.v[x] < r2.v[x])\n");
		fprintf(cuda, "\t\t\t\treturn false;\n");
		fprintf(cuda, "\t\t}\n");
		fprintf(cuda, "\t\treturn false;\n");
    		fprintf(cuda, "\t}\n");
		fprintf(cuda, "};\n\n");
	}
	fprintf(cuda, "#endif\n");
	fclose(cuda);
	free(str);
}
