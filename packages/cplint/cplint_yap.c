/* 
	LPAD and CP-Logic interpreter
	
Copyright (c) 2007, Fabrizio Riguzzi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.


	This file contains the functions for interfacing Yap and C
	The arguments of the predicate compute_prob are parsed and translated into C data 
	structures
*/

#include "cplint.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>


unsigned long dividend;

FILE *open_file (char *filename, const char *mode);
void reverse(char s[]);
static int compute_prob(void);

void createVars(array_t * vars, YAP_Term t,DdManager * mgr, array_t * bVar2mVar,int create_dot,  char inames[1000][20])
/* adds the boolean variables to the BDD and returns
an array_t containing them (array_t is defined in the util library of glu)
returns also the names of the variables to be used to save the ADD in dot format
 */
{
     	YAP_Term  varTerm,probTerm;
	int varIndex,nVal,i,b;
	variable v;	
	char numberVar[10],numberBit[10];
	double p;
	b=0;

	while(YAP_IsPairTerm(t))
	{
		varTerm=YAP_HeadOfTerm(t);
		varIndex=YAP_IntOfTerm(YAP_HeadOfTerm(varTerm));

      		varTerm=YAP_TailOfTerm(varTerm);
		nVal=YAP_IntOfTerm(YAP_HeadOfTerm(varTerm));
		varTerm=YAP_TailOfTerm(varTerm);
      		probTerm=YAP_HeadOfTerm(varTerm);
		v.nVal=nVal;
		v.nBit=(int)ceil(log(nVal)/log(2));
		v.probabilities=array_alloc(double,0);
		v.booleanVars=array_alloc(DdNode *,0);
		for (i=0;i<nVal;i++)
		{
			if (create_dot)
			{
				strcpy(inames[b+i],"X");
				sprintf(numberVar,"%d",varIndex);
				strcat(inames[b+i],numberVar);
				strcat(inames[b+i],"_");
				sprintf(numberBit,"%d",i);
				strcat(inames[b+i],numberBit);
			}
			p=YAP_FloatOfTerm(YAP_HeadOfTerm(probTerm));
			array_insert(double,v.probabilities,i,p);
			probTerm=YAP_TailOfTerm(probTerm);
			array_insert(DdNode *,v.booleanVars,i,Cudd_bddIthVar(mgr,b+i));
			array_insert(int,bVar2mVar,b+i,varIndex);
		}
		Cudd_MakeTreeNode(mgr,b,nVal,MTR_FIXED);
		b=b+nVal;
		array_insert(variable,vars,varIndex,v);
		t=YAP_TailOfTerm(t);
	}
}

void createExpression(array_t * expression, YAP_Term t)
/* returns the expression as an array_t of terms (cubes) starting from the prolog lists of terms
each term is an array_t of factors obtained from a prolog list of factors
each factor is a couple (index of variable, index of value) obtained from a prolog list containing 
two integers
*/
{
     	YAP_Term  termTerm,factorTerm;
	factor f;	
	int i,j;
	array_t * term;

	i=0;
	while(YAP_IsPairTerm(t))
	{
		term=array_alloc(factor,0);
		termTerm=YAP_HeadOfTerm(t);
		j=0;
		while(YAP_IsPairTerm(termTerm))
		{
			factorTerm=YAP_HeadOfTerm(termTerm);
			f.var=YAP_IntOfTerm(YAP_HeadOfTerm(factorTerm));
			f.value=YAP_IntOfTerm(YAP_HeadOfTerm(YAP_TailOfTerm(factorTerm)));
			array_insert(factor,term,j,f);
			termTerm=YAP_TailOfTerm(termTerm);
			j++;
		}
		array_insert(array_t *,expression,i,term);
		t=YAP_TailOfTerm(t);
		i++;
	}
}

static int compute_prob(void)
/* this is the function that implements the compute_prob predicate used in pp.pl
*/
{
	YAP_Term out,arg1,arg2,arg3,arg4;
	array_t * variables,* expression, * bVar2mVar;
	DdNode * function, * add;
	DdManager * mgr;
	int nBVar,i,j,intBits,create_dot;
        FILE * file;
        DdNode * array[1];
        char * onames[1];
        char inames[1000][20];
	char * names[1000];
	GHashTable  * nodes; /* hash table that associates nodes with their probability if already 
				computed, it is defined in glib */
	//Cudd_ReorderingType order;
	arg1=YAP_ARG1;
	arg2=YAP_ARG2;
	arg3=YAP_ARG3;
	arg4=YAP_ARG4;

  	mgr=Cudd_Init(0,0,CUDD_UNIQUE_SLOTS,CUDD_CACHE_SLOTS,0);
	variables=array_alloc(variable,0);
	bVar2mVar=array_alloc(int,0);
	create_dot=YAP_IntOfTerm(arg4);
	createVars(variables,arg1,mgr,bVar2mVar,create_dot,inames);
        //Cudd_PrintInfo(mgr,stderr);

	/* automatic variable reordering, default method CUDD_REORDER_SIFT used */
	//printf("status %d\n",Cudd_ReorderingStatus(mgr,&order));
	//printf("order %d\n",order);

	Cudd_AutodynEnable(mgr,CUDD_REORDER_SAME); 
/*	 Cudd_AutodynEnable(mgr, CUDD_REORDER_RANDOM_PIVOT);
	printf("status %d\n",Cudd_ReorderingStatus(mgr,&order));
        printf("order %d\n",order);
	printf("%d",CUDD_REORDER_RANDOM_PIVOT);
*/


	expression=array_alloc(array_t *,0);
	createExpression(expression,arg2);	

	function=retFunction(mgr,expression,variables);
	/* the BDD build by retFunction is converted to an ADD (algebraic decision diagram)
	because it is easier to interpret and to print */
	add=Cudd_BddToAdd(mgr,function);
	//Cudd_PrintInfo(mgr,stderr);

	if (create_dot)
	/* if specified by the user, a dot file for the BDD is written to cpl.dot */
	{	
		nBVar=array_n(bVar2mVar);
		for(i=0;i<nBVar;i++)
		   names[i]=inames[i];
	  	array[0]=add;
		onames[0]="Out";
		file = open_file("cpl.dot", "w");
		Cudd_DumpDot(mgr,1,array,names,onames,file);
  		fclose(file);
	}
	
	nodes=g_hash_table_new(my_hash,my_equal);
	intBits=sizeof(unsigned int)*8;
	/* dividend is a global variable used by my_hash 
	   it is equal to an unsigned int with binary representation 11..1 */ 
	dividend=1;
	for(j=1;j<intBits;j++)
	{
		dividend=(dividend<<1)+1;
	}
	out=YAP_MkFloatTerm(Prob(add,variables,bVar2mVar,nodes));
	g_hash_table_foreach (nodes,dealloc,NULL);
	g_hash_table_destroy(nodes);
	Cudd_Quit(mgr);
	array_free(variables);
 	array_free(bVar2mVar);
	array_free(expression);
    	return(YAP_Unify(out,arg3));
}
/*
int compare(char *a, char *b)
{
	int aval,bval;
	aval=(int) *((DdNode **)a);
	aval=(int) *((DdNode **)b);

	if (aval<bval)
		return -1;
	else
 		if (aval>bval)
			return 1;
		else
			return 0;
}
*/
void init_my_predicates()
/* function required by YAP for intitializing the predicates defined by a C function*/
{
     YAP_UserCPredicate("compute_prob",compute_prob,4);
}
 FILE *
open_file(char *filename, const char *mode)
/* opens a file */
{
    FILE *fp;

    if ((fp = fopen(filename, mode)) == NULL) {
        perror(filename);
        exit(1);
    }
    return fp;

}
void reverse(char s[])
/* reverses a string */
{
	int i,c,j;
	for (i=0,j=strlen(s)-1;i<j;i++,j--)
	{
		c=s[i];
		s[i]=s[j];
		s[j]=c;
	}
}

gint my_equal(gconstpointer v,gconstpointer v2)
/* function used by GHashTable to compare two keys */
{
	DdNode *a,*b;
	a=*(DdNode **)v;
	b=*(DdNode **)v2;
	return (a==b);
}
guint my_hash(gconstpointer key)
/* function used by GHashTable to hash a key */
{
	unsigned int h;
	h=(unsigned int)((unsigned long) *((DdNode **)key) % dividend);
	return h;
}
void  dealloc(gpointer key,gpointer value,gpointer user_data)
{
	free(key);
	free(value);
}
