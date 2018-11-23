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



FILE *open_file (char *file_name, const char *mode);
static YAP_Bool compute_prob(void);

variables  createVars(YAP_Term t,DdManager * mgr, int create_dot,  char inames[1000][20])
/* adds the boolean variables to the BDD and returns
the array vars containing them 
returns also the names of the variables to be used to save the ADD in dot format
 */
{
  YAP_Term  varTerm,probTerm;
  int varIndex,nVal,i,b;
  variable v;  
  char numberVar[10],numberBit[10];
  double p;
  variables  vars;
  
  vars.varar=NULL;
  vars.bVar2mVar=NULL;

  b=0;
  vars.nVar=0;
  varIndex=0;
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
    v.probabilities=(double *) malloc(nVal* sizeof(double));
    v.booleanVars=(DdNode * *) malloc(v.nBit* sizeof(DdNode *));
    for (i=0;i<nVal;i++)
    {
      p=YAP_FloatOfTerm(YAP_HeadOfTerm(probTerm));
      v.probabilities[i]=p;
      probTerm=YAP_TailOfTerm(probTerm);
    }
    for (i=0;i<v.nBit;i++)
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
      v.booleanVars[i]=Cudd_bddIthVar(mgr,b+i);
      vars.bVar2mVar=(int *)realloc(vars.bVar2mVar,(b+i+1)*sizeof(int));
      vars.bVar2mVar[b+i]=varIndex;
    }
    Cudd_MakeTreeNode(mgr,b,v.nBit,MTR_FIXED);
    b=b+v.nBit;
    vars.varar=(variable *) realloc(vars.varar,(varIndex+1)* sizeof(variable));
    vars.varar[varIndex]=v;
    t=YAP_TailOfTerm(t);
  }
  vars.nVar=varIndex+1;
  vars.nBVar=b;
  return vars;
}

expr createExpression(YAP_Term t)
/* returns the expression as an array_t of terms (cubes) starting from the prolog lists of terms
each term is an array_t of factors obtained from a prolog list of factors
each factor is a couple (index of variable, index of value) obtained from a prolog list containing 
two integers
*/
{
  YAP_Term  termTerm,factorTerm;
  factor f;  
  int i,j;
  term term1;
  expr expression;

  expression.terms=NULL;
  i=0;
  while(YAP_IsPairTerm(t))
  {
    term1.factors=NULL;
    termTerm=YAP_HeadOfTerm(t);
    j=0;
    while(YAP_IsPairTerm(termTerm))
    {
      factorTerm=YAP_HeadOfTerm(termTerm);
      f.var=YAP_IntOfTerm(YAP_HeadOfTerm(factorTerm));
      f.value=YAP_IntOfTerm(YAP_HeadOfTerm(YAP_TailOfTerm(factorTerm)));
      term1.factors=(factor *)realloc(term1.factors,(j+1)* sizeof(factor));
      term1.factors[j]=f;
      termTerm=YAP_TailOfTerm(termTerm);
      j++;
    }
    term1.nFact=j;
    expression.terms=(term *)realloc(expression.terms,(i+1)* sizeof(term));
    expression.terms[i]=term1;
    t=YAP_TailOfTerm(t);
    i++;
  }
  expression.nTerms=i;
  return(expression);
}

static YAP_Bool compute_prob(void)
/* this is the function that implements the compute_prob predicate used in pp.pl
*/
{
  YAP_Term out,arg1,arg2,arg3,arg4;
  variables  vars;
  expr expression; 
  DdNode * function;
  DdManager * mgr;
  int nBVar,i,create_dot;
  FILE * file;
  DdNode * array[1];
  double prob;
  char const * onames[1];
  char inames[1000][20];
  char const * names[1000];
  tablerow * nodes;
  
  arg1=YAP_ARG1;
  arg2=YAP_ARG2;
  arg3=YAP_ARG3;
  arg4=YAP_ARG4;

  mgr=Cudd_Init(0,0,CUDD_UNIQUE_SLOTS,CUDD_CACHE_SLOTS,0);
  create_dot=YAP_IntOfTerm(arg4);
  vars=createVars(arg1,mgr,create_dot,inames);

  //Cudd_PrintInfo(mgr,stderr);
  
  /* automatic variable reordering, default method CUDD_REORDER_SIFT used */
  //printf("status %d\n",Cudd_ReorderingStatus(mgr,&order));
  //printf("order %d\n",order);

  Cudd_AutodynEnable(mgr,CUDD_REORDER_SAME); 
/*   Cudd_AutodynEnable(mgr, CUDD_REORDER_RANDOM_PIVOT);
  printf("status %d\n",Cudd_ReorderingStatus(mgr,&order));
        printf("order %d\n",order);
  printf("%d",CUDD_REORDER_RANDOM_PIVOT);
*/


  expression=createExpression(arg2);  

  function=retFunction(mgr,expression,vars);

  /* the BDD build by retFunction is converted to an ADD (algebraic decision diagram)
  because it is easier to interpret and to print */
  //add=Cudd_BddToAdd(mgr,function);
  //Cudd_PrintInfo(mgr,stderr);

  if (create_dot)
  /* if specified by the user, a dot file for the BDD is written to cpl.dot */
  {  
    nBVar=vars.nBVar;
    for(i=0;i<nBVar;i++)
      names[i]=inames[i];
    array[0]=function;
    onames[0]="Out";
    file = open_file("cpl.dot", "w");
    Cudd_DumpDot(mgr,1,array,names,onames,file);
    fclose(file);
  }
  nodes=init_table(vars.nBVar);
  prob=Prob(function,vars,nodes);
  out=YAP_MkFloatTerm(prob);
  destroy_table(nodes,vars.nBVar);

  Cudd_Quit(mgr);
  for(i=0;i<vars.nVar;i++)
  {
    free(vars.varar[i].probabilities);
    free(vars.varar[i].booleanVars);
  }
  free(vars.varar);
  free(vars.bVar2mVar);
  for(i=0;i<expression.nTerms;i++)
  {
    free(expression.terms[i].factors);
  }
  free(expression.terms);
  return(YAP_Unify(out,arg3));
}

void init_my_predicates()
/* function required by YAP for intitializing the predicates defined by a C function*/
{
     YAP_UserCPredicate("compute_prob",compute_prob,4);
}

FILE * open_file(char *file_name, const char *mode)
/* opens a file */
{
  FILE *fp;

  if ((fp = fopen(file_name, mode)) == NULL)
  {
    perror(file_name);
    exit(1);
  }
  return fp;
}

