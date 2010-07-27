/*
	LPAD and CP-Logic interpreter
	
Copyright (c) 2007, Fabrizio Riguzzi

This package uses the library cudd, see http://vlsi.colorado.edu/~fabio/CUDD/
for the relative license.


	This file contains the definition of Prob and ProbBool plus the functions
	for building the BDD
*/


#include "cplint.h"
#include <stdlib.h>

int correctPosition(int index,variable v, DdNode * node,int posBVar);



DdNode * retFunction(DdManager * mgr,expr expression, variables v)
/* given an expression term1+term2+...+termn, returns the BDD that implements that function */  
{
  term term1;
  DdNode * tNode, * tmp, *tmp1;
  int i;
  
  i=0;
  tNode=Cudd_ReadLogicZero(mgr);
  Cudd_Ref(tNode);
  while(i<expression.nTerms)
  {
    term1=expression.terms[i];
    tmp=retTerm(mgr,term1,v);
    Cudd_Ref(tmp);
    tmp1=Cudd_bddOr(mgr,tNode,tmp);
    Cudd_Ref(tmp1);
    Cudd_RecursiveDeref(mgr,tNode);
    tNode=tmp1;
    i++;    
  } 
  return tNode;
}

DdNode * retTerm(DdManager * mgr,term t, variables v)
/* given a term V1=v1 and V2=v2 ... Vn=vn, returns the BDD that implements that function */  
{
  factor f;
  DdNode * fNode, * tmp, *tmp1;
  int i;
  
  i=0;
  fNode=Cudd_ReadOne(mgr);
  Cudd_Ref(fNode);
  while (i<t.nFact)
  {
    f=t.factors[i];
    tmp=retFactor(mgr,f,v);
    Cudd_Ref(tmp);
    tmp1= Cudd_bddAnd(mgr,fNode,tmp);
    Cudd_Ref(tmp1);
    Cudd_RecursiveDeref(mgr,fNode);
    fNode=tmp1;
    i++;
  } 
  return fNode;
}

DdNode * retFactor(DdManager * mgr, factor f, variables vars)
/* given a factor V=v, returns the BDD that implements that function */  
{
  int varIndex;
  int value;
  int i;
  int bit;
  variable v;
  DdNode * node, *booleanVar, * tmp;
  DdNode  ** booleanVars;
  
  
  varIndex=f.var;
  value=f.value;
  v=vars.varar[varIndex];
  booleanVars=v.booleanVars;
  i=v.nBit-1;
  node=Cudd_ReadOne(mgr);
  Cudd_Ref(node);
  /* booelan var with index 0 in v.booleanVars is the most significant */
  do {
    booleanVar=booleanVars[i];
    bit=value & 01;
    if (bit)
    {
      tmp=Cudd_bddAnd(mgr,node,booleanVar);
      Cudd_Ref(tmp);
    }
    else
      {
      tmp=Cudd_bddAnd(mgr,node,Cudd_Not(booleanVar));
      Cudd_Ref(tmp);
      }
    value=value>>1;
    i--;
    Cudd_RecursiveDeref(mgr,node);
    node=tmp;
  } while (i>=0);
  return node;

}



double Prob(DdNode *node, variables vars, GHashTable * nodes)
/* compute the probability of the expression rooted at node
nodes is used to store nodes for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,mVarIndex,nBit;
  variable v;
  double res;
  double value;
  double * value_p;
  DdNode **key;
  double *rp;

  index=node->index;
  if (Cudd_IsConstant(node))
  {
    value=node->type.value;
   return value;
  }
  else
{
	
	value_p=g_hash_table_lookup(nodes,&node);
	if (value_p!=NULL)
	{
		return *value_p;
	}
	else
  	{
    		mVarIndex=vars.bVar2mVar[index];
    		v=vars.varar[mVarIndex];
    		nBit=v.nBit;
    		res=ProbBool(node,0,nBit,0,v,vars,nodes);
		key=(DdNode **)malloc(sizeof(DdNode *));
		*key=node;
		rp=(double *)malloc(sizeof(double));
		*rp=res;
		g_hash_table_insert(nodes, key, rp);
    		return res;
  	}
}
}

double ProbBool(DdNode *node, int bits, int nBit,int posBVar,variable v,
variables vars, GHashTable * nodes)
/* explores a group of binary variables making up the multivalued variable v */
{
  DdNode *T,*F;
  double p,res;
  double * probs;
 
  probs=v.probabilities;
  if (nBit==0)
  {
    if (bits>=v.nVal)
      return 0;
    else
    {
      p=probs[bits];
      res=p*Prob(node,vars,nodes);
      return res;
    }
  }
  else
  {
    if (correctPosition(node->index,v,node,posBVar))
    {
      T = node->type.kids.T;
      F = node->type.kids.E;
      bits=bits<<1;
      
      res=ProbBool(T,bits+1,nBit-1,posBVar+1,v,vars,nodes)+ 
        ProbBool(F,bits,nBit-1,posBVar+1,v,vars,nodes);
      return res;
    }
    else
    {
      
      bits=bits<<1;
      res=ProbBool(node,bits+1,nBit-1,posBVar+1,v,vars,nodes)+ 
        ProbBool(node,bits,nBit-1,posBVar+1,v,vars,nodes);
      return res;
    }
  }
}

int correctPosition(int index,variable v, DdNode * node,int posBVar)
/* returns 1 is the boolean variable with index posBVar is in the correct position 
currently explored by ProbBool */
{
  DdNode * bvar;
  
  bvar=v.booleanVars[posBVar];
  return bvar->index==index;
}
