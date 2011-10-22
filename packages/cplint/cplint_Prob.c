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
  do 
  {
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



double Prob(DdNode *node, variables vars, tablerow * nodes)
/* compute the probability of the expression rooted at node
nodes is used to store nodes for which the probability has alread been computed
so that it is not recomputed
 */
{
  int index,mVarIndex,nBit,comp;
  variable v;
  double res;
  double * value_p;
  DdNode *nodereg;

  index=Cudd_NodeReadIndex(node);
  comp=Cudd_IsComplement(node);
  if (Cudd_IsConstant(node))
  {
    if (comp)
      return 0.0;
    else
      return 1.0;
  }
  else
  {
    nodereg=Cudd_Regular(node);
    value_p=get_value(nodes,nodereg);
    if (value_p!=NULL)
    {
      if (comp)
        return 1-*value_p;
      else
        return *value_p;
    }
    else
    {
      mVarIndex=vars.bVar2mVar[index];
      v=vars.varar[mVarIndex];
      nBit=v.nBit;
      res=ProbBool(node,0,nBit,0,v,mVarIndex,vars,nodes,0);
      add_node(nodes,nodereg,res);
      if (comp)
        return 1-res;
      else
        return res;
    }
  }
}

double ProbBool(DdNode *node, int bits, int nBit,int posBVar,variable v,
  int mVarIndex,
  variables vars, tablerow * nodes,int comp)
/* explores a group of binary variables making up the multivalued variable v */
{
  DdNode *T,*F;
  double p,res;
  double * probs;
  int comp1,comp2,index,indexF,mVarIndexF;

  probs=v.probabilities;
  if (nBit==0)
  {
    if (bits>=v.nVal)
      return 0;
    else
    {
      p=probs[bits];
      if (comp)
        res=p*(1-Prob(node,vars,nodes));
      else
        res=p*Prob(node,vars,nodes);
      return res;
    }
  }
  else
  {
    index=Cudd_NodeReadIndex(node);
    if (correctPosition(index,v,node,posBVar))
    {
      T = Cudd_T(node);
      F = Cudd_E(node);
      bits=bits<<1;
      comp1=Cudd_IsComplement(F);
      res=ProbBool(T,bits+1,nBit-1,posBVar+1,v,mVarIndex,vars,nodes,comp);
      indexF=Cudd_NodeReadIndex(F);
      if (Cudd_IsConstant(F))
        mVarIndexF=-1;
      else
        mVarIndexF=vars.bVar2mVar[indexF];  
      if (mVarIndexF==mVarIndex)
        comp2=(comp1 && !comp) || (!comp1 && comp);
      else
        comp2=comp;
      res=res+ ProbBool(F,bits,nBit-1,posBVar+1,v,mVarIndex,vars,nodes,comp2);
      return res;
    }
    else
    {
      bits=bits<<1;
      res=ProbBool(node,bits+1,nBit-1,posBVar+1,v,mVarIndex,vars,nodes,comp)+ 
      ProbBool(node,bits,nBit-1,posBVar+1,v,mVarIndex,vars,nodes,comp);
      return res;
    }
  }
}

int correctPosition(int index,variable v, DdNode * node,int posBVar)
/* returns 1 is the boolean variable with index posBVar is in the correct position 
currently explored by ProbBool */
{
  DdNode * bvar;
  int ind;

  bvar=v.booleanVars[posBVar];
  ind=Cudd_NodeReadIndex(bvar);
  return ind==index;
}

double * get_value(tablerow *tab,  DdNode *node) {
  int i;
  int index = Cudd_NodeReadIndex(node);

  for(i = 0; i < tab[index].cnt; i++) 
  {
    if (tab[index].row[i].key == node)
    {
      return &tab[index].row[i].value;
    }
  }
  return NULL;
}

void destroy_table(tablerow *tab, int boolVars)
{
  int i;
  for (i = 0; i < boolVars; i++) 
  {
    free(tab[i].row);
  }
  free(tab);
}
tablerow* init_table(int boolVars) {
  int i;

  tablerow *tab;
  tab = (tablerow *) malloc(sizeof(rowel) * boolVars);
  for (i = 0; i < boolVars; i++) 
  {
    tab[i].row = NULL;
    tab[i].cnt = 0;
  }
  return tab;
}


void add_node(tablerow *tab, DdNode *node, double value) 
{
  int index = Cudd_NodeReadIndex(node);

  tab[index].row = (rowel *) realloc(tab[index].row, 
    (tab[index].cnt + 1) * sizeof(rowel));
  tab[index].row[tab[index].cnt].key = node;
  tab[index].row[tab[index].cnt].value = value;
  tab[index].cnt += 1;                                        
}

