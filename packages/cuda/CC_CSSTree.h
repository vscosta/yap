#ifndef CSSTREE_H
#define CSSTREE_H

#include <iostream>
#include <math.h>
using namespace std;

#define divRoundUp(n,s)    (((n) / (s)) + ((((n) % (s)) > 0) ? 1 : 0))
#define CSS_TREE_FANOUT 33
//we use implicit pointer to perform the addressing.

typedef int Record;

class CC_GenericTree
{
public:
	int numRecord;
	Record *data;
	//we use the BFS layout as the default layout.
	int numNode;
	int level;
	int gResult;
	CC_GenericTree(){}
	//we assume that numR=2^i. Otherwise, we pad the array with -1 from the beginning.
	//we also assume that the record are sorted by the key.
	CC_GenericTree(Record *d, int numR)
	{
		data=d;
		numRecord=numR;	
	}
	virtual ~CC_GenericTree()
	{
	}
	virtual int search(int key)=0;

};

class CC_CSSTree:public CC_GenericTree
{
public:
	int *ntree;
	int fanout;
	int blockSize;
	int *vStart;
	int *vG;//vG[0] is used in computing the position for level 1.
	int numKey;
	CC_CSSTree(Record *d, int numR, int f):CC_GenericTree(d,numR)
	{
		fanout=f;
		blockSize=fanout-1;
		int numLeaf=divRoundUp(numR,blockSize);		
		level=1;
		int temp=numLeaf;
		while(temp>1)
		{
			temp=divRoundUp(temp, fanout);
			level++;
		}
		numNode=(int)((pow((double)fanout,(double)level)-1)/(fanout-1));
		numKey=numNode*blockSize;
		ntree=new int[numKey];
		vStart=new int[level];
		vG=new int[level];
#ifdef DEBUG
		cout<<numLeaf<<","<<level<<", "<<numNode<<endl;
#endif
		//layout the tree from bottom up.
		int i=0,j=0,k=0;
		int startNode=0;
		int endNode=0;
		int startKey, endKey;
		int curIndex;
		for(i=0;i<numNode;i++)
			ntree[i]=-1;
		//for <level-1>, i.e., the leaf level. [start,end]
		for(i=0;i<level;i++)//level
		{
			startNode=(int)((pow((double)fanout,(double)i)-1)/(fanout-1));
			endNode=(int)((pow((double)fanout,(double)(i+1))-1)/(fanout-1));
			for(j= startNode;j< endNode;j++)//which node
			{
				startKey=j*blockSize;
				endKey=startKey+blockSize;
				for(k=startKey;k<endKey;k++)
				{
					curIndex=(int)(blockSize*pow((double)fanout,(double)(level-i-1))*(k+1-startNode*blockSize+(j-startNode))-1);
					if(curIndex<numRecord+blockSize)
					{
						if(curIndex>=numRecord)
							curIndex=numRecord-1;
						ntree[k]=data[curIndex];
					}
					else
						break;
				}
			}
		}
	}
	~CC_CSSTree()
	{
		delete [] ntree;
		delete [] vStart;
		delete [] vG;
	}
	virtual int search(int key);
	void print()
	{
		int i=0, j=0;
		int k=0;
		int startNode=0;
		int endNode=0;
		int startKey, endKey;
		for(i=0;i<level;i++)//level
		{
			cout<<"Level, "<<i<<endl;
			startNode=(int)((pow((double)fanout,(double)i)-1)/(fanout-1));
			endNode=(int)((pow((double)fanout,(double)(i+1))-1)/(fanout-1));
			for(j= startNode;j< endNode;j++)//which node
			{
				cout<<"Level, "<<i<<", Node, "<<j<<": ";
				startKey=j*blockSize;
				endKey=startKey+blockSize;
				for(k=startKey;k<endKey;k++)
				{
					cout<<ntree[k]<<", ";	
				}
				cout<<endl;
			}
		}
		for(i=0;i<numRecord;i++)
		{
			cout<<data[i]<<", ";
			if(i%(fanout-1)==(fanout-2))
			cout<<"*"<<endl;
		}

		

	}


};


#endif

