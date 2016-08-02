#include "CC_CSSTree.h"

//return the start position of searching the key.
int CC_CSSTree::search(int key)
{
	int i=0;
	int curIndex=0;
	int curNode=0;
	int j=0;
	//search
	for(i=0;i<level;i++)
	{
		for(j=0;j<blockSize;j++)
		{
			if(ntree[curIndex+j]==-1)
				break;
			if(key<=ntree[curIndex+j])
				break;
		}
		curNode=(fanout*(curNode)+j+1);
		curIndex=curNode*blockSize;
//#ifdef DEBUG
//		cout<<curNode<<", "<<j<<", "<<ntree[curIndex]<<";   ";
//#endif
	}
	curIndex=(curNode-numNode)*blockSize;
	if(curIndex>numRecord) curIndex=numRecord-1;
	//cout<<"I: "<<curIndex<<", ";//cout<<endl;
	return curIndex;
}
