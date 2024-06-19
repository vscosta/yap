#include <stdio.h>

#include "pqueue.h"
#include "iqueue.h"
//http://www.koders.com/c/fid7B82D8DDECE4EDC672F17D970458033C4079A615.aspx?s=queue
#define INT_VALUE 1000

//typedef int (*ComparisonFunction)(void*, void*);
int compare_int (const int *a, const int *b)
{
  fprintf(stderr,"comparing %i %i \n",*a,*b);
  int temp = *a - *b;
  //  return (a<b) ? 1 : -1;

  //  return -1;
  if (temp < 0)
    return 1;
  else if (temp > 0)
    return -1;
  else
    return 0;
}


int main (int argc, char* argv[]) {
    int val1 = 1;
    int val2 = 2;
    int val3 = 3;
    int val4 = 4;

    Queue q = QueueNew();

    QueuePutOnPriority(q, &val1, INT_VALUE,(ComparisonFunction)compare_int);
    QueuePutOnPriority(q, &val3, INT_VALUE,(ComparisonFunction)compare_int);
    QueuePutOnPriority(q, &val2, INT_VALUE,(ComparisonFunction)compare_int);
    QueuePutOnPriority(q, &val4, INT_VALUE,(ComparisonFunction)compare_int);

    QueueItem qptr = q->head;
    while (qptr != NULL) {
        int* val = (int*) qptr->element;
        printf("value: %d\n", *val);
        qptr = qptr->next;
    }

    QueueIterator qiter = QueueIteratorNew(q, 1);

    while (qiter->currentItem != NULL) {
        int* val = (int*) qiter->currentItem->element;
        printf("iterator value: %d\n", *val);
        QueueIteratorAdvance(qiter);
    }
}
