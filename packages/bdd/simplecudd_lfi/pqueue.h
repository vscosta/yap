/******************************************************************
**
** QUEUE.H:
**
**    ADT Queue Implementation
**
** This file is part of Apt Abstract Data Types (ADT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
******************************************************************/
//http://www.koders.com/c/fid7B82D8DDECE4EDC672F17D970458033C4079A615.aspx?s=queue

/*
** This ADT, originally written in 1989, provides a general queue
** implementation--in C--which is the equivalent of the Java Vector.
*/

#ifndef QUEUE_H
#define QUEUE_H

/* ---------- Headers */

#include "apt.h"

/* ---------- Types */

typedef struct _QueueItem {
  void *element;
  int type;
  struct _QueueItem *next;
} _QueueItem, *QueueItem;

typedef struct _Queue {
  struct _QueueItem *head;
  struct _QueueItem *tail;
  int size;
} _Queue, *Queue;

/* ---------- Exported Function Prototypes */

#ifdef __ANSI_C__
void QueueApply(Queue, ApplyFunction);
void QueueApply1(Queue, void*, ApplyFunction1);
void QueueApply2(Queue, void*, void*, ApplyFunction2);
void QueueApply3(Queue, void*, void*, void*, ApplyFunction3);
void *QueueCAR(Queue);
Queue QueueCDR(Queue);
void QueueDispose(Queue, DisposeFunction);
void *QueueFind(Queue, void*, ComparisonFunction);
void *QueueFindAndRemove(Queue, void*, ComparisonFunction);
void *QueueFindAndRemoveType(Queue, void*, ComparisonFunction, int);
void *QueueFindType(Queue, int);
void *QueueFindTypeAndRemove(Queue, int);
void *QueueGet(Queue);
QueueItem QueueHead(Queue);
void *QueueItemElement(QueueItem);
int QueueItemType(QueueItem);
void *QueueLook(Queue);
Queue QueueNew(void);
QueueItem QueueNext(QueueItem);
void QueuePut(Queue, void*, int);
void QueuePutOnPriority(Queue, void*, int, ComparisonFunction);
QueueItem QueueSeek(Queue,int);
int QueueSize(Queue);
QueueItem QueueTail(Queue);
#else
void QueueApply();
void QueueApply1();
void QueueApply2();
void QueueApply3();
void *QueueCAR();
Queue QueueCDR();
void QueueDispose();
void *QueueFind();
void *QueueFindAndRemove();
void *QueueFindAndRemoveType();
void *QueueFindType();
void *QueueFindTypeAndRemove();
void *QueueGet();
QueueItem QueueHead();
void *QueueItemElement();
int QueueItemType();
void *QueueLook();
Queue QueueNew();
QueueItem QueueNext();
void QueuePut();
void QueuePutOnPriority();
QueueItem QueueSeek();
int QueueSize();
QueueItem QueueTail();
#endif /* __ANSI_C__ */

#endif /* QUEUE_H */
