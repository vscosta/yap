/******************************************************************
**
** IQUEUE.H:
**
**    ADT Queue Iterator Implementation
**
** This file is part of Apt Abstract Data Types (ADT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
******************************************************************/

#ifndef IQUEUE_H
#define IQUEUE_H

/* ---------- Headers */

#include "pqueue.h"

/* ---------- Types */

typedef struct _QueueIterator {
  int position;
  Queue queue;
  QueueItem currentItem, previousItem;
} _QueueIterator, *QueueIterator;

/* ---------- Exported Function Prototypes */

#ifdef __ANSI_C__
QueueIterator QueueIteratorNew(Queue,int);
void QueueIteratorDispose(QueueIterator);

int QueueIteratorAtTop(QueueIterator);
int QueueIteratorAtBottom(QueueIterator);
int QueueIteratorAtPosition(QueueIterator,int);

int QueueIteratorPosition(QueueIterator);
void *QueueIteratorCurrentData(QueueIterator);
void *QueueIteratorPreviousData(QueueIterator);

void QueueIteratorAdvance(QueueIterator);
void QueueIteratorBackup(QueueIterator);
void QueueIteratorAbsoluteSeek(QueueIterator,int);
void QueueIteratorRelativeSeek(QueueIterator,int);

#else
QueueIterator QueueIteratorNew();
void QueueIteratorDispose();

int QueueIteratorAtTop();
int QueueIteratorAtBottom();
int QueueIteratorAtPosition();

int QueueIteratorPosition();
void *QueueIteratorCurrentData();
void *QueueIteratorPreviousData();

void QueueIteratorAdvance();
void QueueIteratorBackup();
void QueueIteratorAbsoluteSeek();
void QueueIteratorRelativeSeek();

#endif /* __ANSI_C__ */

#endif /* QUEUE_H */
