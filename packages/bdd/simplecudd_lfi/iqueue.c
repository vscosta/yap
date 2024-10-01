/******************************************************************
**
** IQUEUE.C:
**
**    ADT Queue Iterator Implementation
**
** This file is part of Apt Abstrct Data Types (ADT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
** The concept of an iterator for an Abstract Data Type is derived
** from research on Smalltalk.
******************************************************************/

/* ---------- C Headers */

//#include "cheaders.h"

/* ---------- Headers */

#include "adterror.h"
#include "allocate.h"
#include "iqueue.h"

/* ---------- Private Function Prototypes */

#ifdef __ANSI_C__
#else
#endif

/* ---------- Functions */

/*
**
** QueueIteratorDispose
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueIteratorDispose(QueueIterator qi)
#else
void QueueIteratorDispose(qi)
QueueIterator qi;
#endif
{
  free(qi);
}

/*
**
** QueueIteratorNew
**
*/
PUBLIC
#ifdef __ANSI_C__
QueueIterator QueueIteratorNew(Queue q, int start)
#else
QueueIterator QueueIteratorNew(q,start)
Queue q;
int start;
#endif
{
  QueueIterator qi;

  if (q)
     qi = ((QueueIterator)Allocate(sizeof(_QueueIterator)));
  else {
    ADTError(ADT_QueueIter, E_NullQueue, "QueueIteratorNew");
    return NULL;
  }
  if (qi) {
     qi->queue = q;
     QueueIteratorAbsoluteSeek(qi, start);
  } else
    ADTError(ADT_QueueIter, E_Allocation, "QueueIteratorNew");
  return (qi);
}

/*
**
** QueueIteratorAtTop
**
*/
PUBLIC
#ifdef __ANSI_C__
int QueueIteratorAtTop(QueueIterator qi)
#else
int QueueIteratorAtTop(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorAtTop");
  return QueueIteratorAtPosition(qi,1);
}

/*
**
** QueueIteratorAtBottom
**
*/
PUBLIC
#ifdef __ANSI_C__
int QueueIteratorAtBottom(QueueIterator qi)
#else
int QueueIteratorAtBottom(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorAtBottom");
  return QueueIteratorAtPosition(qi,0);
}

/*
**
** QueueIteratorAtPosition
**
*/
PUBLIC
#ifdef __ANSI_C__
int QueueIteratorAtPosition(QueueIterator qi, int position)
#else
int QueueIteratorAtPosition(qi,position)
QueueIterator qi;
int position;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorAtPosition");
  if (position <= 0)
    position += (QueueSize(qi->queue) + 1);
  return qi->position == position;
}

/*
**
** QueueIteratorPosition
**
*/
PUBLIC
#ifdef __ANSI_C__
int QueueIteratorPosition(QueueIterator qi)
#else
int QueueIteratorPosition(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorPosition");
  return qi->position;
}

/*
**
** QueueIteratorCurrentData
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueIteratorCurrentData(QueueIterator qi)
#else
void *QueueIteratorCurrentData(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorCurrentData");
  return QueueItemElement(qi->currentItem);
}

/*
**
** QueueIteratorPreviousData
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueIteratorPreviousData(QueueIterator qi)
#else
void *QueueIteratorPreviousData(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorPreviousData");
  return QueueItemElement(qi->previousItem);
}

/*
**
** QueueIteratorAdvance
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueIteratorAdvance(QueueIterator qi)
#else
void QueueIteratorAdvance(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorAdvance");
  qi->previousItem = qi->currentItem;
  qi->currentItem = QueueNext(qi->previousItem);
  if (qi->previousItem) qi->position++;
}

/*
**
** QueueIteratorBackup
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueIteratorBackup(QueueIterator qi)
#else
void QueueIteratorBackup(qi)
QueueIterator qi;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorBackup");
  QueueIteratorRelativeSeek(qi, -1);
}

/*
**
** QueueIteratorAbsoluteSeek
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueIteratorAbsoluteSeek(QueueIterator qi, int position)
#else
void QueueIteratorAbsoluteSeek(qi,position)
QueueIterator qi;
int position;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorAbsoluteSeek");
  if (position <= 0)
    position += (QueueSize(qi->queue) + 1);
  if (position <= 0)
    ADTError(ADT_QueueIter, E_Seek, "QueueIteratorAbsoluteSeek");
  /* Here, we know position is positive */
  if (position > QueueSize(qi->queue) + 1)
    qi->position = QueueSize(qi->queue) + 1;
  else
    qi->position = position;
  qi->previousItem = QueueSeek(qi->queue,position-1);
  if (qi->previousItem)
    qi->currentItem = QueueNext(qi->previousItem);
  else
    qi->currentItem = QueueSeek(qi->queue,position);
}

/*
**
** QueueIteratorRelativeSeek
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueIteratorRelativeSeek(QueueIterator qi,int disp)
#else
void QueueIteratorRelativeSeek(qi,disp)
QueueIterator qi;
int disp;
#endif
{
  if (!qi)
    ADTError(ADT_QueueIter, E_NullQueueIter, "QueueIteratorRelativeSeek");
  QueueIteratorAbsoluteSeek(qi,qi->position+disp);
}
