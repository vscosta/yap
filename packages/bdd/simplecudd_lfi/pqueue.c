/******************************************************************
**
** QUEUE.C:
**
**    ADT Queue Implementation
**
** This file is part of Apt Abstrct Data Types (ADT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
******************************************************************/

/* ---------- C Headers */

//#include "cheaders.h"

/* ---------- Headers */

#include "apt.h"

#include "allocate.h"
#include "pqueue.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---------- Private Function Prototypes */

#ifdef __ANSI_C__
PRIVATE void QueueDisposeItem(QueueItem, DisposeFunction);
PRIVATE QueueItem QueueGetItem(Queue);
PRIVATE QueueItem QueueNewItem(void*, int);
PRIVATE void QueuePutItem(Queue, QueueItem);
PRIVATE QueueItem QueueRemoveItem(Queue, QueueItem);
//PRIVATE int QueueCompareEqual(void *, void *);
#else
PRIVATE void QueueDisposeItem();
PRIVATE QueueItem QueueGetItem();
PRIVATE QueueItem QueueNewItem();
PRIVATE void QueuePutItem();
PRIVATE QueueItem QueueRemoveItem();
//PRIVATE int QueueCompareEqual();
#endif

/* ---------- Functions */

/*
**
** QueueApply
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueApply(Queue q, ApplyFunction f)
#else
void QueueApply(q,f)
Queue q;
ApplyFunction f;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    (*f)(item->element);
  }
}

/*
**
** QueueApply1
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueApply1(Queue q, void *p1, ApplyFunction1 f)
#else
void QueueApply1(q,p1,f)
Queue q;
void *p1;
ApplyFunction1 f;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    (*f)(item->element,p1);
  }
}

/*
**
** QueueApply2
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueApply2(Queue q, void *p1, void *p2, ApplyFunction2 f)
#else
void QueueApply2(q,p1,p2,f)
Queue q;
void *p1;
void *p2;
ApplyFunction2 f;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    (*f)(item->element,p1,p2);
  }
}

/*
**
** QueueApply3
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueApply3(Queue q, void *p1, void* p2, void *p3, ApplyFunction3 f)
#else
void QueueApply3(q,p1,p2,p3,f)
Queue q;
void *p1;
void *p2;
void *p3;
ApplyFunction3 f;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    (*f)(item->element,p1,p2,p3);
  }
}

/*
**
** QueueCAR
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueCAR(Queue q)
#else
void *QueueCAR(q)
Queue q;
#endif
{
  if (q != NULL && q->head != NULL)
    return QueueItemElement(q->head);
  return NULL;
}

/*
**
** QueueCDR
**
*/
PUBLIC
#ifdef __ANSI_C__
Queue QueueCDR(Queue q)
#else
Queue QueueCDR(q)
Queue q;
#endif
{
  if (q != NULL && q->head != NULL) {
    Queue new = QueueNew();
    new->head = q->head->next;
    new->tail = new->head != NULL ? q->tail : NULL;
    new->size = q->size-1;
    return new;
  } else
    return NULL;
}

/*
**
** QueueDispose
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueueDispose(Queue q, DisposeFunction f)
#else
void QueueDispose(q,f)
Queue q;
DisposeFunction f;
#endif
{
  QueueItem item;
  QueueItem next = NULL;

  for (item = q->head; item != NULL; item = next) {
    next = item->next;
    QueueDisposeItem(item,f);
  }
  free(q);
}

/*
**
** QueueDisposeItem
**
*/
PRIVATE
#ifdef __ANSI_C__
void QueueDisposeItem(QueueItem item, DisposeFunction f)
#else
void QueueDisposeItem(item,f)
QueueItem item;
DisposeFunction f;
#endif
{
  if (f) (*f)(item->element);
  free(item);
}

/*
**
** QueueFind
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueFind(Queue q, void *element, ComparisonFunction f)
#else
void *QueueFind(q,element,f)
Queue q;
void *element;
ComparisonFunction f;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    if ((*f)(element,item->element) == 0) break;
  }
  return (item != NULL ? item->element : NULL);
}

/*
**
** QueueFindAndRemove
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueFindAndRemove(Queue q, void *element,
                             ComparisonFunction f)
#else
void *QueueFindAndRemove(q, element, f)
Queue q;
void *element;
ComparisonFunction f;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    if ((*f)(element,item->element) == 0) break;
  }
  item = QueueRemoveItem(q,item);
  return (item != NULL ? item->element : NULL);
}
/*
**
** QueueFindAndRemoveType
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueFindAndRemoveType(Queue q, void *element,
                             ComparisonFunction f, int type)
#else
void *QueueFindAndRemoveType(q, element, f, type)
Queue q;
void *element;
ComparisonFunction f;
int type;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    if ((*f)(element,item->element) == 0 && item->type == type) break;
  }
  item = QueueRemoveItem(q,item);
  return (item != NULL ? item->element : NULL);
}

/*
**
** QueueFindType
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueFindType(Queue q, int type)
#else
void *QueueFindType(q,type)
Queue q;
int type;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    if (item->type == type) break;
  }
  return (item != NULL ? item->element : NULL);
}

/*
**
** QueueFindTypeAndRemove
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueFindTypeAndRemove(Queue q, int type)
#else
void *QueueFindTypeAndRemove(q,type)
Queue q;
int type;
#endif
{
  QueueItem item;

  for (item = q->head; item != NULL; item = item->next) {
    if (item->type == type) break;
  }
  return (QueueRemoveItem(q,item));
}

/*
**
** QueueGet
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueGet(Queue q)
#else
void *QueueGet(q)
Queue q;
#endif
{
  void *element;
  QueueItem item;

  item = QueueGetItem(q);
  if (item) {
    element = item->element;
    QueueDisposeItem(item,NULL);
  }
  else element = NULL;
  return (element);
}

/*
**
** QueueGetItem
**
*/
PRIVATE
#ifdef __ANSI_C__
QueueItem QueueGetItem(Queue q)
#else
QueueItem QueueGetItem(q)
Queue q;
#endif
{
  QueueItem item;

  if (q->head) {
    item = q->head;
    q->head = q->head->next;
    q->size--;
  }
  else item = NULL;
  return (item);
}

/*
**
** QueueHead
**
*/
PUBLIC
#ifdef __ANSI_C__
QueueItem QueueHead(Queue q)
#else
QueueItem QueueHead(q)
Queue q;
#endif
{
  return q != NULL ? q->head : NULL;
}

/*
**
** QueueItemElement
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueItemElement(QueueItem item)
#else
void *QueueItemElement(item)
QueueItem item;
#endif
{
  return (item->element);
}

/*
**
** QueueItemType
**
*/
PUBLIC
#ifdef __ANSI_C__
int QueueItemType(QueueItem item)
#else
int QueueItemType(item)
QueueItem item;
#endif
{
  return (item->type);
}

/*
**
** QueueLook
**
*/
PUBLIC
#ifdef __ANSI_C__
void *QueueLook(Queue q)
#else
void *QueueLook(q)
Queue q;
#endif
{
  QueueItem item;

  item = q->head;
  return (item != NULL ? item->element : NULL);
}

/*
**
** QueueNew
**
*/
PUBLIC
#ifdef __ANSI_C__
Queue QueueNew(void)
#else
Queue QueueNew()
#endif
{
  Queue q;

  q = ((Queue)Allocate(sizeof(_Queue)));
  if (q) {
    q->head = q->tail = NULL;
    q->size = 0;
  }
  return (q);
}

/*
**
** QueueNewItem
**
*/
PRIVATE
#ifdef __ANSI_C__
QueueItem QueueNewItem(void *element, int type)
#else
QueueItem QueueNewItem(element,type)
void *element;
int type;
#endif
{
  QueueItem item;

  item = ((QueueItem)Allocate(sizeof(_QueueItem)));
  if (item) {
    item->element = element;
    item->type = type;
    item->next = NULL;
  }
  return (item);
}

/*
**
** QueueNext
**
*/
PUBLIC
#ifdef __ANSI_C__
QueueItem QueueNext(QueueItem item)
#else
QueueItem QueueNext(item)
QueueItem item;
#endif
{
  return item != NULL ? item->next : NULL;
}

/*
**
** QueuePut
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueuePut(Queue q, void *element, int type)
#else
void QueuePut(q,element,type)
Queue q;
void *element;
int type;
#endif
{
  QueueItem item;

  item = QueueNewItem(element,type);
  QueuePutItem(q,item);
}

/*
**
** QueuePutItem
**
*/
PRIVATE
#ifdef __ANSI_C__
void QueuePutItem(Queue q, QueueItem item)
#else
void QueuePutItem(q,item)
Queue q;
QueueItem item;
#endif
{
  if (q->head == NULL) {
    q->head = q->tail = item;
  } else {
    q->tail->next = item;
    q->tail = q->tail->next;
  }
  q->size++;
}

/*
**
** QueuePutOnPriority
**
*/
PUBLIC
#ifdef __ANSI_C__
void QueuePutOnPriority(Queue q, void *element, int type,
                        ComparisonFunction f)
#else
void QueuePutOnPriority(q,element,type,f)
Queue q;
void *element;
int type;
ComparisonFunction f;
#endif
{
  QueueItem item;
  item = QueueNewItem(element,type);
  //fprintf(stderr,"searching for location using %p.\n",f);
  if (f == NULL){
    //fprintf(stderr,"comparing function is null %f \n",f);
    exit(1);
    QueuePutItem(q,item);

  }else {
    if (q->head == NULL) QueuePutItem(q,item);
    else {
      QueueItem p, lastp = NULL;
      for (p = q->head; p != NULL; p = p->next) {
	int cval =(*f)(element,p->element);
        if (cval<0){
	  // 	  fprintf(stderr,"%i <=> %i == %i\n",*((int *) element),*((int *)(p->element)),
	  // 		  cval);
	  break;
	}
        lastp = p;
      }
      if (p == q->head) {
        item->next = q->head;
        q->head = item;
      } else if (p == NULL) {
        q->tail->next = item;
        q->tail = q->tail->next;
      } else {
        item->next = p;
        lastp->next = item;
      }
      q->size++;
    }
  }
}

/*
**
** QueueRemoveItem
**
*/
PRIVATE
#ifdef __ANSI_C__
QueueItem QueueRemoveItem(Queue q, QueueItem item)
#else
QueueItem QueueRemoveItem(q,item)
Queue q;
QueueItem item;
#endif
{
  QueueItem this, prev = NULL;

  if (q == NULL) return (NULL);
  if (item == NULL) return (NULL);
  this = q->head;
  while (this && this != item) {
    prev = this; this = this->next;
  }
  if (this == NULL) return (NULL);
  if (this == q->head) q->head = item->next;
  if (this == q->tail) q->tail = prev;
  if (prev) prev->next = this->next;
  q->size--;
  return (item);
}

/*
**
** QueueSeek
**
*/
PUBLIC
#ifdef __ANSI_C__
QueueItem QueueSeek(Queue q, int position)
#else
QueueItem QueueSeek(q,position)
Queue q;
int position;
#endif
{
  QueueItem item;
#if 0
  /* Allow single-level negative addressing of queue items */
  if (position <= 0)
    position += QueueSize(q);
#endif
  /* Seeks which fail will result in NULL, not error conditions */
  if (position <= 0 || position > QueueSize(q))
    return NULL;
  item = QueueHead(q);
  while (--position > 0)
    item = QueueNext(item);
  return item;
}

/*
**
** QueueSize
**
*/
PUBLIC
#ifdef __ANSI_C__
int QueueSize(Queue q)
#else
int QueueSize(q)
Queue q;
#endif
{
  return q != NULL ? q->size : 0;
}

/*
**
** QueueTail
**
*/
PUBLIC
#ifdef __ANSI_C__
QueueItem QueueTail(Queue q)
#else
QueueItem QueueTail(q)
Queue q;
#endif
{
  return q != NULL ? q->tail : NULL;
}

#ifdef UNUSED

PRIVATE
#ifdef __ANSI_C__
int QueueCompareEqual(void *x, void *y)
#else
int QueueCompareEqual(x,y)
void *x, *y;
#endif
{
  return 0;
}

#endif

