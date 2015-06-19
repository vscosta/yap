/******************************************************************
**
** ALLOCATE.C:
**
**    Allocation Routines
**
** This file is part of Apt Computing Tools (ACT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
******************************************************************/

/* ---------- C Headers */

//#include "cheaders.h"

/* ---------- Headers */

#include "apt.h"
#include "allocate.h"

/* ---------- Private Globals */

PRIVATE int bytesAllocated = 0;

/* ---------- Functions */

/*
**
** Allocate
**
*/
PUBLIC
#ifdef __ANSI_C__
void *Allocate(int bytes)
#else
void *Allocate(bytes)
int bytes;
#endif
{
  bytesAllocated += bytes;
  return (void *)calloc(1,bytes);
}

/*
**
** Free
**
*/
PUBLIC
#ifdef __ANSI_C__
void Free(void *memory)
#else
void Free(memory)
void *memory;
#endif
{
  free(memory);
}
