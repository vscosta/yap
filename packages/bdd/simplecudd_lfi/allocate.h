/******************************************************************
**
** ALLOCATE.H:
**
**    Allocation Routines
**
** This file is part of Apt Computing Tools (ACT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
******************************************************************/

#ifndef ALLOCATE_H
#define ALLOCATE_H

/* ----------- Headers */

#include "apt.h"

/* ----------- Exported Function Prototypes */

#ifdef __ANSI_C__
void *Allocate(int);
void Free(void*);
#else
void *Allocate();
void Free();
#endif /* __ANSI_C__ */

#endif /* ALLOCATE_H */
