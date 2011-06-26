/******************************************************************
**
** APT.H:
**
**    Definitions and Types for all APT modules
**
** This file is part of Apt Computing Tools (ACT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
**
******************************************************************/

#ifndef APT_H
#define APT_H

/* ---------- Defines */

#ifndef ERROR
#define ERROR -1
#endif

#ifndef EXTERN
#define EXTERN extern
#endif

#ifndef FAILURE
#define FAILURE -1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef INFINITY
#define INFINITY 32768L
#endif

#ifndef PRIVATE
#define PRIVATE static
#endif

#ifndef PUBLIC
#define PUBLIC
#endif

#ifndef SUCCESS
#define SUCCESS 1
#endif

#ifndef TRUE
#define TRUE 1
#endif

/* ---------- Types */

#define __ANSI_C__

typedef void (*ApplyFunction)(void*);
typedef void (*ApplyFunction1)(void*,void*);
typedef void (*ApplyFunction2)(void*,void*,void*);
typedef void (*ApplyFunction3)(void*,void*,void*,void*);
typedef int (*ComparisonFunction)(void*, void*);
typedef void (*DisposeFunction)(void*);

typedef void (*ApplyFunctionGeneric)(void* target, void* args[]);

#endif /* APT_H */
