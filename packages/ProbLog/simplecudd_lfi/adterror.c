/******************************************************************
**
** ADTERROR.C:
**
**    ADT Error Handler
**
** This file is part of Apt Abstrct Data Types (ADT)
** Copyright (c) 1991 -- Apt Technologies
** All rights reserved
******************************************************************/

#include "adterror.h"
//#include "gprintf.h"

//PUBLIC
//#ifdef __ANSI_C__
void ADTError(char *name, ADTErrorCodes error, char *proc)
//#else
//void ADTError(name,error,proc)
  //char *name;
  //ADTErrorCodes error;
  //char *proc;
//#endif
{
  fprintf(stderr,"ADT Error: ");
  fprintf(stderr," Module: %s\n",name);
  fprintf(stderr," Procedure: %s\n",proc);
  fprintf(stderr," Error Code: %u\n",error);
  exit(1);
}

//PUBLIC
//#ifdef __ANSI_C__
void ADTWarning(char *name, ADTWarningCodes warning, char *proc)
//#else
//void ADTWarning(name,warning,proc)
  //char *name;
  //ADTWarningCodes warning;
  //char *proc;
  //#endif
{

  fprintf(stderr,"ADT Warning: ");
  fprintf(stderr," Module: %s\n",name);
  fprintf(stderr," Procedure: %s\n",proc);
  fprintf(stderr," Warning Code: %u\n",warning);
}
