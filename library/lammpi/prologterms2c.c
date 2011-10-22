/*
Copyright (C) 2004,2005,2006 (Nuno A. Fonseca) <nuno.fonseca@gmail.com>

This program is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License 
as published by the Free Software Foundation; either 
version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


Last rev: $Id: prologterms2c.c,v 1.4 2006-09-28 11:42:51 vsc Exp $
Comments: This file provides a set of functions to convert a prolog term to a C string and back.
*/
#include "config.h"
#include "prologterms2c.h"
#include <stdio.h>
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef COMPRESS
#include "minilzo.h"
#endif

#ifndef Quote_illegal_f
#define Quote_illegal_f         1
#define Ignore_ops_f            2
#define Handle_vars_f           4
#define Use_portray_f           8
#define To_heap_f              16
#endif


#ifdef COMPRESS

#endif

struct buffer_ds buffer; 

/*********************************************************************************************/
// prototypes
void write_msg(const char *fun,const char *file, int line,const char *format, ...);

size_t write_term_to_stream(const int fd,const YAP_Term term);
YAP_Term read_term_from_stream(const int fd);
/*********************************************************************************************/
/*
 * Writes a debug message containing the processid, function name, filename, line, and a user message
 */
void 
write_msg(const char *fun,const char *file, int line,
            const char *format, ...) {
  va_list ap;
 
  va_start(ap, format);
  /* Print the message to stderr */
  fprintf(stderr,
          "[%d:%s in %s at line %d] ", getpid(),fun, file, line);
  vfprintf(stderr, format, ap);
  va_end(ap);
}
/*********************************************************************************************
 * Memory handling functions
 *********************************************************************************************/
/*
 * Adds 'space' to the size of the currently allocated buffer
 */
static void
expand_buffer(const size_t space ) {
  char *oldblock;
  
  // BUFFER_PTR = realloc( BUFFER_PTR, BUFFER_SIZE + space );
  oldblock= BUFFER_PTR;
  BUFFER_PTR = (char*)malloc( BUFFER_SIZE + space );
  if( BUFFER_PTR == NULL ) {
    YAP_Error(0,0,"Prolog2Term: Out of memory.\n");
#ifdef MPI
    MPI_Finalize();
#endif
    YAP_Exit( 1 );
  }
  memcpy(BUFFER_PTR,oldblock,BUFFER_SIZE);

  if(oldblock!=NULL)
    free(oldblock);
	   
  BUFFER_SIZE+=space;
}
/*
 * Changes the size of the buffer to contain at least newsize bytes 
 */
void
change_buffer_size(const size_t newsize) {

  if ( BUFFER_SIZE>=BLOCK_SIZE && BUFFER_SIZE>newsize)
    return;
  if(BUFFER_PTR!=NULL)
    free(BUFFER_PTR);
  BUFFER_PTR = (char*)malloc(newsize);
  if( BUFFER_PTR == NULL ) {
    YAP_Error(0,0,"Prolog2Term: Out of memory.\n");
#ifdef MPI
    MPI_Finalize();
#endif
    YAP_Exit( 1 );
  }
  BUFFER_SIZE=newsize;
}
/*********************************************************************************************
 * I/O functions
 *********************************************************************************************/
/*
 * Function used by YAP to write a char to a string
 */
static void
p2c_putc(const int c) {
  //  if( buffer.size==buffer.len+1 ) 
  if( BUFFER_SIZE==BUFFER_LEN ) { 
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"p2c_putc:buffer expanded: size=%u pos=%u len=%u\n",BUFFER_SIZE,BUFFER_POS,BUFFER_LEN);  
#endif
     expand_buffer( BLOCK_SIZE );    
  }
  BUFFER_PTR[BUFFER_LEN++] = c;   
}
/*
 * Function used by YAP to read a char from a string
 */                                                                                               
/*
 * Writes a term to a stream.
 */
size_t 
write_term_to_stream(const int fd,const YAP_Term term) {

  RESET_BUFFER;
  
  YAP_Write( term, p2c_putc, 3);               // 3=canonical
  if (write(fd,(void*)&BUFFER_LEN,sizeof(size_t)) < 0) {// write size of term
    YAP_Error(0,0,"Prolog2Term: IO error in write term size.\n");
    return -1;
  }
  if (write(fd,(void*)BUFFER_PTR,BUFFER_LEN) < 0) {     // write term
    YAP_Error(0,0,"Prolog2Term: IO error in write.\n");
    return -1;
  }
  return BUFFER_LEN;
}
/*
 * Read a prolog term from a stream
 * (the prolog term must have been writen by the write_term_to_stream)
 */
YAP_Term 
read_term_from_stream(const int fd) {
  size_t size; 

  RESET_BUFFER;    
  read(fd,(void*)&size,sizeof(size_t)); // read the size of the term
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"read_term_from_stream>>>>size:%d\n",size);  
#endif
  if ( size> BUFFER_SIZE)
    expand_buffer(size-BUFFER_SIZE);
  if (!read(fd,BUFFER_PTR,size)) {
    YAP_Error(0,0,"Prolog2Term: IO error in read.\n");
  };            // read term from stream
  return YAP_ReadBuffer( BUFFER_PTR , NULL);
}
/*********************************************************************************************
 * Conversion: Prolog Term->char[] and char[]->Prolog Term
 *********************************************************************************************/
/*
 * Converts a term t into a string.  
 * The ascii representation of t is
 * copied to ptr if it occupies less than size. 
 */
char* 
term2string(char *const ptr,size_t *size, const YAP_Term t) {
  char *ret;

  RESET_BUFFER;

  YAP_Write( t, p2c_putc, 3 );// canonical
  p2c_putc('\0');             //add terminator

  if (BUFFER_LEN<=*size) {    // user allocated buffer size is ok
    memcpy(ptr,BUFFER_PTR,BUFFER_LEN); // copy data to user block
    ret=ptr;
    *size=BUFFER_LEN;
  } else {                    // user buffer is too small
    ret=BUFFER_PTR;
    *size=BUFFER_LEN; 
    //DEL_BUFFER;
  }
  return ret;
}
/*
 * Converts a string with a ascci representation of a term into a Prolog term.
 */
YAP_Term 
string2term(char *const ptr,const size_t *size) {
  YAP_Term t;
  struct buffer_ds b;

  b.size=b.len=b.pos=0;
  if (BUFFER_PTR!=ptr) {    //
#ifdef DEBUG
    write_msg(__FUNCTION__,__FILE__,__LINE__,"copy buffer string2term\n");
#endif
    COPY_BUFFER_DS(buffer,b); // keep a copy of buffer_ds
    BUFFER_PTR=ptr;           // make the buffer use the buffer provided
    BUFFER_LEN=*size;
    BUFFER_SIZE=BUFFER_LEN;
  } else {   // b aux. struct. not needed
    b.ptr=NULL;
  }
  BUFFER_POS=0;
  t = YAP_ReadBuffer( BUFFER_PTR , NULL );
  if ( t==FALSE ) {
    write_msg(__FUNCTION__,__FILE__,__LINE__,"FAILED string2term>>>>size:%d %d %s\n",BUFFER_SIZE,strlen(BUFFER_PTR),NULL);
    exit(1);
  }

  if (b.ptr!=NULL)
    COPY_BUFFER_DS(b,buffer);
  
#ifdef DEBUG
  write_msg(__FUNCTION__,__FILE__,__LINE__,"ending: buffer(ptr=%p;size=%d;pos=%d;len=%d)\n",BUFFER_PTR,BUFFER_SIZE,BUFFER_POS,BUFFER_LEN);
#endif

  return t;
}                                                                                   
