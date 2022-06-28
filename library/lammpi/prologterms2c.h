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


Last rev: $Id: prologterms2c.h,v 1.2 2006-06-04 19:02:07 nunofonseca Exp $
Comments: This file provides a set of functions to convert a prolog term to a C string and back.
*/
#ifndef PROLOGTERMS2C_H
#define PROLOGTERMS2C_H 1
#include "Yap.h"
#ifndef _yap_c_interface_h
#include <YapInterface.h>
//#include <yap_structs.h>
#endif

#ifndef size_t
#include <unistd.h>
#endif
#include <stdarg.h>
/*
 * Converts a term t into a string.  
 * An internal representation of t is
 * copied to ptr if it occupies less than size. Otherwise the
 * necessary memory is aloccated (dyn_ptr) and the ascii
 * representation of the term is copied to there.
 */
char* term2string(const YAP_Term t);
/*
 * Converts a string with a ascci representation of a term into a term.
 * The ascii representation of t is
 * copied to ptr if it occupies less than *size. Otherwise the
 * necessary memory is aloccated  and the ascii
 * representation of the term is copied to there.
 */
YAP_Term string2term(char *const ptr,const size_t *size);
/*
 * Read a prolog term from a stream
 * (the prolog term must have been writen by the write_term_to_stream)
 */
YAP_Term read_term_from_stream(const int fd);
/*
 * Writes a term to a stream.
 */
size_t write_term_to_stream(const int fd,const YAP_Term term);
/*
 * Changes the size of the buffer to contain at least newsize bytes.
 * Useful to reduce the number of reallocs,mallocs, and frees
 */
void change_buffer_size(const size_t newsize);

void write_msg(const char *fun,const char *file, int line,const char *format, ...);
/*********************************************************************************************
 * Macros to manipulate the buffer
 *********************************************************************************************/

#define BLOCK_SIZE 4096

#if THREADS
#define buffer (buffer[YAP_ThreadSelf()])
#endif

// deletes the buffer (all fields) but does not release the memory of the buffer.ptr
#define DEL_BUFFER()   
//  informs the prologterm2c module that the buffer is now used and should not be messed
#define USED_BUFFER()  DEL_BUFFER()
// initialize buffer
#define RESET_BUFFER() \
  {if (buffer.ptr) free(buffer.ptr); buffer.ptr = NULL; buffer.len = buffer.size = 0; buffer.pos=0;}
#define BUFFER_PTR   buffer.ptr
#define BUFFER_SIZE  buffer.size
#define BUFFER_LEN   buffer.len
#define BUFFER_POS   buffer.pos
// copies two buffers
#define COPY_BUFFER_DS(src,dst) {dst.size=src.size;dst.len=src.len;dst.ptr=src.ptr;dst.pos=src.pos;}


#define buffer LOCAL_mpi_buffer

#endif
